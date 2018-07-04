#' Create New Record Form
#'
#' Creates the form that allows the addition of a new record to the table,
#' based on the context settings.
#' 
#' @param .context List. The shinyXT context.
#' 
#' @return A list of Shiny views.
#' 
#' @family form functions
#' 
#' @export
createForm <- function(.context) {
    
    # extract from .context
    xt <- getConfigMode(.context)
    .options <- xt[[".options"]]
    
    row_df <- qval(.options$xt_row, .context$row_df)
    if (NROW(row_df) == 0L || NCOL(row_df) == 0L) return(NULL)
    checkDataConfig(row_df, xt)
    
    # POSIXct to str, leave Dates alone (in order to use dateInput)
    format_POSIXct <- qval(.options[["format_POSIXct"]], format_utc)
    row_df <- row_df %>%
        dplyr::mutate_if(is_POSIXct, format_POSIXct)
    
    # visible ui elements in the order given by xt
    xt <- xt %>%
        discard_at(c(".default", ".options")) %>%
        purrr::keep(~ .$visible) %>%
        purrr::keep(~ isNotEmptyChr(.$widget))
    
    id_prefix <- .options$id_prefix
    row_lst <- list()
    row_idx <- 1L
    for (xtc in xt) {
        # extract widget call
        if (is.list(xtc$widget)) {
            what <- xtc$widget[[1L]]
            argm <- xtc$widget[-1L] # keep any other arg as list
        } else {
            what <- xtc$widget
            argm <- list()
        }
        
        # special widgets first: br and hr
        if (what == "br") {
            ctrl <- NULL
            row_idx <- row_idx + 1L
        } else if (what == "hr") {
            ctrl <- shiny::hr()
            row_idx <- row_idx + 1L
        } else {
            # all ui controls start with a prefix
            col_name <- xtc$col_name
            inputId <- paste0(id_prefix, col_name)
            value <- row_df[[col_name]]
            
            # eval / add to argm
            argm[["inputId"]] <- qval(argm[["inputId"]], inputId)
            argm[["label"]] <- qval(argm[["label"]], xtc$ui_name)
            argm[["width"]] <- qval(argm[["width"]], xtc$width)
            
            # extra armg, control specific
            if (what %in% c("textInput", "dateInput", "numericInput")) {
                argm[["value"]] <- qval(argm[["value"]], value)
            } else if (what %in% c("selectInput", "selectizeInput")) {
                argm[["choices"]] <- qval(argm[["choices"]], value)
                argm[["selected"]] <- qval(argm[["selected"]], value)
            }
            
            # create control
            ctrl1 <- do.call(what, argm)
            
            # display tooltip only if provides additional info
            if (isNotEmptyChr(xtc$hover) && (xtc$hover != xtc$ui_name)) {
                ctrl2 <- bsplus::bs_embed_tooltip(
                    tag = ctrl1, title = xtc$hover, placement = "left")
            } else {
                ctrl2 <- ctrl1
            }
            
            # disable (view only) where needed
            if (xtc$enabled) {
                ctrl3 <- ctrl2
            } else {
                ctrl3 <- shinyjs::disabled(ctrl2)
            }
            
            # put control inside a column to make it look pretty
            if (xtc$column_width > 0) {
                ctrl <- shiny::column(width = xtc$column_width, ctrl3)
            } else {
                ctrl <- ctrl3
            }
        }
        
        # add to list of controls
        if (!is.null(ctrl)) {
            pos_lst <- if (length(row_lst) == row_idx) {
                row_lst[[row_idx]]
            } else {
                list()
            }
            row_lst[[row_idx]] <- c(pos_lst, list(ctrl))
        }
    }
    
    for (i in seq_along(row_lst)) {
        # remove NULL controls (no widget, hover, column)
        pos_lst <-
            row_lst[[i]] %>%
            purrr::discard(~ is.null(.))
        row_lst[[i]] <- do.call(shiny::fluidRow, pos_lst)
    }
    
    # hack: need to coll bsplus::use_bs_tooltip() for each custom ui
    row_lst <- c(row_lst, list(bsplus::use_bs_tooltip()))
    ui <- shiny::tagList(row_lst)
    
    ui
}


#' Validate Form Based on a List of Input Views
#' 
#' Called when the user inserts values in the form and the input requires
#'   validation, based on the rules specified in the shinyXT context.
#' 
#' @param input_lst List of views to validate.
#' @param .context List. The shinyXT context.
#' 
#' @return A list
#' @family form functions
#'
#' @examples
#' shiny::observeEvent({
#'     input$my_input
#' }, {
#'     shiny::reactiveValuesToList(input) # get all the inputs on the page
#'     vld_lst <- validateForm(input_lst, .context)
#' })
#' 
#' @export
validateForm <- function(input_lst, .context) {
    
    # extract from context
    xt <- getConfigMode(.context)
    .options <- xt[[".options"]]
    
    # keep only inputs that have the right prefix; remove prefix
    id_prefix <- .options$id_prefix
    pattern <- paste0("^", id_prefix)
    input_lst <- purrr::keep(input_lst, grepl(pattern, names(input_lst)))
    names(input_lst) <- gsub(pattern, "", names(input_lst))
    
    # get an empty row, which has the right R classes
    empty_row_df <- getNARow(.context)
    row_df <- empty_row_df
    msg <- NULL
    tryCatch({
        # call pre_validate (modifies raw shiny inputs, ads .msg filed if error)
        input_lst <- qval(.options$pre_validate, input_lst)
        if (!is.null(input_lst$.msg)) {
            stop(input_lst$.msg)
        }
        
        for (cn in colnames(row_df)) {
            xtc <- xt[[cn]]
            value <- input_lst[[cn]]
            
            # cannot have values of length 0, assume NA / NULL in db
            if (length(value) == 0L) {
                value[1L] <- NA
            }
            
            # non scalars are not allowed, use pre_validate
            if (length(value) != 1L) {
                stop("input has multiple values for: ", xtc$ui_name, ";\n",
                     "use pre_validate to collapse multiple values")
            }
            
            # re-cast to match desired xt classes
            if (class(value)[1L] != xtc$class) {
                value <- switch(
                    xtc$class,
                    integer = as.integer(value),
                    numeric = as.numeric(value),
                    character = as.character(value),
                    Date = as.Date(value, origin = as.Date("1970-01-01")),
                    POSIXct = as.POSIXct(value, tz = "UTC"),
                    stop("do not know how to covert to ", xtc$class)
                )
            }
            
            # empty string char (artifact of selectize) converted to NA
            if (!is.na(value) && is.character(value) && value == "") {
                value <- NA_character_
            }
            
            # column level validation
            v_msg <- qval(xtc$validate)
            if (!is.null(v_msg)) stop(v_msg, " for: ", xtc$ui_name)
            
            row_df[[cn]] <- value
        }
        
        # table level validation
        v_msg <- qval(.options$validate)
        if (!is.null(v_msg)) stop(v_msg)
        
        # finally, call finalize
        row_df <- qval(.options$finalize, row_df)
        
    }, error = function(e) {
        msg <<- e$message
        row_df <<- empty_row_df
    })
    
    list(row_df = row_df, msg = msg)
}
