#' @export
createForm <- function(.context) {
    
    # extract from context
    tbl <- tc_get_subset(.context)
    tc <- tc_get_mode(.context)
    tc_ui <- tc[['.ui']]
    row_df <- qval(tc_ui$pre_ui, .context$row_df)
    if (NROW(row_df) == 0L || NCOL(row_df) == 0L) return(NULL)
    
    # POSIXct to str, leave Dates alone to use dateInput
    row_df <- row_df %>%
        dplyr::mutate_if(is_POSIXct, format_utc)
    # visible ui elements in the order within tc
    tc <- tc %>%
        discard_at(c('.dt', '.ui')) %>%
        purrr::keep(~ .$visible) %>%
        purrr::keep(~ shiny::isTruthy(.$widget))
    
    row_lst <- list()
    row_idx <- 1L
    for (tcc in tc) {
        # extract widget call
        if (is.list(tcc$widget)) {
            what <- tcc$widget[[1L]]
            argm <- tcc$widget[-1L]  #keep any other args
        } else {
            what <- tcc$widget
            argm <- list()
        }
        if (what == 'br') {
            ctrl <- NULL
            row_idx <- row_idx + 1L
        } else if (what == 'hr') {
            ctrl <- shiny::hr()
            row_idx <- row_idx + 1L
        } else {
            # all ui controls start with prefix `edit_`
            col_name <- tcc$col_name
            if (gsub('[^a-zA-Z0-9_]', '', col_name) != col_name) {
                stop("only [a-zA-Z0-9_] allowed in col names, see ", col_name)
            }
            inputId <- paste0('edit_', col_name)
            value <- row_df[[col_name]]
            
            # eval / add to argm
            argm[['inputId']] <- qval(argm[['inputId']], inputId)
            argm[['label']] <- qval(argm[['label']], tcc$ui_name)
            argm[['width']] <- qval(argm[['width']], '100%')
            # extra armg, control specific
            if (what %in% c('textInput', 'dateInput', 'numericInput')) {
                argm[['value']] <- qval(argm[['value']], value)
            } else if (what %in% c('selectInput', 'selectizeInput')) {
                argm[['choices']] <- qval(argm[['choices']], value)
                argm[['selected']] <- qval(argm[['selected']], value)
            }
            ctrl1 <- do.call(what, argm)
            
            # display tooltip only if provides additional info
            if (shiny::isTruthy(tcc$info) && (tcc$info != tcc$ui_name)) {
                ctrl2 <- bsplus::bs_embed_tooltip(
                    tag = ctrl1, title = tcc$info, placement = 'left')
            } else {
                ctrl2 <- ctrl1
            }
            # disable (view only) where needed
            if (tcc$enabled) {
                ctrl3 <- ctrl2
            } else {
                ctrl3 <- shinyjs::disabled(ctrl2)
            }
            ctrl <- shiny::column(width = tcc$width, ctrl3)
        }
        
        # add to list
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
        pos_lst <- row_lst[[i]] %>%
            purrr::discard(~ is.null(.))
        row_lst[[i]] <- do.call(shiny::fluidRow, pos_lst)
    }
    row_lst <- c(row_lst, list(bsplus::use_bs_tooltip()))
    ui <- shiny::tagList(row_lst)
    
    ui
}


#' @export
validateForm <- function(input_lst, .context) {
    
    # extract from context
    tbl <- tc_get_subset(.context)
    tc <- tc_get_mode(.context)
    tc_ui <- tc[['.ui']]
    
    na_row_df <- tc_na_row_df(.context)
    row_df <- na_row_df
    msg <- NULL
    tryCatch({
        # call pre_validate (modifies raw shiny inputs)
        input_lst <- qval(tc_ui$pre_validate, input_lst)
        if (!is.null(input_lst$.msg)) {
            stop(input_lst$.msg)
        }
        
        for (cn in colnames(row_df)) {
            tcc <- tc[[cn]]
            if (class(row_df[[cn]])[1L] != tcc$class) {
                stop("config mismatch for: ", tcc$ui_name)
            }
            val <- input_lst[[cn]]
            if (length(val) == 0L) {
                val[1L] <- NA
            }
            if (length(val) != 1L) {
                stop("input is not a scalar for: ", tcc$ui_name)
            }
            
            # check class
            if (class(val)[1L] != tcc$class) {
                val <- switch(
                    tcc$class,
                    integer = as.integer(val),
                    numeric = as.numeric(val),
                    character = as.character(val),
                    Date = as.Date(val, origin = as.Date('1970-01-01')),
                    POSIXct = as.POSIXct(val, tz = 'UTC'),
                    stop("do not know how to covert to ", tcc$class)
                )
            }
            if (!is.na(val) && is.character(val) && val == '') {
                val <- NA_character_
            }
            
            # column level validation
            v_msg <- qval(tcc$validate)
            if (!is.null(v_msg)) stop(v_msg, " for: ", tcc$ui_name)
            
            row_df[[cn]] <- val
        }
        
        # table level validation
        v_msg <- qval(tc_ui$validate)
        if (!is.null(v_msg)) stop(v_msg)
        
        # do not add duplicates (edit case)
        if (.context$mode == 'edit') {
            skip_cols <- c('edit_date', 'edit_user')
            old_df <- 
                dplyr::slice(tbl, .context$row_idx) %>%
                dplyr::select(-dplyr::one_of(skip_cols))
            new_df <- 
                row_df %>%
                dplyr::select(-dplyr::one_of(skip_cols))
            if (all(purrr::map2_lgl(old_df, new_df, identical))) {
                stop("No changes detected, database will not be updated.")
            }
        }
        
        # finally, call pre_db
        row_df <- qval(tc_ui$pre_db, row_df)
        
    }, error = function(e) {
        msg <<- e$message
        row_df <<- na_row_df
    })
    
    list(row_df = row_df, msg = msg)
}
