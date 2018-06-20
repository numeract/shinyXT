#' Add JS scripts
#' 
#' Adds JS scripts to be used with Data Table calls.
#' 
#' @return A string representing a JS script
#' 
#' @export
jsDT <- function(script_name = c("4col")) {
    
    script_name <- match.arg(script_name)
    
    js <- if (script_name == "4col") {
        .XT$jsdt_4col
    }
    
    # min js by removing extra spaces?
    js
}

#' Format a Data Table.
#' 
#' Formats the DT based on the options specified in the configuration.
#' 
#' @param dt Data table.
#' @param xt Config mode from context.
#' 
#' @seealso \code{\link{createDT}}
#'  
#'  @return  the newly formatted DataTable
#' @examples
#' xt <- getConfigMode(.context)
#' dt <- DT::datatable(
#'     df,
#'     options = options_lst,
#'     selection = .options$selection %||% 'single',
#'     rownames = FALSE,
#'     extensions = extensions,
#'     colnames = getConfigField(df, xt, 'ui_name'),
#'     filter = .options$filter %||% 'none',
#'     escape = -skip_escape_idx
#'  ) %>%
#'     formatDT(xt0)
#' 
#' @export
formatDT <- function(dt, xt) {
    
    # extract format functions
    .options <- xt[[".options"]]
    format_Date <- qval(.options[["format_Date"]], format_utc)
    format_POSIXct <- qval(.options[["format_POSIXct"]], format_utc)
    
    # assumed called after checkDataConfig(), usually from createDT(),
    col_names <- colnames(dt$x$data)
    xt <- xt[col_names]
    
    # .? (rounding)
    for (i in 0:6) {
        round_cols <- names(purrr::keep(xt, ~ .$format == paste0(".", i)))
        if (length(round_cols) > 0L) {
            dt <- DT::formatRound(dt, round_cols, digits = i)
        }
    }
    
    # %? (percent)
    for (i in 0:6) {
        pct_cols <- names(purrr::keep(xt, ~ .$format == paste0("%", i)))
        if (length(pct_cols) > 0L) {
            dt <- DT::formatPercentage(dt, pct_cols, digits = i)
        }
    }
    
    # Date
    date_cols <- names(purrr::keep(xt, ~ .$format == "date"))
    for (cn in date_cols) {
        dt$x$data[[cn]] <- format_Date(dt$x$data[[cn]])
    }
    
    # Date/POSIXct
    dttm_cols <- names(purrr::keep(xt, ~ .$format == "dttm"))
    for (cn in dttm_cols) {
        dt$x$data[[cn]] <- format_POSIXct(dt$x$data[[cn]])
    }
    
    # url
    url_cols <- names(purrr::keep(xt, ~ .$format == "url"))
    for (cn in url_cols) {
        txt1 <- dt$x$data[[cn]]
        txt2 <- dplyr::if_else(
            grepl("^http[s]?://", txt1), txt1, paste0("http://", txt1))
        txt3 <- paste0("<a href='", txt2, "' target='_blank'>", txt1, "</a>")
        txt3[is.na(txt1)] <- NA_character_
        dt$x$data[[cn]] <- txt3
    }
    
    # email
    email_cols <- names(purrr::keep(xt, ~ .$format == "email"))
    for (cn in email_cols) {
        txt1 <- dt$x$data[[cn]]
        txt2 <- paste0("mailto:", txt1)
        txt3 <- paste0("<a href='", txt2, "' target='_blank'>", txt1, "</a>")
        txt3[is.na(txt1)] <- NA_character_
        dt$x$data[[cn]] <- txt3
    }
    
    # align
    column_defs <- dt$x$options$columnDefs
    column_def <- list()
    # convert R/L/C to className
    align <- unname(purrr::map_chr(xt,  ~ .$align %||% ""))
    l_align <- which(align == "L") - 1L
    if (length(l_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = "dt-left", targets = l_align)))
    }
    c_align <- which(align == "C") - 1L
    if (length(c_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = "dt-center", targets = c_align)))
    }
    r_align <- which(align == "R") - 1L
    if (length(r_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = "dt-right", targets = r_align)))
    }
    # align className take precedence over any previous className
    column_defs <- c(column_def, column_defs)
    dt$x$options$columnDefs <- column_defs
    
    dt
}


#' Create a Data Table.
#' 
#' Creates a DT based on the settings specified in the context.
#' 
#' @return A DataTable  
#' @seealso \code{\link{formatDT}}
#' 
#' @export
createDT <- function(.context) {
    
    # extract from .context
    tbl <- getFilteredTbl(.context)
    xt <- getConfigMode(.context)
    .options <- xt[[".options"]]
    
    # pre_dt callback
    df <- qval(.options$pre_dt, tbl)
    checkDataConfig(df, xt)
    
    col_names <- colnames(df)
    # drop dot fields, use col order
    xt0 <- xt
    xt <- xt[col_names]
    
    # hidden columns, check first .options$visible
    if (!is.null(.options$visible)) {
        visible_msk <- col_names %in% .options$visible
    } else {
        visible_msk <- getConfigField(df, xt, "visible")
    }
    # js col idx starts at 0
    hidden_idx <- which(!visible_msk) - 1L
    column_defs <- list(list(visible = FALSE, targets = hidden_idx))
    
    options_lst <- list(
        lengthChange = TRUE,
        lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
        paging = TRUE,
        pageLength = .options$pageLength %||% 10,
        searching = TRUE,
        scrollX = TRUE,
        dom = "Blfrtip",
        ordering = TRUE,            # enable or disable ordering of columns
        columnDefs = column_defs
    )
    extensions <- character()
    
    if (!is.null(.options$buttons)) {
        options_lst$buttons <- DT::JS(.options$buttons)
        extensions <- c(extensions, "Buttons")
    }
    
    # do not escape url and email columns
    skip_escape_idx <- which(purrr::map_lgl(
        xt, ~ (.$format %||% "") %in% c("url", "email")))
    
    dt <- DT::datatable(
        df,
        options = options_lst,
        selection = .options$selection %||% "single",
        rownames = FALSE,
        extensions = extensions,
        colnames = getConfigField(df, xt, "ui_name"),
        filter = .options$filter %||% "none",
        escape = -skip_escape_idx
    ) %>%
        formatDT(xt0)
    
    dt
}
