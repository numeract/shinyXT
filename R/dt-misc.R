#' @export
checkConfig <- function(df, tc, if_error = stop, ...) {
    
    # if_error is a function that accepts an message
    # e.g. cat, message, warning, stop
    # arguments in ... will be passed to if_error()
    
    col_names <- colnames(df)
    tc_names <- names(tc)
    
    if (!all(col_names %in% tc_names)) {
        idx <- which(!(col_names %in% tc_names))
        if (length(idx) > 0L) {
            msg <- paste(col_names[idx], collapse = ', ')
            msg <- paste("No Table Config field for:", msg)
        }
        if_error(msg, ...)
        
        # if we do not stop, return FALSE
        FALSE
    } else {
        TRUE
    }
}


#' @export
getConfigField <- function(df, tc, field) {
    
    col_names <- colnames(df)
    # return values from tc in the order of the df columns
    unname(unlist(purrr::map(tc[col_names], field)))
}


#' @export
formatDT <- function(dt, tc) {
    
    # assumed called after dtCheck(), usually from dt_create(), 
    col_names <- colnames(dt$x$data)
    tc <- tc[col_names]
    
    # .? (rounding)
    for (i in 0:6) {
        round_cols <- names(purrr::keep(tc, ~ .$format == paste0('.', i)))
        if (length(round_cols) > 0L) {
            dt <- DT::formatRound(dt, round_cols, digits = i)
        }
    }
    
    # %? (percent)
    for (i in 0:6) {
        pct_cols <- names(purrr::keep(tc, ~ .$format == paste0('%', i)))
        if (length(pct_cols) > 0L) {
            dt <- DT::formatPercentage(dt, pct_cols, digits = i)
        }
    }
    
    # date/POSIXct
    dttm_cols <- names(purrr::keep(tc, ~ .$format %in% c('date', 'dttm')))
    for (cn in dttm_cols) {
        dt$x$data[[cn]] <- format_utc(dt$x$data[[cn]])
    }
    
    # url
    url_cols <- names(purrr::keep(tc, ~ .$format == 'url'))
    for (cn in url_cols) {
        txt1 <- dt$x$data[[cn]]
        txt2 <- dplyr::if_else(
            grepl('^http[s]?://', txt1), txt1, paste0('http://', txt1))
        txt3 <- paste0("<a href='", txt2, "' target='_blank'>", txt1, "</a>")
        txt3[is.na(txt1)] <- NA_character_
        dt$x$data[[cn]] <- txt3
    }
    
    # email
    email_cols <- names(purrr::keep(tc, ~ .$format == 'email'))
    for (cn in email_cols) {
        txt1 <- dt$x$data[[cn]]
        txt2 <- paste0('mailto:', txt1)
        txt3 <- paste0("<a href='", txt2, "' target='_blank'>", txt1, "</a>")
        txt3[is.na(txt1)] <- NA_character_
        dt$x$data[[cn]] <- txt3
    }
    
    # align
    column_defs <- dt$x$options$columnDefs
    column_def <- list()
    # convert R/L/C to className
    align <- unname(purrr::map_chr(tc,  ~ .$align %||% ''))
    l_align <- which(align == 'L') - 1L
    if (length(l_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = 'dt-left', targets = l_align)))
    }
    c_align <- which(align == 'C') - 1L
    if (length(c_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = 'dt-center', targets = c_align)))
    }
    r_align <- which(align == 'R') - 1L
    if (length(r_align) > 0L) {
        column_def <- c(column_def, list(
            list(className = 'dt-right', targets = r_align)))
    }
    # align className take precedence over any previous className
    column_defs <- c(column_def, column_defs)
    dt$x$options$columnDefs <- column_defs
    
    dt
}


#' @export
createDT <- function(.context) {
    
    # extract from .context
    tbl <- tc_get_subset(.context)
    tc <- tc_get_mode(.context)
    tc_dt <- tc[['.dt']]
    df <- qval(tc_dt$pre_dt, tbl)
    checkConfig(df, tc)
    
    col_names <- colnames(df)
    # drop .xx fields, use col order
    tc <- tc[col_names]
    
    # hidden columns, check first tc_dt$visible
    if (!is.null(tc_dt$visible)) {
        visible <- col_names %in% tc_dt$visible
    } else {
        visible <- getConfigField(df, tc, 'visible')
    }
    hidden_idx <- which(!visible) - 1L
    column_defs <- list(list(visible = FALSE, targets = hidden_idx))
    
    options_lst <- list(
        lengthChange = TRUE,
        lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
        paging = TRUE,
        pageLength = tc_dt$pageLength %||% 10,
        searching = TRUE,
        scrollX = TRUE,
        dom = 'Blfrtip', 
        ordering = TRUE,            # enable or disable ordering of columns
        columnDefs = column_defs
    )
    extensions <- character()
    
    if (!is.null(tc_dt$buttons)) {
        options_lst$buttons <- DT::JS(tc_dt$buttons)
        extensions <- c(extensions, 'Buttons')
    }
    
    # do not escape url columns
    skip_escape_idx <- which(purrr::map_lgl(tc, ~ (.$format %||% '') == 'url'))
    
    dt <- DT::datatable(
        df,
        options = options_lst,
        selection = tc_dt$selection %||% 'multiple',
        rownames = FALSE,
        extensions = extensions,
        colnames = getConfigField(df, tc, 'ui_name'),
        filter = tc_dt$filter %||% 'none',
        escape = -skip_escape_idx
    ) %>%
        formatDT(tc)
    
    dt
}
