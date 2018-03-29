#' @export
brXT <- function() {
    list(
        widget = 'br',
        visible = TRUE
    )
}


#' @export
hrXT <- function() {
    list(
        widget = 'hr',
        visible = TRUE
    )
}


#' @export
getFullTbl <- function(.context, tbl_name = .context$tbl_name) {
    
    if (!is.character(tbl_name)) stop('tbl_name should be character')
    tbl <- .context$tbl_lst[[tbl_name]]
    if (is.null(tbl)) stop("cannot find table ", tbl_name)
    
    tbl
}


#' @export
getFilteredTbl <- function(.context, tbl_name = .context$tbl_name) {
    
    tbl <- getFullTbl(.context, tbl_name)
    filter_idx <- .context$filter_lst[[tbl_name]]
    # ok if filter is missing/NULL, do not drop any rows
    if (length(filter_idx) > 0L) {
        tbl <- tbl[filter_idx, , drop = FALSE]
    }
    
    tbl
}


#' @export
getEmptyRow <- function(.context, tbl_name = .context$tbl_name) {
    
    if(!is.character(tbl_name)) stop('tbl_name should be character')
    # returns an one row df filled with NAs, preserving R classes
    .context$tbl_lst[[tbl_name]] %>%
        dplyr::slice(1:2) %>%
        tibble::add_row(.before = 1L) %>%
        dplyr::slice(1L)
}


#' @export
choices <- function(var_col, 
                    .context, 
                    tbl_name = .context$tbl_name,
                    keep_na = c('if_any', 'omit_na', 'insert_na')
) {
    keep_na = match.arg(keep_na)
    if (tbl_name == 'all') tbl_name <- names(.context$tbl_lst)
    
    y <- character()
    for (tn in tbl_name) {
        df <- .context$tbl_lst[[tn]]
        x <- df[[var_col]]
        if (length(x) > 0L) {
            x[x == ''] <- NA_character_
            yy <- sort(unique(x), na.last = FALSE)
            y <- c(y, yy)
        }
    }
    y <- sort(unique(y), na.last = FALSE)
    
    z <- if (is.na(y[1])) {
        if (keep_na == 'omit_na') stats::na.omit(y) else y
    } else {
        if (keep_na == 'insert_na') c(NA_character_, y) else y
    }
    z[is.na(z)] <- ''
    
    unique(z)
}


#' @export
isNotEmptyChr <- function(x) {
    
    # NULL and zero length vectors / lists
    if (length(x) == 0L) return(FALSE)
    # all NAs
    if (length(stats::na.omit(x)) == 0L) return(FALSE)
    # one empty character
    if (is.character(x) && length(x) == 1L && x == '') return(FALSE)
    
    TRUE
}
