#' Add a HTML Line Break.
#' 
#' @export
brXT <- function() {
    list(
        widget = "br",
        visible = TRUE
    )
}


#' Add a HTML Horizonal Row.
#' 
#' @export
hrXT <- function() {
    list(
        widget = "hr",
        visible = TRUE
    )
}


#' Get Entire Table
#' 
#' Returns the table from the contexts' table list.
#' 
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @seealso @seealso \code{\link{getFilteredTbl}}
#' 
#' @export
getFullTbl <- function(.context, tbl_name = .context$tbl_name) {
    
    if (!rlang::is_scalar_character(tbl_name)) {
        stop("tbl_name should be character")
    }
    tbl <- .context$tbl_lst[[tbl_name]]
    if (is.null(tbl)) stop("cannot find table ", tbl_name)
    
    tbl
}


#' Get Filtered Table
#' 
#' Returns a filtered table from the contexts' table list and filter list.
#' 
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @seealso @seealso \code{\link{getFilteredTbl}}
#' 
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


#' Get an Empty Row
#' 
#' Returns a one row data frame of NA's.
#' 
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @export
getEmptyRow <- function(.context, tbl_name = .context$tbl_name) {
    
    if (!is.character(tbl_name)) stop("tbl_name should be character")
    # returns an one row df filled with NAs, preserving R classes
    .context$tbl_lst[[tbl_name]] %>%
        dplyr::slice(1:2) %>%
        tibble::add_row(.before = 1L) %>%
        dplyr::slice(1L)
}

# TODO: doc (Mike)

#' @export
choices <- function(var_col,
                    .context,
                    tbl_name = .context$tbl_name,
                    keep_na = c("if_any", "omit_na", "insert_na")
) {
    keep_na <- match.arg(keep_na)
    if (tbl_name == "all") tbl_name <- names(.context$tbl_lst)
    
    y <- character()
    for (tn in tbl_name) {
        df <- .context$tbl_lst[[tn]]
        x <- df[[var_col]]
        if (length(x) > 0L) {
            x[x == ""] <- NA_character_
            yy <- sort(unique(x), na.last = FALSE)
            y <- c(y, yy)
        }
    }
    y <- sort(unique(y), na.last = FALSE)
    
    z <- if (is.na(y[1])) {
        if (keep_na == "omit_na") stats::na.omit(y) else y
    } else {
        if (keep_na == "insert_na") c(NA_character_, y) else y
    }
    z[is.na(z)] <- ""
    
    unique(z)
}


#' Check for Empty Character
#' 
#' Checks if input is NOT empty character.
#' 
#' @param x Input to be checked.
#' 
#' @export
isNotEmptyChr <- function(x) {
    
    # NULL and zero length vectors / lists
    if (length(x) == 0L) return(FALSE)
    # all NAs
    if (length(stats::na.omit(x)) == 0L) return(FALSE)
    # one empty character
    if (is.character(x) && length(x) == 1L && x == "") return(FALSE)
    
    TRUE
}
