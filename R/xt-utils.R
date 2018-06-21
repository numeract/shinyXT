#' Add an HTML Line Break.
#' 
#' @return A list with line break settings
#' 
#' @export
brXT <- function() {
    list(
        widget = "br",
        visible = TRUE
    )
}


#' Add an HTML Horizonal Row.
#' 
#' @return A list with horizontal row settings
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
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @return The table from the contexts' table list.
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
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @return A filtered table from the contexts' table list and filter list.
#' @seealso \code{\link{getFilteredTbl}}
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


#' Returns an one row data frame filled with NAs
#' 
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' 
#' @return A data frame, preserving column classes
#' 
#' @export
getNARow <- function(.context, tbl_name = .context$tbl_name) {
    
    if (!is.character(tbl_name)) stop("`tbl_name` should be character")
    
    .context$tbl_lst[[tbl_name]] %>%
        dplyr::slice(1:2) %>%
        tibble::add_row(.before = 1L) %>%
        dplyr::slice(1L)
}


#' Creates a "choices" vector for \code{shiny::selectizeInput()} 
#' 
#' @param var_col Name of the column to extract unique values
#' @param .context List. The shinyXT context.
#' @param tbl_name Name of the requested table.
#' @param keep_na How to handle missing values: \code{"if_any"} (default) 
#'   keeps \code{NAs}, \code{"omit_na"} drops \code{NAs}, 
#'   \code{"insert_na"} makes sure \code{NA} is the first value.
#' 
#' @return A character vector
#' 
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
#' @return Logical: TRUE if NOT empty character and FALSE otherwise
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
