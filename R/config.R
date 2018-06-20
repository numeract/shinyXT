extract_mode <- function(x, mode, XT) {
    # do not export, separate function to allow testing
    
    if (is.list(x) && all(names(x) %in% XT$valid_mode)) {
        x[[mode]]
    } else {
        x
    }
}


add_col_default <- function(col_lst, default_lst) {
    
    # do not export, separate function to allow testing
    if (!is.list(col_lst)) stop("col_lst must be a list")
    if (!is.list(default_lst)) stop("default_lst must be a list")
    
    # col_lst may be empty
    if (length(default_lst) == 0L)
        stop("default_lst must not be an empty list")
    
    nms <- names(default_lst)
    if (is.null(nms) || ("" %in% nms))
        stop("default_lst must contain only named elements")
    
    for (nm in nms) {
        col_lst[[nm]] <- col_lst[[nm]] %||% default_lst[[nm]]
    }
    
    col_lst
}


#' Extracts Configuration Mode from Context
#' 
#' Extracts from second level in xt_lst,
#' if it contains only valid modes.
#' If any level 2 (only!) field (e.g. validate) is a list of mode fields,
#' \code{getConfigMode()} will be used to extract only that mode.
#  Valid modes: dt, edit, add.
#' 
#' @param .context List. The shinyXT context
#' @param mode The name of the config mode belonging to the context
#' 
#' @return A list representing the settings of a specific context mode
#' 
#' @family config functions
#' 
#' @examples getConfigMode(.context = context, mode = .context$mode)
#' 
#' @export
getConfigMode <- function(.context, mode = .context$mode) {
    
    xt <- .context$xt_lst[[.context$tbl_name]]
    xt <- xt %>%
        purrr::map(
            ~ purrr::map(., extract_mode, mode = mode, XT = .XT))
    # populate values from .default
    non_dot_idx <- which(!grepl("^\\.", names(xt)))
    
    xt <- xt %>%
        purrr::map_at(
            non_dot_idx, add_col_default, default_lst = xt$.default)
    
    xt
}

#' Check XT Configuration Columns
#' 
#' Checks whether an XT config has
#' enough column info for a data table (data frame)
#' 
#' @param tbl Data table (data frame) containing the actual data the shiny app
#' works with
#' @param xt A list representing the settings of a specific context mode
#' @param if_error  Function that accepts a message
#' e.g. cat, message, warning, stop
#' @param ... Arguments in ... will be passed to if_error()
#' 
#' @return Logical. TRUE if XT config contains the nacessary info for tbl
#' 
#' @family config functions
#' 
#' @export
checkDataConfig <- function(tbl, xt, if_error = stop, ...) {
    
    col_names <- colnames(tbl)
    xt_names <- names(xt)
    
    if (!all(col_names %in% xt_names)) {
        # nice error message
        idx <- which(!(col_names %in% xt_names))
        if (length(idx) > 0L) {
            msg <- paste(col_names[idx], collapse = ", ")
            msg <- paste("No XT Config field for:", msg)
        }
        if_error(msg, ...)
        # if we do not stop, return FALSE
        FALSE
    } else {
        TRUE
    }
}

#' Gets the Confinguration Field
#' 
#' Returns values from an xt field in the order of the \code{tbl} columns
#' 
#' @param tbl Data table (data frame) containing the actual data the shiny app
#'  works with
#' @param xt A list representing the settings of a specific context mode
#' 
#' @param field_name A character representing the config field
#' 
#' @return A character representing the config field
#' 
#' @family config functions
#' 
#' @export
getConfigField <- function(tbl, xt, field_name) {
    
    col_names <- colnames(tbl)
    # TODO: drop/ignore dot fields in xt
    unname(unlist(purrr::map(xt[col_names], field_name)))
}
