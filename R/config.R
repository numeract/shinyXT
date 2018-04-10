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

#' Extracts config mode from context
#' 
#' Extracts from second level in xt_lst, 
#' if it contains only valid modes  
#' 
#' @param .context The shinyXT context
#' @param mode The name of the config mode belonging to the context
#' 
#' @return returns a list representing the extracted context mode
#' 
#' @export
getConfigMode <- function(.context, mode = .context$mode) {
    
    xt <- .context$xt_lst[[.context$tbl_name]]
    xt <- xt %>%
        purrr::map(~ purrr::map(., extract_mode, mode = mode, XT = .XT))
    # populate values from .default
    non_dot_idx <- which(!grepl('^\\.', names(xt)))
    xt <- xt %>%
        purrr::map_at(non_dot_idx, add_col_default, default_lst = xt$.default)
    
    xt
}

#' Check XT config columns
#' 
#' Checks whether an xt Config has
#' enough col info for a tbl (a data frame)
#' @export
checkDataConfig <- function(tbl, xt, if_error = stop, ...) {
    
    # if_error is a function that accepts a message
    # e.g. cat, message, warning, stop
    # arguments in ... will be passed to if_error()
    
    col_names <- colnames(tbl)
    xt_names <- names(xt)
    
    if (!all(col_names %in% xt_names)) {
        # nice error message
        idx <- which(!(col_names %in% xt_names))
        if (length(idx) > 0L) {
            msg <- paste(col_names[idx], collapse = ', ')
            msg <- paste("No XT Config field for:", msg)
        }
        if_error(msg, ...)
        
        # if we do not stop, return FALSE
        FALSE
    } else {
        TRUE
    }
}


#' @export
getConfigField <- function(tbl, xt, field_name) {
    # returns values from an xt filed in the order of the tbl columns
    
    col_names <- colnames(tbl)
    # TODO: drop/ignore dot fields in xt
    unname(unlist(purrr::map(xt[col_names], field_name)))
}
