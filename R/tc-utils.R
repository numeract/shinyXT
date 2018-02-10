#' @export
tc_get_subset <- function(.context, tbl_name = .context$tbl_name) {
    
    tbl <- .context$tbl_lst[[tbl_name]]
    filter_idx <- .context$filter_lst[[tbl_name]]
    if (length(filter_idx) > 0L) {
        tbl <- tbl[filter_idx, , drop = FALSE]
    }
    
    tbl
}


#' @export
tc_get_mode <- function(.context) {
    
    extract_mode <- function(x, tc_mode) {
        if (is.list(x) && any(.TC$modes %in% names(x))) {
            x[[tc_mode]]
        } else {
            x
        }
    }
    
    tc <- .context$tc_lst[[.context$tbl_name]] %>%
        purrr::map(~ purrr::map(., extract_mode, tc_mode = .context$mode))
    
    tc
}
