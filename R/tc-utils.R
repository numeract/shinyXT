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


#' @export
tc_na_row_df <- function(.context, tbl_name = .context$tbl_name) {
    
    # start with a df filled with NAs (maintains R classes)
    row_df <- .context$tbl_lst[[tbl_name]] %>%
        dplyr::slice(1:2) %>%
        tibble::add_row(.before = 1L) %>%
        dplyr::slice(1L)
    
    row_df
}
