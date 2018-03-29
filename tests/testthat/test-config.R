context("test config functions")

# Used in order to test functions with argument .context
context_default <- list(
    tbl_name = 'onetable',
    tbl_lst = list(
        onetable = onetable_df
    ),
    filter_lst = NULL,
    mode = NULL,
    xt_lst = list(
        onetable = onetable_xt
    )
)

test_that("extract_mode(x, mode, XT) works", {
    
    x <- list(dt = 'dt', edit = 'edit', add = 'add')
    expect_equal(extract_mode(x = x, mode = 'edit', XT = .XT), 'edit')
})


test_that("extract_mode(x, mode, XT) works without all x names in .XT$valid_mode", {
    
    x <- list(dt = 'dt', edit = 'edit', add = 'add', additional = 'additional')
    expect_equal(extract_mode(x = x, mode = 'edit', XT = .XT), x)
})


test_that("extract_mode(x, mode, XT) works with non-list input", {
    
    x <- c('dt', 'edit', 'add')
    expect_equal(extract_mode(x = x, mode = 'edit', XT = .XT), x)
})


test_that("add_col_default(col_lst, default_lst) works", {
    
    col_lst <- list()
    default_lst <- list(col1 = c(1, 2, 3), col2 = c(4, 5, 6))
    result <- add_col_default(col_lst, default_lst)
    
    expect_equal(names(result), names(default_lst))
})


test_that("add_col_default(col_lst, default_lst) works for non-empty col_lst", {
    
    col_lst <- list(col1 = 2)
    default_lst <- list(col1 = c(1, 2, 3), col2 = c(4, 5, 6))
    result <- add_col_default(col_lst, default_lst)
    
    expect_equal(result$col1, 2)
})


test_that("add_col_default(col_lst, default_lst) stops when empty defalut_lst", {
    
    col_lst <- list(col1 = 2)
    default_lst <- list()
    
    expect_error(add_col_default(col_lst, default_lst))
})


test_that("add_col_default(col_lst, default_lst) stops when unnamed defalut_lst", {
    
    col_lst <- list(col1 = 2)
    default_lst <- list(1, 2, 3)
    
    expect_error(add_col_default(col_lst, default_lst))
})
