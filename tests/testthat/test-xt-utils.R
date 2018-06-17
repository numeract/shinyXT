context("test xt-utils")

# Used in order to test functions with argument .context
context_default <- list(
    tbl_name = "onetable",
    tbl_name2 = "onetable2",
    tbl_lst = list(
        onetable = onetable_df
    ),
    filter_lst = NULL,
    mode = NULL,
    xt_lst = list(
        onetable = onetable_xt
    )
)

test_that("getFullTbl() works", {
    expect_equal(getFullTbl(context_default),
                 context_default$tbl_lst[[context_default$tbl_name]])
})


test_that("getFullTbl() stops with wrong tbl_name", {
    
    expect_error(getFullTbl(context_default, tbl_name = "name"))
})


test_that("getFullTbl() stops with numeric tbl_name", {
    
    expect_error(getFullTbl(context_default, tbl_name = 1))
    expect_error(getFullTbl(context_default, tbl_name = 2))
})


test_that("getFilteredTbl() works", {
    
    expect_equal(getFilteredTbl(context_default),
                 context_default$tbl_lst[[context_default$tbl_name]])
})


test_that("getEmptyRow() works with context_default", {
    
    expectation <- data.frame(
        a0_num = NA_real_, 
        b_int = NA_integer_,
        c_chr = NA_character_, 
        d_date = as.Date(NA),
        e_dttm = as.POSIXct(NA), 
        p0_num = NA_real_,
        p1_num = NA_real_, 
        u_url = NA_character_,
        w_email = NA_character_,
        stringsAsFactors = FALSE
    )
    expect_equal(getEmptyRow(context_default), expectation)
})


test_that("getEmptyRow() stops with non-character tbl_name", {
    
    expect_error(getEmptyRow(context_default, 1))
    expect_error(getEmptyRow(context_default, NA))
    expect_error(getEmptyRow(context_default, NULL))
})


test_that("getEmptyRow() stops with non-valid tbl_name", {
    
    expect_error(getEmptyRow(context_default, "name"))
    expect_error(getEmptyRow(context_default, NA_character_))
    expect_error(getEmptyRow(context_default, character()))
})



test_that("choices() works", {
     expectation <- LETTERS[1:4]
     expect_equal(choices("c_chr", context_default), expectation)
})


test_that("choices() works, with all tables", {
    expectation <- LETTERS[1:4]
    expect_equal(choices("c_chr", context_default, tbl_name = "all"), expectation)
})


test_that("isNotEmptyChr() works", {
    expect_true(isNotEmptyChr("not empty"))
})


test_that("isNotEmptyChr() works with character(0)", {
    expect_false(isNotEmptyChr(character(0)))
})


test_that("isNotEmptyChr() works with NAs", {
    expect_false(isNotEmptyChr(NA))
    expect_false(isNotEmptyChr(NA_character_))
    expect_false(isNotEmptyChr(c(NA, NA, NA)))
})


test_that("isNotEmptyChr() works with empty string", {
    expect_false(isNotEmptyChr(""))
})


test_that("isNotEmptyChr() works with NULL", {
    expect_false(isNotEmptyChr(NULL))
})


test_that("isNotEmptyChr() works with character vector", {
    expect_true(isNotEmptyChr(c("c1", "c2")))
})
