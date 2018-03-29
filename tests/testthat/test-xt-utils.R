context('test xt-utils')

source('../../inst/examples/onetable/onetable_df.R') 
source('../../inst/examples/onetable/onetable_xt.R') 

context_default <- list(
    tbl_name = 'onetable',
    tbl_name2 = 'ama',
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

