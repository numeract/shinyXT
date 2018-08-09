context("app-function")

test_that("shinyXTExample() works", {
    # Don't run these tests on the CRAN build servers
    skip_on_cran()
    
    # Use compareImages=FALSE in order to avoid conflicts between test
    # screenshots made on different operating systems
   # shinytest::expect_pass(
    #    shinytest::testApp("R/example/", compareImages = FALSE))
})
