# based on https://deanattali.com/2015/04/21/r-package-shiny-app/

#' Runs shinyXT examples included with the package
#' 
#' Runs the specified shinyXT example within a shiny app, 
#'  based on the configuration files found in the same folder 
#'  (see folder \code{examples/}).
#' 
#' @param example_name String that represents name of the example.
#' 
#' @export
shinyXTExample <- function(example_name = "onetable") {
    
    matched_examples <- list.files(
        system.file("examples", package = "shinyXT"))
    
    match.arg(example_name, matched_examples)
    
    app_dir <- system.file(
        "examples", example_name, package = "shinyXT")
    
    shiny::runApp(app_dir, display.mode = "normal")
}
