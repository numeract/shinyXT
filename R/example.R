#' based on https://deanattali.com/2015/04/21/r-package-shiny-app/
#' @export
shinyXTExample <- function(example_name = 'onetable') {
    
    matched_examples <- list.files(system.file("examples", package = "shinyXT"))
    
    match.arg(example_name, matched_examples)
    
    app_dir <- system.file("examples", example_name, package = "shinyXT")
    shiny::runApp(app_dir, display.mode = "normal")
}
