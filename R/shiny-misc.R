#' @export
progressInfo <- function(message, 
                          detail = "Begin",
                          max = 10L,
                          shiny_session = NULL,
                          quiet = FALSE
) {
    max <- as.integer(max)
    
    if (quiet) {
        # quite mode: calls do not do anything
        
        list(
            inc = function(detail) {
                invisible(NULL)
            },
            set = function(detail = NULL, value = NULL) {
                invisible(NULL)
            },
            close = function(detail = "Done") {
                invisible(NULL)
            }
        )
    } else if (is.null(shiny_session)) {
        # console mode: print
        
        di <- 0L
        fmt <- paste0("%s [%#", nchar(max), "d/%d]: %s")
        cat(sprintf(fmt, message, 0L, max, detail), '\n')
        
        list(
            inc = function(detail) {
                if (is.null(detail)) detail <- ''
                di <<- di + 1L
                cat(sprintf(fmt, message, di, max, detail), '\n')
                invisible(NULL)
            },
            set = function(detail = NULL, value = NULL) {
                if (is.null(detail)) detail <- ''
                if (!is.null(value)) di <<- as.integer(value)
                cat(sprintf(fmt, message, di, max, detail), '\n')
                invisible(NULL)
            },
            close = function(detail = "Done!") {
                if (is.null(detail)) detail <- ''
                di <<- max
                cat(sprintf(fmt, message, di, max, detail), '\n')
                invisible(NULL)
            }
        )
    } else {
        # shiny mode: use default progress bar
        
        progress <- shiny::Progress$new(shiny_session, min = 0, max = max)
        progress$set(message = message, detail = detail)
        
        list(
            inc = function(detail) {
                progress$inc(amount = 1, detail = detail)
            },
            set = function(detail = NULL, value = NULL) {
                progress$set(value = value, detail = detail)
            },
            close = function(detail = "Done") {
                progress$set(value = max, detail = detail)
                progress$close()
            }
        )
    }
}


#' @export
downloadButtonWithIcon <- function(outputId, 
                                   label = "Download", 
                                   class = NULL, 
                                   icon = shiny::icon("download"),
                                   ...
) {
    shiny::tags$a(
        id = outputId, 
        class = paste("btn btn-default shiny-download-link", class), 
        href = "", 
        target = "_blank", 
        download = NA, 
        icon, 
        label, 
        ...
    )
}
