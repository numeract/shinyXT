#' Displays the Progress Info
#' 
#' Displays progress info taking into account the mode the user is in. If the
#'   \code{quiet} mode is activated, the function call doesn't display anything.
#'   If the user is in console mode, the progress information is printed on the
#'   console. If the function is called in a valid Shiny session,
#'   a default progress bar shows progress information.
#' 
#' @param  message Character, the message to be displayed to the user.
#'  \code{NULL} in order to hide the current message
#' @param detail Character, the detail to be displayed to the user. If the user
#'   is in a Shiny session, detail message will be shown with a de-emphasized
#'   appearance relative to message
#' @param max Numeric, value that represents the end of the progress bar
#' @param shiny_session The shiny session object, provided by shinyServer
#' @param quiet Logical, whether or not to display progress info
#' 
#' @return A list with progress functions to call:
#' \item{name inc(detail)}{Increment progress counter} 
#' \item{name set(detail = NULL, value = NULL)}{Set progress counter 
#'   to \code{value}} 
#' \item{name close(detail = "Done")}{Terminate the progress bar. Best if called
#'   from \code{on.exit()}.} 
#' 
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
        cat(sprintf(fmt, message, 0L, max, detail), "\n")
        
        list(
            inc = function(detail) {
                if (is.null(detail)) detail <- ""
                di <<- di + 1L
                cat(sprintf(fmt, message, di, max, detail), "\n")
                invisible(NULL)
            },
            set = function(detail = NULL, value = NULL) {
                if (is.null(detail)) detail <- ""
                if (!is.null(value)) di <<- as.integer(value)
                cat(sprintf(fmt, message, di, max, detail), "\n")
                invisible(NULL)
            },
            close = function(detail = "Done!") {
                if (is.null(detail)) detail <- ""
                di <<- max
                cat(sprintf(fmt, message, di, max, detail), "\n")
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

#' Creates a Download Button with Icon
#' 
#' Has the same functionality as \code{shiny::downloadButton} but it also 
#' allows the user to customize the button by adding an icon to it.
#' It has a corresponding \code{downloadHandler}
#' in the server function.
#' 
#' @param outputId Character, the name of the output slot to which the
#'   corresponding downloadHandler is assigned.
#' @param label the label that appears on the button
#' @param class additional CSS classes
#' @param icon  the icon that appears on the button
#' @param ... named attributes applied to the button
#' 
#' @return A HTML tag object which can be also rendered as HTML 
#' 
#' @seealso \code{\link{shiny::downloadButton}} 
#' 
#' @export
downloadButtonWithIcon <- function(outputId,
                                   label = "Download",
                                   class = NULL,
                                   icon = shiny::icon("download"),
                                   ...) {
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
