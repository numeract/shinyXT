# functions from Nmisc, import from Nmisc later when stable
# do not export these functions, do not add to doc

# lang ----
qval <- function(x, default = NULL, envir = parent.frame()) {
    
    # see ?mode (abot same as class); do not accept `(`
    val <- if (class(x)[1L] %in% c("call", "name")) {
        tryCatch({
            eval(x, envir = envir)
        }, error = function(e) {
            warning(e$message)
            NULL
        })
    } else {
        x
    }
    
    val %||% default
}


# date-time ----
is_POSIXct <- function(x) inherits(x, "POSIXct")


#' Format Date and POSIXct
#' 
#' Converts Date and POSIXct objects to the format given as input.
#' 
#' @param x A Date or POSIXct object to be converted.
#' @param format A character string. The default format is
#'   "\%Y-\%m-\%d" for Date and "\%Y-\%m-\%d \%H:\%M:\%S" for POSIXct.
#' @param usetz Logical. If TRUE, the time zone abbreviation is
#'   appended to the output. Applicable only if an POSIXct object.
#' 
#' @return A character string representing the formatted date.
#' @seealso \code{\link{format.Date}}, \code{\link{format.POSIXct}}
#' 
#' @examples
#' format_utc(Sys.time(), format = "%Y-%m-%d", usetz = FALSE)
#' 
#' @export
format_utc <- function(x, format = NULL, usetz = TRUE) {
    
    if (inherits(x, "Date")) {
        if (is.null(format)) {
            format <- "%Y-%m-%d"
        }
        format.Date(x, format = format)
    } else if (inherits(x, "POSIXct")) {
        if (is.null(format)) {
            format <- "%Y-%m-%d %H:%M:%S"
        }
        format.POSIXct(x, format = format, tz = "UTC", usetz = usetz)
    } else {
        stop("not a Date/POSIXct")
    }
}


# purrr-misc ----
keep_at <- function(.x, .at) {
    
    if (length(.at) == 0L) return(.x[0L])
    if (any(is.na(.at))) stop("`.at`` must not contain NA's")
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::keep(.x, .p)
}


discard_at <- function(.x, .at) {
    
    if (length(.at) == 0L) return(.x)
    if (any(is.na(.at))) stop("`.at` must not contain NA's")
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::discard(.x, .p)
}
