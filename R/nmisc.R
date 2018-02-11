# functions from Nmisc, import from Nmisc later when stable
# do not export these functions, do not add to doc

# lang ----
qval <- function(x, default = NULL, envir = parent.frame()) {
    
    val <- if (class(x)[1L] == 'call') {
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
        format.POSIXct(x, format = format, tz = 'UTC', usetz = usetz)
    } else {
        stop("not a Date/POSIXct")
    }
}


# purrr-misc ----
keep_at <- function(.x, .at) {
    
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
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::discard(.x, .p)
}
