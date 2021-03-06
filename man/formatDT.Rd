% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt-misc.R
\name{formatDT}
\alias{formatDT}
\title{Format a Data Table.}
\usage{
formatDT(dt, xt)
}
\arguments{
\item{dt}{A DT DataTable.}

\item{xt}{Config mode from context.}
}
\value{
the newly formatted DT DataTable
}
\description{
Formats the DT Data Table based on the options
specified in the configuration.
}
\examples{
onetable_df <- data.frame(
    a0_num = c(1, 2, 3, 4)
)

onetable_xt <- list(
    
    .default = list(
        col_name = NA_character_,
        class = NA_character_,
        ui_name = NA_character_,
        hover = NULL,
        widget = NULL,
        format = NULL,
        column_width = 3,   
        width = "100\%", 
        visible = TRUE,
        enabled = TRUE,
        validate = NULL
    ),
    
    .options = list(),
    a0_num = list(
        col_name = "a0_num",
        class = "numeric",
        ui_name = "Col A0",
        hover = "Numeric, no decimals",
        widget = "numericInput",
        format = "",
        validate = NULL
    )
)


context_default <- list(
    tbl_name = "onetable",
    tbl_lst = list(
        onetable = onetable_df
    ),
    filter_lst = NULL,
    mode = "dt",
    xt_lst = list(
        onetable = onetable_xt
    )
)

xt <- shinyXT::getConfigMode(context_default)

dt <- shinyXT::createDT(context_default) \%>\%
    shinyXT::formatDT(xt = xt)
    
}
\seealso{
\code{\link{createDT}}
}
