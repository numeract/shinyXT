# shinyXT - eXtensible Table framework for R/Shiny
[![Travis build status](https://travis-ci.org/numeract/shinyXT.svg?branch=master)](https://travis-ci.org/numeract/shinyXT)
[![Coverage status](https://codecov.io/gh/numeract/shinyXT/branch/master/graph/badge.svg)](https://codecov.io/github/numeract/shinyXT?branch=master)

**The package is currently under active development, please expect major 
changes while the API stabilizes.**

## Motivation

The package allows the configuration and formatting of Shiny DataTable and Form 
templates that are meant to be re-used, as well as other Shiny functionalities.


## Installation

```
# install.packages("devtools")
devtools::install_github("numeract/shinyXT")
```

### Simple Example

```
library(shiny)
library(shinyXT)
onetable_df <- data.frame(
    a0_num = c(1, 2, 3, 4),
    b_int = 1:4,
    c_chr = LETTERS[1:4],
    d_date = as.Date(c("2018-01-01", "2018-01-02", "2018-01-03", "2018-01-04"))
)

onetable_xt <- list(
    
    # .default ----
    # default fields and values for all columns
    .default = list(
        col_name = NA_character_,
        class = NA_character_,
        ui_name = NA_character_,
        hover = NULL,
        widget = NULL,
        format = NULL,
        column_width = 3,   # width for shiny::column(), 0 for no column
        width = "100%",     # control width within the column
        visible = TRUE,
        enabled = TRUE,
        validate = NULL
    ),
    
    # .options ----
    # options for the whole table
    .options = list(
        # functions to convert Date/Time to character
        format_Date = quote(format_utc),
    
        # overwrite individual col visibility
        visible = c("a0_num", "a2_num", "c_chr", "d_date")
    ),
    
    # a0_num ----
    a0_num = list(
        col_name = "a0_num",
        class = "numeric",
        ui_name = "Col A0",
        hover = "Numeric, no decimals",
        widget = "numericInput",
        format = "",
        validate = NULL
    ),
    
    # a2_num ----
    a2_num = list(
        col_name = "a2_num",
        class = "numeric",
        ui_name = "Col A2",
        hover = "Numeric, 2 decimals",
        widget = list(
            name = "numericInput",
            step = 0.01
        ),
        format = ".2",
        visible = FALSE,    # do not show in form
        validate = NULL
    ),
    
    # b_int ----
    b_int = list(
        col_name = "b_int",
        class = "integer",
        ui_name = "Col B",
        hover = "Integer",
        widget = "numericInput",
        format = "",
        validate = NULL
    ),
    
    # c_chr ----
    c_chr = list(
        col_name = "c_chr",
        class = "character",
        ui_name = "Col C",
        hover = "Character",
        widget = list(                  # drop down w/ column values
            name = "selectizeInput",
            choices = quote(choices("c_chr", .context))
        ),
        format = "",
        validate = NULL
    ),
    
    # d_date ----
    d_date = list(
        col_name = "d_date",
        class = "Date",
        ui_name = "Col D",
        hover = "Date (date only), not missing",
        widget = "dateInput",
        format = "date",
        validate = NULL
    )
)


ui <- shinyUI(fluidPage( 
    tabPanel(
        title = "View",
        p(),
        DT::dataTableOutput("view_dt"))
    )
)


server <- shinyServer(function(input, output, session) {
    
    # Data ----
    # load data and create default context
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
    
    # View ----
    output$view_dt <- DT::renderDataTable({
        .context <- context_default
        shinyXT::createDT(.context)
    })
})

shinyApp(ui, server)
```
