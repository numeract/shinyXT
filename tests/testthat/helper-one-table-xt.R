# xt configuration file, including callbacks

xt_pre_dt <- function(tbl, xt, .context) {
    
    tibble::add_column(tbl, a2_num = tbl$a0_num, .after = "a0_num")
}


xt_row <- function(tbl, xt, .context) {
    
    # another way to handle mode (to have only one xt_row callback)
    if (.context$mode == "edit") {
        # pick row from filtered tbl
        row_df <- dplyr::slice(tbl, .context$row_idx)
    } else {
        row_df <- getEmptyRow(.context)
    }
    
    # update dttm field to display the most recent value to the user
    row_df[["e_dttm"]] <- lubridate::now("UTC")
    
    row_df
}


xt_pre_validate <- function(input_lst, .context) {
    # do nothing
    input_lst
}


xt_validate_not_missing <- function(x) {
    
    if (!isNotEmptyChr(x)) return("value is missing")
    
    # return NULL if successful
    NULL
}


xt_validate_edit <- function(row_df, .context) {
    
    if (row_df$a0_num > 9) return("a0_num should be less than or equal to 9")
    
    # return NULL if successful
    NULL
}


xt_validate_add <- function(row_df, .context) {
    
    # add validation has an extra step compared with edit validation
    if (is.na(row_df$a0_num)) return("a0_num is missing")
    
    xt_validate_edit(row_df, .context)
}


xt_finalize <- function(row_df, .context) {
    
    # update dttm field with the most recent time
    row_df[["e_dttm"]] <- lubridate::now("UTC")
    
    row_df
}


# if any level 2 (only!) field (e.g. validate) is a list of mode fields,
# getConfigMode() will be used to extract only that mode
# valid modes: dt, edit, add
onetable_xt <- list(
    
    # .default ----
    # default fileds and values for all columns
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
        format_POSIXct = quote(format_utc),
        
        # dt ----
        # generate df: e.g. add new columns
        # possible arguments: tbl (filtered active tbl), xt (active xt), .context
        pre_dt = quote(xt_pre_dt(tbl, xt, .context)),
        # overwrite individual col visibility
        visible = c("a0_num", "a2_num", "c_chr", "d_date", "e_dttm",
                    "u_url", "w_email"),
        pageLength = 10,
        buttons = jsDT("4col"),
        selection = "single",
        filter = "top",
        
        # edit / add ----
        # shiny prefix for all ui widgets
        id_prefix = "xt_",
        # generate df_row: e.g. update dates
        # possible arguments: tbl (filtered active tbl), xt (active xt), .context
        xt_row = quote(xt_row(tbl, xt, .context)),
        # processing of raw shiny inputs to match xt
        pre_validate = quote(xt_pre_validate(input_lst, .context)),
        # error messages for _ui inputs (does not change their values)
        validate = list(
            edit = quote(xt_validate_edit(row_df, .context)),
            add = quote(xt_validate_add(row_df, .context))
        ),
        # final step before updating tbl, e.g. update/touch dates
        finalize = quote(xt_finalize(row_df, .context))
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
    
    # <br> ----
    .br = brXT(),
    
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
        validate = quote(xt_validate_not_missing(value))
    ),
    
    # e_dttm ----
    e_dttm = list(
        col_name = "e_dttm",
        class = "POSIXct",
        ui_name = "Col E",
        hover = "POSIXct (date & time)",
        widget = "textInput",
        format = "dttm",
        validate = NULL
    ),
    
    # <hr> ----
    .hr = hrXT(),
    
    # p0_num ----
    p0_num = list(
        col_name = "p0_num",
        class = "numeric",
        ui_name = "Col P0",
        hover = "Percentage, no decimals",
        widget = "numericInput",
        format = "%0",
        validate = NULL
    ),
    
    # p1_num ----
    p1_num = list(
        col_name = "p1_num",
        class = "numeric",
        ui_name = "Col P2",
        hover = "Percentage, 2 decimals",
        widget = "numericInput",
        format = "%2",
        validate = NULL
    ),
    
    # <br> ----
    .br = brXT(),
    
    # u_url ----
    u_url = list(
        col_name = "u_url",
        class = "character",
        ui_name = "Col U",
        hover = "URL w/ link",
        widget = "textInput",
        format = "url",
        validate = NULL
    ),
    
    # w_email ----
    w_email = list(
        col_name = "w_email",
        class = "character",
        ui_name = "Col W",
        hover = "email w/ link",
        widget = "textInput",
        format = "email",
        validate = NULL
    )
    
)
