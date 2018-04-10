# data, usually loaded from an external source


onetable_df <- data.frame(
    a0_num = c(1, 2, 3, 4),
    b_int = 1:4,
    c_chr = LETTERS[1:4],
    d_date = as.Date(c("2018-01-01", "2018-01-02", "2018-01-03", "2018-01-04")),
    e_dttm = as.POSIXct(
        c("2018-01-01 01:00:00", "2018-01-01 02:00:00",
          "2018-01-01 03:00:00", "2018-01-01 04:00:00"), tz = "UTC"),
    p0_num = c(1, 2, 3, 4) / 10,
    p1_num = c(1, 2, 3, 4) / 10,
    u_url = "www.example.com",
    w_email = "example@example.com",
    stringsAsFactors = FALSE
)
