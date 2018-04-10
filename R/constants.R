# use internally, do not export

.XT <- list(
    
    # .default
    .default = list(
        # TODO
    ),
    
    # .options
    .options = list(
        # TODO
    ),
    
    # valid
    valid_mode = c("dt", "edit", "add"),
    valid_class = c("numeric", "integer", "character",
                     "Date", "POSIXct"),
    
    
    # js scripts
    jsdt_4col =
        '[{
            extend: "collection",
            text: "Show/Hide Columns",
            buttons: [ "columnsToggle" ],
            collectionLayout: "fixed four-column"
        }]',
    
    NULL
)
