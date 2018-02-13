library(shiny)

source('onetable_df.R')
source('onetable_xt.R')

shinyServer(function(input, output, session) {
    
    # Data ----
    # load data and create default context
    # everything that is in context is available to callbacks
    # the context can contain several tables, only one being "active"
    context_default <- list(
        tbl_name = 'onetable',
        tbl_lst = list(
            onetable = onetable_df
        ),
        filter_lst = NULL,
        mode = NULL,
        xt_lst = list(
            onetable = onetable_xt
        )
    )
    
    # reactive data
    rv <- reactiveValues(
        filter_lst = NULL,
        mode = 'dt',
        row_idx = NULL
    )
    
    # use flags to keep track of operations performed & messages
    flag <- reactiveValues(
        edit = NULL
    )
    
    
    # UI ----
    shinyjs::toggleState(
        condition = FALSE, selector = "#main_panel li a[data-value=Edit]")
    
    # View ----
    output$view_dt <- DT::renderDataTable({
        .context <- context_default
        .context$mode <- rv$mode
        shinyXT::createDT(.context)
    })
    
    
    # Edit ----
    observeEvent({
        input$edit_action
    }, {
        row_idx <- input$view_dt_rows_selected
        req(row_idx)
        
        # show Edit tab then go there 
        shinyjs::toggleState(
            condition = TRUE, selector = "#main_panel li a[data-value=Edit]")
        shinyjs::toggleState(
            condition = FALSE, selector = "#main_panel li a[data-value=View]")
        updateTabsetPanel(
            session = session,
            inputId = 'main_panel',
            selected = 'Edit'
        )
        
        # clear edit message
        flag$edit <- NULL
        
        # lock in context info
        rv$mode <- NULL
        rv$mode <- 'edit'
        rv$row_idx <- row_idx
    })
    
    
    observeEvent({
        input$add_action
    }, {
        # show Edit tab then go there
        shinyjs::toggleState(
            condition = TRUE, selector = "#main_panel li a[data-value=Edit]")
        shinyjs::toggleState(
            condition = FALSE, selector = "#main_panel li a[data-value=View]")
        updateTabsetPanel(
            session = session,
            inputId = 'main_panel',
            selected = 'Edit'
        )
        
        # clear edit message
        flag$edit <- NULL
        
        # lock in context info
        rv$mode <- NULL
        rv$mode <- 'add'
        rv$row_idx <- NULL
    })
    
    
    output$edit_ui <- renderUI({
        mode <- rv$mode
        req(mode %in% c('edit', 'add'))
        if (mode == 'edit') {
            row_idx <- isolate(rv$row_idx)
            req(row_idx)
        } else {
            row_idx <- NULL
        }
        
        # context: all the info needed by ui, validation, etc. functions
        .context <- context_default
        .context$mode <- mode
        .context$row_idx <- row_idx
        createForm(.context)
    })
    
    
    observeEvent({
        input$edit_ok
    }, {
        mode <- rv$mode
        req(mode %in% c('edit', 'add'))
        if (mode == 'edit') {
            row_idx <- isolate(rv$row_idx)
            req(row_idx)
        } else {
            row_idx <- NULL
        }
        
        # validate inputs
        input_lst <- reactiveValuesToList(input)
        .context <- context_default
        .context$mode <- mode
        .context$row_idx <- row_idx
        vld_lst <- validateForm(input_lst, .context)
        
        if (is.null(vld_lst$msg)) {
            # go to View
            shinyjs::toggleState(
                condition = TRUE, selector = "#main_panel li a[data-value=View]")
            shinyjs::toggleState(
                condition = FALSE, selector = "#main_panel li a[data-value=Edit]")
            updateTabsetPanel(
                session = session,
                inputId = 'main_panel',
                selected = 'View'
            )
            
            # clear edit message
            flag$edit <- NULL
            
            # update/clear context info
            tbl <- dplyr::bind_rows(
                context_default$tbl_lst$onetable, vld_lst$row_df)
            context_default$tbl_lst$onetable <<- tbl
            
            # update mode
            rv$mode <- NULL
            rv$mode <- 'dt'
        } else {
            flag$edit <- vld_lst$msg
        }
    })
    
    
    observeEvent({
        input$edit_cancel
    }, {
        # go to View
        shinyjs::toggleState(
            condition = TRUE, selector = "#main_panel li a[data-value=View]")
        shinyjs::toggleState(
            condition = FALSE, selector = "#main_panel li a[data-value=Edit]")
        updateTabsetPanel(
            session = session,
            inputId = 'main_panel',
            selected = 'View'
        )
        
        # clear edit message
        flag$edit <- NULL
        
        # update mode
        rv$mode <- NULL
        rv$mode <- 'dt'
    })
    
    
    output$edit_txt <- renderText({
        
        flag$edit
    })
    
    
})
