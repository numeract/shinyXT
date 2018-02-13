library(shiny)


shinyUI(fluidPage(
    # use shinyjs to show/hide enable/disable elements
    shinyjs::useShinyjs(),
    # use bsplus for tooltips
    bsplus::use_bs_tooltip(),
    
    titlePanel("ShinyXT Example: One Table"),
    
    sidebarLayout(
        sidebarPanel(
            actionButton(
                inputId = 'edit_action',
                label = "Edit Selected Record"
            ),
            actionButton(
                inputId = 'add_action',
                label = "Add a new Record"
            ), 
            width = 3
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = 'View',
                    p(),
                    DT::dataTableOutput('view_dt')
                ),
                tabPanel(
                    title = 'Edit',
                    p(),
                    uiOutput('edit_ui'),
                    hr(),
                    actionButton(
                        inputId = 'edit_ok',
                        label = "Validate and Apply",
                        icon = icon('check')
                    ),
                    actionButton(
                        inputId = 'edit_cancel',
                        label = "Cancel",
                        icon = icon('undo')
                    ),
                    p(),
                    textOutput('edit_txt', inline = TRUE)
                ),
                
                id = 'main_panel',
                selected = 'View',
                type = 'tabs'
            ), 
            width = 9
        )
    )
))
