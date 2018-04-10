# the "main" file for the shiny app

# !diagnostics suppress=.APP_UI, .APP_SERVER


# sources other shiny files start with `app_` for easy grouping
# avoid ui.R and server.R so that [Run App] runs only this file


# source constants, settings and engine only once
source("main.R")


# additional shiny libraries (not used by engine)
library(shiny)
# library(bsplus)


# shiny related
source("app_ui.R")
source("app_server.R")



# run ----
shiny::shinyApp(ui = .APP_UI, server = .APP_SERVER)
