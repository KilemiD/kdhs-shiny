# Shiny >= 1.5 automatically sources every file in R/ before running app.R.
# That breaks this app: R/ui.R builds the `ui` object at source time, which calls
# mod_*_ui() from modules/ — a directory Shiny does not autoload. The module
# functions would not exist yet.
#
# This file switches the autoloader off, so app.R's explicit source() calls
# control the order: data_loader -> survey_setup -> helpers -> modules -> ui -> server.
# Do not delete it. See https://shiny.posit.co/r/reference/shiny/latest/loadsupport
