library(shiny)
library(DT)

shinyUI(fluidPage(
    navbarPage(
        "Montgomery Pets",
        tabPanel(
            "Inspector",
            uiOutput("inspectorTitle"),
            textOutput("inspectorSelected"),
            DTOutput("inspectorTable"),
        ),
        tabPanel(
            "Explorer",
            uiOutput("explorerTitle")
        )
    )
))
