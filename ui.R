library(shiny)
library(DT)

shinyUI(fluidPage(
    theme = "styles.css",
    navbarPage(
        "Montgomery Pets",
        tabPanel(
            "Inspector",
            uiOutput("inspectorTitle"),
            fluidRow(
                column(9, uiOutput("inspectorSelected")),
                column(3, actionButton("random", "Inspect random pet"))
            ),
            uiOutput("inspectorImage"),
            DTOutput("inspectorTable"),
        ),
        tabPanel(
            "Explorer",
            uiOutput("explorerTitle")
        )
    )
))
