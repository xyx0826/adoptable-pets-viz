library(shiny)
library(DT)

source("scripts/theme.R")

shinyUI(fluidPage(
    theme = theme,
    navbarPage(
        "Montgomery Pets",
        tabPanel(
            "Welcome",
            includeMarkdown("welcome.Rmd")
        ),
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
            uiOutput("explorerTitle"),
            # First plot and description
            fluidRow(
                column(
                    6, align = "center",
                    plotOutput("explorerPlot1"),
                    textOutput("explorerPlot1Text")
                )
            ),
            # Second plot and controls
            fluidRow(
                # column(6, plotOutput("explorerPlot1")),
                column(6, plotOutput("explorerPlot2"))
            )
        ),
        tabPanel(
            "Takeaways",
            includeMarkdown("takeaways.Rmd")
        )
    )
))
