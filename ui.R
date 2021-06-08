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
            "Overview",
            uiOutput("overviewTitle"),
            includeMarkdown("overview.Rmd"),
            uiOutput("overviewSlider"),
            plotOutput("overviewPlot")
        ),
        tabPanel(
            "Inspector",
            uiOutput("inspectorTitle"),
            includeMarkdown("inspector.Rmd"),
            fluidRow(
                column(9, uiOutput("inspectorSelected")),
                column(3, actionButton("random", "Inspect random pet"))
            ),
            fluidRow(
                column(4, uiOutput("inspectorImage")),
                column(4, plotOutput("inspectorMap")),
                column(4, plotOutput("inspectorPlot"))
            ),
            DTOutput("inspectorTable"),
        ),
        tabPanel(
            "Explorer",
            uiOutput("explorerTitle"),
            includeMarkdown("explorer.Rmd"),
            # First plot and description
            fluidRow(
                column(5, plotOutput("explorerPlot1")),
                column(4, uiOutput("explorerPlot1Text"))
            ),
            # Second plot and controls
            fluidRow(
                column(
                    4,
                    sidebarPanel(width = 10,
                        radioButtons(
                            "explorerSpecies",
                            label = "View up to 6 top breeds for species:",
                            choices = c("All", "Cats", "Dogs", "Birds and Others"),
                            selected = "All"
                        )
                    )
                ),
                column(5, plotOutput("explorerPlot2"))
            )
        ),
        tabPanel(
            "Takeaways",
            includeMarkdown("takeaways.Rmd")
        )
    )
))
