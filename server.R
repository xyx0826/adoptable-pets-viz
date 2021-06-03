#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(DT)

source("scripts/utils.R")
source("scripts/plot.R")

# Global dataframe
df <- getData()

# Define server logic required to draw a histogram
shinyServer(function (input, output) {
    # Listen for button click to select a random pet
    observeEvent(input$random, {
        # To control a DT, use its proxy
        dataTableProxy("inspectorTable") %>%
            # Randomize a row index
            selectRows(round(runif(1, 1, nrow(df))))
    })

    # Create static titles
    output$inspectorTitle = renderUI({
        tags$h3(paste("Get to know the", nrow(df), "pets for adoption in Montgomery County, MD."))
    })
    output$explorerTitle = renderUI({
        tags$h3("Quick summaries of Montgomery County, MD's adoptable pets.")
    })
    
    # Inspector selected info
    selIndex <- reactive({
        input$inspectorTable_rows_selected
    })
    output$inspectorSelected = renderUI({
        if (length(selIndex()) == 0) {
            tags$span("Select a pet to view its information.")
        } else {
            selRow <- df[selIndex(),]
            # Format summary text
            text <- paste0(
                # No asterisks in names
                str_remove(selRow$Pet.Name, "\\*"),
                # Hide species if unspecified
                if (selRow$Animal.Type == "Other")
                    ""
                else
                    paste(" the", tolower(selRow$Animal.Type)),
                " is a ",
                # Pretty-print ages
                prettyPrintAge(selRow$Pet.Age),
                " ",
                # TODO: pretty-print breed
                tolower(selRow$Breed),
                "."
            )
            tags$p(text)
        }
    })
    output$inspectorImage = renderUI({
        selRow <- df[selIndex(),]
        tags$img(
            src = selRow$URL.Link
        )
    })
    
    # Inspector table
    output$inspectorTable = renderDT({
        df %>%
            rename(ID = Animal.ID) %>%
            rename("Intake type" = Intake.Type) %>%
            rename("Intake date" = In.Date) %>%
            rename(Name = Pet.Name) %>%
            rename(Species = Animal.Type) %>%
            rename(Size = Pet.Size) %>%
            rowwise() %>%
            mutate(Age = round(Pet.Age, digits = 2)) %>%
            mutate(Color = paste(Color, collapse = " / ")) %>%
            as.data.frame() %>%
            select(-Pet.Age, -URL.Link, -Crossing)
        },
        selection = "single"
    )
})
