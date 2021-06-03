#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

source("scripts/utils.R")

# Define server logic required to draw a histogram
shinyServer(function (input, output) {
    # Load data and create static titles
    df <- getData()
    output$inspectorTitle = renderUI({
        tags$h3(paste("Get to know the", nrow(df), "pets for adoption in Montgomery County, MD."))
    })
    output$explorerTitle = renderUI({
        tags$h3("Quick summaries of Montgomery County, MD's adoptable pets.")
    })
    
    # Inspector selected info
    selectedRow <- reactive({
        input$inspectorTable_rows_selected
    })
    output$inspectorSelected = renderText(
        if (length(selectedRow()) == 0) "Select a row to view its details." else paste("Selected row:", selectedRow())
    )
    
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
