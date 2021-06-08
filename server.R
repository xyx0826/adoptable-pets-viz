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
    output$overviewTitle = renderUI({
        tags$h3("Browse Montgomery County, MD's adoptable pets on a time axis.")
    })
    output$inspectorTitle = renderUI({
        tags$h3(paste("Get to know the", nrow(df), "pets for adoption in Montgomery."))
    })
    output$explorerTitle = renderUI({
        tags$h3("Quick summaries of Montgomery's adoptable pets.")
    })
    
    ######## Overview ########
    
    # Overview page-wide date selector
    output$overviewSlider = renderUI({
        dmin <- min(df$In.Date)
        dmax <- max(df$In.Date)
        sliderInput(
            "dt",
            "View time range on the plot:",
            dmin, dmax,
            value = c(dmin, dmax),
            timeFormat = "%b %d, %Y"
        )
    })

    # Overview date plot
    output$overviewPlot = renderPlot({
        plotDate(input$dt[1], input$dt[2])
    })
    
    ######## Inspector ########
    
    # Inspector table selection logic
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
                # Lowercase breed
                tolower(selRow$Breed),
                "."
            )
            tags$p(text)
        }
    })
    
    # Inspector image
    output$inspectorImage = renderUI({
        selRow <- df[selIndex(),]
        tags$img(
            src = selRow$URL.Link
        )
    })
    
    # Inspector map
    output$inspectorMap = renderPlot({
        map = map_data("county", region = "maryland")
        title <- "Select a stray to see where it was found."
        selRow <- NA
        if (length(selIndex()) > 0) {
            # Row selected
            selRow <- df[selIndex(),]
            if (is.na(selRow$lng) | is.na(selRow$lat)) {
                # No coordinate, discard this row
                row <- NA
            } else {
                # Coordinates valid, format title
                title <- paste("Location of recovery for", selRow$Pet.Name)
            }
        }

        plot <- ggplot(map) +
            geom_polygon(
                aes(long, lat, group = group),
                fill = "blue4", color = "aliceblue", size = 0.1
            )
        if (is.list(selRow)) {
            # Plot the dot and zoom in on it
            plot <- plot +
                # Zoom in
                coord_fixed(
                    xlim = c(selRow$lng - 0.7, selRow$lng + 0.7),
                    ylim = c(selRow$lat - 0.35, selRow $ lat + 0.35)
                ) +
                # Plot point
                geom_point(aes(selRow$lng, selRow$lat, size = 3), color = "red") +
                # Plot street address
                geom_text(
                    data = selRow,
                    aes(lng, lat, label = Crossing, color = "red"),
                    hjust = "middle", vjust = 0, nudge_y = 0.07
                )
        } else {
            # No geo data, use vanilla axes
            plot <- plot +
                coord_fixed()
        }
        plot +
            ggtitle(title) +
            theme(legend.position = "none")
    })
    
    # Inspector box plot
    output$inspectorPlot = renderPlot({
        title <- "Age distribution of adoptable pets."
        selRow <- NA
        if (length(selIndex()) > 0) {
            # Row selected
            selRow <- df[selIndex(),]
            title <- paste0(
                selRow$Pet.Name, "'s age in relation to other adoptable pets."
            )
        }
        
        plot <- ggplot(df, aes(Pet.Age, 0)) +
            geom_boxplot(outlier.shape = NA) +
            geom_jitter(width = 0.5)
        if (is.list(selRow)) {
            plot <- plot +
                geom_vline(xintercept = selRow$Pet.Age, color = "red") +
                geom_text(
                    data = selRow,
                    aes(Pet.Age, 0, label = Pet.Age, color = "red"),
                    hjust = 0, vjust = 0, nudge_x = 0.5
                )
        }
        plot +
            ggtitle(title) +
            # Hack to remove Y axis
            scale_y_discrete(labels = NULL, breaks = NULL) +
            labs(y = "") +
            theme(legend.position = "none")
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
            select(-Pet.Age, -URL.Link, -Crossing, -lat, -lng, -hex)
        },
        selection = "single"
    )
    
    ######## Explorer ########
    
    # Explorer plots
    output$explorerPlot1 <- renderPlot({
        plotSpecies()
    })
    output$explorerPlot1Text <- renderUI({
        tags$em(prettyPrintSpecies())
    })
    output$explorerPlot2 <- renderPlot({
        if (input$explorerSpecies == "All") {
            plotBreeds(df)
        } else {
            data <- df %>%
                filter(str_detect(input$explorerSpecies, Animal.Type))
            plotBreeds(data)
        }
    })
    
    ######## Takeaways ########
    
    # Takeaways plot
    output$takeawaysPlot <- renderPlot({
        plotSpecies()
    })
})
