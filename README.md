# **London Marathon Analysis 2024**
## *Overview*
This repository contains the analysis of the London Marathon 2024 dataset, focusing on marathon finish times across various countries and categories (Elite and Mass). The primary objective of this analysis is to visualize and explore the distribution of finish times, identify patterns, and compare performances between different countries.

The analysis involves:

- Data processing and cleaning
- Country-level performance analysis
- Visualisation of the data using interactive plots
- A Shiny app to filter and visualize marathon performance by country, continent, and category

Features:
- Interactive Violin Plot: Visualises the distribution of marathon finish times across countries, highlighting differences between continents and categories.
- Data Filtering: Users can filter the data by continent, marathon category (Elite or Mass), and minimum number of participants.
- Dynamic Titles: The Shiny app dynamically updates the plot title and other outputs based on user-selected filters.

Also included is other possible plots
To alter the violin plot to the box plot copy and paste the below code between lines 195-253 on the rscript file.

server <- function(input, output) {
  
  # Reactive filtered data based on user inputs
  filtered_data <- reactive({
    data <- final_results
    
    # Filter by continent
    if (input$continent_filter == "All Countries") {
      data <- data
    } else if (input$continent_filter == "Top 5 Countries by Participants") {
      top_countries <- country_counts_df %>%
        arrange(desc(Count)) %>%
        head(5) %>%
        pull(Country)
      data <- data %>% filter(Country %in% top_countries)
    } else {
      data <- data %>% filter(Continent == input$continent_filter)
    }
    
    # Filter by category
    if (input$category_filter != "Both") {
      category <- input$category_filter
      data <- data %>% filter(Category == category)
    }
    
    # Filter by minimum participants
    min_participants <- input$min_participants
    country_filter <- country_counts_df %>%
      filter(Count >= min_participants) %>%
      pull(Country)
    data <- data %>% filter(Country %in% country_filter)
    
    return(data)
  })
  
  # Dynamic title based on filters
  output$dynamic_title <- renderText({
    title <- paste(input$category_filter, "Category -", input$continent_filter)
    title <- paste(title, sprintf("(Min. Participants: %d)", input$min_participants))
    paste("Showing Data for", title)
  })
# Render boxplot
  output$boxplot <- renderPlotly({
    data <- filtered_data()
    
    plot_ly(data, 
            y = ~Time, 
            color = ~Country, 
            type = "box", 
            boxmean = "sd") %>%
      layout(
        title = paste("Finish Times by Country"),
        xaxis = list(title = "Country"),
        yaxis = list(title = "Finish Time (minutes)")
      )
  })
}


## *Setup*
*Prerequisites*

To run the analysis and Shiny app, you need to have the following packages installed in R:

library(shiny)
library(plotly)
library(rvest)
library(tidyverse)
library(countrycode)
library(rsconnect)

Data provided by London marathon is of free use, participants must opt out of allowing their data for use by third parties

## Acknowledgments

Data Source: The data used in this analysis is sourced from the official London Marathon results and publicly available marathon data.

Shiny: This project utilizes the Shiny package for building interactive web applications in R.