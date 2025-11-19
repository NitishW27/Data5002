#Graph Maternal Mortality vs Life Expectancy with Region Selector
install.packages("readr")


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
clean_wide3 <- read_csv("clean_wide3.csv")

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Maternal Health Dashboard"),
  dashboardSidebar(
    # Year slider - UPDATED FOR DISCRETE YEARS
    sliderInput("year", "Select Year:",
                min = 2010,
                max = 2020,
                value = 2015,  # Default to middle year
                step = 5,  # Step of 5 years to match your data
                sep = "",
                ticks = TRUE),
    
    # Region selector (USING COLUMN J NOW)
    selectInput("regions", "Select Regions:",  # Changed from "countries" to "regions"
                choices = unique(clean_wide3$region_name),  # Column J - region names
                multiple = TRUE,  # Allow multiple selection
                selected = unique(clean_wide3$region_name)[1:5]),  # Default: first 5 regions
    
    # Additional options
    checkboxInput("trend_line", "Show Trend Line", value = TRUE),
    checkboxInput("label_points", "Label Regions", value = FALSE),  # Changed from "Countries" to "Regions"
    
    # Info box
    helpText("Select year and regions to update the scatterplot")  # Changed from "countries" to "regions"
  ),
  dashboardBody(
    fluidRow(
      # Main scatterplot
      box(
        title = "Maternal Mortality vs Life Expectancy",
        plotOutput("scatterPlot", height = 500),
        width = 8
      ),
      
      # Summary statistics
      box(
        title = "Summary Statistics",
        verbatimTextOutput("summaryStats"),
        width = 4
      )
    ),
    
    fluidRow(
      # Data table
      box(
        title = "Filtered Data",
        dataTableOutput("dataTable"),
        width = 12
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive data filtering - NOW FILTERING BY REGION NAME
  filtered_data <- reactive({
    req(input$year, input$regions)  # Changed from input$countries to input$regions
    
    clean_wide3 %>%
      filter(Year == input$year,
             region_name %in% input$regions)  # Filter by region_name (column J)
  })
  
  # Scatterplot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters") +
               theme_void())
    }
    
    p <- ggplot(data, aes(x = Life_expectancy_at_birth_for_both_sexes_years,
                          y = Maternal_mortality_ratio_deaths_per_100_000_population,
                          color = region_name)) +  # Color by region_name instead of column A
      geom_point(size = 4, alpha = 0.7) +
      labs(x = "Life Expectancy at Birth (years)",
           y = "Maternal Mortality Ratio (deaths per 100,000 population)",
           color = "Region") +  # Changed from "Country" to "Region"
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Add trend line if requested
    if (input$trend_line) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed")
    }
    
    # Add region labels if requested
    if (input$label_points) {
      p <- p + geom_text(aes(label = region_name),  # Label with region_name
                         vjust = -0.5, hjust = 0.5, size = 3)
    }
    
    p
  })
  
  # Summary statistics
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data available")
    }
    
    cat("Selected Year:", input$year, "\n")
    cat("Number of Regions:", length(unique(data$region_name)), "\n\n")  # Changed from countries to regions
    
    cat("Life Expectancy:\n")
    cat("  Mean:", round(mean(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n")
    cat("  Min:", round(min(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n")
    cat("  Max:", round(max(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n\n")
    
    cat("Maternal Mortality:\n")
    cat("  Mean:", round(mean(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
    cat("  Min:", round(min(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
    cat("  Max:", round(max(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
  })
  
  # Data table
  output$dataTable <- renderDataTable({
    data <- filtered_data()
    
    # Select only relevant columns for display
    if (nrow(data) > 0) {
      data %>% select(Region = region_name,  # Changed from Country to Region
                      Year, 
                      Life_Expectancy = Life_expectancy_at_birth_for_both_sexes_years,
                      Maternal_Mortality = Maternal_mortality_ratio_deaths_per_100_000_population)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
