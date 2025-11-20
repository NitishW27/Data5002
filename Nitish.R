library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
clean_wide3 <- read_csv("clean_wide3.csv")
clean_wide3$Year <- as.numeric(clean_wide3$Year)

# Standardize naming for merging for the map
map_data <- clean_wide3 %>%
  mutate(region_name = trimws(region_name))

# Load world map data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(region_name = trimws(name_long))

ui <- dashboardPage(
  dashboardHeader(title = "Global Maternal Health Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Maternal vs Life Expectancy", tabName = "viz1", icon = icon("chart-scatter")),
      menuItem("Time Series Analysis", tabName = "viz2", icon = icon("chart-line")),
      menuItem("Regional Comparison", tabName = "viz3", icon = icon("globe")),
      menuItem("Choropleth Map", tabName = "viz4", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              h2("Global Maternal Health Dashboard - Overview"),
              fluidRow(
                box(
                  title = "Project Introduction", width = 12, status = "primary", solidHeader = TRUE,
                  "This comprehensive dashboard analyzes global maternal health indicators across countries and regions. 
            Explore the relationship between maternal mortality, life expectancy, population growth, and other key health metrics through 
            interactive visualizations that reveal patterns and trends over time.",
                  br(), br(),
                  "Key indicators analyzed:",
                  tags$ul(
                    tags$li("Maternal Mortality Ratio"),
                    tags$li("Life Expectancy at Birth"),
                    tags$li("Population Growth Rate"),
                    tags$li("Under-5 Mortality Rate"),
                    tags$li("Female Life Expectancy"),
                    tags$li("Fertility Rate")
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("total_regions_box"),
                valueBoxOutput("year_range_box"),
                valueBoxOutput("data_sources_box")
              ),
              fluidRow(
                box(
                  title = "How to Use This Dashboard", width = 12, status = "info",
                  tags$ul(
                    tags$li("Use the sidebar to navigate between different visualizations"),
                    tags$li("Each visualization includes interactive filters and controls"),
                    tags$li("Hover over data points for detailed information"),
                    tags$li("Interpretation boxes provide key insights for each chart")
                  )
                )
              )
      ),
      
      # Visualization 1: Maternal Mortality vs Life Expectancy
      tabItem(tabName = "viz1",
              h2("Maternal Mortality vs Life Expectancy Analysis"),
              fluidRow(
                box(
                  title = "Controls", width = 3, status = "warning", solidHeader = TRUE,
                  sliderInput("year1", "Select Year:",
                              min = 2010, max = 2020, value = 2015, step = 5, sep = "", ticks = TRUE),
                  selectInput("regions1", "Select Regions:", 
                              choices = unique(clean_wide3$region_name),
                              multiple = TRUE, 
                              selected = unique(clean_wide3$region_name)[1:5]),
                  checkboxInput("trend_line1", "Show Trend Line", value = TRUE),
                  checkboxInput("label_points1", "Label Regions", value = FALSE),
                  helpText("Compare maternal mortality and life expectancy across regions for selected years.")
                ),
                box(
                  title = "Scatter Plot: Maternal Mortality vs Life Expectancy", width = 6, 
                  status = "primary", solidHeader = TRUE,
                  plotOutput("scatterPlot", height = 500)
                ),
                box(
                  title = "Interpretation", width = 3, status = "info", solidHeader = TRUE,
                  "This scatterplot reveals the inverse relationship between maternal mortality and life expectancy.",
                  br(), br(),
                  "Key Insights:",
                  tags$ul(
                    tags$li("Regions with higher life expectancy typically show lower maternal mortality"),
                    tags$li("The trend line demonstrates the strong negative correlation"),
                    tags$li("Outliers may indicate regions with unique healthcare challenges"),
                    tags$li("Use year slider to observe changes over time")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics", width = 4, status = "success",
                  verbatimTextOutput("summaryStats")
                ),
                box(
                  title = "Filtered Data", width = 8, status = "success",
                  dataTableOutput("dataTable1")
                )
              )
      ),
      
      # Visualization 2: Time Series Analysis
      tabItem(tabName = "viz2",
              h2("Female-Focused Mortality & Life Expectancy - Time Series"),
              fluidRow(
                box(
                  title = "Controls", width = 3, status = "warning", solidHeader = TRUE,
                  selectInput("region2", "Select Country/Area:",
                              choices = sort(unique(clean_wide3$region_name)),
                              selected = sort(unique(clean_wide3$region_name))[1],
                              multiple = TRUE),
                  checkboxGroupInput("vars", "Indicators to show:",
                                     choices = c("Maternal mortality ratio" = "mmr",
                                                 "Under-5 mortality rate" = "u5mr",
                                                 "Female life expectancy" = "fle"),
                                     selected = c("mmr", "fle")),
                  helpText("Track multiple health indicators over time for selected regions.")
                ),
                box(
                  title = "Time Series Plot", width = 6, status = "primary", solidHeader = TRUE,
                  plotOutput("ts_plot", height = 500)
                ),
                box(
                  title = "Interpretation", width = 3, status = "info", solidHeader = TRUE,
                  "This time series visualization shows trends in key health indicators over time.",
                  br(), br(),
                  "Key Insights:",
                  tags$ul(
                    tags$li("Observe improvements or declines in health indicators"),
                    tags$li("Compare multiple metrics simultaneously"),
                    tags$li("Identify turning points or significant changes"),
                    tags$li("Assess progress towards health-related SDGs")
                  )
                )
              )
      ),
      
      # Visualization 3: Regional Comparison - UPDATED WITH SEPARATE BOXES
      tabItem(tabName = "viz3",
              h2("Regional Comparison Analysis"),
              fluidRow(
                box(
                  title = "Controls", width = 3, status = "warning", solidHeader = TRUE,
                  sliderInput("year3", "Select Year:",
                              min = 2010, max = 2024, value = 2015, step = 5, sep = "", ticks = TRUE),
                  selectInput("regions3", "Compare Regions:", 
                              choices = unique(clean_wide3$region_name),
                              multiple = TRUE, 
                              selected = unique(clean_wide3$region_name)[1:8]),
                  helpText("Compare multiple regions side by side for detailed analysis.")
                ),
                box(
                  title = "Regional Comparison Plot", width = 6, status = "primary", solidHeader = TRUE,
                  plotOutput("regionalPlot", height = 500)
                ),
                box(
                  title = "Interpretation", width = 3, status = "info", solidHeader = TRUE,
                  "Direct comparison of regions reveals disparities and patterns in health outcomes.",
                  br(), br(),
                  "Key Insights:",
                  tags$ul(
                    tags$li("Identify regional disparities in health outcomes"),
                    tags$li("Compare performance across similar regions"),
                    tags$li("Spot best-performing regions for case studies"),
                    tags$li("Understand geographic patterns in health metrics")
                  )
                )
              ),
              # NEW: Separate boxes for top and bottom performers
              fluidRow(
                box(
                  title = "🏆 Global Top Performers (All-time Average)", width = 6, status = "success", solidHeader = TRUE,
                  tableOutput("top5Table")
                ),
                box(
                  title = "📊 Global Lowest Performers (All-time Average)", width = 6, status = "danger", solidHeader = TRUE,
                  tableOutput("bottom5Table")
                )
              ),
              fluidRow(
                box(
                  title = "Regional Performance Summary", width = 12, status = "info",
                  verbatimTextOutput("regionalSummaryStats")
                )
              )
      ),
      
      # Visualization 4: Choropleth Map
      tabItem(tabName = "viz4",
              h2("Global Health Indicators - Choropleth Map"),
              fluidRow(
                box(
                  title = "Map Controls", width = 3, status = "warning", solidHeader = TRUE,
                  sliderInput("year4", "Select Year:",
                              min = 2010, max = 2024, value = 2015, step = 5, sep = ""),
                  selectInput("indicator", "Select Indicator to Map:",
                              choices = c(
                                "Maternal mortality ratio (per 100k)" = "Maternal_mortality_ratio_deaths_per_100_000_population",
                                "Female life expectancy (years)" = "Life_expectancy_at_birth_for_females_years",
                                "Under-five mortality (per 1,000)" = "Under_five_mortality_rate_for_both_sexes_per_1_000_live_births",
                                "Fertility rate" = "Total_fertility_rate_children_per_women",
                                "Population growth (%)" = "Population_annual_rate_of_increase_percent",
                                "Life expectancy both sexes (years)" = "Life_expectancy_at_birth_for_both_sexes_years"
                              ),
                              selected = "Maternal_mortality_ratio_deaths_per_100_000_population"),
                  helpText("Explore global patterns of health indicators across countries.")
                ),
                box(
                  title = "Interactive World Map", width = 6, status = "primary", solidHeader = TRUE,
                  leafletOutput("map", height = 500)
                ),
                box(
                  title = "Interpretation", width = 3, status = "info", solidHeader = TRUE,
                  "This choropleth map visualizes health indicators across countries worldwide.",
                  br(), br(),
                  "Key Insights:",
                  tags$ul(
                    tags$li("Identify global patterns and regional disparities"),
                    tags$li("Compare countries' performance on selected indicators"),
                    tags$li("Spot geographic clusters of high/low values"),
                    tags$li("Understand global health inequalities"),
                    tags$li("Hover over countries for detailed values")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Map Legend & Information", width = 12, status = "info",
                  "The color intensity represents the value of the selected indicator. Darker colors indicate higher values for most indicators, 
            except for life expectancy where darker colors represent better outcomes. Gray areas indicate missing data."
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Value boxes for overview
  output$total_regions_box <- renderValueBox({
    valueBox(
      length(unique(clean_wide3$region_name)), "Regions Analyzed", 
      icon = icon("globe"), color = "green"
    )
  })
  
  output$year_range_box <- renderValueBox({
    valueBox(
      "2010-2024", "Data Period", 
      icon = icon("calendar"), color = "blue"
    )
  })
  
  output$data_sources_box <- renderValueBox({
    valueBox(
      "UN Data", "Data Source", 
      icon = icon("database"), color = "purple"
    )
  })
  
  # Visualization 1: Maternal Mortality vs Life Expectancy
  filtered_data1 <- reactive({
    req(input$year1, input$regions1)
    clean_wide3 %>%
      filter(Year == input$year1,
             region_name %in% input$regions1)
  })
  
  output$scatterPlot <- renderPlot({
    data <- filtered_data1()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters") +
               theme_void())
    }
    
    p <- ggplot(data, aes(x = Life_expectancy_at_birth_for_both_sexes_years,
                          y = Maternal_mortality_ratio_deaths_per_100_000_population,
                          color = region_name)) +
      geom_point(size = 4, alpha = 0.7) +
      labs(x = "Life Expectancy at Birth (years)",
           y = "Maternal Mortality Ratio (deaths per 100,000 population)",
           color = "Region") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if (input$trend_line1) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed")
    }
    
    if (input$label_points1) {
      p <- p + geom_text(aes(label = region_name), vjust = -0.5, hjust = 0.5, size = 3)
    }
    
    p
  })
  
  output$summaryStats <- renderPrint({
    data <- filtered_data1()
    
    if (nrow(data) == 0) {
      return("No data available")
    }
    
    cat("Selected Year:", input$year1, "\n")
    cat("Number of Regions:", length(unique(data$region_name)), "\n\n")
    
    cat("Life Expectancy:\n")
    cat("  Mean:", round(mean(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n")
    cat("  Min:", round(min(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n")
    cat("  Max:", round(max(data$Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE), 1), "\n\n")
    
    cat("Maternal Mortality:\n")
    cat("  Mean:", round(mean(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
    cat("  Min:", round(min(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
    cat("  Max:", round(max(data$Maternal_mortality_ratio_deaths_per_100_000_population, na.rm = TRUE), 1), "\n")
  })
  
  output$dataTable1 <- renderDataTable({
    data <- filtered_data1()
    if (nrow(data) > 0) {
      data %>% select(Region = region_name, Year, 
                      Life_Expectancy = Life_expectancy_at_birth_for_both_sexes_years,
                      Maternal_Mortality = Maternal_mortality_ratio_deaths_per_100_000_population)
    }
  })
  
  # Visualization 2: Time Series Analysis
  filtered_data2 <- reactive({
    req(input$region2)
    clean_wide3 %>%
      filter(region_name %in% input$region2,
             !is.na(Year))
  })
  
  ts_long <- reactive({
    req(input$vars)
    
    dat <- filtered_data2()
    all_names <- names(dat)
    indicator_cols <- character(0)
    
    if ("mmr" %in% input$vars) {
      idx <- grep("maternal.*mortality", all_names, ignore.case = TRUE)
      indicator_cols <- c(indicator_cols, all_names[idx])
    }
    if ("u5mr" %in% input$vars) {
      idx <- grep("under.*five.*mortality", all_names, ignore.case = TRUE)
      indicator_cols <- c(indicator_cols, all_names[idx])
    }
    if ("fle" %in% input$vars) {
      idx <- grep("life_expectancy.*females", all_names, ignore.case = TRUE)
      indicator_cols <- c(indicator_cols, all_names[idx])
    }
    
    indicator_cols <- unique(indicator_cols)
    
    if (length(indicator_cols) == 0) {
      return(tibble(
        Year = numeric(0),
        region_name = character(0),
        indicator = character(0),
        indicator_label = character(0),
        value = numeric(0)
      ))
    }
    
    dat %>%
      select(region_name, Year, all_of(indicator_cols)) %>%
      pivot_longer(
        cols = -c(Year, region_name),
        names_to = "indicator",
        values_to = "value"
      ) %>%
      mutate(
        indicator_label = case_when(
          grepl("maternal.*mortality", indicator, ignore.case = TRUE) ~
            "Maternal mortality ratio\n(per 100,000 live births)",
          grepl("under.*five.*mortality", indicator, ignore.case = TRUE) ~
            "Under-5 mortality rate\n(per 1,000 live births)",
          grepl("life_expectancy.*females", indicator, ignore.case = TRUE) ~
            "Female life expectancy at birth\n(years)",
          TRUE ~ indicator
        )
      )
  })
  
  output$ts_plot <- renderPlot({
    dat <- ts_long()
    
    validate(
      need(nrow(dat) > 0,
           "No data available for this selection (or indicators not found in dataset).")
    )
    
    yr_range <- range(dat$Year, na.rm = TRUE)
    region_label <- paste(input$region2, collapse = ", ")
    
    ggplot(dat,
           aes(x = Year,
               y = value,
               colour = region_name,
               group = interaction(region_name, indicator_label))) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      facet_wrap(~ indicator_label, scales = "free_y", ncol = 1) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      labs(
        title = paste0("Maternal health & survival in: ", region_label),
        subtitle = paste0(yr_range[1], "–", yr_range[2]),
        x = "Year",
        y = NULL
      )
  })
  
  # Visualization 3: Regional Comparison
  filtered_data3 <- reactive({
    req(input$year3, input$regions3)
    clean_wide3 %>%
      filter(Year == input$year3,
             region_name %in% input$regions3)
  })
  
  output$regionalPlot <- renderPlot({
    data <- filtered_data3()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters") +
               theme_void())
    }
    
    ggplot(data, aes(x = reorder(region_name, Life_expectancy_at_birth_for_both_sexes_years), 
                     y = Life_expectancy_at_birth_for_both_sexes_years,
                     fill = region_name)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(x = "Region", y = "Life Expectancy at Birth (years)",
           title = paste("Life Expectancy by Region -", input$year3)) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # NEW: Global Top 5 Countries Table
  output$top5Table <- renderTable({
    all_time_data <- clean_wide3 %>%
      group_by(region_name) %>%
      summarise(
        Avg_Life_Expectancy = mean(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        Data_Points = sum(!is.na(Life_expectancy_at_birth_for_both_sexes_years)),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Avg_Life_Expectancy), Data_Points > 0) %>%
      arrange(desc(Avg_Life_Expectancy)) %>%
      head(5) %>%
      mutate(
        Rank = 1:5,
        Country = region_name,
        `Life Expectancy` = round(Avg_Life_Expectancy, 1)
      ) %>%
      select(Rank, Country, `Life Expectancy`)
    
    all_time_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # NEW: Global Bottom 5 Countries Table
  output$bottom5Table <- renderTable({
    all_time_data <- clean_wide3 %>%
      group_by(region_name) %>%
      summarise(
        Avg_Life_Expectancy = mean(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        Data_Points = sum(!is.na(Life_expectancy_at_birth_for_both_sexes_years)),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Avg_Life_Expectancy), Data_Points > 0) %>%
      arrange(Avg_Life_Expectancy) %>%
      head(5) %>%
      mutate(
        Rank = (n_distinct(clean_wide3$region_name) - 4):n_distinct(clean_wide3$region_name),
        Country = region_name,
        `Life Expectancy` = round(Avg_Life_Expectancy, 1)
      ) %>%
      select(Rank, Country, `Life Expectancy`)
    
    all_time_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # UPDATED: Regional Summary Stats - now focuses only on current selection
  output$regionalSummaryStats <- renderPrint({
    data <- filtered_data3()
    
    if (nrow(data) == 0) {
      return("No data available for selected filters")
    }
    
    # Current selection statistics
    current_stats <- data %>%
      summarise(
        Mean = mean(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        Min = min(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        Max = max(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        Count = n()
      )
    
    # Calculate global average for context
    global_avg <- clean_wide3 %>%
      group_by(region_name) %>%
      summarise(
        Avg_Life_Expectancy = mean(Life_expectancy_at_birth_for_both_sexes_years, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      summarise(Global_Avg = mean(Avg_Life_Expectancy, na.rm = TRUE)) %>%
      pull(Global_Avg)
    
    performance_diff <- current_stats$Mean - global_avg
    
    cat("CURRENT SELECTION ANALYSIS\n")
    cat("==========================\n\n")
    
    cat("SELECTION OVERVIEW\n")
    cat("──────────────────\n")
    cat("Year:", input$year3, "\n")
    cat("Regions selected:", current_stats$Count, "\n")
    cat("Average life expectancy:", round(current_stats$Mean, 1), "years\n")
    cat("Range:", round(current_stats$Min, 1), "-", round(current_stats$Max, 1), "years\n")
    cat("Performance gap:", round(current_stats$Max - current_stats$Min, 1), "years\n\n")
    
    cat("GLOBAL CONTEXT\n")
    cat("──────────────\n")
    cat("Global average (all countries):", round(global_avg, 1), "years\n")
    cat("Your selection is ", 
        ifelse(performance_diff > 0, "above", "below"), 
        " global average by ", abs(round(performance_diff, 1)), " years\n\n", sep = "")
    
    
    
    # Performance distribution in current selection
    high_perf <- sum(data$Life_expectancy_at_birth_for_both_sexes_years > 75, na.rm = TRUE)
    medium_perf <- sum(data$Life_expectancy_at_birth_for_both_sexes_years >= 65 & 
                         data$Life_expectancy_at_birth_for_both_sexes_years <= 75, na.rm = TRUE)
    low_perf <- sum(data$Life_expectancy_at_birth_for_both_sexes_years < 65, na.rm = TRUE)
    
    cat("\nPERFORMANCE DISTRIBUTION IN SELECTION\n")
    cat("─────────────────────────────────────\n")
    cat("• High (>75 years):", high_perf, "regions\n")
    cat("• Medium (65-75 years):", medium_perf, "regions\n")
    cat("• Low (<65 years):", low_perf, "regions\n")
  })
  
  # Visualization 4: Choropleth Map
  # Reactive for map data
  map_data_reactive <- reactive({
    req(input$year4)
    
    world %>%
      left_join(
        map_data %>% filter(Year == input$year4),
        by = "region_name"
      )
  })
  
  # Observer to update year slider based on indicator selection
  observeEvent(input$indicator, {
    if (input$indicator == "Maternal_mortality_ratio_deaths_per_100_000_population") {
      updateSliderInput(
        session, "year4",
        min = 2010,
        max = 2020,
        value = 2020,
        step = 5
      )
    } else {
      updateSliderInput(
        session, "year4",
        min = 2010,
        max = 2024,
        value = 2024,
        step = 5
      )
    }
  })
  
  output$map <- renderLeaflet({
    dat <- map_data_reactive()
    var <- input$indicator
    
    # Create better labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s<br/>Year: %s",
      dat$region_name,
      names(which(choices == input$indicator)),
      ifelse(is.na(dat[[var]]), "No data", round(dat[[var]], 2)),
      input$year4
    ) %>% lapply(htmltools::HTML)
    
    # Create color palette
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = dat[[var]],
      na.color = "#f0f0f0",
      reverse = input$indicator %in% c("Life_expectancy_at_birth_for_females_years", 
                                       "Life_expectancy_at_birth_for_both_sexes_years")
    )
    
    leaflet(dat) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(dat[[var]]),
        weight = 0.6,
        color = "white",
        fillOpacity = 0.85,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = dat[[var]],
        position = "bottomright",
        title = names(which(choices == input$indicator)),
        opacity = 0.85,
        na.label = "No Data"
      )
  })
  
  # Define choices for the indicator names
  choices <- c(
    "Maternal mortality ratio (per 100k)" = "Maternal_mortality_ratio_deaths_per_100_000_population",
    "Female life expectancy (years)" = "Life_expectancy_at_birth_for_females_years",
    "Under-five mortality (per 1,000)" = "Under_five_mortality_rate_for_both_sexes_per_1_000_live_births",
    "Fertility rate" = "Total_fertility_rate_children_per_women",
    "Population growth (%)" = "Population_annual_rate_of_increase_percent",
    "Life expectancy both sexes (years)" = "Life_expectancy_at_birth_for_both_sexes_years"
  )
}

# Run the application
shinyApp(ui = ui, server = server)