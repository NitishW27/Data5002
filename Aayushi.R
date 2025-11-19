# app.R -------------------------------------------------------------

# ---- libraries ----
library(shiny)
library(tidyverse)

# ---- data ----
clean_wide3 <- read.csv("clean_wide3.csv")
clean_wide3$Year <- as.numeric(clean_wide3$Year)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Female-focused mortality & life expectancy – time series"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label   = "Select country/area:",
        choices = sort(unique(clean_wide3$region_name)),
        selected = sort(unique(clean_wide3$region_name))[1]
      ),
      
      checkboxGroupInput(
        inputId = "vars",
        label   = "Indicators to show:",
        choices = c(
          "Maternal mortality ratio" = "mmr",
          "Under-5 mortality rate"   = "u5mr",
          "Female life expectancy"   = "fle"
        ),
        selected = c("mmr", "fle")
      )
    ),
    
    mainPanel(
      plotOutput("ts_plot", height = "500px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    clean_wide3 %>%
      filter(region_name == input$region,
             !is.na(Year))
  })
  
  ts_long <- reactive({
    req(input$vars)
    
    dat <- filtered_data()
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
        indicator = character(0),
        indicator_label = character(0),
        value = numeric(0)
      ))
    }
    
    dat %>%
      select(Year, all_of(indicator_cols)) %>%
      pivot_longer(
        cols      = -Year,
        names_to  = "indicator",
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
           "No data available for this country / selection (or indicators not found in dataset).")
    )
    
    yr_range <- range(dat$Year, na.rm = TRUE)
    
    ggplot(dat, aes(x = Year, y = value,
                    colour = indicator_label,
                    group = indicator_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      labs(
        title    = paste0("Maternal health & survival in ", input$region),
        subtitle = paste0(yr_range[1], "–", yr_range[2]),
        x = "Year",
        y = NULL
      )
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
