library(shiny)
library(tidyverse)

combined <- readr::read_csv("final_data.csv",show_col_types = FALSE)

year_min    <- min(combined$Year, na.rm = TRUE)
year_max    <- max(combined$Year, na.rm = TRUE)
age_choices <- sort(unique(combined$Age_group))
sex_choices <- sort(unique(combined$Sex))

ui <- fluidPage(
  titlePanel("Prince Edward Island Population Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sex",
        label   = "Select sex:",
        choices = sex_choices,
        selected = "Total"
      ),
      selectInput(
        inputId = "age_group",
        label   = "Select age group (for trend plot):",
        choices = age_choices,
        selected = "All ages"
      ),
      sliderInput(
        inputId = "year",
        label   = "Select year (for age distribution):",
        min     = year_min,
        max     = year_max,
        value   = year_max,
        step    = 1,
        sep     = ""   
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend over time",
                 plotOutput("trendPlot")
        ),
        tabPanel("Age distribution in a year",
                 plotOutput("ageDistPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  trend_data <- reactive({
    combined %>%
      filter(
        Sex == input$sex,
        Age_group == input$age_group
      ) %>%
      arrange(Year)
  })
  
  output$trendPlot <- renderPlot({
    df <- trend_data()
    validate(
      need(nrow(df) > 0, "No data for this combination of sex and age group.")
    )
    
    ggplot(df, aes(x = Year, y = Population)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      labs(
        title = paste0("Population Trend (", input$sex, ", ", input$age_group, ")"),
        x = "Year",
        y = "Population"
      ) +
      theme_minimal(base_size = 14)
  })
  
  age_dist_data <- reactive({
    combined %>%
      filter(
        Sex == input$sex,
        Year == input$year,
        !Age_group %in% c("All ages", "Median age") 
      ) %>%
      arrange(Population)
  })
  
  output$ageDistPlot <- renderPlot({
    df <- age_dist_data()
    validate(
      need(nrow(df) > 0, "No age-group data for this year and sex.")
    )
    
    ggplot(df, aes(x = reorder(Age_group, Population), y = Population)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste0("Age Distribution in ", input$year,
                       " (", input$sex, ")"),
        x = "Age group",
        y = "Population"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
