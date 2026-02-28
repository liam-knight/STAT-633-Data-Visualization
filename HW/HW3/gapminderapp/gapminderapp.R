library(tidyverse)

df <- gapminder::gapminder

ui <- fluidPage(
  titlePanel("Gapminder"),
  sidebarLayout(
    sidebarPanel(
      "Interactive plotting of gapminder data using R shiny",
      #checklist for "Choose a continent"
      checkboxGroupInput(
        inputId = "continent",
        label = "Choose a continent",
        choices = levels(df$continent),
        selected = levels(df$continent)
      ),
      #slider for "Income Percentiles of interest"
      sliderInput(
        inputId = "percentile",
        label = "Income percentiles of interest",
        min = 0,
        max = 100,
        value = c(0,100)
      ),
      #slider for "Year"
      sliderInput(
        inputId = "year",
        label = "Year",
        min = 1952,
        max = 2007,
        value = 1952,
        step = 5,
        sep = "",
        animate = animationOptions(interval = 500, loop = FALSE)
      )
    ),
    mainPanel(
      plotOutput(outputId = "gapminder_plot")
    )
  )
)

server <- function(input, output) {
  output$gapminder_plot <- renderPlot({
    #store input selections
    selected_year <- input$year
    selected_continents <- input$continent    
    selected_percentile <- input$percentile
    
    df |>
      #group by year so that percentiles are recalculated based on the dataset
      #for each year, rather than GDPs for 1952 being compared against
      #GDPs for 2007, for example
      group_by(year) |>
      #create new variable for percentile
      mutate(percentile = 100 * percent_rank(gdpPercap)) |>
      ungroup() |>
      #filter by input selections
      filter(year == selected_year) |>          
      filter(continent %in% selected_continents) |> 
      filter(percentile <= selected_percentile) |>
      ggplot(aes(gdpPercap, lifeExp, color = continent)) +
      scale_x_log10(limits = c(1e2, 1e5)) +
      geom_point(aes(size = pop)) +
      ylim(0, 80) +
      theme(legend.title = element_blank())
  })
}

shinyApp(ui, server)