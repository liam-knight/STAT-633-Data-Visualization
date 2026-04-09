library(tidyverse)
library(plotly)
library(shiny)
library(readr)

#country names that were omitted from the example app:
not_countries <- c("Africa Eastern and Southern",
                   "Africa Western and Central",
                   "Arab World",
                   "Central Europe and the Baltics",
                   "Caribbean small states",
                   "East Asia & Pacific (excluding high income)",
                   "Early-demographic dividend",
                   "East Asia & Pacific",
                   "Europe & Central Asia (excluding high income)",
                   "Europe and Central Asia",
                   "Euro area",
                   "European Union",
                   "Fragile and conflict affected situations",
                   "High income",
                   "Heavily indebted poor countries (HIPC)",
                   "IBRD only",
                   "IDA & IBRD total",
                   "IDA total",
                   "IDA blend",
                   "IDA only",
                   "Not classified",
                   "Latin America & Caribbean (excluding high income)",
                   "Lao PDR",
                   "Latin America & Caribbean",
                   "Least developed countries: UN classification",
                   "Low income",
                   "Lower middle income",
                   "Low & middle income",
                   "Late-demographic dividend",
                   "Middle East & North Africa",
                   "Middle income",
                   "North Macedonia",
                   "Middle East & North Africa (excluding high income)",
                   "North America",
                   "OECD members",
                   "Other small states",
                   "Pre-demographic dividend",
                   "Pacific island small states",
                   "Post-demographic dividend",
                   "South Asia",
                   "Sub-Saharan Africa (excluding high income)",
                   "Sub-Saharan Africa",
                   "Small states",
                   "Syrian Arab Republic",
                   "East Asia & Pacific (IDA & IBRD countries)",
                   "Europe & Central Asia (IDA & IBRD countries)",
                   "Latin America & the Caribbean (IDA & IBRD countries)",
                   "Middle East & North Africa (IDA & IBRD countries)",
                   "South Asia (IDA & IBRD)",
                   "Sub-Saharan Africa (IDA & IBRD countries)",
                   "Upper middle income",
                   "World",
                   "Andorra",
                   "American Samoa",
                   "Channel Islands",
                   "Faroe Islands",
                   "Guam",
                   "Isle of Man",
                   "Monaco",
                   "St. Martin (French part)",
                   "Marshall Islands",
                   "Northern Mariana Islands",
                   "French Polynesia",
                   "Puerto Rico",
                   "Somalia",
                   "Turks and Caicos Islands",
                   "Argentina")

#read and wrangle data
cpi <- 
  read_csv("data/cpi_world.csv", na = c("", ".."), skip = 4) %>%
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  rename(Country = `Country Name`) %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "CPI") %>%
  mutate(Year = as.integer(str_extract(string = Year, pattern = "\\d\\d\\d\\d"))) %>%
  #remove observations that are not in example app
  filter(!Country %in% not_countries)


ui <- fluidPage(
  titlePanel("Homework 7, Question 2"),
  sidebarLayout(
    sidebarPanel(
      "",
      #select "Countries"
      selectizeInput(
        inputId = "countries",
        label = "Countries",
        choices = cpi$Country,
        selected = c(
          "United States",
          "China",
          "Russian Federation",
          "Germany",
          "Denmark",
          "United Kingdom",
          "Portugal",
          "Finland",
          "Malta")
        ,
        multiple = TRUE
      ),
      #slider for "Year"
      sliderInput(
        inputId = "year",
        label = "Base Year",
        min = min(cpi$Year, na.rm = TRUE),
        max = max(cpi$Year, na.rm = TRUE),
        value = 2010,
        step = 1,
        sep = ""
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "cpi_plot"),
      htmlOutput("warn_msg")
    )
  )
)

server <- function(input, output, session) {
  output$cpi_plot <- renderPlotly({
    #store input selections
    selected_year <- input$year
    selected_countries <- input$countries
    
    #filter for selected countries
    df <- cpi %>%
      filter(Country %in% selected_countries)
    
    #get CPIs for base years
    base_cpis <- df %>%
      filter(Year == selected_year) %>%
      select(Country, CPI_base = CPI)
    
    #find the missing values/NAs
    na_rows <- base_cpis[is.na(base_cpis$CPI_base), ]
    #if missing values, get message 
    if (nrow(na_rows) > 0) {
      output$warn_msg <- renderUI({
        HTML(
          paste0(
            "<b>Warning</b><br/>", "The following countries were omitted from the plot as they do not have CPI values for the base year ", 
            selected_year, 
            ":<br/>",
            paste(na_rows$Country, collapse = ", "),
            ".")
        )
      }) 
    } else {
      output$warn_msg <- renderUI(NULL)
    }
    
    #drop NAs
    base_cpis_no_NAs <- base_cpis %>%
      filter(!is.na(CPI_base))
    
    #join and get index for each CPI
    df1 <- df %>%
      left_join(base_cpis_no_NAs, by = "Country") %>%
      mutate(CPI_index = (CPI/CPI_base)*100)
    
    #get limits for black dotted lines
    x_range <- range(df1$Year, na.rm = TRUE)
    y_range <- range(df1$CPI_index, na.rm = TRUE)
    y_min_for_vline <- min(0, y_range[1])
    y_max_for_vline <- max(100, y_range[2])
    
    #plotly line plot
    plot_ly(
      df1,
      x = ~Year,
      y = ~CPI_index,
      color = ~Country,
      type = "scatter",
      mode = "lines"
    ) %>%
      #black dot
      add_trace(
        x = selected_year,
        y = 100,
        type = "scatter",
        mode = "markers",
        marker = list(color = "black", size = 10),
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste0("(", selected_year, ", 100)")
      ) %>%
      #horizontal dotted line
      add_trace(
        x = c(1960, selected_year),
        y = c(100, 100),
        type = "scatter",
        mode = "lines",
        line = list(color = "black", dash = "dot", width = 1),
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      #vertical dotted line 
      add_trace(
        x = c(selected_year, selected_year),
        y = c(0, 100),
        type = "scatter",
        mode = "lines",
        line = list(color = "black", dash = "dot", width = 1),
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = paste0("CPI (", selected_year, " = 100)"))
      )
    
    
    
  })
}

shinyApp(ui, server)

