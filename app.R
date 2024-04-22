library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header     header     header   ",
    "filter     emissions  emissions",
    "timeseries timeseries removals ",
    "timeseries timeseries removals "
  ),
  row_sizes = c(
    "45px",
    "1.72fr",
    "0.28fr",
    "1fr"
  ),
  col_sizes = c(
    "200px",
    "0.8fr",
    "1.2fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "filter",
    card_header(
      "Filter",
      selectInput(
        inputId = "Select_GHG",
        label = "GHG",
        choices = list("All" = "All", "CO2" = "CO2", "CH4" = "CH4", "N2O" = "N2O")
      )
    ),
    card_body(
      sliderInput(
        inputId = "bins",
        label = "Number of Bins",
        min = 12,
        max = 100,
        value = 30,
        width = "100%"
      ),
      numericInput(
        inputId = "numRows",
        label = "Number of table rows",
        value = 10,
        min = 1,
        step = 1,
        width = "100%"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "UK Greenhouse Gas Inventory",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "timeseries",
    full_screen = TRUE,
    card_header("Time Series - Net Emissions by Sector"),
    card_body(plotlyOutput(outputId = "plot"))
  ),
  grid_card(
    area = "removals",
    full_screen = TRUE,
    card_header("Negative Emissions (Removals)"),
    card_body(plotlyOutput(outputId = "plot"))
  ),
  grid_card(
    area = "emissions",
    full_screen = TRUE,
    card_header("Greenhouse Gas Emissions"),
    card_body(plotlyOutput(outputId = "plot"))
  )
)


server <- function(input, output) {
   
  
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  
}

shinyApp(ui, server)
  
