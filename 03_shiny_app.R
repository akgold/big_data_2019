library(dygraphs)
library(xts)
library(shinythemes)
library(shiny)
library(dplyr)

con <- spark_connect(master = "local")
df <- tbl(con, "stocks")
p <- tbl(con, "preds")

df <- inner_join(df, p)

choices <- tibble::tibble(
    names = c("Financial Services",
              "Gold and Silver",
              "Merchandising",
              "Metals and Minerals",
              "Oil and Gas",
              "Paper and Forest Products",
              "Utilities"),
    series = c("financial_services",
               "gold_and_silver",
               "merchandising",
               "metals_and_minerals",
               "oil_and_gas",
               "paper_and_forest_products",
               "utilities")
)

ui <- fluidPage(
    theme = shinytheme("paper"),
    titlePanel("Stock Prices"),
    sidebarLayout(
        sidebarPanel(
            selectInput("series", "Stock Series",
                        setNames(choices$series, choices$names))
        ),
        mainPanel(align="center",
                  dygraphOutput("dygraph", width = "75%", height = "500px"),
                  p(),
                  verbatimTextOutput("dateRangeText")
        )
    )
)

server <- function(input, output) {

    # Get the right series
    dat <- reactive({
        df %>%
            select(date, real = input$series, pred = !!paste0(input$series, "_pred")) %>%
            collect()
    })

    # Turn into time series
    tseries <- reactive({
        cbind(xts(dat()$real, dat()$date),
              xts(dat()$pred, dat()$date))
    })

    # Create Label
    lab <- reactive({
        glue::glue("{choices$names[choices$series == input$series]} Prices over Time")
    })

    output$dygraph <- renderDygraph({
        dygraph(tseries(), main = lab()) %>%
            dySeries("xts.dat...real..dat...date.",
                     label = "Real",
                     color = rgb(70, 130, 180, maxColorValue = 255)) %>%
            dySeries("xts.dat...pred..dat...date.",
                     label = "Predicted",
                     color = "#52AA5E") %>%
            dyOptions(axisLineWidth = 1.5,
                      drawGrid = FALSE,
                      axisLineColor = "darkgrey",
                      axisLabelFontSize = 15) %>%
            dyRangeSelector(fillColor = "lightsteelblue", strokeColor = "white")
    })

    output$asof <- renderText({
        paste("Data as of:", as.character(max(dat()$date)))
    })

}

shinyApp(ui = ui, server = server)
