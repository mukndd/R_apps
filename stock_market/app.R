library(shiny)
library(quantmod)

ui <- fluidPage(
    titlePanel("Stock Market Simulator"),
    sidebarLayout(
        sidebarPanel(
            textInput("stock", "Enter Stock Symbol (e.g., AAPL, GOOG):", value = "AAPL"),
            dateRangeInput("dateRange", "Select Date Range:",
                           start = Sys.Date() - 30, end = Sys.Date()),
            actionButton("getData", "Get Stock Data"),
            br(),
            textOutput("status")
        ),
        mainPanel(
            plotOutput("stockPlot"),
            tableOutput("stockTable")
        )
    )
)

server <- function(input, output, session) {
    stockData <- reactiveVal(NULL)

    observeEvent(input$getData, {
        tryCatch({
            stockData(getSymbols(input$stock, src = "yahoo", from = input$dateRange[1],
                                 to = input$dateRange[2], auto.assign = FALSE))
        }, error = function(e) {
            stockData(NULL)
            showNotification("Failed to fetch stock data. Check symbol or internet connection.", type = "error")
        })
    })

    output$status <- renderText({
        if (is.null(stockData())) "No data fetched yet." else "Data fetched successfully!"
    })

    output$stockPlot <- renderPlot({
        if (!is.null(stockData())) chartSeries(stockData(), name = input$stock)
    })

    output$stockTable <- renderTable({
        if (!is.null(stockData())) head(stockData(), 10)
    })
}

shinyApp(ui, server)
