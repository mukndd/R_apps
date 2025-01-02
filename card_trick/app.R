library(shiny)

ui <- fluidPage(
    titlePanel("Card Trick"),
    sidebarLayout(
        sidebarPanel(
            selectInput("suit", "Choose a Suit:", choices = c("Hearts", "Diamonds", "Clubs", "Spades")),
            sliderInput("rank", "Choose a Card Rank (1 = Ace, 13 = King):", min = 1, max = 13, value = 1),
            actionButton("reveal", "Reveal My Card"),
            textOutput("magicOutput")
        ),
        mainPanel(
            h4("Your Card:"),
            textOutput("cardResult"),
            br(),
            plotOutput("magicPlot")  # A little visual flair
        )
    )
)

server <- function(input, output, session) {
    card_name <- reactive({
        rank <- switch(as.character(input$rank),
                       "1" = "Ace", "11" = "Jack", "12" = "Queen", "13" = "King", as.character(input$rank))
        paste(rank, "of", input$suit)
    })

    output$cardResult <- renderText({
        if (input$reveal > 0) card_name() else "Choose your card and click Reveal!"
    })

    output$magicOutput <- renderText({
        if (input$reveal > 0) "I knew your card all along!"
    })

    output$magicPlot <- renderPlot({
        if (input$reveal > 0) {
            barplot(rep(1, 13), names.arg = 1:13, col = "skyblue",
                    main = paste("You Chose:", card_name()))
        }
    })
}

shinyApp(ui, server)
