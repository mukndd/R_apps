library(shiny)

ui <- fluidPage(
    titlePanel("Connect 4 Game"),
    sidebarLayout(
        sidebarPanel(
            actionButton("reset", "Reset Game"),
            textOutput("turnText"),
            br(),
            textOutput("statusText")
        ),
        mainPanel(
            plotOutput("gameBoard", height = "500px", click = "plot_click")
        )
    )
)

server <- function(input, output, session) {
    board <- matrix(0, nrow = 6, ncol = 7)
    turn <- reactiveVal(1)

    reset_board <- function() {
        board <<- matrix(0, nrow = 6, ncol = 7)
        turn(1)
    }

    output$turnText <- renderText({
        paste("Player", turn(), "'s turn")
    })

    output$gameBoard <- renderPlot({
        par(mar = c(2, 2, 2, 2))
        image(t(board)[, nrow(board):1], col = c("white", "red", "yellow"), axes = FALSE)
        abline(h = seq(0, 1, length.out = nrow(board) + 1), v = seq(0, 1, length.out = ncol(board) + 1), col = "black")
    })

    observeEvent(input$plot_click, {
        x <- floor(input$plot_click$x * ncol(board)) + 1
        if (x >= 1 && x <= ncol(board)) {
            y <- which(board[, x] == 0)[1]
            if (!is.na(y)) {
                board[y, x] <- turn()
                turn(ifelse(turn() == 1, 2, 1))
            }
        }
    })

    observeEvent(input$reset, {
        reset_board()
    })
}

shinyApp(ui, server)
