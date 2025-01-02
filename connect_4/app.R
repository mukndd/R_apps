library(shiny)

# UI for Connect 4 Game
ui <- fluidPage(
  titlePanel("Connect 4 Game"),
  fluidRow(
    column(12,
           plotOutput("board", height = "600px", click = "board_click"),
           h4("Game Status:"),
           textOutput("status"),
           actionButton("reset", "Restart Game")
    )
  )
)

# Server for Connect 4 Game
server <- function(input, output, session) {
  board <- matrix(0, nrow = 6, ncol = 7)
  game_state <- reactiveValues(
    board = board,
    current_player = 1,
    status = "Player 1's Turn"
  )

  output$board <- renderPlot({
    plot(NULL, xlim = c(0.5, 7.5), ylim = c(0.5, 6.5), xlab = "", ylab = "", axes = FALSE)
    grid(nx = 7, ny = 6, col = "black")
    for (i in 1:6) {
      for (j in 1:7) {
        if (game_state$board[i, j] == 1) {
          symbols(j, i, circles = 0.4, bg = "red", add = TRUE, inches = FALSE)
        } else if (game_state$board[i, j] == 2) {
          symbols(j, i, circles = 0.4, bg = "yellow", add = TRUE, inches = FALSE)
        }
      }
    }
  })

  observeEvent(input$board_click, {
    col <- round(input$board_click$x)
    if (col < 1 || col > 7) return()
    
    row <- max(which(game_state$board[, col] == 0))
    if (is.na(row)) return()

    game_state$board[row, col] <- game_state$current_player

    if (check_win(game_state$board, game_state$current_player)) {
      game_state$status <- paste("Player", game_state$current_player, "Wins!")
    } else if (all(game_state$board != 0)) {
      game_state$status <- "It's a Draw!"
    } else {
      game_state$current_player <- 3 - game_state$current_player
      game_state$status <- paste("Player", game_state$current_player, "'s Turn")
    }
  })

  output$status <- renderText({ game_state$status })

  observeEvent(input$reset, {
    game_state$board <- matrix(0, nrow = 6, ncol = 7)
    game_state$current_player <- 1
    game_state$status <- "Player 1's Turn"
  })
}

# Function to Check for Wins
check_win <- function(board, player) {
  directions <- list(
    c(1, 0), c(0, 1), c(1, 1), c(1, -1)
  )
  for (dir in directions) {
    dx <- dir[1]
    dy <- dir[2]
    for (x in 1:6) {
      for (y in 1:7) {
        if (board[x, y] == player) {
          count <- 1
          for (step in 1:3) {
            nx <- x + dx * step
            ny <- y + dy * step
            if (nx >= 1 && nx <= 6 && ny >= 1 && ny <= 7 && board[nx, ny] == player) {
              count <- count + 1
            } else {
              break
            }
          }
          if (count == 4) return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

shinyApp(ui = ui, server = server)
