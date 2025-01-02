library(shiny)

# UI definition remains the same
ui <- fluidPage(
  titlePanel("Card Trick Game"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("startGame", "Start New Game"),
      uiOutput("gameControls")
    ),
    
    mainPanel(
      h3("Card Trick Game"),
      verbatimTextOutput("cardDisplay"),
      verbatimTextOutput("gameStatus")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values to store game state
  gameState <- reactiveValues(
    original_card_list = c("clubs_king", "hearts_king", "spades_king", "diamonds_king",
                           "clubs_queen", "hearts_queen", "spades_queen", "diamonds_queen"),
    current_step = 0,
    left_cards = c(),
    right_cards = c(),
    colors = list(spades="black", clubs="black", hearts="red", diamonds="red")
  )
  
  # Start new game
  observeEvent(input$startGame, {
    gameState$current_step <- 1
    gameState$original_card_list <- c("clubs_king", "hearts_king", "spades_king", "diamonds_king",
                                      "clubs_queen", "hearts_queen", "spades_queen", "diamonds_queen")
  })
  
  # Dynamic UI controls based on game state
  output$gameControls <- renderUI({
    if (gameState$current_step == 0) {
      return(NULL)
    }
    
    if (gameState$current_step == 1) {
      return(
        div(
          p("Choose a card from the list above (don't tell me which one!)"),
          actionButton("chosen", "I've chosen a card")
        )
      )
    }
    
    if (gameState$current_step %in% c(2,3,4)) {
      return(
        div(
          p("Do you see your chosen card in this list?"),
          actionButton("yesButton", "Yes"),
          actionButton("noButton", "No")
        )
      )
    }
  })
  
  # Display cards and game status
  output$cardDisplay <- renderText({
    if (gameState$current_step == 0) {
      return("Press 'Start New Game' to begin")
    }
    
    if (gameState$current_step == 1) {
      return(paste("Available cards:", paste(gameState$original_card_list, collapse = ", ")))
    }
    
    if (gameState$current_step %in% c(2,3,4)) {
      # Split cards into left and right
      n <- length(gameState$original_card_list)
      even_indices <- seq(2, n, 2)
      odd_indices <- seq(1, n, 2)
      
      if (gameState$current_step == 3) {
        gameState$left_cards <- rev(gameState$original_card_list[odd_indices])
        gameState$right_cards <- rev(gameState$original_card_list[even_indices])
      } else {
        gameState$left_cards <- gameState$original_card_list[odd_indices]
        gameState$right_cards <- gameState$original_card_list[even_indices]
      }
      
      return(paste("Cards to show:", paste(gameState$left_cards, collapse = ", ")))
    }
    
    if (gameState$current_step == 5) {
      # Final reveal logic - recreating Python's exact logic
      n <- length(gameState$original_card_list)
      
      # First split into list1 and list2
      list1 <- c()
      list2 <- c()
      list3 <- c()
      
      # Replicate Python's index-based splitting
      for (i in seq_along(gameState$original_card_list)) {
        if ((i-1) %% 2 == 0) {
          list1 <- c(list1, gameState$original_card_list[i])
        } else {
          list2 <- c(list2, gameState$original_card_list[i])
        }
      }
      
      # Split list1 again to get list3
      for (i in seq_along(list1)) {
        if ((i-1) %% 2 == 0) {
          list3 <- c(list3, list1[i])
        }
      }
      
      # Get final card - it's the first card in list3
      
      
      # Get suit from SECOND card in list3
      suit_card <- list3[2]
      suit <- strsplit(suit_card, "_")[[1]][1]
      
      # Get gender from first card in list2
      card_parts <- strsplit(list2[1], "_")[[1]]
      gender <- card_parts[2]
      gender <- if(gender == "king") "queen" else "king"
      # Get color based on suit
      color <- gameState$colors[[suit]]
      final_card <- paste0(suit, "_", gender)
      return(paste0(
        "Here's what I know about your card:\n",
        "Gender: ", gender, "\n",
        "Color: ", color, "\n",
        "Suit: ", suit, "\n",
        "Your card was: ", final_card
      ))
    }
  })
  
  # Handle user responses
  observeEvent(input$chosen, {
    gameState$current_step <- 2
  })
  
  observeEvent(input$yesButton, {
    if (gameState$current_step <= 4) {
      if (gameState$current_step == 3) {
        gameState$original_card_list <- c(gameState$left_cards, gameState$right_cards)
      } else {
        gameState$original_card_list <- c(gameState$right_cards, gameState$left_cards)
      }
      gameState$current_step <- gameState$current_step + 1
    }
  })
  
  observeEvent(input$noButton, {
    if (gameState$current_step <= 4) {
      if (gameState$current_step == 3) {
        gameState$original_card_list <- c(gameState$right_cards, gameState$left_cards)
      } else {
        gameState$original_card_list <- c(gameState$left_cards, gameState$right_cards)
      }
      gameState$current_step <- gameState$current_step + 1
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)