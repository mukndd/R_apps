# Load required libraries
library(shiny)
library(neuralnet)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Neural Network Prediction on mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("hidden_nodes", "Number of Hidden Nodes:", min = 1, max = 10, value = 5),
      sliderInput("train_split", "Training Data Split (%):", min = 50, max = 90, value = 70),
      actionButton("train", "Train Neural Network"),
      textInput("user_input", "Enter a custom comment or observation:", value = ""),
      textOutput("user_comment")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$train, {
    # Prepare the dataset
    data <- mtcars
    data <- scale(data)  # Normalize the data
    
    # Split into training and testing sets
    set.seed(123)
    train_indices <- sample(1:nrow(data), size = round(input$train_split / 100 * nrow(data)))
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
    # Train the neural network
    formula <- as.formula("mpg ~ .")
    nn <- neuralnet(formula, data = as.data.frame(train_data), hidden = input$hidden_nodes, linear.output = TRUE)
    
    # Make predictions
    test_features <- as.data.frame(test_data[, -1])
    predictions <- compute(nn, test_features)$net.result
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Actual = test_data[, "mpg"],
      Predicted = predictions
    )
    
    # Render the plot
    output$plot <- renderPlot({
      ggplot(plot_data, aes(x = Actual, y = Predicted)) +
        geom_point(color = "blue") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Actual vs. Predicted MPG", x = "Actual MPG", y = "Predicted MPG") +
        theme_minimal()
    })
  })
  
  # Display the user's comment
  output$user_comment <- renderText({
    if (input$user_input != "") {
      paste("Your observation:", input$user_input)
    } else {
      "Enter a comment or observation to see it here."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
