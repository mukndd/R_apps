# Load Shiny library
library(shiny)

# Define UI for the application
ui <- fluidPage(
  titlePanel("BMI Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("weight", "Enter your weight (kg):", value = 70, min = 1),
      numericInput("height", "Enter your height (cm):", value = 170, min = 1),
      numericInput("age", "Enter your age (years):", value = 25, min = 1),
      numericInput("activity", "Enter your activity level (1-5):", value = 3, min = 1, max = 5),
      actionButton("calculate", "Calculate BMI")
    ),
    mainPanel(
      textOutput("bmi"),
      textOutput("category"),
      textOutput("suggestion"),
      textOutput("calories")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    weight <- input$weight
    height <- input$height / 100  # Convert height to meters
    age <- input$age
    activity <- input$activity
    
    bmi <- weight / (height^2)
    category <- ifelse(bmi < 18.5, "Underweight",
                       ifelse(bmi < 24.9, "Normal weight",
                              ifelse(bmi < 29.9, "Overweight", "Obese")))
    
    suggestion <- ifelse(category == "Underweight", "Consider a balanced diet with more calories.",
                         ifelse(category == "Normal weight", "Keep up the good work! Maintain a healthy lifestyle.",
                                ifelse(category == "Overweight", "Incorporate more physical activity into your routine.",
                                       "Consult a healthcare provider for personalized advice.")))
    
    # Calculate calorie ranges
    bmr <- ifelse(age < 18, 0, (10 * weight) + (6.25 * input$height) - (5 * age) + 5)  # Simplified Mifflin-St Jeor for males
    calories_maintain <- bmr * c(1.2, 1.375, 1.55, 1.725, 1.9)[activity]
    calories_gain <- calories_maintain + 500
    calories_deficit <- calories_maintain - 500
    
    calorie_text <- paste(
      "To maintain weight: ", round(calories_maintain, 0), "kcal/day\n",
      "To gain weight: ", round(calories_gain, 0), "kcal/day\n",
      "To lose weight: ", round(calories_deficit, 0), "kcal/day",
      sep = "")
    
    output$bmi <- renderText({ paste("Your BMI: ", round(bmi, 1)) })
    output$category <- renderText({ paste("Category: ", category) })
    output$suggestion <- renderText({ paste("Suggestion: ", suggestion) })
    output$calories <- renderText({ calorie_text })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
