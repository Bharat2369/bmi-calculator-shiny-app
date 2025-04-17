# Load required packages
library(shiny)
library(shinythemes)

# --- UI Section --- #
ui <- fluidPage(
  # Apply a modern theme ("flatly") to your app
  theme = shinytheme("flatly"),

  # Custom CSS to style the title, sidebar, buttons, and text output
  tags$head(
    tags$style(HTML("
      .title {
        text-align: center;
        font-size: 28px;
        color: #2c3e50;
        margin-top: 20px;
      }
      .well {
        background-color: #f9f9f9;
        border: none;
      }
      .btn-primary {
        background-color: #2c3e50;
        border-color: #2c3e50;
      }
      .btn-primary:hover {
        background-color: #1a242f;
        border-color: #1a242f;
      }
      #bmi {
        font-size: 20px;
        font-weight: bold;
      }
    "))
  ),

  # Title of the app
  titlePanel(div("Enhanced BMI Calculator", class = "title")),

  # Layout that divides the page into a sidebar (for inputs) and a main panel (for outputs)
  sidebarLayout(
    # Sidebar: where the user enters their data
    sidebarPanel(
      numericInput("weight", "Enter your weight (kg):", value = 70, min = 30, max = 200),
      numericInput("height", "Enter your height (cm):", value = 170, min = 100, max = 250),
      selectInput("gender", "Select your gender:", choices = c("Male", "Female")),
      actionButton("calc", "Calculate BMI", class = "btn-primary")
    ),

    # Main panel: where the results are shown
    mainPanel(
      h3("Your BMI Result"),
      verbatimTextOutput("bmi"),   # This text output will show your BMI and category
      plotOutput("bmiPlot"),       # This plot output will display a bar chart of BMI categories

      # Tabs for Documentation and About info
      tabsetPanel(
        tabPanel("Documentation",
                 h4("How to Use the BMI Calculator"),
                 p("1. Enter your weight (in kg) and height (in cm) in the boxes on the left."),
                 p("2. Select your gender (this is for potential future customization)."),
                 p("3. Click the 'Calculate BMI' button to compute your BMI."),
                 p("4. Your BMI result and a visual bar chart showing standard BMI categories will be displayed on the right.")
        ),
        tabPanel("About",
                 h4("About This Application"),
                 p("This BMI Calculator was built using R and the Shiny framework."),
                 p("It uses the 'shinythemes' package for a modern, clean look, and the code dynamically calculates your BMI using the formula:"),
                 p("BMI = weight (kg) / [height (m)]^2"),
                 p("The app is designed to be intuitive and easy to use, with full documentation available right inside the app.")
        )
      )
    )
  )
)

# --- Server Section --- #
server <- function(input, output, session) {

  # Calculate BMI only when the "Calculate BMI" button is clicked.
  bmi <- eventReactive(input$calc, {
    weight <- input$weight              # Get the entered weight
    height_m <- input$height / 100       # Convert height from centimeters to meters
    bmi_value <- weight / (height_m^2)    # Compute BMI (weight divided by height squared)
    round(bmi_value, 2)                 # Round to 2 decimal places
  })

  # Display the computed BMI and its weight category as text.
  output$bmi <- renderText({
    bmi_value <- bmi()  # Get the computed BMI
    if (is.null(bmi_value)) {
      return("Press the 'Calculate BMI' button to compute your BMI.")
    }

    # Determine the weight category based on the BMI value
    category <- ifelse(bmi_value < 18.5, "Underweight",
                       ifelse(bmi_value < 25, "Normal weight",
                              ifelse(bmi_value < 30, "Overweight", "Obese")))

    paste("Your BMI is", bmi_value, "which places you in the", category, "category.")
  })

  # Create a bar chart that shows standard BMI categories and where your BMI falls.
  output$bmiPlot <- renderPlot({
    bmi_value <- bmi()
    if (is.null(bmi_value)) return(NULL)

    # Define the BMI categories and cutoff values
    categories <- c("Underweight", "Normal", "Overweight", "Obese")
    cutoff <- c(0, 18.5, 25, 30, 50)  # Define boundaries between categories
    category_colors <- c("skyblue", "lightgreen", "yellow", "tomato")

    # Calculate the height of each category's bar
    bar_heights <- diff(cutoff)

    # Draw the bar plot with labels and colors
    bp <- barplot(
      bar_heights,
      names.arg = categories,
      col = category_colors,
      main = "BMI Categories",
      ylab = "BMI Value",
      ylim = c(0, max(cutoff))
    )

    # Draw a horizontal line at the user's BMI value
    abline(h = bmi_value, col = "blue", lwd = 2)
    # Add a label next to the line showing the BMI value
    text(x = max(bp), y = bmi_value, labels = paste("Your BMI:", bmi_value),
         pos = 3, col = "blue")
  })
}

# --- Run the App --- #
shinyApp(ui, server)
