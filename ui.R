library(shiny)

fluidPage(
  
  # App title ----
  titlePanel("Shiny Text"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "program",
                  label = "Choose a program:",
                  choices = c("E")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "day",
                   label = "workout day",
                   value = 4)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)