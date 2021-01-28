library(shiny)

fluidPage(
  
  # App title ----
  titlePanel("Weightlifting App"),
  
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
      tags$b("Simple counter using reactiveValues() - An example"),
      br(),
      actionButton("ex1_add1", "ex1 + 1"),
      actionButton("ex1_sub1", "ex1 - 1"),
      actionButton("ex2_add1", "ex2 + 1"),
      actionButton("ex2_sub1", "ex2 - 1"),
      actionButton("ex3_add1", "ex3 + 1"),
      actionButton("ex3_sub1", "ex3 - 1"),
      actionButton("reset", "set to 0"),
      br(),
      textOutput("count1"),
      textOutput("count2"),
      textOutput("count3"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      br(),
      
      # checkbox
      checkboxInput("somevalue", "Some value", FALSE),
      verbatimTextOutput("value")
      
      

    )
  )
)