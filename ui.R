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
                  choices = c("A","B","C","D","E"),
                  selected = "E"),
      
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
      actionButton("ex1_add1", "ex + 1"),
      actionButton("ex1_sub1", "ex - 1"),
      actionButton("reset", "set to 0"),
      br(),
      textOutput("count1"),
      textOutput("count2"),
      textOutput("count3"),
      textOutput("count4"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      br(),
      
      #
      selectInput(inputId = "exercise",
                  label = "exercise being performed:",
                  choices = c(1,2,3,4)),
      
      hr(),
      actionButton('start','Start'),
      actionButton('stop','Stop'),
      #actionButton('reset','Reset'),
      tags$hr(),
      textOutput('timeleft')

    )
  )
)