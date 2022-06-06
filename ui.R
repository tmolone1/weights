library(shiny)
library(shinyjs)
fluidPage(
  
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             background-color:#FF5733;
             position:fixed;
             top: calc(25%);
             left: calc(50%);
             }
             ")
    ),
  ),
  useShinyjs(),
  # App title ----
  titlePanel("Weightlifting App"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric entry for number of exercises ----
      numericInput(inputId = "n_ex",
                   label = "number of exercises",
                   value = 3),
      
      # Input: Selector for choosing dataset ----
      numericInput(inputId = "reps",
                   label = "repetitions per set:",
                   value = 10),
      
      # Input: Numeric entry for RPE ----
      numericInput(inputId = "rpe",
                   label = "RPE",
                   value = 8),
      
      # Input: Numeric entry for RPE ----
      numericInput(inputId = "sets",
                   label = "sets to perform:",
                   value = 3),
      
      # Input: bodyweight only
      checkboxInput(inputId = "bodyweight",
                    label = "bodyweight only") 
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      br(),
      # Output: Verbatim text for data summary ----
      tags$b("Counters for tracking exercise sets"),
      br(),
      actionButton("ex1_add1", "ex + 1"),
      actionButton("ex1_sub1", "ex - 1"),
      actionButton("reset", "set to 0"),
      br(),
      textOutput("count1"),
      textOutput("count2"),
      textOutput("count3"),
      textOutput("count4"),
      br(),
      
      #
      uiOutput("selection_input"),
      
      hr(),
      actionButton('start','Start'),
      actionButton('stop','Stop'),
      #actionButton('reset','Reset'),
      tags$hr(),
      textOutput('timeleft')

    )
  )
)