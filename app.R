library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)
source("global.R")

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "./.secrets"
)

googledrive::drive_auth() 
gs4_auth(token = drive_token())

ui <- {dashboardPage(
  dashboardHeader(    title = "Weightlifting App"
                  ),
  dashboardSidebar(
    collapsed = FALSE, 
    div(htmlOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("Workout", tabName = "workout", icon = icon("dumbbell")),
      menuItem("Workout Plan", tabName = "plan", icon = icon("globe")),
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("History", tabName = "history", icon = icon("timeline")),
      menuItem("My Info", tabName = "profile", icon = icon("heart-pulse")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem(tabName = "workout", uiOutput("tab1UI")),
      tabItem(tabName = "plan", uiOutput("tab2UI")),
      tabItem(tabName = "calendar", uiOutput("tab3UI")),
      tabItem(tabName = "history", uiOutput("tab4UI")),
      tabItem(tabName = "profile", uiOutput("tab5UI")),
      tabItem(tabName = "about", uiOutput("tab6UI"))
    )
  )
)
}


server <- function(input, output, session) {
  
  datasetInput <- reactive({
    if (input$bodyweight == 'mixed') {
      exercises %>% mutate(selectorder = priority*runif(nrow(exercises))) %>% arrange(selectorder) %>% head(input$n_ex)
    }
    if (input$bodyweight == 'equipment') {
      rows<-nrow(exercises %>% filter(bodyweight=='n'))
      exercises %>% filter(bodyweight=='n') %>% mutate(selectorder = priority*runif(rows)) %>% arrange(selectorder) %>% head(input$n_ex)
    }
    else {
      rows<-nrow(exercises %>% filter(bodyweight=='y'))
      exercises %>% filter(bodyweight=='y') %>% mutate(selectorder = priority*runif(rows)) %>% arrange(selectorder) %>% head(input$n_ex)
    }
  })
  
  output$sel_table_view <- renderDataTable({
    datasetInput() %>% 
      mutate(reps=rep(input$reps,input$n_ex), rpe=rep(input$rpe, input$n_ex), sets=rep(input$sets, input$n_ex)) %>% 
      mutate(try_weight=floor(vol_at_rpe8to9*(rpe/8.5)/reps/sets)) %>% select(exercise, reps, sets, try_weight)
  }, options = list(dom='t'))
  
  history<-reactive(googlesheets4::read_sheet(drive_get("~/Fitness/Weights/history")$id) %>% tibble())
  
  output$history_view <- DT::renderDataTable(history(), options = list(scrollX = TRUE))
  
  
  # workout
  output$tab1UI <- renderUI({
    box(width = NULL, status = "primary",
        sidebarLayout(
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
            radioButtons(inputId = "bodyweight",
                         label = "workout type",
                         choices = c("no equipment", "equipment", "mixed"),
                         selected= "mixed"),
            
            actionButton('log','Log This Workout')
          
          ),
          mainPanel(
            h4(strong("Today's Workout")),
            dataTableOutput(outputId = "sel_table_view"),
            hr(),
            h5('Timer'),
            actionButton('start','Start'),
            actionButton('stop','Stop'),
            actionButton('reset','Reset'),
            textOutput('timeleft'),
            hr(),
            
          )
        )
    )
  })
  
  #training plan
  output$tab2UI <- renderUI({
    
  })
  
  #calendar
  output$tab3UI <- renderUI({
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = "Note:", "")
      ),
      fluidRow(
        box(title = "Rename Table", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel(
              textInput(),
              actionButton())
        ),
        box(title = "Rename Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        ),
        box(title = "Add Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        )
      )
    )
  })
  
  #History
  output$tab4UI <- renderUI({
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info", style = ""),
        dataTableOutput(outputId = "history_view")
        
    )
  })
  
  # Profile
  output$tab5UI <- renderUI({

  })
  
  
  observeEvent(input$create_table, {
    # gather all the colnames into a list
    col_names_list = list()
    for (i in seq_len(input$ncols)) {
      col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
    }
    # updateSelectInput(session, "sel_table_1", choices = dbListTables(db))
    showModal(modalDialog(
      title = "Success",
      "The table has been successfully created.",
      footer = modalButton("OK"), easyClose = TRUE ) )
  })
  
  # Initialize the timer, not active.
  timer <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  update_interval = 0.1 # How many seconds between timer updates?
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time passed: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(100, session)
    isolate({
      if(active())
      {
        timer(round(timer()+update_interval,2))
      }
      
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(0); active(FALSE)})
}
  
shinyApp(ui, server)
