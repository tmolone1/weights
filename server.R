library(shiny)
function(input, output, session) {
  
  # Return the requested dataset ----
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
  
  output$selection_input<- renderUI(selectInput(inputId = "exercise",
              label = "exercise being performed:",
              choices = 1:input$n_ex))
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput() %>% mutate(reps=rep(input$reps,input$n_ex), rpe=rep(input$rpe, input$n_ex), sets=rep(input$sets, input$n_ex)) %>% mutate(try_weight=floor(vol_at_rpe8to9*(rpe/8.5)/reps/sets)) %>% select(exercise, reps, sets, try_weight)
  })
  
  # counter buttons
  counter <- reactiveValues(countervalue = 0, count2=0, count3 = 0, count4=0) # Defining & initializing the reactiveValues object

  
  observeEvent(input$reset, {
    counter$countervalue <- 0
    counter$count2 <- 0
    counter$count3 <- 0
    counter$count4 <- 0 # if the reset button is clicked, set the counter value to zero
  })
  output$count1 <- renderText({
    paste("Counter Value 1 is ", counter$countervalue)
    # print the latest value stored in the reactiveValues object
  })
  output$count2 <- renderText({
    paste("Counter Value 2 is ", counter$count2)
    # print the latest value stored in the reactiveValues object
  })
  output$count3 <- renderText({
    paste("Counter Value 3 is ", counter$count3)
    # print the latest value stored in the reactiveValues object
  })
  output$count4 <- renderText({
    paste("Counter Value 4 is ", counter$count4)
    # print the latest value stored in the reactiveValues object
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
  observeEvent(input$ex1_add1, {active(TRUE); timer(0)})
  observeEvent(req(input$ex1_add1, input$exercise==1), {counter$countervalue <- counter$countervalue + 1})
  observeEvent(req(input$ex1_add1, input$exercise==2), {counter$count2 <- counter$count2 + 1})
  observeEvent(req(input$ex1_add1, input$exercise==3), {counter$count3 <- counter$count3 + 1})
  observeEvent(req(input$ex1_add1, input$exercise==4), {counter$count4 <- counter$count4 + 1})
  observeEvent(req(input$ex1_sub1, input$exercise==1), {counter$countervalue <- counter$countervalue - 1})
  observeEvent(req(input$ex1_sub1, input$exercise==2), {counter$count2 <- counter$count2 - 1})
  observeEvent(req(input$ex1_sub1, input$exercise==3), {counter$count3 <- counter$count3 - 1})
  observeEvent(req(input$ex1_sub1, input$exercise==4), {counter$count4 <- counter$count4 - 1})
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(0); active(FALSE)})
  observeEvent(input$start, {delay(100000, showNotification(paste("NEXT SET!!! GET AFTER IT!!"), duration = 30))})
  observeEvent(input$ex1_add1, {delay(100000, showNotification(paste("NEXT SET!!! GET AFTER IT!!"), duration = 30))})
}

