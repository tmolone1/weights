library(shiny)
function(input, output, session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$program,
           "E" = e_program)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput() %>% filter(Workout==paste0("E",input$day))
  })
  
  # counter buttons
  counter <- reactiveValues(countervalue = 0, count2=0, count3 = 0, count4=0) # Defining & initializing the reactiveValues object

  
  observeEvent(input$reset, {
    counter$countervalue <- 0                     # if the reset button is clicked, set the counter value to zero
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
  
}