library(shiny)
function(input, output) {
  
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
  counter <- reactiveValues(countervalue = 0, count2=0, count3 = 0) # Defining & initializing the reactiveValues object

  
  observeEvent(input$ex1_add1, {
    counter$countervalue <- counter$countervalue + 1     # if the add button is clicked, increment the value by 1 and update it
  })
  observeEvent(input$ex1_sub1, {
    counter$countervalue <- counter$countervalue - 1  # if the sub button is clicked, decrement the value by 1 and update it
  })
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
  
  # checkbox
  output$value <- renderText({ input$somevalue })
   

}