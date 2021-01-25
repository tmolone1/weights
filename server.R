library(shiny)
function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$program,
           "E" = e_program)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput() %>% filter(Workout==paste0("E",input$day))
  })
  
}