#This file outlines functions used in the searching across projects section of the front page 

#switch the name rendered in the across search description based on select input choice
switchAcrossName <- observeEvent(input$acrossSearchTypeSelect, {
  updateTextInput(session, "acrossSearch", label=paste("Search for a ", tolower(input$acrossSearchTypeSelect), " across projects"))
})

#render the table based on what was searched for across projects
acrossVariableTable <- observeEvent(input$across, {
  #right now only capability for group, variable, and sample type
  info_table <- NULL
  #call the correct function
  if (strcmp(input$acrossSearchTypeSelect, "Group")) {
    info_table <- getGroupAcross(values$con, input$acrossSearch)
  } else if (strcmp(input$acrossSearchTypeSelect, "Sample Type")) {
    info_table <- getSampleTypeAcross(values$con, input$acrossSearch)
  } else if (strcmp(input$acrossSearchTypeSelect, "Variable")) {
    info_table <- getVariableAcross(values$con, input$acrossSearch)
  }
  if (nrow(info_table) == 0) {
    output$acrossFail <- renderText("No results.")
    output$acrossInfo <- renderDataTable(info_table)
    return()
  }
  output$acrossFail <- renderText("")
  if (strcmp(input$acrossSearchTypeSelect, "Variable")) {
    #special formatting for the variable across table
    for (i in 1:nrow(info_table)) {
      info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[{}\"]", '')
      info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[,]", ', ')
    }
  }
  output$acrossInfo <- renderDataTable(info_table)
})

#help buttons
output$acrossSearchHelp <- renderUI({
  tipify(bsButton("pC2", "Help", icon=icon("question-circle"),  size = "extra-small"),
         "Group = Outcomes of interest    Variables = Other measurements       Sample Type = Cell or blood type",
         placement = "right")
})
output$acrossTextSearchHelp <- renderUI({
  tipify(bsButton("pC3", "Help", icon=icon("question-circle"),  size = "extra-small"),
         "Enter text into a relaxed search. Partial terms are fine. Searching for nothing returns all possible values",
         placement = "right")
})
