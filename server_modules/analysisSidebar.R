#renderings for the analysis sidebar
output$output1 <- renderUI({
  selec_var()
  if(selec_var()[[3]] == 3 | selec_var()[[3]] == 4){
    listb[[selec_var()[[3]]]]
  }
  else{
    listb[[paste(selec_var()[[3]],input$sub_function,sep="-")]]
  }
})

output$output2 <- renderUI({
  selec_var()
  if(selec_var()[[3]] == 1 ){
    listb[[paste(selec_var()[[3]],input$sub_function,input$sub_subfunction,sep="-")]]
  }
  else{
    return()
  }
})

output$select_subfunc <- renderUI({
  selec_var()
  lista[[as.numeric(selec_var()[[3]])]]
})
