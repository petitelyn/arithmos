output$selectTime <- renderUI({
  dataset <- values$data
  d <- NULL
  if(input$select_time == 1){
    #d <- unique(as.numeric(gsub("^.*?_","",colnames(dataset))))
    for(i in colnames(dataset)){
      if(gsub("^.*?_","",i) !=i){
        d <- c(d,gsub("^.*?_","",i))
      }
    }
    #d <- unique(gsub("^.*?_","",colnames(dataset)))
    d <- unique(d)
    d <- d[!is.na(d)]
    selectizeInput("choose_time", "Select timepoint", choices = sort(d), 
                   multiple = T)
  }
})
outputOptions(output, 'selectTime', suspendWhenHidden=FALSE)



output$selectAll <- renderUI({
  dataset <- values$data
  if(input$select_time == 1){
    radioButtons("select_all", "Select all variables within the timepoint?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
  }
  else if(input$select_time == 2){
    radioButtons("select_all", "Select all variables?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
  }
})
outputOptions(output, 'selectAll', suspendWhenHidden=FALSE)


output$choose_var <- renderUI({
  dataset <- values$data[-1]
  if(input$select_time == 1){
    
    c1 <- colnames(dataset[,which(gsub("^.*?_","",colnames(dataset)) == colnames(dataset))])
    c2 <- NULL
    for(i in input$choose_time){
      c2 <- c(c2,colnames(dataset[,which(gsub("^.*?_","",colnames(dataset)) == i)]))
    }
    
    if(input$select_all == 2){
      selectizeInput("choose_variable", "Select explanatory variables", choices = c(c1,c2), 
                     multiple = T)
    }
    else if(input$select_all == 1){
      selectInput("choose_variable", "Select explanatory variables", choices = c(c1,c2), 
                  multiple = T, selectize = F, selected = c(c1,c2), size = 10)
    }
  }
  
  else{
    if(input$select_all == 2){
      selectizeInput("choose_variable", "Select explanatory variables", choices = colnames(values$data)[-1], 
                     multiple = T)
    }
    else if(input$select_all == 1){
      selectInput("choose_variable", "Select explanatory variables", choices = colnames(values$data)[-1], 
                  multiple = T, selectize = F, selected = colnames(values$data)[-1], size = 10)
    }
  }
})
outputOptions(output, 'choose_var', suspendWhenHidden=FALSE)


output$select_cat_var <- renderUI({
  dataset <- values$data[-1]
  if(length(input$choose_variable) > 0){
    dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
    ave_uniq <- NULL
    uniq_var <- NULL
    for (i in unique(gsub("_.*","",colnames(dataset)))){
      n <- 0
      for (j in 1:length(colnames(dataset))){
        if(gsub("_.*","",colnames(dataset)[j]) == i){
          n <- n + length(unique(dataset[,j]))
        }
      }
      aver <- n / length(colnames(dataset[,gsub("_.*","",colnames(dataset)) %in% i,drop = FALSE]))
      uniq_var <- c(uniq_var,i)
      ave_uniq <- c(ave_uniq,aver)
    }
    
    df <- data.frame(ave_uniq,uniq_var,stringsAsFactors = F)
    df <- df[order(df[,1]),]
    
    #Preselect suspected categorical variables
    var_name <- NULL
    for(i in 1:length(colnames(dataset))){
      dataset[,i] <- as.numeric(dataset[,i])
      if(all_missing(dataset[,i])){
        var_name <- c(var_name,colnames(dataset)[i])
      }
    }
    var_name <- unique(gsub("_.*","",var_name))
    
    selectizeInput("cat_variable", "Select categorical explanatory variables", choices = df[,2],selected = var_name, multiple = T)
  }
  else {
    helpText("No explanatory variables are selected.")
  }
})
outputOptions(output, 'select_cat_var', suspendWhenHidden=FALSE)


output$help <- renderUI({
  dataset <- values$data
  dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
  if(length(unique(gsub("_.*","",colnames(dataset)))) > 1){
    helpText("The variables in the list above are sorted by its likelihood to be a categorical variable. 
             Variables suspected to be categorical have been preselected.")
  }
  })  
outputOptions(output, 'help', suspendWhenHidden=FALSE)


output$select_func <- renderUI({
  selectInput("main_function",
              "Select main function",
              choices = c("Statistics" = 1,
                          "Correlation" = 2,
                          "Principal Component Analysis" = 3,
                          "Hierarchical Clustering" = 4)
  )
})
outputOptions(output, 'select_func', suspendWhenHidden=FALSE)


# output$help1 <- renderUI({
#   values$data
#   helpText("Click the Select button after you have finished selecting the explanatory variables, 
#             the group variable and the main function.")
# })
# outputOptions(output, 'help1', suspendWhenHidden=FALSE)



output$select_var <- renderUI({
  values$data
  actionButton('select_variable', "Select")
})
outputOptions(output, 'select_var', suspendWhenHidden=FALSE)


selec_var <- eventReactive(input$select_variable,{
  dataset <- values$data[-1]
  dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
  exp_var <- dataset[,!gsub("_.*","",colnames(dataset)) %in% input$cat_variable,drop = FALSE]
  cat_var <- dataset[,gsub("_.*","",colnames(dataset)) %in% input$cat_variable,drop = FALSE]
  
  for(i in 1:length(colnames(exp_var))){
    exp_var[,i] <- as.numeric(exp_var[,i])
  }
  
  if(length(colnames(cat_var)) > 0){
    for(i in 1:length(colnames(cat_var))){
      #cat_var[,i][is.na(cat_var[,i])] <- "NA"
      cat_var[,i] <- as.factor(cat_var[,i])
    }
  }
  
  list(exp_var,  cat_var, input$main_function)
})

output$help2 <- renderUI({
  selec_var()
  helpText("You have selected ",length(selec_var()[[1]])," continuous variables, ",
           length(selec_var()[[2]]), " categorical variables and ",lst[[as.numeric(selec_var()[[3]])]],
           " as the main function.")
})
outputOptions(output, 'help2', suspendWhenHidden=FALSE)


output$warning1 <- renderUI({
  selec_var()
  if(length(input$cat_variable) == 0){
    helpText("Warning: No categorical variables are selected.")
  }
})
outputOptions(output, 'warning1', suspendWhenHidden=FALSE)


output$warning2 <- renderUI({
  var_name <- NULL
  dataset <- selec_var()[[1]]
  for(i in 1:length(colnames(dataset))){
    if(all_missing(dataset[,i])){
      var_name <- c(var_name,colnames(dataset)[i])
    }
  }
  var_name <- unique(gsub("_.*","",var_name))
  
  if(length(var_name) == 1){
    helpText(paste("Warning: The following variable is suspected to be categorical:",var_name))
  }
  
  else if(length(var_name) > 1){
    help_text <- var_name[1]
    for(i in 2:length(var_name)){
      if(i == length(var_name)){
        help_text <- paste(help_text, "&", var_name[i])
      }
      else{
        help_text <- paste(help_text, ", ", var_name[i], sep = "")
      }
    }
    helpText(paste("Warning: The following variables are suspected to be categorical:",help_text))
  }
  
  else{
    return()
  }
})