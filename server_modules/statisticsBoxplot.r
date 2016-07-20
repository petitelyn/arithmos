#Function to produce boxplot
makePlot1.2 <- function(text_size){
  dataset <- selec_var()[[1]]
  
  #Produce boxplot for the first 20 selected variables
  if(length(input$choose_variable1.2) > 20){
    variable <- dataset[,colnames(dataset) %in% input$choose_variable1.2[1:20],drop = FALSE]
  }
  else{
    variable <- dataset[,colnames(dataset) %in% input$choose_variable1.2,drop = FALSE]
  }
  
  #Convert the dataset into long format to plot the boxplot
  df <- melt(variable)
  
  p <- ggplot(df, aes(variable, value, fill = variable)) + 
    geom_boxplot(width = (0.05 * length(colnames(variable)))) +
    theme(text = element_text(size=text_size), 
          axis.text.x = element_text(angle=45, hjust=1,margin=margin(10,0,0,0)),
          axis.text.y = element_text(margin=margin(0,10,0,0)),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.title.x = element_text(margin=margin(20,0,0,0)),
          axis.title.y = element_text(margin=margin(0,20,0,0)),
          plot.title = element_text(margin=margin(0,0,20,0)),
          legend.key.height = unit(2.5, "line")) +
    ggtitle(input$main1.2)
  p
}

#Function to produce help text for the boxplot
helpText1.2 <- function(){
  info1 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
  info2 <- paste("")
  info3 <- paste("Recommended number of variables selected: <= 20.")
  info4 <- paste("If more than 20 variables are selected, only the first 20 will be plotted.")
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
}

listb[["1-2"]] <- tagList(h3("Boxplot"),
                          uiOutput("Select_all1.2"),
                          uiOutput("Choice1.2"),
                          uiOutput("Main1.2"),
                          plotOutput("plot1.2", height = "1000px"),
                          downloadButton("downloadPlot1.2", "Download plot as pdf"),
                          actionButton("help1.2","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.2")))

output$Main1.2 <- renderUI({
  if(length(input$choose_variable1.2) > 0){
    textInput("main1.2", "Key in the title of boxplot")
  }
})

#Enable the "Help" button  to produce the help text when clicked
observeEvent(input$help1.2, {
  toggle("helptext1.2")
})

output$helptext1.2 <- renderPrint({
  helpText1.2()
})

output$Select_all1.2 <- renderUI({
  radioButtons("select_all1.2", paste("Select all", length(selec_var()[[1]]), "continuous variables for the boxplot?"), choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
})


output$Choice1.2 <- renderUI({
  if(input$select_all1.2 == 2){
    selectizeInput("choose_variable1.2", "Select variables for the boxplot (max 20)", choices = colnames(selec_var()[[1]]), 
                   multiple = T)
  }
  else if(input$select_all1.2 == 1){
    
    #Limit the size of box such that it only displays a maximum of 10 variables
    if(length(selec_var()[[1]]) > 10){
      selectInput("choose_variable1.2", "Select variables for the boxplot (max 20)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = 10)
    }
    else{
      selectInput("choose_variable1.2", "Select variables for the boxplot (max 20)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = length(selec_var()[[1]]))
    }
  }
})

output$downloadPlot1.2 <- downloadHandler(
  filename = function() {
    paste('Boxplot','.pdf', sep='')},
  content = function(file) {
    ggsave(file, makePlot1.2(15), dpi = 300, width = 30, height = 35, units = "cm")})

output$plot1.2 <- renderPlot({
  makePlot1.2(15)
})