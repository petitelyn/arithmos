#Function to produce p-value text such as p<0.01** and p=0.03*
abbreviateSTR_P <- function(value, prefix, alpha){
  lst = c()
  for (item in value) {
    
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    
    item <- round(item, 3)
    
    if (item < 0.01) {
      item <- '<.01**'
    }
    
    else if (item < 0.05){
      item <- paste(item,"*",sep="")
    }
    
    item1 <- as.character(paste("=", item, sep = ""))
    lst <- c(lst, paste(prefix, item1 , sep = ""))
  }
  return(lst)
}

#Function to produce correlation text such as r=0.01 and r=0.102
abbreviateSTR_R <- function(value, prefix){  
  lst = c()
  for (item in value) {
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    item <- round(item, 3) 
    item <- as.character(paste("=", item, sep = ""))
    lst <- c(lst, paste(prefix, item , sep = ""))
  }
  return(lst)
}

#Function to produce the correlation matrix
makePlot2.2 <- function(text_size){
  n <- length(selec_var()[[1]])
  variable <- selec_var()[[1]]
  if(length(input$choose_variable2.2) >= 2){
    if(input$type2 == "pearson"){
      r <- "r"
    }
    
    if(input$type2 == "spearman"){
      r <- "rho"
    }
    
    variable <- variable[,colnames(variable) %in% input$choose_variable2.2,drop = FALSE]
    if(length(input$choose_variable2.2) > 15){
      variable <- variable[,1:15]
    }
    
    info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
    p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
    
    j <- NULL
    for (i in 1:length(info[1,])){
      if (all_missing(info[,i])){
        j <- c(j,i)
      }
    }
    
    if(length(j) > 0){
      info <- info[,-j]
      p.mat <- p.mat[,-j]      
    }
    
    
    cordata <- melt(info)
    pdata <- melt(p.mat)
    
    cordata$labelr = abbreviateSTR_R(melt(cordata)$value, prefix = r)
    cordata$labelP = abbreviateSTR_P(melt(pdata)$value, prefix = 'p', 0.05)
    
    cordata$label = paste(cordata$labelr, "\n", 
                          cordata$labelP, sep = "")
    
    if(input$displaysig2.2 == T){
      for(i in 1:length(rownames(cordata))){
        if(!is.na(pdata$value[i]) & pdata$value[i] > input$alpha2.2){
          cordata$label[i] <- NA
        }
      }
    }
    
    cordata.lower = subset(cordata[lower.tri(info, diag = T),])
    cordata.lower$Var1 <- with(cordata.lower, factor(cordata.lower$Var1, levels = rev(levels(cordata.lower$Var1))))
    
    txtsize <- par('din')[2] / 2
    p <- ggplot(cordata.lower, aes(x=Var1, y=Var2)) + geom_tile(aes(fill = value), colour = "black") + theme_classic() +
      theme(legend.key.height = unit(2.5, "line"),
            axis.text.x = element_text(size = text_size, angle=45, hjust=TRUE, margin=margin(10,0,0,0)),
            axis.text.y = element_text(size = text_size, margin=margin(0,10,0,0)),
            plot.title = element_text(size = text_size, margin=margin(0,0,10,0))) +
      xlab("") + ylab("") + 
      geom_text(label = cordata.lower$label, size = (10/(length(colnames(variable))**0.5))) +
      # scale_fill_gradient2(name=r,limit = c(-1,1),low = "#BB4444", high = "Yellow") +
      scale_fill_gradientn(name=r, colours = rev(rainbow(20*10, start = 0/6, end = 4/6))) +
      ggtitle(input$main2.2) +
      annotate("text", x = length(p.mat[,1]), y = length(p.mat[,1]) , label = paste("** p<0.01", " * p<0.05", sep = "\n"), size = 3)
    
    p
  } 
  else({
    return()
  })
}

#Function to produce the help text for the correlation plot
helpText2.2 <- function(){
  info1 <- paste("The plot above displays the correlation plot filled with a colour gradient.")
  info2 <- paste("")
  info3 <- paste("Maximum number of variables displayed: 15")
  info4 <- paste("If more than 15 variables are selected, only the first 15 will be plotted.")
  
  cat(sprintf(info1), "\n")
  cat(sprintf(info2), "\n")
  cat(sprintf(info3), "\n")
  cat(sprintf(info4), "\n")
}

listb[["2-2"]] <- tagList(h3("Correlation Matrix"),
                          
                          br(),
                          
                          fluidRow(column(6,uiOutput("Select_all2.2")),
                                   column(6,numericInput("alpha2.2", "Input Significance Level", 0.05, min = 0, max = 1))),
                          
                          br(),
                          
                          fluidRow(column(6,uiOutput("Choice2.2")),
                                   column(6,checkboxInput("displaysig2.2", "Display only significant variables"),
                                          uiOutput("uiExample2.2"))),
                          br(),
                          
                          uiOutput("Main2.2"),
                          plotOutput("Plot2.2", height = "800px"),
                          downloadButton('downloadPlot2.2', 'Download plot as pdf'),
                          actionButton("help2.2","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext2.2")))


output$uiExample2.2 <- renderUI({
  tipify(bsButton("pB22", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Removes label for insignificant correlation values.")
})

output$Select_all2.2 <- renderUI({
  radioButtons("select_all2.2", paste("Select all", length(colnames(selec_var()[[1]])), "continuous variables for the correlation matrix?"), choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
})

output$Choice2.2 <- renderUI({
  if(input$select_all2.2 == 2){
    selectizeInput("choose_variable2.2", "Select at least 2 continuous variables for the correlation matrix (max 15)", choices = colnames(selec_var()[[1]]), 
                   multiple = T)
  }
  else if(input$select_all2.2 == 1){
    if(length(colnames(selec_var()[[1]])) > 5){
      selectInput("choose_variable2.2", "Select at least 2 continuous variables for the correlation matrix (max 15)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = 5)
    }
    else{
      selectInput("choose_variable2.2", "Select at least 2 continuous variables for the correlation matrix (max 15)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = length(colnames(selec_var()[[1]])))
    }
  }
})

output$Main2.2 <- renderUI({
  if(length(input$choose_variable2.2) > 1){
    textInput("main2.2", "Key in the title of correlation plot")
  }
})

#Enables the "Help" button to produce the help text for the correlation plot
observeEvent(input$help2.2, {
  toggle("helptext2.2")
})

output$helptext2.2 <- renderPrint({
  helpText2.2()
})
output$downloadPlot2.2 <- downloadHandler(
  filename = function() {
    paste('Correlation Matrix','.pdf', sep='')},
  content = function(file) {
    ggsave(file, makePlot2.2(15), dpi = 300, height = 30, width = 30, units = "cm")})

output$Plot2.2 <- renderPlot({
  makePlot2.2(15)
})