#Function to produce correlation table 
makeText2.1.1 <- reactive({
  variable <- selec_var()[[1]]
  info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
  info
})

#Function to produce P value table 
makeText2.1.2 <- reactive({
  variable <- selec_var()[[1]]
  p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
  p.mat
})

#Function to produce the results table for correlation significance
makeResults2.3.1 <- reactive({
  dataset <- makeTable2.3()
  dataset[,1] <- as.character(dataset[,1])
  info1 <- paste("Most statistically significant variable: ",dataset[1,1])
  info2 <- paste("No. of variables with p < 0.05: ",sum(dataset$PValue < 0.05,na.rm=T))
  info3 <- paste("No. of variables with q < 0.05: ",sum(dataset$QValue < 0.05))
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
})

#Function to produce the help text for the correlation significance table
helpText2.3.1 <- reactive({
  if(input$type2 == "pearson"){
    r <<- "r"
  }
  
  if(input$type2 == "spearman"){
    r <<- "rho"
  }
  
  info1 <- paste("The headers in the table below represents")
  info2 <- paste("")
  info3 <- paste("Variable: Name of variables")
  info4 <- paste("n: Number of paired samples")
  info5 <- paste(r, " = correlation coefficient", sep = "")
  info6 <- paste("PValue: Significance level")
  info7 <- paste("QValue: Adjusted P-Value")
  
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n") 
  cat(info4, "\n")
  cat(info5, "\n")
  cat(info6, "\n")
  cat(info7, "\n")
})

#Function to produce correlation Significance Table
makeTable2.3 <- reactive({
  
  dataset <- selec_var()[[1]]
  n <- NULL
  for(i in rownames(makeText2.1.2())){
    df <- dataset[,colnames(dataset) %in% c(input$choose_variable_2.3.1,i)]
    df <- na.omit(df)
    n <- c(n,length(rownames(df)))
  }
  
  df <- data.frame(Variable = rownames(makeText2.1.2()),
                     n = n,
                     r = as.numeric(makeText2.1.1()[,colnames(makeText2.1.1()) %in% input$choose_variable_2.3.1]),
                     PValue = as.numeric(makeText2.1.2()[,colnames(makeText2.1.2()) %in% input$choose_variable_2.3.1]),
                     stringsAsFactors = F)
  
  if(input$type2 == "spearman"){
    colnames(df)[3] <- "rho"
  }
  
  df <- df[-which(df[,1] == input$choose_variable_2.3.1),]
  df <- df[is.na(df$PValue) == F,]
  p_value <- df$PValue
  df <- df[order(df$PValue),]
  
  #Insert QValue
  p <- as.numeric(df$PValue[!is.na(df$PValue)])
  if(max(p) < 0.7){
    q_value <- qvalue(p,lambda=seq(0,0.85,0.05))$qvalues
  }
  else if(max(p) >= 0.7){
    q_value <- qvalue(p)$qvalues
  }
  df$QValue <- c(q_value,rep(NA,count_missing(df$PValue)))
  
  df <- df[df$PValue <  input$choose_alpha_level2.3,]
  df
})

#Function to produce the help text for the scatterplot
makeText2.3.2 <- reactive({
  info1 <- paste("The graph above displays the scatter plot between variables that")
  info2 <- paste("have statistically significant correlation between each other.")
  cat(info1, "\n")
  cat(info2, "\n")
})

#Function to produce scatterplot
makePlot2.3 <- function(text_size){
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- cbind(variable[,colnames(variable) %in% input$choose_variable_2.3.2,drop = FALSE],
                    variable[,colnames(variable) %in% input$choose_variable_2.3.3,drop = FALSE])
  ID <- values$data[,1,drop = F]
  
  if(input$type2.3.1 == 1){
    if(length(colnames(group)) > 2){  
      group <- group[,colnames(group) %in% input$type2.3.2,drop = FALSE]
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2,3,4)] <- c("ID","Group","Var1","Var2")
    }
    else{
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2,3,4)] <- c("ID","Group","Var1","Var2")
    }
    
    #Tells ggplot to plot VarX on the X axis, VarY on the Y axis, and
    #to give a different colour for each dot (of size 2) for different groups
    p <- ggplot(df, aes(x=Var1, y=Var2)) + geom_point(size=2, aes(colour=Group))  +
      scale_colour_hue(l=50) + # Use a slightly darker palette than normal
      geom_smooth(method=lm,   # Add linear regression lines
                  se=FALSE,    # Don't add shaded confidence region
                  aes(colour = Group), # Give a different colour for each group
                  fullrange=TRUE) + #Extrapolate the line to fill the entire graph
      
      #Adjustments of theme for the scatterplot
      theme(text = element_text(size=text_size),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.line.y = element_line(colour = "black", size = 1),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            axis.text.x = element_text(margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            plot.title = element_text(margin=margin(0,0,10,0)),
            legend.key.height = unit(2.5, "line")) + xlab(input$choose_variable_2.3.2) + ylab(input$choose_variable_2.3.3) +
      scale_colour_discrete(name = input$type2.3.2) + ggtitle(input$main2.3)
  }
  
  else if(input$type2.3.1 == 2){
    df <- cbind(ID, variable)
    df <- na.omit(df)
    colnames(df)[c(1,2,3)] <- c("ID","Var1","Var2")
    
    #Tells ggplot to plot VarX on the X axis and VarY on the Y axis
    p <- ggplot(df, aes(x=Var1, y=Var2)) + geom_point(size=2) + 
      scale_colour_hue(l=50) + # Use a slightly darker palette than normal
      geom_smooth(method=lm,   # Add linear regression lines
                  se=FALSE,    # Don't add shaded confidence region
                  fullrange=TRUE) + #Extrapolate the line to fill the entire graph
      theme(text = element_text(size=text_size),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.line.y = element_line(colour = "black", size = 1),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            axis.text.x = element_text(margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$choose_variable_2.3.2) + ylab(input$choose_variable_2.3.3) +
      ggtitle(input$main2.3)
  }
  p
}

listb[["2-3"]] <- tagList(h3("Significance Table"),
                          fluidRow(
                            column(6, uiOutput('Choice2.3.1')),
                            column(6, uiOutput("Choose_alpha_level2.3"))
                          ),
                          
                          br(),
                          
                          downloadButton('downloadData2.3', 'Download data'),
                          actionButton("help2.3.1","Help",icon=icon("question-circle")),
                          actionButton('get2.3.1', 'Get results'),
                          hidden(verbatimTextOutput("helptext2.3.1")),
                          hidden(verbatimTextOutput("getResults2.3.1")),
                          dataTableOutput('table2.3'),
                          
                          br(),
                          
                          h3("Scatterplot"),
                          fluidRow(
                            column(6, uiOutput("Type2.3.1")),
                            column(6, uiOutput("Type2.3.2"))
                          ),
                          
                          fluidRow(
                            column(6, uiOutput('Choice2.3.2')),
                            column(6, uiOutput('Choice2.3.3'))
                          ),
                          
                          hidden(verbatimTextOutput("text2.3.2")),
                          textInput("main2.3", "Key in the title of scatterplot"),
                          uiOutput("Plot2.3"),
                          downloadButton('downloadPlot2.3', 'Download plot as pdf'),
                          actionButton("inter2.3","Display interactivity"),
                          actionButton("help2.3","Help",icon=icon("question-circle")),
                          
                          uiOutput("inter_2.3"),
                          uiOutput("help_2.3"),
                          
                          br(),
                          
                          fluidRow(
                            column(6, hidden(uiOutput("Title2.3.1")),
                                   hidden(uiOutput("uiExample2.3.1")),
                                   hidden(verbatimTextOutput("hover_info2.3"))),
                            column(6, hidden(uiOutput("Title2.3.2")),
                                   hidden(uiOutput("uiExample2.3.2")),
                                   hidden(verbatimTextOutput("brush_info2.3")))
                          ))

#Enables the "Display Interactivity" button to produce interaction table for the scatterplot
observeEvent(input$inter2.3, {
  toggle("uiExample2.3.1")
  toggle("uiExample2.3.2")
  toggle("Title2.3.1")
  toggle("hover_info2.3")
  toggle("Title2.3.2")
  toggle("brush_info2.3")
})

output$uiExample2.3.1 <- renderUI({
  tipify(bsButton("pB31", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays the sample which the mouse is hovered on.")
})

output$uiExample2.3.2 <- renderUI({
  tipify(bsButton("pB32", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays samples which the mouse selects on the plot.")
})

#Enables the "Help" button to produce the help text for the correlation significance table when clicked
observeEvent(input$help2.3.1, {
  toggle("helptext2.3.1")
})

output$helptext2.3.1 <- renderPrint({
  helpText2.3.1()
})

#Enables the "Get Results" button to produce the results for the correlation significance table when clicked
observeEvent(input$get2.3.1, {
  toggle("getResults2.3.1")
})

output$getResults2.3.1 <- renderPrint({
  makeResults2.3.1()
})

output$downloadData2.3 <- downloadHandler(
  filename = function() {
    paste('Significance Table','.csv', sep='')},
  content = function(file) {
    df <- makeTable2.3()
    
    ########################################################################################################################
    #Insert some basic documentation in the first 7 rows of the data frame, such as title of project, data and time.
    d <- data.frame()
    size <- dim(df)
    d[1:(size[1]+5),] <-NA
    d[,1:(size[2])] <- NA
    d[1,1] <- "Name of Project:"
    d[1,2] <- input$projectChoice
    d[2,1] <- "Date & Time:"
    d[2,2] <- as.character(Sys.time())
    d[3,1] <- "Timezone:"
    d[3,2] <- as.character(Sys.timezone())
    d[4,1] <- "The data is based on the following files and variables :"
    
    nam <- "List of files:"
    for (i in input$studyChoices){
      nam <- paste(nam,"\n",i,sep = "")
    }
    d[4,2] <- nam
    
    nam1 <- "List of variables:"
    for (i in input$choose_variable){
      nam1 <- paste(nam1,"\n",i,sep = "")
    }
    d[4,3] <- nam1
    
    d[6,1] <- paste("This dataset is processed by removing rows that contain more than (and equal to) ",
                    input$row_cutoff, "% missing values and removing columns that contain more than (and equal to) ",
                    input$col_cutoff, "% missing values.", sep = "")
    ########################################################################################################################
    
    d[8,1] <- paste("Title: Correlation Significance Table for", input$choose_variable_2.3.1)
    d[9,1] <- paste("Type of correlation:",input$type2)
    
    d[11,] <- colnames(df)
    df[,1] <- as.character(df[,1])
    d[12:(size[1]+11),] <- df
    
    colnames(d) <- rep("", length(colnames(d)))
    write.csv(d, file, row.names = F,na="")})

output$Choice2.3.1 <- renderUI({
  selectizeInput("choose_variable_2.3.1", "Select a variable", choices = colnames(selec_var()[[1]]))
})

output$Choose_alpha_level2.3 <- renderUI({
  numericInput("choose_alpha_level2.3", "Input significance level (between 0 and 1)", value = 0.05, min = 0, max = 1)
})

output$table2.3 <- renderDataTable({
  makeTable2.3()
})

output$Title2.3 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    h3("Scatterplot")
  }
})

output$text2.3.2 <- renderPrint({
  makeText2.3.2()
})

output$Main2.3 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    textInput("main2.3", "Key in the title of scatterplot")
  }
})

output$Type2.3.1 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    radioButtons("type2.3.1", "Display by group variable?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
  }
  else{
    return()
  }
})

downloadtitle2.3.2 <- reactive({
  if(input$type2.3.1 == 1){
    paste(input$choose_variable_2.3.3,"Scatterplot by", input$type2.3.2)
  }
  else if(input$type2.3.1 == 2){
    paste(input$choose_variable_2.3.3,"Scatterplot")
  }
})

output$Type2.3.2 <- renderUI({
  if(input$type2.3.1 == 1){
    selectizeInput("type2.3.2", "Select group variable", choices = colnames(selec_var()[[2]]), multiple = F)
  }
  else{
    return()
  }
})

output$Choice2.3.2 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    selectizeInput("choose_variable_2.3.2", "Variable X (preselected beforehand)", choices = input$choose_variable_2.3.1)
  }
})

output$Choice2.3.3 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    selectizeInput("choose_variable_2.3.3", paste("Select variable Y (p < ",input$choose_alpha_level2.3,")",sep=""), choices = as.character(makeTable2.3()[,1]))
  }
})


output$downloadPlot2.3 <- downloadHandler(
  filename = function() {
    paste(downloadtitle2.3.2(),".pdf", sep='')},
  content = function(file) {
    ggsave(file, makePlot2.3(15), dpi = 300, height = 20, width = 30, units = "cm")})


output$Plot2.3 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    plotOutput("plot2.3", height = "500px", hover = "plot1_hover2.3", brush = "plot1_brush2.3")
  }
})

output$plot2.3 <- renderPlot({
  makePlot2.3(15)
})

output$Title2.3.1 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    h3("Hovered point")
  }
})

output$Title2.3.2 <- renderUI({
  if(length(rownames(makeTable2.3())) > 0){
    h3("Selected points")
  }
})


output$hover_info2.3<- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- variable[,colnames(variable) %in% c(input$choose_variable_2.3.2, input$choose_variable_2.3.3),drop = FALSE]
  ID <- values$data[,1,drop = F]
  
  if(input$type2.3.1 == 1){
    if(length(colnames(group)) > 2){  
      group <- group[,colnames(group) %in% input$type2.3.2,drop = FALSE]
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type2.3.2)
    }
    else{
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type2.3.2)
    }
    nearPoints(df, input$plot1_hover2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3, threshold = 10, maxpoints = 1)
  }
  
  else{
    df <- cbind(ID, variable)
    df <- na.omit(df)
    colnames(df)[1] <- c("Subject ID")
    
    nearPoints(df, input$plot1_hover2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3, threshold = 10, maxpoints = 1)
  }
})

output$brush_info2.3 <- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- variable[,colnames(variable) %in% c(input$choose_variable_2.3.2, input$choose_variable_2.3.3),drop = FALSE]
  ID <- values$data[,1,drop = F]
  
  if(input$type2.3.1 == 1){
    if(length(colnames(group)) > 2){  
      group <- group[,colnames(group) %in% input$type2.3.2,drop = FALSE]
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type2.3.2)
    }
    else{
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type2.3.2)
    }
    brushedPoints(df, input$plot1_brush2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3)
  }
  
  else{
    df <- cbind(ID, variable)
    df <- na.omit(df)
    colnames(df)[1] <- c("Subject ID")
    
    brushedPoints(df, input$plot1_brush2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3)
  }
  
})