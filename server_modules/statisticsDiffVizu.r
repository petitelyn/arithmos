#Function to produce the title of visuzaliation plot
makeTitle1.3.2 <- function(){
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    "Scatterplot"
  }
  else if(input$var_interest %in% colnames(group) & input$select1.3.2 %in% colnames(dataset)){
    "Boxplot"
  }
  else if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(group)){
    "Boxplot"
  }
  else{
    "Barplot"
  }
}

#Function to produce the visuzaliation plot
makePlot1.3.2 <- function(){
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  #Scatterplot, Y(Continuous) vs X(Continuous)
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    X_var <- dataset[,colnames(dataset) %in% input$select1.3.2,drop = FALSE]
    Y_var <- dataset[,colnames(dataset) %in% input$var_interest,drop = FALSE]
    X_var[,1] <- as.numeric(X_var[,1])
    Y_var[,1] <- as.numeric(Y_var[,1])
    ID <- values$data[,1,drop=F]
    
    if(input$type1.3.2.1 == 1){
      if(length(colnames(group)) > 2){  
        group <- group[,colnames(group) %in% input$type1.3.2.2,drop = FALSE]
        df <- cbind(ID, group, X_var, Y_var)
        df <- na.omit(df)
        df[,2] <- as.factor(df[,2])
        colnames(df)[c(1,2,3,4)] <- c("ID","Group","VarX","VarY")
      }
      else{
        df <- cbind(ID, group, X_Var, Y_var)
        df <- na.omit(df)
        df[,2] <- as.factor(df[,2])
        colnames(df)[c(1,2,3,4)] <- c("ID","Group","VarX","VarY")
      }
      
      #Tells ggplot to plot VarX on the X axis, VarY on the Y axis, and
      #to give a different colour for each dot (of size 2) for different groups
      p <- ggplot(df, aes(x=VarX, y=VarY)) + geom_point(size=2, aes(colour=Group)) +
        scale_colour_hue(l=50) + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    aes(colour = Group), # Give a different colour for each group
                    fullrange=TRUE) +  #Extrapolate the line to fill the entire graph
        
        #Adjustments of theme for the scatterplot
        theme(text = element_text(size=15),
              axis.line.x = element_line(colour = "black", size = 1),
              axis.line.y = element_line(colour = "black", size = 1),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              axis.text.x = element_text(margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$select1.3.2) + ylab(input$var_interest) +
        ggtitle(input$main1.3.2)
    }
    
    
    else if(input$type1.3.2.1 == 2){
      df <- cbind(ID, X_var, Y_var)
      df <- na.omit(df)
      colnames(df)[c(1,2,3)] <- c("ID","VarX","VarY")
      
      #Tells ggplot to plot VarX on the X axis, VarY on the Y axis
      p <- ggplot(df, aes(x=VarX, y=VarY)) + geom_point(size=2) + 
        scale_colour_hue(l=50) + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    fullrange=TRUE) + #Extrapolate the line to fill the entire graph
        
        #Adjustments of theme for the scatterplot
        theme(text = element_text(size=15),
              axis.line.x = element_line(colour = "black", size = 1),
              axis.line.y = element_line(colour = "black", size = 1),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              axis.text.x = element_text(margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$select1.3.2) + ylab(input$var_interest) +
        ggtitle(input$main1.3.2)
    }
    p
  }
  
  #Boxplot, Y(Continuous) vs X(Categorical)
  else if(input$var_interest %in% colnames(group) & input$select1.3.2 %in% colnames(dataset)){
    variable <- dataset[,colnames(dataset) %in% input$select1.3.2,drop=F]
    group_var <- group[,colnames(group) %in% input$var_interest,drop=F]
    
    df <- cbind(variable,group_var)
    colnames(df) <- c("VarX","VarY")
    df <- na.omit(df)
    
    #Tells ggplot to plot VarX (must be a factor) on the X axis, VarY on the Y axis, and
    #to give a fixed width for each boxplot
    #coord_flip() transpose the graph
    p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY )) + coord_flip() + geom_boxplot(width = (0.2 * length(unique(df$VarY)))) +
      
      #Adjustments of theme for the boxplot
      theme(text = element_text(size=15), 
            axis.text.x = element_text(margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.key.height = unit(2.5, "line"),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            plot.title = element_text(margin=margin(0,0,20,0))) + xlab(input$var_interest) + ylab(input$select1.3.2) +
      ggtitle(input$main1.3.2) + scale_fill_discrete(name = input$var_interest)
    p
  }
  
  #Boxplot, Y(Categorical) vs X(Continuous)
  else if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(group)){
    variable <- dataset[,colnames(dataset) %in% input$var_interest,drop=F]
    group_var <- group[,colnames(group) %in% input$select1.3.2,drop=F]
    
    df <- cbind(variable,group_var)
    colnames(df) <- c("VarX","VarY")
    df <- na.omit(df)
    
    #Tells ggplot to plot VarX (must be a factor) on the X axis, VarY on the Y axis, and
    #to give a fixed width for each boxplot
    p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY )) + geom_boxplot(width = (0.2 * length(unique(df$VarY)))) +
      
      #Adjustments of theme for the boxplot
      theme(text = element_text(size=15), 
            axis.text.x = element_text(hjust=1, margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.key.height = unit(2.5, "line"),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            plot.title = element_text(margin=margin(0,0,20,0))) + xlab(input$select1.3.2) + ylab(input$var_interest) +
      ggtitle(input$main1.3.2) + scale_fill_discrete(name = input$select1.3.2)
    p
  }
  
  #Barplot, Y(Categorical) vs X(Categorical)
  else{
    group2 <- group[,colnames(group) %in% input$var_interest,drop=F]
    group1 <- group[,colnames(group) %in% input$select1.3.2,drop=F]
    df <- na.omit(cbind(group1,group2))
    
    df <- data.frame(df)
    #Give the frequency table for each combination of categorical variable
    freq=table(col(df), as.matrix(df))
    counts <- ddply(df, .(df[,1], df[,2]), nrow)
    
    colnames(counts) <- c("VarX","VarY","value")
    
    #Plotting the barplot
    ggplot(counts, aes(VarX, value)) +   
      geom_bar(aes(fill = VarY), position = "dodge", stat="identity", colour = "black") + xlab(input$select1.3.2) + ylab("Freq") +
      ggtitle(input$main1.3.2) + scale_fill_discrete(name = input$var_interest) + theme(text = element_text(size=15))
  }
}

#Function to produce the help text to explain each visuzaliation plot
helpText1.3.2 <- function(){
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  #Scatterplot, Y(Continuous) vs X(Continuous)
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    info1 <- paste("The scatterplot illustrates the relationship between 2 continuous variables.")
    info2 <- paste("A line of best fit is drawn in the plot.")
    cat(info1, "\n")
    cat(info2, "\n")
  }
  
  #Boxplot, Y(Continuous) vs X(Categorical)
  else if(input$var_interest %in% colnames(group) & input$select1.3.2 %in% colnames(dataset)){
    info1 <- paste("The boxplot illustrates the difference between groups of a continuous variables.")
    info2 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
    info3 <- paste("The dots, if any, represent the outliers.")
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
  }
  
  #Boxplot, Y(Categorical) vs X(Continuous)
  else if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(group)){
    info1 <- paste("The boxplot illustrates the difference between groups of a continuous variables.")
    info2 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
    info3 <- paste("The dots, if any, represent the outliers.")
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
  }
  
  #Barplot, Y(Categorical) vs X(Categorical)
  else{
    info1 <- paste("The barplot shows the number of samples in a categorical variables for each group.")
    cat(info1, "\n")
  }
}

output$select1_3_2 <- renderUI({
  if(input$sub_subfunction == 2){
    df1 <- makeTable1.3.1()
    df1[,1] <- as.character(df1[,1])
    selectizeInput("select1.3.2", paste("Select significant variable (p < 0.05)"), 
                   choices = na.omit(df1[df1$PValue < 0.05,1]))
  }
  else{
    return()
  }
})

listb[["1-3-2"]] <- tagList(uiOutput("title1.3.2"),
                            fluidRow(
                              column(4, uiOutput("Type1.3.2.1")),
                              column(8, uiOutput("Type1.3.2.2"))
                            ),
                            textInput("main1.3.2", "Key in the title of plot"),
                            plotOutput("plot1_3_2",hover = "plot1_hover1.3.2", brush = "plot1_brush1.3.2"),
                            br(),
                            uiOutput("lst1.3.2"),
                            
                            br(),
                            
                            uiOutput("lst1.3.3"),
                            
                            br(),
                            hidden(verbatimTextOutput("helptext1.3.2")))

#Produce interaction table only when both variables are continuous
output$lst1.3.3 <- renderUI({
  dataset <- selec_var()[[1]]
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    tagList(fluidRow(
      column(6, hidden(textOutput("hovered1.3.2")),
             hidden(uiOutput("uiExample1.3.2.1")),
             hidden(verbatimTextOutput("hover_info1.3.2"))),
      column(6, hidden(textOutput("selected1.3.2")),
             hidden(uiOutput("uiExample1.3.2.2")),
             hidden(verbatimTextOutput("brush_info1.3.2")))))
  }
})

output$title1.3.2 <- renderUI({
  h3(makeTitle1.3.2())
})

output$Type1.3.2.1 <- renderUI({
  dataset <- selec_var()[[1]]
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    radioButtons("type1.3.2.1", "Display by group variable?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
  }
  else{
    return()
  }
})

output$Type1.3.2.2 <- renderUI({
  dataset <- selec_var()[[1]]
  if(input$type1.3.2.1 == 1){
    selectizeInput("type1.3.2.2", "Select group variable", choices = colnames(selec_var()[[2]]), multiple = F)
  }
  else{
    return()
  }
})

output$plot1_3_2 <- renderPlot({
  makePlot1.3.2()
})

output$lst1.3.2 <- renderUI({
  dataset <- selec_var()[[1]]
  if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
    tagList(downloadButton('downloadPlot1.3.2', 'Download plot as pdf'),
            actionButton("inter1.3.2","Display interactivity"),
            actionButton("help1.3.2","Help",icon=icon("question-circle")))
  }
  else{
    tagList(downloadButton('downloadPlot1.3.2', 'Download plot as pdf'),
            actionButton("help1.3.2","Help",icon=icon("question-circle")))
  }
})

output$downloadPlot1.3.2 <- downloadHandler(
  filename = function() {
    paste(makeTitle1.3.2(),'.pdf', sep='')},
  content = function(file) {
    ggsave(file, makePlot1.3.2(), dpi = 300, width = 30, height = 20, units = "cm")})

#Enables the "Display Interactivity" button to produce interaction table for the scatterplot when clicked
observeEvent(input$inter1.3.2, {
  toggle("uiExample1.3.2.1")
  toggle("uiExample1.3.2.2")
  toggle("hovered1.3.2")
  toggle("hover_info1.3.2")
  toggle("selected1.3.2")
  toggle("brush_info1.3.2")
})

output$uiExample1.3.2.1 <- renderUI({
  tipify(bsButton("pB1321", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays the sample which the mouse is hovered on.")
})

output$uiExample1.3.2.2 <- renderUI({
  tipify(bsButton("pB1322", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays samples which the mouse selects on the plot.")
})

output$hovered1.3.2 <- renderText({
  "Hovered Point"
})

output$hover_info1.3.2 <- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- variable[,colnames(variable) %in% c(input$select1.3.2, input$var_interest),drop = FALSE]
  ID <- values$data[,1,drop = F]
  
  if(input$type1.3.2.1 == 1){
    if(length(colnames(group)) > 2){  
      group <- group[,colnames(group) %in% input$type1.3.2.2,drop = FALSE]
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type1.3.2.2)
    }
    else{
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type1.3.2.2)
    }
    nearPoints(df, input$plot1_hover1.3.2, xvar = input$select1.3.2, yvar = input$var_interest, threshold = 10, maxpoints = 1)
  }
  
  else{
    df <- cbind(ID, variable)
    df <- na.omit(df)
    colnames(df)[1] <- c("Subject ID")
    
    nearPoints(df, input$plot1_hover1.3.2, xvar = input$select1.3.2, yvar = input$var_interest, threshold = 10, maxpoints = 1)
  }
})

output$selected1.3.2 <- renderText({
  "Selected Points"
})

output$brush_info1.3.2 <- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- variable[,colnames(variable) %in% c(input$select1.3.2, input$var_interest),drop = FALSE]
  ID <- values$data[,1,drop = F]
  
  if(input$type1.3.2.1 == 1){
    if(length(colnames(group)) > 2){  
      group <- group[,colnames(group) %in% input$type1.3.2.2,drop = FALSE]
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type1.3.2.2)
    }
    else{
      df <- cbind(ID, group, variable)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2)] <- c("Subject ID",input$type1.3.2.2)
    }
    brushedPoints(df, input$plot1_brush1.3.2, xvar = input$select1.3.2, yvar = input$var_interest)
  }
  
  else{
    df <- cbind(ID, variable)
    df <- na.omit(df)
    colnames(df)[1] <- c("Subject ID")
    
    brushedPoints(df, input$plot1_brush1.3.2, xvar = input$select1.3.2, yvar = input$var_interest)
  }
})

#Enable the "Help" button  to produce the results table when clicked
observeEvent(input$help1.3.2, {
  toggle("helptext1.3.2")
})

output$helptext1.3.2 <- renderPrint({
  helpText1.3.2()
})