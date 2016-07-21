#Function to produce correlation and p-value table for up to a maximum of 15 variables
makeText2.1 <- reactive({
  n <- length(colnames(selec_var()[[1]]))
  if(input$select2.1 == 1){
    #Gives a correlation table of max 15 variables
    if(n > 15){
      variable <- selec_var()[[1]][,1:15]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
      info
    }
    #Need at least 2 variables to give a correlation table
    else if(n > 2){
      variable <- selec_var()[[1]]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
      info
    }
    else{
      return()
    }
  }
  else{
    #Gives a correlation significance table of max 15 variables
    if(n > 15){
      variable <- selec_var()[[1]][,1:15]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
      p.mat
    }
    #Need at least 2 variables to give a correlation significance table
    else if(n > 2){
      variable <- selec_var()[[1]]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
      p.mat
    }
    else{
      return()
    }
  }
})

#Function to produce correlation and p-value table with no limits to the number of variables 
makeText2.1_1 <- reactive({
  n <- length(colnames(selec_var()[[1]]))
  if(input$select2.1 == 1){
    if(n > 2){
      variable <- selec_var()[[1]]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
      info
    }
    else{
      return()
    }
  }
  else{
    if(n > 2){
      variable <- selec_var()[[1]]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
      p.mat
    }
    else{
      return()
    }
  }
})

#Function to produce the help text for the correlation and p-value table
helpText2.1 <- reactive({
  if(input$select2.1 == 1){
    info1 <- paste("The table below displays the pairwise correlation values and")
    info2 <- paste("displays up to a maximum of 15 continuous variables.")
    info3 <- paste("")
    info4 <- paste("Download the data to view all of them.")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
  }
  else if(input$select2.1 == 2){
    info1 <- paste("The table below displays the pairwise significance values and")
    info2 <- paste("displays up to a maximum of 15 continuous variables.")
    info3 <- paste("")
    info4 <- paste("Download the data to view all of them.")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
  }
})

listb[["2-1"]] <- tagList(radioButtons("select2.1", paste("Select which table to view"), 
                                       choices = c("Correlation Table" = 1, "P-Value Table" = 2), inline = T),
                          downloadButton('downloadData2.1', 'Download data'),
                          actionButton("help2.1","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext2.1")),
                          verbatimTextOutput('text2.1'))

#Enables the "Help" button to produce the help text for the correlation and p-value table when clicked
observeEvent(input$help2.1, {
  toggle("helptext2.1")
})

output$helptext2.1 <- renderPrint({
  helpText2.1()
})

output$downloadData2.1 <- downloadHandler(
  filename = function() {
    if(input$select2.1 == 1){
      paste('Correlation Table','.csv', sep='')
    }
    else if(input$select2.1 == 2){
      paste('P Value Table','.csv', sep='')
    }},
  
  content = function(file) {
    df <- makeText2.1_1()
    df <- cbind(rownames(df),df)
    df[1,1] <- NA
    
    ########################################################################################################################
    #Insert some basic documentation in the first 7 rows of the data frame, such as title of project, data and time.
    d <- data.frame()
    size <- dim(df)
    d[1,] <-NA
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
    
    if(input$select2.1 == 1){
      d[8,1] <- paste("Title: Correlation Table for Continuous Variables")
    }
    else if(input$select2.1 == 2){
      d[8,1] <- paste("Title: P-Value Table for Continuous Variables")
    }
    d[9,1] <- paste("Type of correlation:",input$type2)
    
    d[11,] <- colnames(df)
    d[12:(size[1]+11),] <- df
    d[12:(size[1]+11),1] <- rownames(df)
    d[11,1] <- NA
    
    colnames(d) <- rep("", length(colnames(d)))
    write.csv(d, file,na="",row.names=F)
  }
)

output$text2.1 <- renderPrint({
  makeText2.1()
})