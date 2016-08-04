#Produce the statistics for continuous/ categorical variables
makeText1.1 <- reactive({
  
  #Limit to 3 significant figures
  options(digits = 3)
  
  if(input$select1.1 == 1){
    variable <- selec_var()[[1]]
    info <- describe(variable)
    
    #Count missing values
    if (length(variable) > 1){
      Missing <- NULL
      for (j in 1:length(variable)){
        Missing <- c(Missing, count_missing(variable[,j]))
      }
    }
    else{
      Missing <- count_missing(variable[,1])
    }
    
    #Remove unnecessary information produced by the "describe" function
    info <- cbind(info[2],Missing,info[c(3:5,8,9,10)])
    info
  }
  
  else if(input$select1.1 == 2){
    cat_var <- selec_var()[[2]]
    row_names <- NULL
    n <- NULL
    for (i in 1:length(colnames(cat_var))){
      cat_var[,i] <- as.character(cat_var[,i])
      
      #COnvert missing values to string "NA" so that the R function could count the number of missing values
      cat_var[,i][is.na(cat_var[,i])] <- "NA"
      a <- table(cat_var[,i])
      row_nam <- unique(cat_var[,i])[1]
      n_nam <- a[names(a)==unique(cat_var[,i])[1]]
      
      #If number of groups > 1, concatenate the group names and number of samples in that group together
      if(length(unique(cat_var[,i])) > 1){
        for(j in unique(cat_var[,i])[-1]){
          row_nam <- paste(row_nam,j,sep=",")
          n_nam <- paste(n_nam,a[names(a)==j],sep=",")
          
        }
      }
      
      #Insert an open and close bracket to the concatenated names and samples of the group
      row_nam <- paste(colnames(cat_var)[i], " (", row_nam, ") ", sep="")
      n_nam <- paste("(",n_nam,")",sep="")
      
      row_names <- c(row_names,row_nam)
      n <- c(n,n_nam)
    }
    
    df <- data.frame(n = n)
    rownames(df) <- row_names
    df <- format(df, justify = "left")
    print(df,right=F)
  }
})

#Function to produce the help text that describes the statistics table
helpText1.1 <- reactive({
  if(input$select1.1 == 1){
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("n: number of present samples")
    info4 <- paste("missing: numer of missing samples")
    info5 <- paste("mean: average")
    info6 <- paste("sd: standard deviation")
    info7 <- paste("median: 50 percentile value")
    info8 <- paste("min: minimum value")
    info9 <- paste("max: maximum value")   
    info10 <- paste("range: max - min")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
    cat(info5, "\n")
    cat(info6, "\n")
    cat(info7, "\n")
    cat(info8, "\n")
    cat(info9, "\n")
    cat(info10, "\n")
  }
  else if(input$select1.1 == 2){
    cat_var <- selec_var()[[2]]
    #Counts the number of samples in each categorical variable
    a <- table(cat_var[,1])
    
    #Number of samples in the first group in the first categorical variable
    text_num <- a[names(a)==unique(cat_var[,1])[1]]
    #Name of the first group in the first categorical variable
    text_group <- unique(cat_var[,1])[1]
    
    #Concatenate the group names and number of samples if the number of groups in the first
    #categorical variable is greater than 1
    if(length(unique(cat_var[,1])) > 1){
      for(i in 2:length(unique(cat_var[,1]))){
        if(i != length(unique(cat_var[,1]))){
          text_num <- paste(text_num,a[names(a)==unique(cat_var[,1])[i]],sep=", ")
          text_group <- paste(text_group,unique(cat_var[,1])[i],sep=", ")
        }
        else{
          text_num <- paste(text_num,a[names(a)==unique(cat_var[,1])[i]],sep=" & ")
          text_group <- paste(text_group,unique(cat_var[,1])[i],sep=" & ")
        }
      }
    }
    
    info1 <- paste("The table below displays the sample size of each group for each categorical variable.")
    info2 <- paste("")
    info3 <- paste("i.e.",colnames(cat_var)[1], " has ",
                   text_num, " samples for group ",text_group," respectively.",sep="")
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
  }
})

listb[["1-1"]] <- tagList(radioButtons("select1.1", paste("Select which statistics table to view"), 
                                       choices = c("Continuous variable" = 1, "Categorical variable" = 2), inline = T),
                          downloadButton('downloadData1.1', 'Download data'),
                          actionButton("help1.1","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.1")),
                          verbatimTextOutput('text1.1'))

#Enable the "Help" button to produce the help text when clicked
observeEvent(input$help1.1, {
  toggle("helptext1.1")
})

output$helptext1.1 <- renderPrint({
  helpText1.1()
})

output$downloadData1.1 <- downloadHandler(
  filename = function() {
    if(input$select1.1 == 1){
      paste('exp_stats','.csv', sep='')
    }
    else if(input$select1.1 == 2){
      paste('cat_stats','.csv', sep='')
    }},
  
  content = function(file) {
    df <- makeText1.1()
    if(input$select1.1 == 1){
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
      
      d[8,1] <- paste("Title: Statistics Table for Explanatory Variables")
      
      d[10,] <- colnames(df)
      d[11:(size[1]+10),] <- df
      d[11:(size[1]+10),1] <- rownames(df)
      d[10,1] <- NA
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(selec_var()[[1]], file,na="",row.names=F)
    }
    
    else if(input$select1.1 == 2){
      
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
      
      
      d[8,1] <- paste("Title: Statistics Table for Categorical Variables")
      
      d[10,2] <- "n"
      d[11:(size[1]+10),2] <- df
      d[11:(size[1]+10),1] <- rownames(df)
      d[10,1] <- NA
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file,na="",row.names=F)
    }
  }
)

output$text1.1 <- renderPrint({
  makeText1.1()
})