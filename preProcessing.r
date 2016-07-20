process_data <- observeEvent(input$preProcess, {
  withProgress(message="Processing data", {
    
    dataset <- values$fulldata
    values$name <<- NULL
    if("Remarks" %in% substr(colnames(dataset),1,7)){
      dataset <- dataset[,-which( substr(colnames(dataset),1,7) == "Remarks")]
    }
    
    if("Birth date baby" %in% substr(colnames(dataset),1,15)){
      dataset <- dataset[,-which( substr(colnames(dataset),1,15) == "Birth date baby")]
    }
    
    if("Subject Initials" %in% substr(colnames(dataset),1,16)){
      dataset <- dataset[,-which( substr(colnames(dataset),1,16) == "Subject Initials")]
    }
    
    R <- 1
    C <- 1
    incProgress(0.25)      
    while(length(R) != 0 & length(C) != 0){
      A <- apply(dataset,2,count_missing)
      A <- A / length(dataset[,3])
      C <- which(is_greater(A, input$col_cutoff / 100) == TRUE)
      if (length(C) > 0){
        dataset <- dataset[,-C]
      }
      
      dataset_var <- dataset[,-c(1:2)]
      
      B <- apply(dataset_var,1,count_missing)
      B <- B/ length(dataset_var[1,])
      R <- which(is_greater(B, input$row_cutoff / 100) == TRUE)
      incProgress(0.25)
      if (length(R) > 0){
        dataset <- dataset[-R,]
      }
    }
    
    incProgress(0.25)
    num <- NULL
    for (i in 1:length(colnames(dataset))){
      dataset[,i][dataset[,i] == "NA"] <- NA
      
      if(gsub("^.*?_","",colnames(dataset)[i]) == -1){
        colnames(dataset)[i] <- gsub("_.*","",colnames(dataset)[i])
      }
      
      # if(gsub("^.*?_","",colnames(dataset)[i]) == 0){
      #   colnames(dataset)[i] <- paste(gsub("_.*","",colnames(dataset)[i]),"At Birth",sep ="_")
      # }
      # 
      # if(gsub("^.*?_","",colnames(dataset)[i]) == 5){
      #   colnames(dataset)[i] <- paste(gsub("_.*","",colnames(dataset)[i]),"Week 16",sep ="_")
      # }
      
      if(length(unique(na.omit(dataset[,i]))) == 1){
        values$name <<- c(values$name,colnames(dataset[i]))
        num <- c(num,i)
      }
    }
    if(length(num > 0)){
      num <- -num
      dataset <- dataset[num]
    }
  })
  session$sendCustomMessage (type="switch", "view")
  output$mergedTable <- renderTable(NULL)
  values$data <<- dataset
})

makePreText <- function(){
  full_dataset <- values$fulldata
  proc_dataset <- values$data
  
  info1 <- paste("Total number of samples before pre-processing:", length(full_dataset[,1]))
  info2 <- paste("Total number of variables before pre-processing:",length(colnames(full_dataset[-1])))
  info3 <- paste("Remaining number of samples after pre-processing:", length(proc_dataset[,1]))
  info4 <- paste("Remaining number of variables after pre-processing:",length(colnames(proc_dataset[-1])))
  info5 <- paste("")
  
  sample_removed <- NULL
  var_removed <- NULL
  constVar_removed <- values$name
  
  for(i in full_dataset[,1]){
    if(!i %in% proc_dataset[,1]){
      sample_removed <- c(sample_removed,i)
    }
  }
  
  for(i in colnames(full_dataset)){
    if(gsub("^.*?_","",i) == -1){
      i <- gsub("_.*","",i)
    }
    # if(gsub("^.*?_","",i) == 0){
    #   i <- paste(gsub("_.*","",i),"At Birth",sep ="_")
    # }
    # if(gsub("^.*?_","",i) == 5){
    #   i <- paste(gsub("_.*","",i),"Week 16",sep ="_")
    # }
    if(!i %in% colnames(proc_dataset)){
      if(!i %in% constVar_removed){
        var_removed <- c(var_removed,i)
      }
    }
  }
  
  n <- max(length(sample_removed),length(var_removed),length(constVar_removed))
  sample_removed <- c(sample_removed,rep("",n-length(sample_removed)))
  var_removed <- c(var_removed,rep("",n-length(var_removed)))
  constVar_removed <- c(constVar_removed,rep("",n-length(constVar_removed)))
  df <- data.frame(n1 = sample_removed, n2 = var_removed, n3 = constVar_removed, stringsAsFactors = F)
  colnames(df) <- c("ID of samples removed", "Variables removed", "Constant variables removed")
  
  cat(sprintf(info1), "\n")
  cat(sprintf(info2), "\n")
  cat(sprintf(info3), "\n")
  cat(sprintf(info4), "\n")
  cat(sprintf(info5), "\n")
  df
}

observeEvent(input$preProcess, {
  output$PreText <- renderUI({
    verbatimTextOutput("preText")
  })
  output$preText <- renderPrint({
    makePreText()
  })
  output$downloadMerged <- downloadHandler(
    filename = function() {
      paste("merged_file_info","csv",sep=".")
    },
    content <- function(file) {
      df <- makePreText()
      full_dataset <- values$fulldata
      proc_dataset <- values$data
      
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
      
      d[6,1] <- paste("This dataset is processed by removing rows that contain more than (and equal to) ",
                      input$row_cutoff, "% missing values and removing columns that contain more than (and equal to) ",
                      input$col_cutoff, "% missing values.", sep = "")
      
      
      d[8,1] <- paste("Title: Pre-procesing Info")
      
      d[10,1] <- paste("Total number of samples before pre-processing:", length(full_dataset[,1]))
      d[11,1] <- paste("Total number of variables before pre-processing:",length(colnames(full_dataset[-1])))
      d[12,1] <- paste("Remaining number of samples before after-processing:", length(proc_dataset[,1]))
      d[13,1] <- paste("Remaining number of variables before after-processing:",length(colnames(proc_dataset[-1])))
      
      d[15,] <- colnames(df)
      d[16:(size[1]+15),] <- df
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file,na="",row.names=F)
    }
  )
})

observeEvent(input$viewMerged, {
  output$mergedTable <- renderTable(values$data)
  output$PreText <- renderUI({
    textOutput("preText")
  })
  output$preText <- renderText("")
  
  output$downloadMerged <- downloadHandler(
    filename = function() {
      paste("merged_file","csv",sep=".")
    },
    content <- function(file) {
      d <- data.frame()
      size <- dim(values$data)
      d[1:(size[1]+5),] <- NA
      d[,1:size[2]] <- NA
      d[1,1] <- "Name of Project:"
      d[1,2] <- input$projectChoice
      d[2,1] <- "Date & Time:"
      d[2,2] <- as.character(Sys.time())
      d[3,1] <- "Timezone:"
      d[3,2] <- as.character(Sys.timezone())
      d[4,1] <- "This dataset is formed by merging the following files:"
      nam <- input$studyChoices[1]
      for (i in input$studyChoices[-1]){
        nam <- paste(nam,"\n",i,sep = "")
      }
      d[4,2] <- nam
      
      d[6,1] <- paste("This dataset is processed by removing rows that contain more than (and equal to) ",
                      input$row_cutoff, "% missing values and removing columns that contain more than (and equal to) ",
                      input$col_cutoff, "% missing values.", sep = "")
      
      d[8,] <- colnames(values$data)
      d[9:(size[1]+8),] <- values$data
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file, row.names = F, na="")
    }
  )
})


beginAnalysis <- observeEvent(input$start,{
  session$sendCustomMessage (type="switch", "analysis")
})

restartButton <- observeEvent(input$restart,{
  #operations to perform upon restarting a session (hitting the home button)
  values$data <- NULL
  session$sendCustomMessage (type="switch", "load")
  output$currentProject <- renderText("")
})