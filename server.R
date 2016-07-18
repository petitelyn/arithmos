library(shiny)
library(shinyjs)
library(shinyBS)
source("helpers.R")


shinyServer(function(input, output, session) {
  values <- reactiveValues(sessionId = NULL)
  values$con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
  values$data <- NULL
  values$fulldata <- NULL
  values$name <- NULL
  
  session$onSessionEnded(function() {
    
    observe(dbDisconnect(values$con))
  })
  
  restartSession <- observeEvent(input$restart,{
    values$data <- NULL
    #output$helptext <- renderText("")
    session$sendCustomMessage (type="switch", "load")
    output$currentProject <- renderText("")
  })
  
  updateStudies <- function(current_project) {
    
    get_project_pk <- sprintf("SELECT pk FROM project WHERE project_code=\'%s\'", current_project)
    project_pk <- dbGetQuery(values$con, get_project_pk)[["pk"]]
    get_studies <- sprintf("SELECT study_name FROM study WHERE study.project_pk=%i", project_pk)
    study_list <- dbGetQuery(values$con, get_studies)[["study_name"]]
    updateSelectInput(session, "studyChoices", choices=study_list, selected=study_list)
    output$acrossInfo <- renderTable(NULL)
    
  }
  
  updateProjects <- function() {
    project_list <- dbGetQuery(values$con, "SELECT project_code FROM project")[["project_code"]]
    updateSelectInput(session, "projectChoice", choices=project_list, select=input$projectChoice)
  }
  
  upload_data  <- observeEvent(input$file,{
    output$uploadError <- renderText("")
    full_name_list <- input$file$name
    datapath_list <- input$file$datapath
    file_name_list <- list(100)
    for (j in 1:length(full_name_list)){
      file_split <- strsplit(full_name_list[[j]], "\\.")[[1]]
      file_type <- file_split[[2]]
      if (!(strcmp(file_type, "xlsm"))) {
        output$uploadError <- renderText("Can only upload .xlsm")
        return()
      }
      file_name_list[[j]] <- file_split[[1]]
    }
    
    count <- 1
    #hardcoded value
    csv_list <- list(100)
    dir.create("TEMPDIR")
    for (p in 1:length(file_name_list)) {
      for (i in 1:2) {
        convert_to_csv <- readWorksheetFromFile(datapath_list[[p]], header=F, sheet=i)
        convert_to_csv[is.na(convert_to_csv)] <- ""
        new_file_name <- paste("TEMPDIR", .Platform$file.sep, file_name_list[p],"_temp",  toString(i), ".csv", sep='')
        write.table(convert_to_csv, new_file_name, na='', quote=F, row.names=F, col.names=F, sep=',')
        csv_list[[count]] <- new_file_name
        count <- count + 1
      }
    }
    
    if (length(csv_list) != 2*length(file_name_list)) {
      output$uploadError("Every .xlsm must be two sheets, with study info first and data second")
      unlink("TEMPDIR", recursive=TRUE)
      return()
    }
    
    
    progress <- shiny::Progress$new()
    progress$set(value=0)
    total_studies <- length(csv_list) / 2
    for (i in seq(1, length(csv_list), 2)) {
      study_name <- strsplit(file_name_list[[ceiling(i/2)]], "\\.")[[1]][[1]]
      progress$inc(0, message = paste("Uploading", study_name))
      return_code <- addStudy(values$con, csv_list[[i]], csv_list[[i+1]], study_name, total_studies, progress)
      if (return_code == -1) {
        output$uploadError <- renderText(sprintf("Error uploading %s", study_name))
        break
      }
    }
    unlink("TEMPDIR", recursive=TRUE)
    updateStudies(input$projectChoice)
    updateProjects()
    progress$close()
    invalidateLater(0, session)
  })
  
  unloadData <- observeEvent(input$studyChoices, {
    output$loadSuccess <- renderText("")
    values$data <- NULL
  })
  
  loadStudies <- observeEvent(input$projectChoice, {
    updateStudies(input$projectChoice)
  })
  
  databaseSearch <- observeEvent(input$search,{
    
    search_query <- sprintf("SELECT pk FROM study WHERE study_name LIKE \'%s\'", input$databaseSearch)
    study_list  <<- dbGetQuery(values$con, search_query)
    if(!(nrow(study_list) == 0)) updateSelectInput(session, "databaseChoice", choices=study_list[,"pk"])
    else updateSelectInput(session, "databaseChoice", choices=dbGetQuery(values$con, "SELECT pk FROM study")["pk"])
    
  })
  
  loadData <- observeEvent(input$load, {
    withProgress(message="Loading data", {
      study_name_list <- input$studyChoices
      pk_list <- list()
      for (i in 1:length(study_name_list)){
        search_query <- sprintf("SELECT pk FROM study WHERE study_name=\'%s\'", study_name_list[[i]])
        pk_list[i] <- dbGetQuery(values$con, search_query)
      }
      incProgress(1/2)
      frame <- getStudyDataFrame(values$con, pk_list)
      wide_format <- spread(frame, "new_name", "value")
      values$data <<- wide_format
      values$fulldata <<- wide_format
      incProgress(1/4)
      output$loadSuccess <- renderText("Data loaded.")
      output$currentProject <- renderText(paste("Project: ", input$projectChoice))
      session$sendCustomMessage (type="switch", "process")
      incProgress(1/4)
    })
  })
  
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
        
        if(gsub("^.*?_","",colnames(dataset)[i]) == 0){
          colnames(dataset)[i] <- paste(gsub("_.*","",colnames(dataset)[i]),"At Birth",sep ="_")
        }
        
        if(gsub("^.*?_","",colnames(dataset)[i]) == 5){
          colnames(dataset)[i] <- paste(gsub("_.*","",colnames(dataset)[i]),"Week 16",sep ="_")
        }
        
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
  

  
  # output$helptext <- renderUI({
  #   if(length(values$name) == 1){
  #     helpText(paste("Note:", values$name," has been removed from the dataset since it is a constant variable."))
  #   }
  #   else if(length(values$name) > 1){
  #     help_text <- paste("Note: ", values$name[1])
  #     for (i in 2:length(values$name)){
  #       if(i != length(values$name)){
  #         help_text <- paste(help_text,values$name[i],sep=", ")
  #       }
  #       else{
  #         help_text <- paste(help_text,values$name[i],sep=" & ")
  #       }
  #     }
  #     helpText(paste(help_text," have been removed from the dataset since they are constant variables."))
  #   }
  #   else{
  #     return()
  #   }
  # })
  
  #outputOptions(output, 'helptext', suspendWhenHidden=FALSE)
  
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
  
  switchAcrossName <- observeEvent(input$acrossSearchTypeSelect, {
    updateTextInput(session, "acrossSearch", label=paste("Search for a ", tolower(input$acrossSearchTypeSelect), " across projects"))
  })
  
  output$acrossSearchHelp <- renderUI({
    tipify(bsButton("pC2", "Help", icon=icon("question-circle"),  size = "extra-small"),
           "Group = Outcomes of interest    Variables = Other measurements",
           placement = "right")
  })
  
  acrossVariableTable <- observeEvent(input$across, {
    #right now only capability for group and variable search
    #if something strange and dire happens, it defaults to variable (but there should only be two options)
    info_table <- NULL
    if (strcmp(input$acrossSearchTypeSelect, "Group")) {
      info_table <- getGroupAcross(values$con, input$acrossSearch)
    } else {
      info_table <- getVariableAcross(values$con, input$acrossSearch)
    }
    if (nrow(info_table) == 0) {
      output$acrossFail <- renderText("No results.")
      output$acrossInfo <- renderDataTable(info_table)
      return()
    }
    output$acrossFail <- renderText("")
    if (!(strcmp(input$acrossSearchTypeSelect, "Group"))) {
      for (i in 1:nrow(info_table)) {
        info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[{}\"]", '')
        info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[,]", ', ')
      }
    }
    output$acrossInfo <- renderDataTable(info_table)
  })
  
  beginAnalysis <- observeEvent(input$start,{
    session$sendCustomMessage (type="switch", "analysis")
  })
  
################################################################################################ 
  
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
  
  lst <- list()
  lst[[1]] <- "Statistics"
  lst[[2]] <- "Correlation"
  lst[[3]] <- "Principal Component Analysis (PCA)"
  lst[[4]] <- "Hierarchical Clustering"

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

######################################################################################  
# List of makeText and makePlot functions
######################################################################################  
  
  makePreText <- function(){
    full_dataset <- values$fulldata
    proc_dataset <- values$data
    
    info1 <- paste("Total number of samples before pre-processing:", length(full_dataset[,1]))
    info2 <- paste("Total number of variables before pre-processing:",length(colnames(full_dataset[-1])))
    info3 <- paste("Remaining number of samples before after-processing:", length(proc_dataset[,1]))
    info4 <- paste("Remaining number of variables before after-processing:",length(colnames(proc_dataset[-1])))
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
      if(gsub("^.*?_","",i) == 0){
        i <- paste(gsub("_.*","",i),"At Birth",sep ="_")
      }
      if(gsub("^.*?_","",i) == 5){
        i <- paste(gsub("_.*","",i),"Week 16",sep ="_")
      }
      if(!i %in% colnames(proc_dataset)){
        var_removed <- c(var_removed,i)
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
  
  makeText1.1 <- reactive({
    options(digits = 3)
    if(input$select1.1 == 1){
      variable <- selec_var()[[1]]
      info <- describe(variable)
      
      if (length(variable) > 1){
        Missing <- NULL
        for (j in 1:length(variable)){
          Missing <- c(Missing, count_missing(variable[,j]))
        }
      }
      else{
        Missing <- count_missing(variable[,1])
      }
      
      info <- cbind(info[2],Missing,info[c(3:5,8,9,10)])
      info
    }
    else if(input$select1.1 == 2){
      cat_var <- selec_var()[[2]]
      row_names <- NULL
      n <- NULL
      for (i in 1:length(colnames(cat_var))){
        cat_var[,i] <- as.character(cat_var[,i])
        cat_var[,i][is.na(cat_var[,i])] <- "NA"
        a <- table(cat_var[,i])
        row_nam <- unique(cat_var[,i])[1]
        n_nam <- a[names(a)==unique(cat_var[,i])[1]]
        
        if(length(unique(cat_var[,i])) > 1){
          for(j in unique(cat_var[,i])[-1]){
            row_nam <- paste(row_nam,j,sep=",")
            n_nam <- paste(n_nam,a[names(a)==j],sep=",")
            
          }
        }
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
      
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
      cat(sprintf(info4), "\n")
      cat(sprintf(info5), "\n")
      cat(sprintf(info6), "\n")
      cat(sprintf(info7), "\n")
      cat(sprintf(info8), "\n")
      cat(sprintf(info9), "\n")
      cat(sprintf(info10), "\n")
    }
    else if(input$select1.1 == 2){
      cat_var <- selec_var()[[2]]
      a <- table(cat_var[,1])
      text_num <- a[names(a)==unique(cat_var[,1])[1]]
      text_group <- unique(cat_var[,1])[1]
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
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
    }
  })
  
  #Basic Statistics Table for explanatory variables
  makeText1.1.1 <- reactive({
    options(digits = 3)
    variable <- selec_var()[[1]]
    info <- describe(variable)
    
    if (length(variable) > 1){
      Missing <- NULL
      for (j in 1:length(variable)){
        Missing <- c(Missing, count_missing(variable[,j]))
      }
    }
    else{
      Missing <- count_missing(variable[,1])
    }
    
    info <- cbind(info[2],Missing,info[c(3:5,8,9,10)])
    info
  })
  
  helpText1.1.1 <- function(){
    info1 <- paste("The table below displays")
    info2 <- paste("")
    info3 <- paste("n: Number of present samples")
    info4 <- paste("missing: Number of missing samples")
    info5 <- paste("mean: average")
    info6 <- paste("sd: standard deviation")
    info7 <- paste("median: 50 percentile value")
    info8 <- paste("min: minimum value")
    info9 <- paste("max: maximum value")   
    info10 <- paste("range: max - min")
    
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    cat(sprintf(info7), "\n")
    cat(sprintf(info8), "\n")
    cat(sprintf(info9), "\n")
    cat(sprintf(info10), "\n")
  }
  
  makeText1.1.2 <- reactive({
    cat_var <- selec_var()[[2]]
    row_names <- NULL
    n <- NULL
    for (i in 1:length(colnames(cat_var))){
      a <- table(cat_var[,i])
      row_nam <- unique(cat_var[,i])[1]
      n_nam <- a[names(a)==unique(cat_var[,i])[1]]
      
      if(length(unique(cat_var[,i])) > 1){
        for(j in unique(cat_var[,i])[-1]){
          row_nam <- paste(row_nam,j,sep=",")
          n_nam <- paste(n_nam,a[names(a)==j],sep=",")
        }
      }
      row_nam <- paste(colnames(cat_var)[i], " (", row_nam, ") ", sep="")
      n_nam <- paste("(",n_nam,")",sep="")
      row_names <- c(row_names,row_nam)
      n <- c(n,n_nam)
    }
    df <- data.frame(n = n)
    rownames(df) <- row_names
    df <- format(df, justify = "left")
    print(df,right=F)
  })
  
  helpText1.1.2 <- reactive({
    cat_var <- selec_var()[[2]]
    a <- table(cat_var[,1])
    text_num <- a[names(a)==unique(cat_var[,1])[1]]
    text_group <- unique(cat_var[,1])[1]
    if(length(unique(cat_var[,1])) > 1){
      for(i in 2:length(unique(cat_var[,1]))){
        if(i != length(unique(cat_var[,1]))){
          text_num <- paste(text_num,a[names(a)==unique(cat_var[,1])[i]],sep=", ")
          text_group <- paste(text,text1,sep=", ")
        }
        else{
          text_num <- paste(text_num,a[names(a)==unique(cat_var[,1])[i]],sep=" & ")
          text_group <- paste(text,text1,sep=" & ")
        }
      }
    }
    info1 <- paste("The table below displays the sample size of each group for each categorical variable.")
    info2 <- paste("")
    info3 <- paste("For example: The variable ",colnames(cat_var)[1], " has ",
                   text_num, " samples for group ",text_group," respectively.",sep="")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
  })
  
  #Boxplot 
  makePlot1.2 <- function(text_size){
    dataset <- selec_var()[[1]]
    if(length(input$choose_variable1.2) > 20){
      variable <- dataset[,colnames(dataset) %in% input$choose_variable1.2[1:20],drop = FALSE]
    }
    else{
      variable <- dataset[,colnames(dataset) %in% input$choose_variable1.2,drop = FALSE]
    }
      
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
  
  helpText1.2 <- function(){
    info1 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
    info2 <- paste("")
    info3 <- paste("Recommended number of variables selected: <= 20.")
    info4 <- paste("If more than 20 variables are selected, only the first 20 will be plotted.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
  }
  
  makeTable1.3.1 <- reactive({
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    
    var_name <- NULL
    type <- NULL
    rho <- NULL
    p_value <- NULL
    q_value <- NULL
    r_squared <- NULL
    n <- NULL
    method <- NULL
    
    #Y(numeric) vs X(numeric,cat)
    if(input$var_interest %in% colnames(dataset)){
      varInterest <- dataset[,colnames(dataset) %in% input$var_interest, drop =F]
      for(i in input$choose_variable){
        if(i %in% colnames(dataset) & i != input$var_interest){
          v <- dataset[,colnames(dataset) %in% i, drop =F]
          a <- cor.test(varInterest[,1],v[,1],alternative = "two.sided",method = "spearman")
          var_name <- c(var_name,i)
          type <- c(type,"Continuous")
          n <- c(n, length(na.omit(cbind(varInterest,v))[,1]))
          rho <- c(rho, a$estimate)
          r_squared <- c(r_squared, (a$estimate)^2)
          p_value <- c(p_value, a$p.value)
          method <- c(method,"Spearman's rank correlation test")
        }
      }
      for(j in input$choose_variable){
        if(j %in% colnames(group) & j != input$var_interest){
          v <- group[,colnames(group) %in% j, drop =F]
          a <- kruskal.test(varInterest[,1],v[,1])
          var_name <- c(var_name,j)
          type <- c(type,"Categorical")
          rho <- c(rho, NA)
          r_squared <- c(r_squared, NA)
          p_value <- c(p_value, a$p.value)
          method <- c(method,"Kruskal-Wallis test")
          
          v_char <- as.character(v[,1])
          v_char[v_char == "NA"] <- NA
          varInterest_char <- as.character(varInterest[,1])
          varInterest_char[varInterest_char == "NA"] <- NA
          n <- c(n, length(na.omit(cbind(varInterest_char,v_char))[,1]))
        }
      }
      # if(max(p_value) < 0.7){
      #   q_value <- qvalue(p_value,lambda=seq(0,0.90,0.05))$qvalues
      # }
      # else if(max(p_value) >= 0.7){
      #   q_value <- qvalue(p_value)$qvalues
      # }
  
      df <- data.frame(Variable = var_name, Type = type, n = n, rho = rho, Rsquared = r_squared, PValue = p_value, Method = method, stringsAsFactors = F)
    }
    
    #Y(categorical) vs X(numeric,cat)
    else if(input$var_interest %in% colnames(group)){
      varInterest <- group[,colnames(group) %in% input$var_interest, drop =F]
      for(i in input$choose_variable){
        if(i %in% colnames(dataset) & i != input$var_interest){
          v <- dataset[,colnames(dataset) %in% i, drop =F]
          
          test <- multinom(varInterest[,1] ~ v[,1])
          a <- Anova(test)
          
          var_name <- c(var_name,i)
          type <- c(type,"Continuous")
          p_value <- c(p_value, a$`Pr(>Chisq)`)
          method <- c(method,"Multinomial Logistic Regression & Likelihood Ratio Test")
          varInterest_char <- as.character(varInterest[,1])
          varInterest_char[varInterest_char == "NA"] <- NA
          n <- c(n, length(na.omit(cbind(varInterest_char,v))[,1]))
        }
      }
      
      for(j in input$choose_variable){
        if(j %in% colnames(group) & j != input$var_interest){
          v <- group[,colnames(group) %in% j, drop =F]
          a <- fisher.test(varInterest[,1],v[,1],workspace = 2000000)
          var_name <- c(var_name,j)
          type <- c(type,"Categorical")
          p_value <- c(p_value, a$p.value)
          method <- c(method,"Fisher's exact test")
          
          v_char <- as.character(v[,1])
          v_char[v_char == "NA"] <- NA
          n <- c(n, length(na.omit(cbind(varInterest,v_char))[,1]))
        }
      }
      # if(max(p_value) < 0.7){
      #   q_value <- qvalue(p_value,lambda=seq(0,0.90,0.05))$qvalues
      # }
      # else if(max(p_value) >= 0.7){
      #   q_value <- qvalue(p_value)$qvalues
      # }
      
      
      
      df <- data.frame(Variable = var_name, Type = type, n = n, PValue = p_value, Method = method, stringsAsFactors = F)
    }
    
    rownames(df) <- var_name
    df <- df[order(df$PValue),]
    df
  })
  
  makeResults1.3.1 <- reactive({
    dataset <- makeTable1.3.1()
    
    dataset <- dataset[order(dataset$PValue),]
    info1 <- paste("Most statistically significant variable: ",dataset[1,1])
    info2 <- paste("No. of variables with p < 0.05: ",sum(dataset$PValue < 0.05))
    #info3 <- paste("No. of variables with q < 0.05: ",sum(dataset$QValue < 0.05))
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    #cat(sprintf(info3), "\n")
  })
  
  helpText1.3.1 <- reactive({
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    
    if(input$var_interest %in% colnames(dataset)){
      info1 <- paste("The headers in the table below represents")
      info2 <- paste("")
      info3 <- paste("Variable: Name of variables")
      info4 <- paste("Type: Variable type")
      info5 <- paste("n: Number of paired samples")
      info6 <- paste("rho (only continuous variables): Spearman's rank correlation value")
      info7 <- paste("RSquared (only continuous variables): rho^2")
      info8 <- paste("PValue: Significance level")
      #info9 <- paste("QValue: Adjusted P-Value")
      info10 <- paste("")   
      info11 <- paste("Method for continuous variables: Spearman's rank correlation test")
      info12 <- paste("Method for categorical variables: Kruskal-Wallis test")
      
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
      cat(sprintf(info4), "\n")
      cat(sprintf(info5), "\n")
      cat(sprintf(info6), "\n")
      cat(sprintf(info7), "\n")
      cat(sprintf(info8), "\n")
      #cat(sprintf(info9), "\n")
      cat(sprintf(info10), "\n")
      cat(sprintf(info11), "\n")
      cat(sprintf(info12), "\n")
    }

    else {
      info1 <- paste("The headers in the table below represents")
      info2 <- paste("")
      info3 <- paste("Variable: Name of variables")
      info4 <- paste("Type: Variable type")
      info5 <- paste("n: Number of paired samples")
      info6 <- paste("PValue: Significance level")
      #info7 <- paste("QValue: Adjusted P-Value")
      info8 <- paste("")   
      info9 <- paste("Method for continuous variable: Multinomial Logistic Regression & Likelihood Ratio Test")
      info10 <- paste("Method for categorical variable: Fisher's exact Test")
      
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
      cat(sprintf(info4), "\n")
      cat(sprintf(info5), "\n")
      cat(sprintf(info6), "\n")
      #cat(sprintf(info7), "\n")
      cat(sprintf(info8), "\n")
      cat(sprintf(info9), "\n")
      cat(sprintf(info10), "\n")
    }
  })
  
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
  
  makePlot1.3.2 <- function(){
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
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
        p <- ggplot(df, aes(x=VarX, y=VarY)) + geom_point(size=2, aes(colour=Group)) +
          scale_colour_hue(l=50) + # Use a slightly darker palette than normal
          geom_smooth(method=lm,   # Add linear regression lines
                      se=FALSE,    # Don't add shaded confidence region
                      aes(colour = Group),
                      fullrange=TRUE) +
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
        p <- ggplot(df, aes(x=VarX, y=VarY)) + geom_point(size=2) + 
          scale_colour_hue(l=50) + # Use a slightly darker palette than normal
          geom_smooth(method=lm,   # Add linear regression lines
                      se=FALSE,    # Don't add shaded confidence region
                      fullrange=TRUE) +
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
  
    else if(input$var_interest %in% colnames(group) & input$select1.3.2 %in% colnames(dataset)){
      variable <- dataset[,colnames(dataset) %in% input$select1.3.2,drop=F]
      group_var <- group[,colnames(group) %in% input$var_interest,drop=F]

      df <- cbind(variable,group_var)
      colnames(df) <- c("VarX","VarY")
      df <- na.omit(df)
      
      p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY )) + coord_flip() + geom_boxplot(width = (0.2 * length(unique(df$VarY)))) +
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
    
    else if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(group)){
      variable <- dataset[,colnames(dataset) %in% input$var_interest,drop=F]
      group_var <- group[,colnames(group) %in% input$select1.3.2,drop=F]
      
      df <- cbind(variable,group_var)
      colnames(df) <- c("VarX","VarY")
      df <- na.omit(df)
      
      p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY )) + geom_boxplot(width = (0.2 * length(unique(df$VarY)))) +
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
    else{
      group2 <- group[,colnames(group) %in% input$var_interest,drop=F]
      group1 <- group[,colnames(group) %in% input$select1.3.2,drop=F]
      df <- na.omit(cbind(group1,group2))
      
      df <- data.frame(df)
      freq=table(col(df), as.matrix(df))
      
      counts <- ddply(df, .(df[,1], df[,2]), nrow)
      colnames(counts) <- c("VarX","VarY","value")
      ggplot(counts, aes(VarX, value)) +   
        geom_bar(aes(fill = VarY), position = "dodge", stat="identity", colour = "black") + xlab(input$select1.3.2) + ylab("Freq") +
        ggtitle(input$main1.3.2) + scale_fill_discrete(name = input$var_interest) + theme(text = element_text(size=15))
    }
  }
  
  helpText1.3.2 <- function(){
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(dataset)){
      info1 <- paste("The scatterplot illustrates the relationship between 2 continuous variables.")
      info2 <- paste("A line of best fit is drawn in the plot.")
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
    }
    else if(input$var_interest %in% colnames(group) & input$select1.3.2 %in% colnames(dataset)){
      info1 <- paste("The boxplot illustrates the difference between groups of a continuous variables.")
      info2 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
    }
    else if(input$var_interest %in% colnames(dataset) & input$select1.3.2 %in% colnames(group)){
      info1 <- paste("The boxplot illustrates the difference between groups of a continuous variables.")
      info2 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of continuous variables.")
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
    }
    else{
      info1 <- paste("The barplot shows the number of samples in a categorical variables for each group.")
      cat(sprintf(info1), "\n")
    }
  }
  
  makeText2.1 <- reactive({
    n <- length(colnames(selec_var()[[1]]))
    if(input$select2.1 == 1){
      if(n > 15){
        variable <- selec_var()[[1]][,1:15]
        info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
        info
      }
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
      if(n > 15){
        variable <- selec_var()[[1]][,1:15]
        p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
        p.mat
      }
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
  
  makeText2.1.1 <- reactive({
    variable <- selec_var()[[1]]
    info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
    info
  })
  
  makeText2.1.2 <- reactive({
    variable <- selec_var()[[1]]
    p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
    p.mat
  })
  
  helpText2.1 <- reactive({
    if(input$select2.1 == 1){
      info1 <- paste("The table below displays the pairwise correlation values and")
      info2 <- paste("displays up to a maximum of 15 continuous variables.")
      info3 <- paste("")
      info4 <- paste("Download the data to view all of them.")
      
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
      cat(sprintf(info4), "\n")
    }
    else if(input$select2.1 == 2){
      info1 <- paste("The table below displays the pairwise significance values and")
      info2 <- paste("displays up to a maximum of 15 continuous variables.")
      info3 <- paste("")
      info4 <- paste("Download the data to view all of them.")
      
      cat(sprintf(info1), "\n")
      cat(sprintf(info2), "\n")
      cat(sprintf(info3), "\n")
      cat(sprintf(info4), "\n")
    }
  })
  
  #Correlation Matrix
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
  
  #Significance Table Results
  makeResults2.3.1 <- reactive({
    dataset <- makeTable2.3()
    dataset[,1] <- as.character(dataset[,1])
    info1 <- paste("Most statistically significant variable: ",dataset[1,1])
    info2 <- paste("No. of variables with p < 0.05: ",sum(dataset$PValue < 0.05))
    #info3 <- paste("No. of variables with q < 0.05: ",sum(dataset$QValue < 0.05))
    cat(info1, "\n")
    cat(info2, "\n")
    #cat(info3, "\n")
  })
  
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
    #info7 <- paste("QValue: Adjusted P-Value")

    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n") 
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    #cat(sprintf(info7), "\n")
  })
  
  #Correlation Significance Table
  makeTable2.3 <- reactive({
    
    dataset <- selec_var()[[1]]
    n <- NULL
    for(i in rownames(makeText2.1.2())){
      df <- dataset[,colnames(dataset) %in% c(input$choose_variable_2.3.1,i)]
      df <- na.omit(df)
      n <- c(n,length(rownames(df)))
    }
    
    if(input$type2 == "pearson"){
      df <- data.frame(Variable = rownames(makeText2.1.2()),
                       n = n,
                       r = as.numeric(makeText2.1.1()[,colnames(makeText2.1.1()) %in% input$choose_variable_2.3.1]),
                       PValue = as.numeric(makeText2.1.2()[,colnames(makeText2.1.2()) %in% input$choose_variable_2.3.1]),
                       stringsAsFactors = F)
    }
    
    if(input$type2 == "spearman"){
      df <- data.frame(Variable = rownames(makeText2.1.2()),
                       n = n,
                       rho = as.numeric(makeText2.1.1()[,colnames(makeText2.1.1()) %in% input$choose_variable_2.3.1]),
                       PValue = as.numeric(makeText2.1.2()[,colnames(makeText2.1.2()) %in% input$choose_variable_2.3.1]),
                       stringsAsFactors = F)
    }
    
    df <- df[-which(df[,1] == input$choose_variable_2.3.1),]
    df <- df[is.na(df$PValue) == F,]
    p_value <- df$PValue
    # if(max(p_value) < 0.7){
    #   q_value <- qvalue(p_value,lambda=seq(0,0.90,0.05))$qvalues
    # }
    # else if(max(p_value) >= 0.7){
    #   q_value <- qvalue(p_value)$qvalues
    # }
    # df$QValue <- q_value
    df <- df[df$PValue <  input$choose_alpha_level2.3,]
    df <- df[order(df[,3]),]
  })
  
  makeText2.3.2 <- reactive({
    info1 <- paste("The graph above displays the scatter plot between variables that")
    info2 <- paste("have statistically significant correlation between each other.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
  })
  
  #Scatterplot
  makePlot2.3 <- function(text_size){
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
        colnames(df)[c(1,2,3,4)] <- c("ID","Group","Var1","Var2")
      }
      else{
        df <- cbind(ID, group, variable)
        df <- na.omit(df)
        df[,2] <- as.factor(df[,2])
        colnames(df)[c(1,2,3,4)] <- c("ID","Group","Var1","Var2")
      }
      p <- ggplot(df, aes(x=Var1, y=Var2)) + geom_point(size=2, aes(colour=Group))  +
        scale_colour_hue(l=50) + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    aes(colour = Group),
                    fullrange=TRUE) +
        theme(text = element_text(size=text_size),
              axis.line.x = element_line(colour = "black", size = 1),
              axis.line.y = element_line(colour = "black", size = 1),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              axis.text.x = element_text(margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              plot.title = element_text(margin=margin(0,0,10,0)),
              legend.key.height = unit(2.5, "line")) + xlab(input$choose_variable_2.3.2) + ylab(input$choose_variable_2.3.3) +
        scale_fill_discrete(name = input$type2.3.2) + ggtitle(input$main2.3)
    }
    
    else if(input$type2.3.1 == 2){
      df <- cbind(ID, variable)
      df <- na.omit(df)
      colnames(df)[c(1,2,3)] <- c("ID","Var1","Var2")
      p <- ggplot(df, aes(x=Var1, y=Var2)) + geom_point(size=2) + 
        scale_colour_hue(l=50) + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    fullrange=TRUE) +
        theme(text = element_text(size=text_size),
              axis.line.x = element_line(colour = "black", size = 1),
              axis.line.y = element_line(colour = "black", size = 1),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              axis.text.x = element_text(margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$choose_variable_2.3.2) + ylab(input$choose_variable_2.3.3) +
        scale_fill_discrete(name = input$type2.3.2) + ggtitle(input$main2.3)
    }
    p
  }
  
  #PCA Results
  makeText3.1 <- reactive({
    x <- makeText3.2()[1,1]
    x <- round((x*100),1)
    y <- makeText3.2()[1,2]
    y <- round((y*100),1)
    info1 <- paste("The first principal component explains", paste(x,"%", sep = ""), "of the total variation of the variables.")
    info2 <- paste("The second principal component explains", paste(y,"%", sep = ""),  "of the total variation of the variables.")
    
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- values$data[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    count <- 2
    info <- cbind(info1, info2)
    for (i in unique(group[,1])){
      count <- count + 1
      n <- length(which(group[,1] == i))
      text <- paste("Number of samples in group",i,":",n)
      info <- cbind(info,text)
    }
    
    cat(info, sep = "\n")
    
  })
  
  #PCA Biplot Results
  makeText3.2 <- function(){
    variable <- selec_var()[[1]]
    
    rownames(variable) <- values$data[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    
    info <- summary(var.pca)
    eigen <- info[[1]]^2
    newinfo <- rbind("Eigenvalues" = eigen, info$importance)
    newinfo[3:4,]
  }
  
  helpText3.1 <- function(){
    info1 <- paste("PCA computes the variables into principal components, which are sorted accordingly to the proportion of variance it explains.")
    info2 <- paste("")
    info3 <- paste("If there are more than 5 principal componetns, only the first 5 will be shown in the table.")
    info4 <- paste("Download the table to view all of them.")
    info5 <- paste("")
    info6 <- paste("Missing values are imputed using regularized iterative PCA algorithm.")
    info7 <- paste("Variables are automatically standardized for PCA.")
    info8 <- paste("Only continuous variables are used for PCA.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    cat(sprintf(info7), "\n")
    cat(sprintf(info8), "\n")
  }
  
  #PCA Biplot
  makePlot3 <- function(text_size){
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group1 <<- group[,which(colnames(group) %in% input$choose_group3)]
    
    rownames(variable) <- values$data[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    
    g <- ggbiplot(var.pca, varname.size = 3, obs.scale = 1, var.scale = 1,
                  choices = c(as.numeric(str_sub(input$type3.2,3)),
                              as.numeric(str_sub(input$type3.3,3))),
                  groups = group1, ellipse = TRUE,
                  circle = F) +
      geom_point(aes(color=group1, size = 3)) + scale_size_identity() +
      theme(legend.direction = 'vertical', legend.position = 'right',
            legend.key.height = unit(2.5, "line"),
            text = element_text(size=text_size),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            axis.text.x = element_text(margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            axis.title = element_text(margin=margin(0,0,20,0))) +
      
      ggtitle(input$main3) + scale_colour_discrete(name = input$choose_group3)
    
    g
  }
  
  helpText3.2 <- function(){
    info1 <- paste("The plot above displays the PCA biplot.")
    info2 <- paste("")
    info3 <- paste("Samples are coloured according to the group variable.")
    info4 <- paste("The arrows represent the position of the variables.")
    info5 <- paste("The samples are located with respect to its value on the variables.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
  }
  
  helpText4 <- function(){
    info1 <- paste("The plot displays the HC dendrogram, which clusters similar samples and variables together.")
    info2 <- paste("")
    info3 <- paste("Selection of group variables determines the label colour of each sample in the dendrogram.")
    info4 <- paste("")
    info5 <- paste("The missing values are imputed using the Kth Nearest Neighbour Algorithm.")
    info6 <- paste("Distance: Euclidean, Cluster Method: Ward")
    info7 <- paste("Variables are automatically standardized for Hierarchical Clustering.")
    info8 <- paste("Only continuous variables are used for Hierarchical Clustering.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    cat(sprintf(info7), "\n")
    cat(sprintf(info8), "\n")
    # cat(sprintf(info6), "\n")
    # cat(sprintf(info7), "\n")
    # cat(sprintf(info8), "\n")
  }
  
  #Interactive heatmap
  makePlot4.1 <- function(){
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group1 <<- group[,which(colnames(group) %in% input$choose_group4)]
    if(class(group1) != "data.frame"){
      group1 <- data.frame(group1)
    }
    
    rownames(variable) <- values$data[,1]
    
    variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
    n_col <- length(colnames(variable))
    n_col <- n_col / 2
    variable <- variable[,1:n_col]
    
    variable <- scale(variable)
    
    p <- heatmaply(t(variable), scale='none', cexCol = 0, 
                   scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colors = rev(rainbow(20*10, start = 0/6, end = 4/6))),
                   distfun = function(x) dist(x,method = "euclidean"),
                   hclustfun = function(x) hclust(x,method = "ward"))
    
    layout(p,
           title = input$main4,
           autosize = FALSE,
           width = 1000,
           height = 1000,
           margin = list(l = 250, r = 50, b = 250, t = 50, pad = 4)
    )
  }
  
  #Non-interactive heatmap
  makePlot4.2 <- function(){
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group1 <- group[,which(colnames(group) %in% input$choose_group4),drop=F]
    if(class(group1) != "data.frame"){
      group1 <- data.frame(group1)
    }
    
    rownames(variable) <- values$data[,1]
    
    variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
    n_col <- length(colnames(variable))
    n_col <- n_col / 2
    variable <- variable[,1:n_col]
    

    variable <- scale(variable)
    cols <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256))
    variable <- pmin(pmax(variable , -3), 3) 
    
    p <- heatmap.2(t(variable), Rowv = input$displayRow4, key = TRUE, scale = "none",
                   density.info="none", trace = "none", main = "title", 
                   cexRow = 1, margins = c(4,8), symkey = F, keysize = 1.0,
                   distfun = function(x) dist(x,method = "euclidean"),
                   hclustfun = function(x) hclust(x,method = "ward"))
    
    g_name <- values$data[,1,drop=F]
    g_colour <- rep(0,length(group1[,1]))
    
    col <- brewer.pal(11,"Spectral")
    
    for (i in 1:length(unique(group1[,1]))){
      for (j in 1:length(group1[,1])){
        if(group1[,1][j] == unique(group1[,1])[i]){
          g_colour[j] <- col[i]
        }
      }
    }
    
    df <- data.frame(cbind(g_name,g_colour), stringsAsFactors = F) 
    df[,2] <- as.character(df[,2])
    
    df2 <- data.frame(rownames(p$carpet))
    group_col <- NULL
    for(i in 1:length(df2[,1])){
      for(j in 1:length(df[,1])){
        if(df2[,1][i] == df[,1][j])
          group_col <- c(group_col,df[,2][j])
      }
    }
    
    leg <- as.character(unique(group1[,1]))
    fil <- col[1:length(unique(group1[,1]))]
    
    my_palette <- rev(rainbow(20*10, start = 0/6, end = 4/6))
    
    if(input$displayRow4){
      a <- NULL
    }

    if(!input$displayRow4){
      a <- NA
    }
    
    heatmap.2(t(variable), Rowv = input$displayRow4, scale ='none', symkey = F, keysize = 1.0, col = cols,
              key = TRUE, key.xlab = "Row Z-Score", density.info="none", trace = "none", main = input$main4, 
              ColSideColors = group_col, labCol = NULL, labRow = a, margins = c(4,14), cexRow = 1.5, cexCol = 1.5,
              distfun = function(x) dist(x,method = "euclidean"),
              hclustfun = function(x) hclust(x,method = "ward"))
    
    par(cex.main=1)
    legend("topright",
           title = input$choose_group4,
           legend = leg, 
           fill = fil, 
           bty="n", y.intersp = 1, cex=1)
  }

######################################################################################  
# Main Page
######################################################################################  
  output$title1 <- renderUI({
    h1(lst[[as.numeric(selec_var()[[3]])]], class="smaller-margins")
  })
  
  lista <- list()
  lista[[1]] <- tagList(h3("Select sub function"),
                        fluidRow(column(4,selectInput("sub_function", label = NULL,
                                                      choices = c("Characterization: Table" = 1,
                                                                  "Visualization: Boxplot" = 2,
                                                                  "Differential Analysis" = 3))),
                                 column(4,uiOutput("subsubfunction")))
                        )
  
  output$subsubfunction <- renderUI({
    if(input$sub_function == 3){
      selectInput("sub_subfunction",
                  label = NULL, 
                  choices = c("Significance test" = 1,
                              "Visualization" = 2))
    }
    else{
      return()
    }
  })

  lista[[2]] <- tagList(selectInput("sub_function",
                                    label = h3("Select sub function"),
                                    choices = c("Table: Correlation & P-Value" = 1,
                                                "Visulization: Correlation Matrix" = 2,
                                                "Advance Search: Significance Table & Scatterplot" = 3)),
                        uiOutput("uiExample2"),
                        radioButtons("type2", "Correlation Type", 
                                     choices = c("Pearson (parametric)" = "pearson",
                                                 "Spearman (non-parametric)" = "spearman"),
                                     inline = T),
                        br())
  
  output$uiExample2 <- renderUI({
    tipify(bsButton("pB2", "Help", icon=icon("question-circle"),  size = "extra-small"),
           "Unlike pearson correlation, spearman correlation is more resistant to outliers.",
           placement = "right")
  })
  
  lista[[3]] <- NULL
  
  lista[[4]] <- NULL
  
  #*****************************************#
  output$select_subfunc <- renderUI({
    selec_var()
    lista[[as.numeric(selec_var()[[3]])]]
  })
  #*****************************************#
  
  listb <- list()
  listb[["1-1"]] <- tagList(radioButtons("select1.1", paste("Select which statistics table to view"), 
                                         choices = c("Continuous variable" = 1, "Categorical variable" = 2), inline = T),
                            downloadButton('downloadData1.1', 'Download data'),
                            actionButton("help1.1","Help",icon=icon("question-circle")),
                            hidden(verbatimTextOutput("helptext1.1")),
                            verbatimTextOutput('text1.1'))
  
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
        
        d[8,1] <- paste("Title: Statistics Table for Explanatory Variables")
        
        d[10,] <- colnames(df)
        d[11:(size[1]+10),] <- df
        d[11:(size[1]+10),1] <- rownames(df)
        d[10,1] <- NA
        
        colnames(d) <- rep("", length(colnames(d)))
        write.csv(d, file,na="",row.names=F)
      }
      
      else if(input$select1.1 == 2){
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
  
  listb[["1-3"]] <- tagList(fluidRow(
                              column(4, uiOutput("varInterest")),
                              column(4, uiOutput("select1_3_2"))
                            ))
  
  output$varInterest <- renderUI({
    selectInput("var_interest", label = "Select outcome of interest", choices = input$choose_variable, multiple = F)
  })
  
  output$select1_3_2 <- renderUI({
    if(input$sub_subfunction == 2){
      df1 <- makeTable1.3.1()
      df1[,1] <- as.character(df1[,1])
      selectizeInput("select1.3.2", paste("Select significant variable (p < 0.05)"), 
                     choices = df1[df1$PValue < 0.05,1])
    }
    else{
      return()
    }
  })
  
  listb[["1-3-1"]] <- tagList(h3("Significance Test"),
                              downloadButton('downloadData1.3.1', 'Download data'),
                              actionButton("help1.3.1","Help",icon=icon("question-circle")),
                              actionButton('get1.3.1', 'Get results'),
                              hidden(verbatimTextOutput("helptext1.3.1")),
                              hidden(verbatimTextOutput("getresults1.3.1")),
                              dataTableOutput("table1.3.1"))
  
  output$downloadData1.3.1 <- downloadHandler(
    filename = function() {
      paste('Significance Table','.csv', sep='')},
    content = function(file) {
      df <- makeTable1.3.1()
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
      

      d[8,1] <- paste("Title: Significance Table for differential analysis.")

      d[9,1] <- paste("Variable of interest:",input$var_interest)
      
      d[11,] <- colnames(df)
      df[,1] <- as.character(df[,1])
      d[12:(size[1]+11),] <- df
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file, row.names = F,na="")})
  
  observeEvent(input$get1.3.1, {
    toggle("getresults1.3.1")
  })
  
  observeEvent(input$help1.3.1, {
    toggle("helptext1.3.1")
  })
  
  output$helptext1.3.1 <- renderPrint({
    helpText1.3.1()
  })
  
  output$getresults1.3.1 <- renderPrint({
    makeResults1.3.1()
  })
  
  output$table1.3.1 <- renderDataTable({
    makeTable1.3.1()
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
  
  observeEvent(input$help1.3.2, {
    toggle("helptext1.3.2")
  })
  
  output$helptext1.3.2 <- renderPrint({
    helpText1.3.2()
  })
  
  listb[["2-1"]] <- tagList(radioButtons("select2.1", paste("Select which table to view"), 
                                         choices = c("Correlation Table" = 1, "P-Value Table" = 2), inline = T),
                            downloadButton('downloadData2.1', 'Download data'),
                            actionButton("help2.1","Help",icon=icon("question-circle")),
                            hidden(verbatimTextOutput("helptext2.1")),
                            verbatimTextOutput('text2.1'))
  
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
  
  listb[["2-3"]] <- tagList(h3("Significance Table"),
                            fluidRow(
                              column(4, uiOutput('Choice2.3.1')),
                              column(8, uiOutput("Choose_alpha_level2.3"))
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
                              column(4, uiOutput("Type2.3.1")),
                              column(8, uiOutput("Type2.3.2"))
                            ),
                            
                            fluidRow(
                              column(4, uiOutput('Choice2.3.2')),
                              column(8, uiOutput('Choice2.3.3'))
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
  
  observeEvent(input$help2.3.1, {
    toggle("helptext2.3.1")
  })
  
  output$helptext2.3.1 <- renderPrint({
    helpText2.3.1()
  })
  
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
      
      d[8,1] <- paste("Title: Correlation Table for Continuous Variables")
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
      selectizeInput("choose_variable_2.3.3", "Select variable Y", choices = as.character(makeTable2.3()[,1]))
    }
  })

  
  output$downloadPlot2.3 <- downloadHandler(
    filename = function() {
      paste('Scatterplot','.pdf', sep='')},
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
  
  listb[["3"]] <- tagList(h3('Table of results explained by PC'),
                          br(),
                          downloadButton('downloadData3', 'Download data'),
                          actionButton("help3.1","Help",icon=icon("question-circle")),
                          actionButton('getPCA', 'Get results'),
                          hidden(verbatimTextOutput("text3.1")),
                          hidden(verbatimTextOutput("helptext3.1")),
                          verbatimTextOutput('text3.2'),
                          
                          br(),
                          
                          fluidRow(
                            column(4, uiOutput("Choice3.1")),
                            column(4, uiOutput("Choice3.2"))
                          ),
                          
                          fluidRow(
                            column(4,uiOutput("chooseGroup3"))
                          ),
                          
                          br(),
                          
                          textInput("main3", "Key in the title of PCA plot"),
                          plotOutput("plot3", height = 800, width = 800, hover = "plot1_hover3", brush = "plot1_brush3"),
                          br(),
                          downloadButton('downloadPlot3', 'Download plot as pdf'),
                          actionButton("inter3","Display interactivity"),
                          actionButton("help3.2","Help",icon=icon("question-circle")),
                          
                          br(), 
                          
                          fluidRow(
                            column(6, hidden(textOutput("hovered3")),
                                   hidden(uiOutput("uiExample3.1")),
                                   hidden(verbatimTextOutput("hover_info3"))),
                            column(6, hidden(textOutput("selected3")),
                                   hidden(uiOutput("uiExample3.2")),
                                   hidden(verbatimTextOutput("brush_info3")))
                          ),
                          
                          br(),
                          
                          hidden(verbatimTextOutput("helptext3.2"))
                          )
  
  output$uiExample3.1 <- renderUI({
    tipify(bsButton("pB31", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "Only categorical variables for group variables.")
  })

  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste('PCA Result','.csv', sep='')},
    content = function(file) {
      df <- makeText3.2()
      df <- cbind(rownames(df),df)
      df[1,1] <- NA
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
      
      d[8,1] <- paste("Title: PCA Results")
      
      d[10,] <- colnames(df)
      d[11:(size[1]+10),] <- df
      d[11:(size[1]+10),1] <- rownames(df)
      d[10,1] <- NA
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file,row.names=F,na="")})
  
  observeEvent(input$getPCA, {
    toggle("text3.1")
  })
  
  output$text3.1 <- renderPrint({
    makeText3.1()
  })
  
  observeEvent(input$help3.1, {
    toggle("helptext3.1")
  })
  
  output$helptext3.1 <- renderPrint({
    helpText3.1()
  })
  
  output$text3.2<- renderPrint(
    if(length(colnames(makeText3.2())) > 5){
      makeText3.2()[,1:5]
    }
    else{
      makeText3.2()
    }
  )
  
  output$Choice3.1 <- renderUI({
    selectizeInput("type3.2", "Select principal component on x axis", 
                   choices = colnames(makeText3.2()))
  })
  
  output$Choice3.2<- renderUI({
    selectizeInput("type3.3", "Select principal component on y axis", 
                   choices = colnames(makeText3.2())[-which(colnames(makeText3.2()) == input$type3.2)])
  })
  
  output$chooseGroup3 <- renderUI({
    selectizeInput("choose_group3", "Select group variable (only categorical variables)",
                   choices = colnames(selec_var()[[2]]), multiple = F)
  })
  
  observeEvent(input$help3.2, {
    toggle("helptext3.2")
  })
  
  output$helptext3.2 <- renderPrint({
    helpText3.2()
  })

  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste('PCA Biplot','.pdf', sep='')},
    content = function(file) {
      ggsave(file, makePlot3(15), dpi = 300, height = 30, width = 30, units = "cm")})
  
  output$plot3 <- renderPlot({
    makePlot3(15)
  })
  
  observeEvent(input$inter3, {
    toggle("uiExample3.1")
    toggle("uiExample3.2")
    toggle("hovered3")
    toggle("hover_info3")
    toggle("selected3")
    toggle("brush_info3")
  })
  
  output$uiExample3.1 <- renderUI({
    tipify(bsButton("pB31", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "Displays the sample which the mouse is hovered on.")
  })
  
  output$uiExample3.2 <- renderUI({
    tipify(bsButton("pB32", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "Displays samples which the mouse selects on the plot.")
  })
  
  output$hovered3 <- renderText({
    "Hovered Point"
  })
  
  output$hover_info3 <- renderPrint({
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- values$data[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    
    var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    df <- cbind(values$data[,1],group,variable,var.pca$x)
    
    colnames(df)[1:2] <- c("Subject#", input$choose_group3)
    
    nearPoints(df, input$plot1_hover3, xvar = input$type3.2, yvar = input$type3.3, threshold = 10, maxpoints = 1)[,c(1:2)]
  })
  
  output$selected3 <- renderText({
    "Selected Points"
  })
  
  output$brush_info3 <- renderPrint({
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- values$data[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    
    df <- cbind(values$data[,1],group,variable,var.pca$x)
    
    colnames(df)[1:2] <- c("Subject#", input$choose_group3)
    
    
    brushedPoints(df, input$plot1_brush3, xvar = input$type3.2, yvar = input$type3.3)[,c(1:2)]
  })
  
  listb[["4"]] <- tagList(uiOutput("chooseGroup4"),
                          checkboxInput("displayRow4", "Display Row Dendrogram", value = T),
                          uiOutput("uiExample4"),
                          
                          br(),
                          br(),

                          textInput("main4", "Key in the title of Cluster Dendrogram"),
                          plotOutput("plot4",height = "800px",width = "1000px"),
                          downloadButton('downloadPlot4', 'Download the plot as png'),
                          actionButton("help4","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext4")))
                          #plotlyOutput("plot4.1",height = "800px"))
  
  output$chooseGroup4 <- renderUI({
    selectizeInput("choose_group4", "Select group variable (only categorical variables)",
                   choices = colnames(selec_var()[[2]]), multiple = F)
  })
  
  output$uiExample4 <- renderUI({
    tipify(bsButton("pB4", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "Clusters similar variables together.")
  })
  
  observeEvent(input$help4, {
    toggle("helptext4")
  })
  
  output$helptext4 <- renderPrint({
    helpText4()
  })
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() {
      paste('Heatmap','.png', sep='')},
    content = function(file) {
      png(file,antialias = "cleartype", height = 700, width = 700)
      print(makePlot4.2())
      dev.off()})
  
  output$plot4 <- renderPlot({
    makePlot4.2()
  })
  
  #****************************************************************#
  output$output1 <- renderUI({
    selec_var()
    if(selec_var()[[3]] == 3 | selec_var()[[3]] == 4){
      listb[[selec_var()[[3]]]]
    }
    else{
      listb[[paste(selec_var()[[3]],input$sub_function,sep="-")]]
    }
  })
  #****************************************************************#
  output$output2 <- renderUI({
    selec_var()
    if(selec_var()[[3]] == 1 ){
      listb[[paste(selec_var()[[3]],input$sub_function,input$sub_subfunction,sep="-")]]
    }
    else{
      return()
    }
  })
  #****************************************************************#
})
