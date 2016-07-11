library(shiny)
library(shinyjs)
library(shinyBS)
source("helpers.R")


shinyServer(function(input, output, session) {
  values <- reactiveValues(sessionId = NULL)
  values$con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
  values$data <- NULL
  
  session$onSessionEnded(function() {
    
    observe(dbDisconnect(values$con))
  })
  
################################################################################################ 
  
  updateStudies <- function(current_project) {
    
    get_project_pk <- sprintf("SELECT pk FROM project WHERE project_code=\'%s\'", input$projectChoice)
    project_pk <- dbGetQuery(values$con, get_project_pk)[["pk"]]
    get_studies <- sprintf("SELECT study_name FROM study WHERE study.project_pk=%i", project_pk)
    study_list <- dbGetQuery(values$con, get_studies)[["study_name"]]
    updateSelectInput(session, "studyChoices", choices=study_list, selected=study_list)
    
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
    updateProjects()
    updateStudies(input$projectChoice)
    progress$close()
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
        incProgress(1/100)
        search_query <- sprintf("SELECT pk FROM study WHERE study_name=\'%s\'", study_name_list[[i]])
        pk_list[i] <- dbGetQuery(values$con, search_query)
      }
      frame <- getStudyDataFrame(values$con, pk_list)
      wide_format <- spread(frame, "new_name", "value")
      values$data <<- wide_format
      output$loadSuccess <- renderText("Data loaded.")
      output$currentProject <- renderText(paste("Project: ", input$projectChoice))
    })
  })
  
  
  process_data <- eventReactive(input$preProcess, {
    withProgress(message="Processing data", {
      
      dataset <- values$data
      
      if("Remarks" %in% substr(names(dataset),1,7)){
        dataset <- dataset[,-which( substr(names(dataset),1,7) == "Remarks")]
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
      for (i in 1:length(colnames(dataset))){
        if(gsub("^.*?_","",colnames(dataset)[i]) == -1){
          colnames(dataset)[i] <- gsub("_.*","",colnames(dataset)[i])
        }
      }
      
    })
    dataset
  })
  
  output$downloadB <- renderUI({
    process_data()
    downloadButton("download_process", "Download merged file")
  })
  
  output$download_process <- downloadHandler(
    filename = function() {
      paste("merged_file","csv",sep=".")
    },
    content <- function(file) {
      d <- data.frame()
      size <- dim(process_data())
      d[1:(size[1]+5),] <- NA
      d[,1:size[2]] <- NA
      d[1,1] <- "Name of Project:"
      d[1,2] <- input$projectChoice
      d[2,1] <- "Date & Time:"
      d[2,2] <- as.character(Sys.time())
      d[3,1] <- "Timezone:"
      d[3,2] <- as.character(Sys.timezone())
      d[4,1] <- "This dataset is formed by merging the following values$data:"
      
      nam <- input$studyChoices[1]
      for (i in input$studyChoices[-1]){
        nam <- paste(nam,"\n",i,sep = "")
      }
      d[4,2] <- nam
      
      d[6,1] <- paste("This dataset is processed by removing rows that contain more than (and equal to) ",
                      input$row_cutoff, "% missing values and removing columns that contain more than (and equal to) ",
                      input$col_cutoff, "% missing values.", sep = "")
      
      d[8,] <- colnames(process_data())
      d[9:(size[1]+8),] <- process_data()
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file, row.names = F, na="")
    }
  )
  
  output$viewB <- renderUI({
    process_data()
    actionButton('view', 'Click to view merged file')
  })
  
  observeEvent(input$view, {
    toggle("merged")
  })
  
  output$merged <- renderTable({
    process_data()
  })
  
  acrossVariableTable <- observeEvent(input$across, {
    info_table <- getVariableAcross(values$con, input$acrossSearchType, input$acrossSelect)
    if (nrow(info_table) == 0) {
      output$acrossFail <- renderText("No results.")
      output$acrossInfo <- renderDataTable(info_table)
      return()
    }
    output$acrossFail <- renderText("")
    if (!(strcmp(input$acrossSearchType, "Group"))) {
      for (i in 1:nrow(info_table)) {
        info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[{}\"]", '')
        info_table[i,"(Day, Samples)"] <- str_replace_all(info_table[i,"(Day, Samples)"],"[,]", ', ')
      }
    }
    output$acrossInfo <- renderDataTable(info_table)
  })
  
  observe({
    if(input$back == T){
      updateCheckboxInput(session,"begin",value=F)
      current_proj <<- input$projectChoice
    }
  })
  
  observe({
    if(input$begin == T){
      updateCheckboxInput(session,"back",value=F)
    }
  })
  
################################################################################################ 
  
  output$selectTime <- renderUI({
    dataset <- process_data()
    if(input$select_time == 1){
      d <- unique(as.numeric(gsub("^.*?_","",colnames(dataset))))
      d <- d[!is.na(d)]
      selectizeInput("choose_time", "Select timepoint", choices = sort(d), 
                     multiple = T)
    }
  })
  
  output$selectAll <- renderUI({
    dataset <- process_data()
    if(input$select_time == 1){
      radioButtons("select_all", "Select all variables within the timepoint?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
    }
    else if(input$select_time == 2){
      radioButtons("select_all", "Select all variables?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
    }
  })
  
  output$choose_var <- renderUI({
    dataset <- process_data()[-1]
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
        selectizeInput("choose_variable", "Select explanatory variables", choices = colnames(process_data())[-1], 
                       multiple = T)
      }
      else if(input$select_all == 1){
        selectInput("choose_variable", "Select explanatory variables", choices = colnames(process_data())[-1], 
                    multiple = T, selectize = F, selected = colnames(process_data())[-1], size = 10)
      }
    }
  })
  
  output$select_cat_var <- renderUI({
    dataset <- process_data()[-1]
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
  
  output$help <- renderUI({
    dataset <- process_data()
    dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
    if(length(unique(gsub("_.*","",colnames(dataset)))) > 1){
      helpText("The variables in the list above are sorted by its likelihood to be a categorical variable. 
               Variables suspected to be categorical have been preselected.")
    }
  })
  
  output$select_func <- renderUI({
    process_data()
    selectInput("main_function",
                "Select main function",
                choices = c("Statistics" = 1,
                            "Correlation" = 2,
                            "Principal Compoment Analysis" = 3,
                            "Hierarchical Clustering & Heatmaps" = 4)
                )
  })
  
  output$help1 <- renderUI({
    process_data()
    helpText("Click the Select button after you have finished selecting the explanatory variables, 
              the group variable and the main function.")
  })
  
  output$select_var <- renderUI({
    process_data()
    actionButton('select_variable', "Select")
  })
  
  selec_var <- eventReactive(input$select_variable,{
    dataset <- process_data()[-1]
    dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
    exp_var <- dataset[,!gsub("_.*","",colnames(dataset)) %in% input$cat_variable,drop = FALSE]
    cat_var <- dataset[,gsub("_.*","",colnames(dataset)) %in% input$cat_variable,drop = FALSE]
    
    for(i in 1:length(colnames(exp_var))){
      exp_var[,i] <- as.numeric(exp_var[,i])
    }
    
    if(length(colnames(cat_var)) > 0){
      for(i in 1:length(colnames(cat_var))){
        cat_var[,i] <- as.factor(as.character(cat_var[,i]))
      }
    }
    
    list(exp_var,  cat_var, input$main_function)
  })
  
  lst <- list()
  lst[[1]] <- "Statistics"
  lst[[2]] <- "Correlation"
  lst[[3]] <- "Principal Component Analysis"
  lst[[4]] <- "Hierarchical Clustering & Heatmaps"

  output$help2 <- renderUI({
    selec_var()
    helpText("You have selected ",length(selec_var()[[1]])," explanatory variables, ",
             length(selec_var()[[2]]), " explanatory variables as categorical variables and ",lst[[as.numeric(selec_var()[[3]])]],
             " as the main function.")
  })
  
  output$warning1 <- renderUI({
    selec_var()
    if(length(input$cat_variable) == 0){
      helpText("Warning: No categorical variables are selected.")
    }
  })
  
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
    info1 <- paste("The table below displays basic statistical information such as sample size, number of missing samples and average value.")
    
    cat(sprintf(info1), "\n")
  }
  
  makeText1.1.2 <- reactive({
    cat_var <- selec_var()[[2]]
    row_names <- NULL
    n <- NULL
    for (i in 1:length(colnames(cat_var))){
      a <- table(cat_var[,i])
      row_nam <- unique(cat_var[,i])[1]
      n_nam <- a[names(a)==unique(cat_var[,i])[1]]
      
      if(length(unique(cat_var[,1])) > 1){
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
    text <- paste(a[names(a)==unique(cat_var[,1])[1]],"samples for group",unique(cat_var[,1])[1])
    if(length(unique(cat_var[,1])) > 1){
      for(i in 2:length(unique(cat_var[,1]))){
        if(i != length(unique(cat_var[,1]))){
          text1 <- paste(a[names(a)==unique(cat_var[,1])[i]],"samples for group",unique(cat_var[,1])[i])
          text <- paste(text,text1,sep=", ")
        }
        else{
          text1 <- paste(a[names(a)==unique(cat_var[,1])[i]],"samples for group",unique(cat_var[,1])[i])
          text <- paste(text," & ", text1,".",sep="")
        }
      }
    }
    info1 <- paste("The table below displays the sample size of each group for each categorical variable.")
    info2 <- paste("")
    info3 <- paste("For example: The variable ",colnames(cat_var)[1], " has ",text,sep="")
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
    info1 <- paste("The boxplot displays the max,min,median, 25th and 75th percentile of non-categorical variables.")
    info2 <- paste("")
    info3 <- paste("For vizualization, recommended number of variables selected should not exceed 20.")
    info4 <- paste("If more than 20 variables are selected, only the first 20 will be plotted.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
  }
  
  makeText1.3.1 <- reactive({
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    
    #Y(numeric) vs X(numeric)
    if(input$var_interest %in% selec_var()[[1]]){
      dataset <- selec_var()[[1]]
      varInterest <- dataset[,colnames(dataset) %in% input$var_interest, drop =F]
      var_name <- NULL
      rho <- NULL
      p_value <- NULL
      q_value <- NULL
      r_squared <- NULL
      n <- NULL
      for(i in input$choose_variable){
        if(i %in% colnames(selec_var()[[1]]) $ i != input$var_interest){
          v <- dataset[,colnames(dataset) %in% i, drop =F]
          a <- cor.test(var_Interest,v,alternative = "two.sided",method = "spearman")
          var_name <- c(var_name,i)
          p-value <- c(p-value, a$p.value)
          rho <- c(rho, a$estimate)
          r_squared <- c(r_squared, a$estimate)
          n <- c(n, length(na.omit(cbind(var_name,v))[,1]))
        }
      }
      q_value <- qvalue(p = pvalues)$qvalues
      #df <- data.frame(n = n, rho = rho, P-value = p_value, Q-value = q_value, R-squared = r_squared)
    }
  })
  
  #Boxplot by group
  makePlot1.3 <- function(text_size){
    if(length(input$choose_variable1.3) > 0){
      variable <- process_data()[,colnames(process_data()) %in% input$choose_variable1.3,drop = FALSE]
      
      group_var <- process_data()[,colnames(process_data()) %in% selec_var()[[2]],drop = FALSE]
      variable$Group <- as.factor(group_var[,1])
      df <- melt(variable, id.vars = "Group")
      
      p <- ggplot(df, aes(variable, value, fill = Group)) + geom_boxplot(width = (0.05 * length(colnames(variable)))) + 
        theme(text = element_text(size=text_size), 
              axis.text.x = element_text(angle=45, hjust=1, margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.key.height = unit(2.5, "line"),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              plot.title = element_text(margin=margin(0,0,20,0))) +
        ggtitle(input$main1.3) 
      p
    }
  }
  
  #Correlation Table
  makeText2.1.1 <- function(){
    variable <- process_data()[,colnames(process_data()) %in% selec_var()[[1]],drop = FALSE]
    info <- cor(variable, use = "pairwise.complete.obs", method = input$type2)
    info
  }
  
  #P Value Table
  makeText2.1.2 <- function(){
    variable <- process_data()[,colnames(process_data()) %in% selec_var()[[1]],drop = FALSE]
    p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2)
    p.mat
  }
  
  #Correlation Matrix
  makePlot2.2 <- function(text_size){
    if(length(input$choose_variable2.2) >= 2){
      if(input$type2 == "pearson"){
        r <- "r"
      }
      
      if(input$type2 == "spearman"){
        r <- "rho"
      }
      
      variable <- process_data()[,colnames(process_data()) %in% input$choose_variable2.2,drop = FALSE]
      
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
      
      j <- NULL
      for (i in 1:length(info[,1])){
        if (all_missing(info[i,])){
          j <- c(j,i)
        }
      }
      
      if(length(j) > 0){
        info <- info[-j,]
        p.mat <- p.mat[-j,]
      }
      
      cordata <- melt(info)
      pdata <- melt(p.mat)
      
      cordata$labelr = abbreviateSTR_R(melt(cordata)$value, prefix = r)
      cordata$labelP = abbreviateSTR_P(melt(pdata)$value, prefix = 'p', 0.05)
      cordata$label = paste(cordata$labelr, "\n", 
                            cordata$labelP, sep = "")
      
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
  }
  
  #Significance Table Results
  makeText2.3.1 <- reactive({
    if(input$type2 == "pearson"){
      r <<- "r"
    }
    
    if(input$type2 == "spearman"){
      r <<- "rho"
    }
    
    info1 <- paste("The table below displays which variables have statistically significant (p<",input$choose_alpha_level2.3,")", sep = "")
    info2 <- paste("correlation values with the selected variable.")
    info3 <- paste("")
    info4 <- paste(r, " = correlation coefficient", sep = "")
    info5 <- paste("p = p-value")
    info6 <- paste("n = number of paired samples")
    info7 <- paste("")
    info8 <- paste("Number of significant variables:", length(makeTable2.3()[,1]))
    
    if(length(makeTable2.3()[,1]) == 0){
      info9 <- paste("Lowest P-value:", 0)
      info10 <- paste("Highest P-value:", 0)
    }
    
    else if(length(makeTable2.3()[,1]) != 0){
      info9 <- paste("Lowest P-value:", round(min(makeTable2.3()[,3]),3))
      if(round(min(makeTable2.3()[,3]),3) < 0.01){
        info9 <- paste("Lowest P-value:", "<0.01")
      }
      
      info10 <- paste("Highest P-value:", round(max(makeTable2.3()[,3]),3))
      if(round(max(makeTable2.3()[,3]),3) < 0.01){
        info10 <- paste("Highest P-value:", "<0.01")
      }
    }
    
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
  })
  
  #Correlation Significance Table
  makeTable2.3 <- function(){
    
    n <- NULL
    for(i in rownames(makeText2.1.2())){
      df <- process_data()[,colnames(process_data()) %in% c(input$choose_variable_2.3.1,i)]
      df <- na.omit(df)
      n <- c(n,length(rownames(df)))
    }
    
    if(input$type2 == "pearson"){
      df <- data.frame(Variable = rownames(makeText2.1.2()),
                       r = as.numeric(makeText2.1.1()[,colnames(makeText2.1.1()) %in% input$choose_variable_2.3.1]),
                       p = as.numeric(makeText2.1.2()[,colnames(makeText2.1.2()) %in% input$choose_variable_2.3.1]),
                       n = n)
    }
    
    if(input$type2 == "spearman"){
      df <- data.frame(Variable = rownames(makeText2.1.2()),
                       rho = as.numeric(makeText2.1.1()[,colnames(makeText2.1.1()) %in% input$choose_variable_2.3.1]),
                       p = as.numeric(makeText2.1.2()[,colnames(makeText2.1.2()) %in% input$choose_variable_2.3.1]),
                       n = n)
    }
    
    df <- df[-which(df[,1] == input$choose_variable_2.3.1),]
    df <- df[is.na(df$p) == F,]
    df <- df[df$p <  input$choose_alpha_level2.3,]
    df <- df[order(df[,3]),]
  }
  
  makeText2.3.2 <- reactive({
    info1 <- paste("The graph below displays the scatter plot between variables that")
    info2 <- paste("have statistically significant correlation between each other.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
  })
  
  #Scatterplot
  makePlot2.3 <- function(text_size){
    variable <- process_data()[,colnames(process_data()) %in% c(input$choose_variable_2.3.2, input$choose_variable_2.3.3),drop = FALSE]
    group <- process_data()[,colnames(process_data()) %in% selec_var()[[2]], drop = FALSE]
    ID <- process_data()[,1, drop = F]
    
    variable_1 <- na.omit(variable)
    group_1 <- group[as.numeric(rownames(variable_1)),]
    ID_1 <- ID[as.numeric(rownames(variable_1)),]
    
    df <- cbind(ID_1, group_1, variable_1)
    df[,2] <- as.factor(df[,2])
    
    colnames(df)[1:2] <- c("ID", "Group")
    
    if(input$type2.3.1 == 1){
      p <- ggplot(df, aes_string(x=input$choose_variable_2.3.2, y=input$choose_variable_2.3.3)) + geom_point(size=2, aes(colour=Group))  +
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
              legend.key.height = unit(2.5, "line")) +
        ggtitle(input$main2.3)
    }
    
    if(input$type2.3.1 == 2){
      p <- ggplot(df, aes_string(x=input$choose_variable_2.3.2, y=input$choose_variable_2.3.3)) + geom_point(size=2) + 
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
              plot.title = element_text(margin=margin(0,0,10,0))) +
        ggtitle(input$main2.3) 
    }
    p
  }
  
  #PCA Biplot Results
  makeText3.2 <- reactive({
    x <- makeText3.1()[3,which(colnames(makeText3.1()) == input$type3.2)]
    x <- round((x*100),1)
    y <- makeText3.1()[3,which(colnames(makeText3.1()) == input$type3.3)]
    y <- round((y*100),1)
    info1 <- paste("The principal component on the x axis explains", paste(x,"%", sep = ""), "of the total variation of the variables.")
    info2 <- paste("The principal component on the y axis explains", paste(y,"%", sep = ""),  "of the total variation of the variables.")
    
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- process_data()[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    count <- 2
    info <- cbind(info1, info2)
    for (i in unique(group[,1])){
      count <- count + 1
      n <- length(which(group[,1] == i))
      text <- paste("Number of samples remaining in group",i,":",n)
      info <- cbind(info,text)
    }
    
    cat(info, sep = "\n")
    
  })
  
  #PCA Results
  makeText3.1 <- function(){
    variable <- selec_var()[[1]][-194]
    
    rownames(variable) <- process_data()[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    if(input$type3.1 == 1){
      var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    }
    
    if(input$type3.1 == 2){
      var.pca <- prcomp(variable, center = FALSE, scale. = FALSE) 
    }
    
    info <- summary(var.pca)
    eigen <- info[[1]]^2
    newinfo <- rbind("Eigenvalues" = eigen, info$importance)
    newinfo
  }
  
  helpText3 <- function(){
    info1 <- paste("PCA computes the variables into principal components, which are sorted accordingly to the proportion of variance it explains.")
    info2 <- paste("")
    info3 <- paste("If there are more than 10 principal componetns, only the first 10 will be shown in the table.")
    info4 <- paste("Download the table to view all of them.")
    info5 <- paste("")
    info6 <- paste("Standardization accounts for the different units and variation of the variables.")
    info7 <- paste("")
    info8 <- paste("Selection of group variables determines the colour of each sample in the biplot.")
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
    
    rownames(variable) <- process_data()[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs

    if(input$type3.1 == 1){
      var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    }
    
    if(input$type3.1 == 2){
      var.pca <- prcomp(variable, center = FALSE, scale. = FALSE) 
    }
   
    g <- ggbiplot(var.pca, varname.size = 4, obs.scale = 1, var.scale = 1,
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
      
      ggtitle(input$main3)
    
    g
  }
  
  helpText4 <- function(){
    info1 <- paste("The plot displays the HC dendrogram, which clusters similar samples and variables together.")
    info2 <- paste("")
    info3 <- paste("Standardization accounts for the different units and variation of the variables.")
    info4 <- paste("")
    info5 <- paste("Selection of group variables determines the label colour of each sample in the dendrogram.")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
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
    
    rownames(variable) <- process_data()[,1]
    
    variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
    n_col <- length(colnames(variable))
    n_col <- n_col / 2
    variable <- variable[,1:n_col]
    
    if(input$type4.1 == 1){
      variable <- scale(variable)
    }
    
    p <- heatmaply(t(variable), scale='none', cexCol = 0, 
                   scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colors = rev(rainbow(20*10, start = 0/6, end = 4/6))),
                   distfun = function(x) dist(x,method = input$type4.2),
                   hclustfun = function(x) hclust(x,method = input$type4.3))
    
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
    
    rownames(variable) <- process_data()[,1]
    
    variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
    n_col <- length(colnames(variable))
    n_col <- n_col / 2
    variable <- variable[,1:n_col]
    
    if(input$type4.1 == 1){
      variable <- scale(variable)
    }
    
    p <- heatmap.2(t(variable), key = TRUE, scale = "none",
                   density.info="none", trace = "none", main = "title", 
                   cexRow = 1, margins = c(4,8),
                   distfun = function(x) dist(x,method = input$type4.2),
                   hclustfun = function(x) hclust(x,method = input$type4.3))
    
    g_name <- process_data()[,1,drop=F]
    g_colour <- rep(0,length(group1[,1]))
    
    col <- brewer.pal(12,"Paired")
    
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
    
    heatmap.2(t(variable), scale ='none', col=my_palette,
              key = TRUE, key.xlab = "Row Z-Score", density.info="none", trace = "none", main = input$main4, 
              ColSideColors = group_col, labCol = NA,margins = c(4,12), cexRow = 1.5,
              distfun = function(x) dist(x,method = input$type4.2),
              hclustfun = function(x) hclust(x,method = input$type4.3))
    
    par(cex.main=1)
    legend("topright",
           title = "Group",
           legend = leg, 
           fill = fil, 
           bty="n", y.intersp = 1, cex=1)
  }

######################################################################################  
# Main Page
######################################################################################  
  output$title1 <- renderUI({
    h1(lst[[as.numeric(selec_var()[[3]])]])
  })
  
  lista <- list()
  lista[[1]] <- tagList(selectInput("sub_function",
                                    label = h3("Select sub function"), 
                                    choices = c("Characterization" = 1,
                                                "Visualization: Boxplot" = 2,
                                                "Analysis" = 3)),
                        selectInput("sub_subfunction",
                                    label = NULL, 
                                    choices = c("Significance test" = 1,
                                                "Visualization" = 2)),
                        br(),
                        uiOutput("varInterest"))
  
  output$varInterest <- renderUI({
    selectInput("var_interest", label = "Select variable of interest", choices = input$choose_variable, multiple = F)
  })
  
  lista[[2]] <- tagList(selectInput("sub_function",
                                    label = h3("Select sub function"),
                                    choices = c("Correlation and P-Value Table" = 1,
                                                "Correlation Matrix" = 2,
                                                "Significance Table and Scatterplot" = 3)),
                        uiOutput("uiExample2"),
                        br(),
                        radioButtons("type2", "Correlation Type", 
                                     choices = c("Pearson (parametric)" = "pearson",
                                                 "Spearman (non-parametric)" = "spearman"),
                                     inline = T),
                        actionButton('Get_results', 'Get results'))
  
  output$uiExample2 <- renderUI({
    tipify(bsButton("pB2", "Help", icon=icon("question-circle"),  size = "extra-small"),
           "Unlike pearson correlation, spearman correlation is more resistant to outliers.",
           placement = "right")
  })
  
  lista[[3]] <- tagList(actionButton('getPCA', 'Get results for PCA biplot'))
  
  lista[[4]] <- tagList(actionButton('Get_results', 'Get results'))
  
  #*****************************************#
  output$select_subfunc <- renderUI({
    selec_var()
    lista[[as.numeric(selec_var()[[3]])]]
  })
  #*****************************************#
  
  listb <- list()
  listb[["1-1"]] <- tagList(h3("Statistics Table for Explanatory Variables"),
                            actionButton("help1.1.1","Help",icon=icon("question-circle")),
                            
                            br(),
                            
                            hidden(verbatimTextOutput("helptext1.1.1")),
                            
                            br(),
                            
                            downloadButton('downloadData1.1.1', 'Download data'),
                            verbatimTextOutput('text1.1.1'),
                            
                            br(),
                            
                            h3("Statistics Table for Categorical Variables"),
                            actionButton("help1.1.2","Help",icon=icon("question-circle")),
                            
                            br(),
                            
                            hidden(verbatimTextOutput("helptext1.1.2")),
                            
                            br(),
                            
                            downloadButton('downloadData1.1.2', 'Download data'),
                            verbatimTextOutput('text1.1.2'))
  
  observeEvent(input$help1.1.1, {
    toggle("helptext1.1.1")
  })
  
  output$helptext1.1.1 <- renderPrint({
    helpText1.1.1()
  })
  
  output$downloadData1.1.1 <- downloadHandler(
    filename = function() {
      paste('exp_stats','.csv', sep='')},
    content = function(file) {
      df <- makeText1.1.1()
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
      d[4,1] <- "The data is based on the following process_data() and variables :"
      
      nam <- "List of values$data:"
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
      write.csv(selec_var()[[1]], file,na="",row.names=F)
      })
  
  output$text1.1.1 <- renderPrint({
    makeText1.1.1()
  })
  
  observeEvent(input$help1.1.2, {
    toggle("helptext1.1.2")
  })
  
  output$helptext1.1.2 <- renderPrint({
    helpText1.1.2()
  })
  
  output$downloadData1.1.2 <- downloadHandler(
    filename = function() {
      paste('cat_stats','.csv', sep='')},
    content = function(file) {
      df <- makeText1.1.2()
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
      d[4,1] <- "The data is based on the following process_data() and variables :"
      
      nam <- "List of values$data:"
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
      d[11:(size[1]+10),2] <- makeText1.1.2()
      d[11:(size[1]+10),1] <- rownames(df)
      d[10,1] <- NA
      
      colnames(d) <- rep("", length(colnames(d)))
      write.csv(d, file,na="",row.names=F)
    })
  
  output$text1.1.2 <- renderPrint({
    makeText1.1.2()
  })
  
  listb[["1-2"]] <- tagList(h3("Boxplot"),
                            actionButton("help1.2","Help",icon=icon("question-circle")),
                            
                            br(),
                            
                            hidden(verbatimTextOutput("helptext1.2")),
                            
                            br(),
                            uiOutput("Select_all1.2"),
                            uiOutput("Choice1.2"),
                            textInput("main1.2", "Key in the title of boxplot"),
                            downloadButton("downloadPlot1.2", "Download plot as PDF"),
                            plotOutput("plot1.2", height = "1000px"))
  
  observeEvent(input$help1.2, {
    toggle("helptext1.2")
  })
  
  output$helptext1.2 <- renderPrint({
    helpText1.2()
  })
  
  output$Select_all1.2 <- renderUI({
    radioButtons("select_all1.2", paste("Select all", length(selec_var()[[1]]), "non-categorical variables for the boxplot?"), choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
  })
  
  output$Choice1.2 <- renderUI({
    if(input$select_all1.2 == 2){
      selectizeInput("choose_variable1.2", "Select variables for the boxplot", choices = colnames(selec_var()[[1]]), 
                     multiple = T)
    }
    else if(input$select_all1.2 == 1){
      if(length(selec_var()[[1]]) > 10){
        selectInput("choose_variable1.2", "Select variables for the boxplot", choices = colnames(selec_var()[[1]]), 
                    multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = 10)
      }
      else{
        selectInput("choose_variable1.2", "Select variables for the boxplot", choices = colnames(selec_var()[[1]]), 
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
  
  listb[["1-3-1"]] <- tagList(h3("Significance Test"),
                              actionButton("help1.3.1","Help",icon=icon("question-circle")),
                              
                              br(),
                              
                              hidden(verbatimTextOutput("helptext1.3.1")),
                              
                              br(),
                              
                              h4("Table for explanatory variable"),
                              verbatimTextOutput("text1.3.1"),
                              
                              br(),
                              
                              h4("Table for categorical variables"),
                              uiOutput("text1.3.2"))
  
  output$text1.3.1 <- renderPrint({
    makeText1.3.1()
  })
  
  output$text1.3.1 <- renderPrint({
    makeText1.3.2()
  })
  
  output$uiExample1.3 <- renderUI({
    tipify(bsButton("pB13", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "Boxplot displays data according to its min/max value and its quartile with a different colour for each group.")
  })
  
  output$Select_all1.3 <- renderUI({
    radioButtons("select_all1.3", paste("Select all", length(selec_var()[[1]]), "explanatory variables for the boxplot?"), choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
  })
  
  output$Choice1.3 <- renderUI({
    if(input$select_all1.3 == 2){
      selectizeInput("choose_variable1.3", "Select variables for the boxplot", choices = selec_var()[[1]], 
                     multiple = T)
    }
    else if(input$select_all1.3 == 1){
      if(length(selec_var()[[1]]) > 10){
        selectInput("choose_variable1.3", "Select variables for the boxplot", choices = selec_var()[[1]], 
                    multiple = T, selectize = F, selected = selec_var()[[1]], size = 10)
      }
      else{
        selectInput("choose_variable1.3", "Select variables for the boxplot", choices = selec_var()[[1]], 
                    multiple = T, selectize = F, selected = selec_var()[[1]], size = length(selec_var()[[1]]))
      }
    }
  })
  
  output$downloadPlot1.3 <- downloadHandler(
    filename = function() {
      paste('Boxplot by group','.pdf', sep='')},
    content = function(file) {
      ggsave(file, makePlot1.3(25), dpi = 300, width = 30, height = 50, units = "cm")})
  
  output$plot1.3 <- renderPlot({
    makePlot1.3(15)
  })
  
  listb[["2-1"]] <- tagList(h3("Correlation Table"),
                            uiOutput("uiExample2.1.1"),
                            
                            br(),
                            
                            downloadButton('downloadData2.1.1', 'Download data'),
                            verbatimTextOutput('text2.1.1'),
                            
                            br(),
                            
                            h3("P-Value Table"),
                            uiOutput("uiExample2.1.2"),
                            
                            br(),
                            
                            downloadButton('downloadData2.1.2', 'Download data'),
                            verbatimTextOutput('text2.1.2'))
  
  output$uiExample2.1.1 <- renderUI({
    tipify(bsButton("pB211", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "The table displays pairwise correlation coefficients between the variables.")
  })
  
  output$uiExample2.1.2 <- renderUI({
    tipify(bsButton("pB212", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "The table displays the p-value for the pairwise correlation coefficients.")
  })
  
  output$downloadData2.1.1 <- downloadHandler(
    filename = function() {
      paste('Correlation Table','.csv', sep='')},
    content = function(file) {
      write.csv(makeText2.1.1(), file)})
  
  output$text2.1.1 <- renderPrint({
    makeText2.1.1()
  }, width = 150)
  
  output$downloadData2.1.2 <- downloadHandler(
    filename = function() {
      paste('P-Value Table','.csv', sep='')},
    content = function(file) {
      write.csv(makeText2.1.2(), file)})
  
  output$text2.1.2 <- renderPrint({
    makeText2.1.2()
  }, width = 150)
  
  listb[["2-2"]] <- tagList(h3("Correlation Matrix"),
                            uiOutput("uiExample2.2"),
                            
                            br(),
                            
                            uiOutput("Select_all2.2"),
                            uiOutput("Choice2.2"),
                            
                            textInput("main2.2", "Key in the title of correlation matrix"),
                            downloadButton('downloadPlot2.2', 'Download the plot as PDF'),
                            plotOutput("Plot2.2", height = "800px"))
  
  output$uiExample2.2 <- renderUI({
    tipify(bsButton("pB22", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "The plot displays the correlation matrix with a colour gradient.")
  })
  
  output$Select_all2.2 <- renderUI({
    radioButtons("select_all2.2", paste("Select all", length(selec_var()[[1]]), "explanatory variables for the boxplot?"), choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
  })
  
  output$Choice2.2 <- renderUI({
    selec_var()
    if(input$select_all2.2 == 2){
      selectizeInput("choose_variable2.2", "Select at least 2 variables for the correlation matrix", choices = selec_var()[[1]], 
                     multiple = T)
    }
    else if(input$select_all2.2 == 1){
      if(length(selec_var()[[1]]) > 10){
        selectInput("choose_variable2.2", "Select at least 2 for the correlation matrix", choices = selec_var()[[1]], 
                    multiple = T, selectize = F, selected = selec_var()[[1]], size = 10)
      }
      else{
        selectInput("choose_variable2.2", "Select at least 2 for the correlation matrix", choices = selec_var()[[1]], 
                    multiple = T, selectize = F, selected = selec_var()[[1]], size = length(selec_var()[[1]]))
      }
    }
  })
  
  output$downloadPlot2.2 <- downloadHandler(
    filename = function() {
      paste('Correlation Matrix','.pdf', sep='')},
    content = function(file) {
      ggsave(file, makePlot2.2(20), dpi = 300, height = 30, width = 30, units = "cm")})
  
  output$Plot2.2 <- renderPlot({
    makePlot2.2(10)
  })
  
  listb[["2-3"]] <- tagList(h3("Significance Table"),
                            uiOutput("Text2.3.1"),
                            downloadButton('downloadData2.3', 'Download data'),

                            br(),
                            br(),
                            br(),

                            fluidRow(
                              column(4, uiOutput('Choice2.3.1')),
                              column(8, uiOutput("Choose_alpha_level2.3"))
                              ),
                            
                            dataTableOutput('table2.3'),

                            br(),
                            br(),
                            br(),
                            
                            uiOutput("Title2.3"),
                            
                            br(),
                            
                            uiOutput("Text2.3.2"),
                            uiOutput("Main2.3"),
                            
                            br(),
                            uiOutput("Type2.3.1"),
                            
                            fluidRow(
                              column(4, uiOutput('Choice2.3.2')),
                              column(8, uiOutput('Choice2.3.3'))
                              ),
                            
                            uiOutput("DownloadPlot2.3"),
                            uiOutput("Plot2.3"),
                            
                            br(),
                            
                            fluidRow(
                              column(6, uiOutput("Title2.3.1"),
                                     uiOutput("Hover_info2.3")),
                              column(6, uiOutput("Title2.3.2"),
                                     uiOutput("Brush_info2.3"))
                              ))
  
  output$Text2.3.1 <- renderUI({
    verbatimTextOutput("text2.3.1")
  })
  
  output$text2.3.1 <- renderPrint({
    makeText2.3.1()
  })
  
  output$downloadData2.3 <- downloadHandler(
    filename = function() {
      paste('Significance Table','.csv', sep='')},
    content = function(file) {
      write.csv(makeTable2.3(), file, row.names = F)})
  
  output$Choice2.3.1 <- renderUI({
    selectizeInput("choose_variable_2.3.1", "Select a variable", choices = selec_var()[[1]])
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
  
  output$Text2.3.2 <- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      verbatimTextOutput("text2.3.2")
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
  })
  
  output$Choice2.3.2 <- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      selectizeInput("choose_variable_2.3.2", "Variable X", choices = input$choose_variable_2.3.1)
    }
  })
  
  output$Choice2.3.3 <- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      selectizeInput("choose_variable_2.3.3", "Select variable Y", choices = as.character(makeTable2.3()[,1]))
    }
  })

  output$DownloadPlot2.3 <- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      downloadButton('downloadPlot2.3', 'Download the plot as pdf')
    }
  })
  
  output$downloadPlot2.3 <- downloadHandler(
    filename = function() {
      paste('Scatterplot','.pdf', sep='')},
    content = function(file) {
      ggsave(file, makePlot2.3(25), dpi = 300, height = 30, width = 50, units = "cm")})
  
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
  
  output$Hover_info2.3<- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      verbatimTextOutput("hover_info2.3")
    }
  })
  
  output$Brush_info2.3<- renderUI({
    if(length(rownames(makeTable2.3())) > 0){
      verbatimTextOutput("brush_info2.3")
    }
  })
  
  output$hover_info2.3<- renderPrint({
    variable <- process_data()[,colnames(process_data()) %in% c(input$choose_variable_2.3.2, input$choose_variable_2.3.3),drop = FALSE]
    group <- process_data()[,colnames(process_data()) %in% selec_var()[[2]], drop = FALSE]
    ID <- process_data()[,1, drop = F]
    
    variable_1 <- na.omit(variable)
    group_1 <- group[as.numeric(rownames(variable_1)),]
    ID_1 <- ID[as.numeric(rownames(variable_1)),]
    
    df <- cbind(ID_1, group_1, variable_1)
    df[,2] <- as.factor(df[,2])
    
    colnames(df)[1:2] <- c("ID", "Group")
    
    nearPoints(df, input$plot1_hover2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3, threshold = 10, maxpoints = 1)
  })
  
  output$brush_info2.3 <- renderPrint({
    variable <- process_data()[,colnames(process_data()) %in% c(input$choose_variable_2.3.2, input$choose_variable_2.3.3),drop = FALSE]
    group <- process_data()[,colnames(process_data()) %in% selec_var()[[2]], drop = FALSE]
    ID <- process_data()[,1, drop = F]
    
    variable_1 <- na.omit(variable)
    group_1 <- group[as.numeric(rownames(variable_1)),]
    ID_1 <- ID[as.numeric(rownames(variable_1)),]
    
    df <- cbind(ID_1, group_1, variable_1)
    df[,2] <- as.factor(df[,2])
    
    colnames(df)[1:2] <- c("ID", "Group")
    
    brushedPoints(df, input$plot1_brush2.3, xvar = input$choose_variable_2.3.2, yvar = input$choose_variable_2.3.3)
  })
  
  listb[["3"]] <- tagList(hidden(verbatimTextOutput("text3.2")),
                          h3('Result of principal component analysis'),
                          actionButton("help3","Help",icon=icon("question-circle")),
                          
                          br(),
                          
                          hidden(verbatimTextOutput("helptext3")),
                          
                          br(),
                          
                          radioButtons("type3.1", "Standardize variables?",
                                       choices = c("Yes" = 1,
                                                   "No" = 2),
                                       inline = T),
                          downloadButton('downloadData3', 'Download data'),
                          verbatimTextOutput('text3.1'),
                          
                          br(),
                          
                          fluidRow(
                            column(6, uiOutput("Choice3.1")),
                            column(6, uiOutput("Choice3.2"))
                          ),
                          
                          br(),
                          br(),
                          
                          fluidRow(column(6,textInput("main3", "Key in the title of PCA plot")),
                                   column(6,uiOutput("chooseGroup3"))),
                          downloadButton('downloadPlot3', 'Download the plot as pdf'),
                          plotOutput("plot3", height = 800, width = 800, hover = "plot1_hover3", brush = "plot1_brush3"),

                          br(),

                          fluidRow(
                            column(6, h4("Hovered point"),
                                   verbatimTextOutput("hover_info3")),
                            column(6, h4("Selected points"),
                                   verbatimTextOutput("brush_info3"))
                            ))
  
  observeEvent(input$getPCA, {
    toggle("text3.2")
  })
  
  output$text3.2 <- renderPrint({
    makeText3.2()
  })
  
  observeEvent(input$help3, {
    toggle("helptext3")
  })
  
  output$helptext3 <- renderPrint({
    helpText3()
  })
  
  output$chooseGroup3 <- renderUI({
    selectizeInput("choose_group3", "Select group variable",
                   choices = colnames(selec_var()[[2]]), multiple = F)
  })
  
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste('PCA Result','.csv', sep='')},
    content = function(file) {
      write.csv(makeText3.1(), file)})
  
  output$text3.1<- renderPrint(
    if(length(colnames(makeText3.1())) > 10){
      makeText3.1()[,1:10]
    }
    else{
      makeText3.1()
    }
  )
  
  output$Choice3.1 <- renderUI({
    selectizeInput("type3.2", "Select first principal component", 
                choices = colnames(makeText3.1()))
  })
  
  output$Choice3.2<- renderUI({
    selectizeInput("type3.3", "Select second principal component", 
                   choices = colnames(makeText3.1())[-which(colnames(makeText3.1()) == input$type3.2)])
  })
 
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste('PCA Biplot','.pdf', sep='')},
    content = function(file) {
      ggsave(file, makePlot3(15), dpi = 300, height = 30, width = 30, units = "cm")})
  
  output$plot3 <- renderPlot({
    makePlot3(15)
  })
  
  output$hover_info3 <- renderPrint({
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- process_data()[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    if(input$type3.1 == 1){
      var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    }
    
    if(input$type3.1 == 2){
      var.pca <- prcomp(variable, center = FALSE, scale. = FALSE) 
    }
    
    
    df <- cbind(process_data()[,1],group,variable,var.pca$x)
    
    colnames(df)[1:2] <- c("Subject#", input$choose_group3)
    
    nearPoints(df, input$plot1_hover3, xvar = input$type3.2, yvar = input$type3.3, threshold = 10, maxpoints = 1)[,c(1:2)]
  })
  
  output$brush_info3 <- renderPrint({
    variable <- selec_var()[[1]]
    group <- selec_var()[[2]]
    group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
    
    rownames(variable) <- process_data()[,1]
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
    
    if(input$type3.1 == 1){
      var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
    }
    
    if(input$type3.1 == 2){
      var.pca <- prcomp(variable, center = FALSE, scale. = FALSE) 
    }
    
    
    df <- cbind(process_data()[,1],group,variable,var.pca$x)
    
    colnames(df)[1:2] <- c("Subject#", input$choose_group3)
    
    brushedPoints(df, input$plot1_brush3, xvar = input$type3.2, yvar = input$type3.3)[,c(1:2)]
  })
  
  listb[["4"]] <- tagList(h3("Hierarchical Clustering & Heatmaps"),
                          actionButton("help4","Help",icon=icon("question-circle")),
                          
                          br(),
                          
                          hidden(verbatimTextOutput("helptext4")),
                          
                          br(),
                          radioButtons("type4.1", "Standardize variable?",
                                       choices = c("Yes" = 1,
                                                   "No" = 2),
                                       inline = T),
                          radioButtons("type4.2", "Distance Type",
                                       c("Euclidean" = "euclidean",
                                         "Maximum" = "maximum",
                                         "Manhattan" = "manhattan",
                                         "Canberra" = "canberra",
                                         "Binary" = "binary",
                                         "Minkowski" = "minkowski"),
                                       selected = "euclidean",
                                       inline = T),
                          radioButtons("type4.3", "Cluster Method",
                                       c("Ward" = "ward",
                                         "Single" = "single",
                                         "Complete" = "complete",
                                         "Average" = "average",
                                         "Mcquitty" = "mcquitty",
                                         "Median" = "median",
                                         "Centroid" = "centroid"),
                                       selected = "ward",
                                       inline = T),
                          uiOutput("chooseGroup4"),
                         
                          br(),

                          h3('Cluster Dendrogram'),
                          uiOutput("uiExample4"),
                          
                          br(),
                          
                          textInput("main4", "Key in the title of Cluster Dendrogram"),
                          downloadButton('downloadPlot4', 'Download the plot as png'),
                          plotOutput("plot4",height = "800px",width = "1000px"))
                          #plotlyOutput("plot4.1",height = "800px"))
  
  output$uiExample4 <- renderUI({
    tipify(bsButton("pB4", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
           "The plot displays the HC dendrogram, which clusters similar samples and variables together.")
  })
  
  observeEvent(input$help4, {
    toggle("helptext4")
  })
  
  output$helptext4 <- renderPrint({
    helpText4()
  })
  
  output$chooseGroup4 <- renderUI({
    selectizeInput("choose_group4", "Select group variable",
                   choices = colnames(selec_var()[[2]]), multiple = F)
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
  
  output$plot4.1 <- renderPlotly({
    makePlot4.1()
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

})
