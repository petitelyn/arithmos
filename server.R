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
  
  source('preProcessing.R', local=TRUE)
  
  switchAcrossName <- observeEvent(input$acrossSearchTypeSelect, {
    updateTextInput(session, "acrossSearch", label=paste("Search for a ", tolower(input$acrossSearchTypeSelect), " across projects"))
  })
  
  output$acrossSearchHelp <- renderUI({
    tipify(bsButton("pC4", "Help", icon=icon("question-circle"),  size = "extra-small"),
           "Group = Outcomes of interest    Variables = Other measurements       Sample type = Cell or tissue type",
           placement = "right")
  })
  output$acrossTextSearchHelp <- renderUI({
    tipify(bsButton("pC3", "Help", icon=icon("question-circle"),  size = "extra-small"),
           "Enter text into a relaxed search. Partial terms are fine. Searching for nothing returns all possible values",
           placement = "right")
  })
  
  acrossVariableTable <- observeEvent(input$across, {
    #right now only capability for group, variable, and sample type
    info_table <- NULL
    if (strcmp(input$acrossSearchTypeSelect, "Group")) {
      info_table <- getGroupAcross(values$con, input$acrossSearch)
    } else if (strcmp(input$acrossSearchTypeSelect, "Sample Type")) {
      info_table <- getSampleTypeAcross(values$con, input$acrossSearch)
    } else if (strcmp(input$acrossSearchTypeSelect, "Variable")) {
      info_table <- getVariableAcross(values$con, input$acrossSearch)
    }
    if (nrow(info_table) == 0) {
      output$acrossFail <- renderText("No results.")
      output$acrossInfo <- renderDataTable(info_table)
      return()
    }
    output$acrossFail <- renderText("")
    if (strcmp(input$acrossSearchTypeSelect, "Variable")) {
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
  
  source('mainControl.R', local=TRUE)
  
  

######################################################################################  
# Main Page
######################################################################################  
  lst <- list()
  lst[[1]] <- "Statistics"
  lst[[2]] <- "Correlation"
  lst[[3]] <- "Principal Component Analysis (PCA)"
  lst[[4]] <- "Hierarchical Clustering"
  
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
  
  source('statisticsTable.R', local=TRUE)
  source('statisticsBoxplot.R', local=TRUE)
  source('statisticsDiffAnal.R', local=TRUE)
  source('statisticsDiffVizu.R', local=TRUE)
  source('correlationTable.R', local=TRUE)
  source('correlationPlot.R', local=TRUE)
  source('correlationSearch.R', local=TRUE)
  source('pca.R', local=TRUE)
  source('hc.R', local=TRUE)
  
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
