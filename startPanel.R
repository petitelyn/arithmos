#This file outlines functions used by the left panel at the start

#sets the chooseable projects in the select input
#sets current project to project of project_pk
updateProjects <- function(project_pk) {
  project_list <- dbGetQuery(values$con, "SELECT project_code FROM project")[["project_code"]]
  current_project <- dbGetQuery(values$con, sprintf("SELECT project_code FROM project WHERE pk=%i", project_pk))[["project_code"]][[1]]
  updateSelectInput(session, "projectChoice", choices=project_list, select=current_project)
}

#upload new study(s)
upload_data  <- observeEvent(input$file,{
  output$uploadError <- renderText("")
  full_name_list <- input$file$name
  datapath_list <- input$file$datapath
  file_name_list <- list()
  for (j in 1:length(full_name_list)){
    #build the list of file names from those chosen by the user
    file_split <- strsplit(full_name_list[[j]], "\\.")[[1]]
    file_type <- file_split[[2]]
    if (!(strcmp(file_type, "xlsm"))) {
      #files must be xlsm format currently
      output$uploadError <- renderText("Can only upload .xlsm")
      return()
    }
    file_name_list[[j]] <- file_split[[1]]
  }
  
  #count the number of files uploaded
  count <- 1
  #hardcoded value
  csv_list <- list()
  #build a temporary directory for the csvs built from the xlsm sheets
  dir.create("TEMPDIR")
  for (p in 1:length(file_name_list)) {
    #convert each 2 sheeted xlsm file to individual csvs to be read from
    for (i in 1:2) {
      #convert the specified xlsm format to csvs 
      convert_to_csv <- readWorksheetFromFile(datapath_list[[p]], header=F, sheet=i)
      convert_to_csv[is.na(convert_to_csv)] <- ""
      new_file_name <- paste("TEMPDIR", .Platform$file.sep, file_name_list[p],"_temp",  toString(i), ".csv", sep='')
      write.table(convert_to_csv, new_file_name, na='', quote=F, row.names=F, col.names=F, sep=',')
      csv_list[[count]] <- new_file_name
      count <- count + 1
    }
  }
  if (length(csv_list) != 2*length(file_name_list)) {
    #if not every xlsm was two sheets
    output$uploadError("Every .xlsm must be two sheets, with study info first and data second")
    unlink("TEMPDIR", recursive=TRUE)
    return()
  }
  #build the custom progress bar 
  progress <- shiny::Progress$new()
  progress$set(value=0)
  #total studies right now must be length of all csvs divided by 2 per the specified format
  total_studies <- length(csv_list) / 2
  for (i in seq(1, length(csv_list), 2)) {
    #upload each individual study from the temporary csvs
    study_name <- strsplit(file_name_list[[ceiling(i/2)]], "\\.")[[1]][[1]]
    progress$inc(0, message = paste("Uploading", study_name))
    return_code <- addStudy(values$con, csv_list[[i]], csv_list[[i+1]], study_name, total_studies, progress)
    if (return_code == -1) {
      #specific error should be printed to console
      output$uploadError <- renderText(sprintf("Error uploading %s", study_name))
      break
    }
  }
  #delete all the temporary csvs
  #there is probably a better way to read directly from the xlsms but I could not get the formatting to work
  unlink("TEMPDIR", recursive=TRUE)
  #update all of the inputs based on the new studies that were uploaded
  updateProjects(return_code)
  #need this extra call to updateStudies to refresh studies 
  updateStudies()
  progress$close()
  invalidateLater(0, session)
})

#updates the current selectable studies
#this function should not be necessary but strangely shiny will not refresh studies through updateProjects 
#if the user uploads a new study for the currently selected project
updateStudies <- function() {
  get_project_pk <- sprintf("SELECT pk FROM project WHERE project_code=\'%s\'", input$projectChoice)
  project_pk <- dbGetQuery(values$con, get_project_pk)[["pk"]]
  get_studies <- sprintf("SELECT study_name FROM study WHERE study.project_pk=%i", project_pk)
  study_list <- dbGetQuery(values$con, get_studies)[["study_name"]]
  updateSelectInput(session, "studyChoices", choices=study_list, selected=study_list)
  output$acrossInfo <- renderTable(NULL)
}

#clear the text output that says "data loaded"
unloadData <- observeEvent(input$studyChoices, {
  output$loadSuccess <- renderText("")
  values$data <- NULL
})

#render the current studies in the select input based on the given project
loadStudies <- observeEvent(input$projectChoice, {
  get_project_pk <- sprintf("SELECT pk FROM project WHERE project_code=\'%s\'", input$projectChoice)
  project_pk <- dbGetQuery(values$con, get_project_pk)[["pk"]]
  get_studies <- sprintf("SELECT study_name FROM study WHERE study.project_pk=%i", project_pk)
  study_list <- dbGetQuery(values$con, get_studies)[["study_name"]]
  updateSelectInput(session, "studyChoices", choices=study_list, selected=study_list)
  output$acrossInfo <- renderTable(NULL)
})

#load the data based on the selected studies
loadData <- observeEvent(input$load, {
  #create a progress bar
  withProgress(message="Loading data", {
    #get a list of study names from the select input
    study_name_list <- input$studyChoices
    pk_list <- list()
    for (i in 1:length(study_name_list)){
      #get the list of primary keys of studies based on the names given
      search_query <- sprintf("SELECT pk FROM study WHERE study_name=\'%s\'", study_name_list[[i]])
      pk_list[i] <- dbGetQuery(values$con, search_query)
    }
    incProgress(1/2)
    #load all of the measurements of all of the given studies in a dataframe in wide format
    wide_format <- getStudyDataWideFormat(values$con, pk_list)
    #load copy of data for processing
    values$data <<- wide_format
    #full data copy will not be processed so user can process multiple times
    values$fulldata <<- wide_format
    incProgress(1/4)
    #show the user the data has been loaded
    output$loadSuccess <- renderText("Data loaded.")
    output$currentProject <- renderText(paste("Project: ", input$projectChoice))
    #switch to the processing UI panel
    session$sendCustomMessage (type="switch", "process")
    incProgress(1/4)
  })
})
