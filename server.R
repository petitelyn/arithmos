library(shiny)
library(shinyjs)
library(shinyBS)
source("helpers.R")

#global code that populates the database if no project table is found
con <- connectDatabase()
if(!(dbExistsTable(con, "project"))) createDatabase(con)
dbDisconnect(con)
#must close all connections

shinyServer(function(input, output, session) {
  #initialize session variables
  values <- reactiveValues(sessionId = NULL)
  values$con <- connectDatabase()
  values$data <- NULL
  values$fulldata <- NULL
  values$name <- NULL
  
  checkData <- reactiveTimer(1000000)
  
  observe({
    checkData()
    con <- connectDatabase()
    get_projects <- "SELECT project_code FROM project"
    project_list <- dbGetQuery(con, get_projects)[["project_code"]]
    updateSelectInput(session, "projectChoice", choices=project_list, selected=input$projectChoice)
    #updateSelectInput(session, "projectChoice", choices=project_list)
    dbDisconnect(con)
  })
  
  session$onSessionEnded(function() {
    #close the session's connection upon exit
    observe(dbDisconnect(values$con))
  })
  
  source("server_modules/startPanel.R", local=TRUE)
  source("server_modules/searchAcross.R", local=TRUE)
  source('server_modules/preProcessing.R', local=TRUE)
  
  source('server_modules/mainControl.R', local=TRUE)
  source('server_modules/analysisSidebar.R', local=TRUE)
  source('server_modules/analysisPage.R', local=TRUE)
  
  #dictionary used in analysis
  listb <- list()
  
  source('server_modules/statisticsTable.R', local=TRUE)
  source('server_modules/statisticsBoxplot.R', local=TRUE)
  source('server_modules/statisticsDiffAnal.R', local=TRUE)
  source('server_modules/statisticsDiffVizu.R', local=TRUE)
  source('server_modules/correlationTable.R', local=TRUE)
  source('server_modules/correlationPlot.R', local=TRUE)
  source('server_modules/correlationSearch.R', local=TRUE)
  source('server_modules/pca.R', local=TRUE)
  source('server_modules/hc.R', local=TRUE)
  

})
