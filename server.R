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
  
  session$onSessionEnded(function() {
    #close the session's connection upon exit
    observe(dbDisconnect(values$con))
  })
  
  source("startPanel.R", local=TRUE)
  source("searchAcross.R", local=TRUE)
  source('preProcessing.R', local=TRUE)
  
  source('mainControl.R', local=TRUE)
  source('analysisSidebar.R', local=TRUE)
  source('analysisPage.R', local=TRUE)
  
  #dictionary used in analysis
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
  

})
