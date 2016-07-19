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
  
  restartSession <- observeEvent(input$restart,{
    #operations to perform upon restarting a session (hitting the home button)
    values$data <- NULL
    session$sendCustomMessage (type="switch", "load")
    output$currentProject <- renderText("")
  })
  
  source("startPanel.R", local=TRUE)
  source("searchAcross.R", local=TRUE)
  source('preProcessing.R', local=TRUE)
  
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
