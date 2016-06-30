library(shiny)
library(plotly)

source("DatabaseCommunication.R")

con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
get_projects <- "SELECT project_code FROM project"
project_list <- dbGetQuery(con, get_projects)[["project_code"]]
get_variable_names <- "SELECT name_full FROM feature_name"
variable_list <- dbGetQuery(con, get_variable_names)
dbDisconnect(con)

shinyUI(fluidPage(theme='bootstrap.css',
  strong(headerPanel(list(tags$head(tags$style("{background-color: black;}")),paste("Arithm","\U00F3","s", " v0.1",sep="")))),
    sidebarLayout(
      sidebarPanel(
          fileInput('file', 'Upload', multiple = T),
          selectInput("projectChoice", "Choose a project", project_list, multiple=F, selectize=F),
          selectInput("studyChoices", "Select studies", NULL, selected=NULL, multiple=T, selectize=F, size=6),
          numericInput("threshold", "Missing Threshold", 0),
          actionButton('load', 'Load'),
          
          width = 3,
     
          
          br(),
          br(),
          br(),
          br(),
          br(),
          
          radioButtons("Select_all", "Select all variables?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T),
          
          br(),
          
          uiOutput("choose_var"),
          uiOutput("help"),
          uiOutput('select_group_var'),
          uiOutput('select_func'),
          uiOutput("help1"),
          uiOutput('select_var'),
          uiOutput("help2")

        
      ),
      
      mainPanel(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        selectInput("acrossVariableSelect", "Select a variable across projects", variable_list, selected=NULL, multiple=F, selectize=T),
        selectInput("chosenVariables", "Selected variables", NULL, selected=NULL, multiple=T, selectize=F, size=10),
        actionButton('upload', "Begin"),
        
        uiOutput("title1"),
        
        br(),
        
        uiOutput("select_subfunc"),
        
        br(),
        br(),
        
        uiOutput("output1")
        )
      )
))