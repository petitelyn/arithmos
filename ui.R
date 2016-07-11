library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)

source("DatabaseCommunication.R")

con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
get_projects <- "SELECT project_code FROM project"
project_list <- dbGetQuery(con, get_projects)[["project_code"]]
dbDisconnect(con)

shinyUI(fluidPage(theme="bootstrap.css", shinyjs::useShinyjs(),
  strong(headerPanel(list(tags$head(
    tags$style("{background-color: black;}")),paste("Arithm","\U00F3","s", " v0.1",sep="")))),
    tags$script(src="relative_x_scrolling.js"),
    sidebarLayout(
      sidebarPanel(
          conditionalPanel(condition = "input.begin == false || input.back == true",
                           fileInput('file', 'Upload', multiple = T),
                           textOutput('uploadError'),
                           selectInput("projectChoice", "Choose a project", project_list, multiple=F, selectize=F),
                           selectInput("studyChoices", "Select studies", NULL, selected=NULL, multiple=T, selectize=F, size=6),
                           actionButton('load', 'Load'),
                           textOutput("loadSuccess"),
                           
                           br(),
                           
                           h3("Pre-Processing"),
                           numericInput("col_cutoff", label = "Remove columns that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                           numericInput("row_cutoff", label = "Remove rows that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                           
                           actionButton('preProcess', 'Process'),
                           uiOutput("downloadB"),
                           uiOutput("viewB"),
                           # fluidRow(column(6,uiOutput("downloadB")),
                           #          column(3,uiOutput("viewB"))),
                           uiOutput("proceed_text"),
                           
                           checkboxInput('begin', "Begin Analysis",value=F),
                           width = 3
                           ),
          
          conditionalPanel(condition = "input.begin == true",
                           checkboxInput('back', "Go Back",value=F),

                           h3(textOutput("currentProject")),
                           br(),
                           radioButtons("select_time", "Select variables by timepoint?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T),
                           uiOutput("selectTime"),
                           uiOutput("selectAll"),

                           uiOutput("choose_var"),
                           hr(),
                           uiOutput('select_cat_var'),
                           uiOutput("help"),
                           hr(),
                           uiOutput('select_func'),
                           uiOutput("help1"),
                           uiOutput('select_var'),
                           uiOutput("help2"),
                           br(),
                           uiOutput("warning1"),
                           br(),
                           uiOutput("warning2")                           
                           ) 
          # ), fixed=TRUE, top="10%", height="50%", left="10%", width="33%"
      ),
      
      mainPanel(
        conditionalPanel(condition = "input.begin == false || input.back == true",
                         tags$style(type="text/css", 
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                         br(),
                         selectInput("acrossSearchType", "Category to search across", choices=c("Group", "Variable")),
                         textInput("acrossSelect","Search for a value across projects"),
                         actionButton("across", "Search Across"),
                         textOutput("acrossFail"),
                         br(),
                         dataTableOutput("acrossInfo"),
                         br(),
                         hidden(tableOutput("merged"))
                         ),
        conditionalPanel(condition = "input.begin == true",
                         verticalLayout(
                           uiOutput("title1"),
                           div(uiOutput("select_subfunc")),
                           div(uiOutput("output1")),
                           div(uiOutput("output2"))
                           )
                         )
        )
      )
))