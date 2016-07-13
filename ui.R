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
  actionButton("restart","",icon=icon("refresh")),
  
    tags$script(src="relative_x_scrolling.js"),
    tags$script(
      HTML(
         " 
          stage = 'load'
          Shiny.addCustomMessageHandler ('switch',function (stage) {
            if (stage == 'load'){
              $('#uploadAndLoad').show()
              $('#acrossSearchPanel').show()
              $('#processing').hide()
              $('#mergedDataPanel').hide()
              $('#viewAndBegin').hide()
              $('#analysisSidebar').hide()
              $('#analysisPanel').hide()
            } else if (stage == 'process'){
              $('#uploadAndLoad').hide()
              $('#processing').show()
              $('#acrossSearchPanel').hide()
         } else if (stage == 'view'){
              $('#viewAndBegin').show()
              $('#mergedDataPanel').show()
         } else if (stage == 'analysis'){
         $('#acrossSearchPanel').hide()
         $('#mergedDataPanel').hide()
         $('#processing').hide()
         $('#viewAndBegin').hide()
         $('#analysisSidebar').show()
         $('#analysisPanel').show()
         }
            
         });
      "     
      )
    ),
    hr(style="width:100%;"), 
    sidebarLayout(
      sidebarPanel(
        div(id="sidebarPanel",
            div(id="uploadAndLoad", 
                h3("Study Selection", class="no-top"),
                fileInput('file', 'Upload', multiple = T),
                textOutput('uploadError'),
                selectInput("projectChoice", "Choose a project", project_list, multiple=F, selectize=F),
                selectInput("studyChoices", "Select studies", NULL, selected=NULL, multiple=T, selectize=F, size=6),
                actionButton('load', 'Load')),
            
            div(id="processing", class="initiallyHidden",
                h3("Data Processing", class="no-top"),
                numericInput("col_cutoff", label = "Remove columns that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                numericInput("row_cutoff", label = "Remove rows that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                actionButton('preProcess', 'Process')),
            
            div(id="viewAndBegin", class="initiallyHidden", 
                h3("Results", style="margin-bottom:5%;"), 
                actionButton('viewMerged', 'View', class="btn-primary"),
                downloadButton("downloadMerged", "Download", class="btn-info"),
                br(),
                br(),
                uiOutput("helptext"),
                actionButton('start', 'Start')),
            
            div(id="analysisSidebar", class="initiallyHidden",
                h3("Data Analysis", class="no-top"),
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
                uiOutput("warning1"),
                uiOutput("warning2")
                ))),
      
      mainPanel(
        div(id="mainPanel",
        div(id="acrossSearchPanel",
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
            h3("Search Across Projects", class="no-top"),
            br(),
            selectInput("acrossSearchType", "Category to search across", choices=c("Group", "Variable")),
            textInput("acrossSelect","Search for a value across projects"),
            actionButton("across", "Search Across"),
            textOutput("acrossFail"),
            br(),
            dataTableOutput("acrossInfo")), 
        
        div(id="mergedDataPanel", class="initiallyHidden",
            tableOutput("mergedTable")),
        
        div(id="analysisPanel", class="initiallyHidden",
            verticalLayout(
              uiOutput("title1"),
              div(uiOutput("select_subfunc")),
              div(uiOutput("output1")),
              div(uiOutput("output2"))))
        )
      ))
))