#file where all of the UI is layed out

library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)

#I wish I did not have to import database communication here but the app needs to list the projects from the start
source("complexDatabaseCommunication.R")
#get the list of projects currently in the database
#could not figure out how to use server.R's connection here
con <- connectDatabase()
get_projects <- "SELECT project_code FROM project"
project_list <- dbGetQuery(con, get_projects)[["project_code"]]
dbDisconnect(con)
#must disconnect always

shinyUI(fluidPage(theme="all_css.css", shinyjs::useShinyjs(),
  strong(headerPanel(list(tags$head(
    #title of project with the place for displaying what current project is
    tags$style("{background-color: black;}")),paste("Arithm","\U00F3","s", " v0.1",sep="")))),
    textOutput("currentProject"),
    #import the correct scripts
    tags$script(src="relative_x_scrolling.js"),
    tags$script(src="switch.js"),
    hr(style="width:100%;"), 
  
    #entire app built with shiny sidebarLayout class
    sidebarLayout(
      sidebarPanel(
        div(id="sidebarPanel",
            
          #restart button, persistent in the sidebar panel
          actionButton("restart","",icon=icon("home"), class="faded-button"),
          #actionButton("refresh", '', icon=icon("refresh")),
        
            div(id="uploadAndLoad", 
                h3("Study Selection", class="no-top"),
                fileInput('file', 'Upload', multiple = T),
                textOutput('uploadError'),
                selectInput("projectChoice", "Choose a project", project_list, multiple=F, selectize=F),
                selectInput("studyChoices", "Select studies", NULL, selected=NULL, multiple=T, selectize=F, size=6),
                actionButton('load', 'Load')),
            
            div(id="processing", class="initiallyHidden",
                h3("Pre-Processing", class="no-top"),
                numericInput("col_cutoff", label = "Remove columns that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                numericInput("row_cutoff", label = "Remove rows that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                actionButton('preProcess', 'Process')),
            
            div(id="viewAndBegin", class="initiallyHidden", 
                h3("Results", style="margin-bottom:5%;"), 
                actionButton('viewMerged', 'View dataset', class="btn-primary"),
                downloadButton("downloadMerged", "Download", class="btn-info"),
                br(),
                br(),
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
                #uiOutput("help1"),

                uiOutput('select_var'),
                uiOutput("help2"),
                uiOutput("warning1"),
                uiOutput("warning2")
                ))),
      
      mainPanel(
        div(id="mainPanel",
        div(id="acrossSearchPanel",
            #the following tags keep errors from being displayed in the server
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
                       ),
            h3(id='acrossSearchHeader', "Search Across Projects", class="no-top", title="Search for a text value in a chosen field across every accessible project"
            ),
            br(),
            selectInput("acrossSearchTypeSelect", "Choose a category to search across", choices=c("Group", "Variable", "Sample Type")),
            uiOutput("acrossSearchHelp"),
            textInput("acrossSearch",""),
            uiOutput("acrossTextSearchHelp"),
            br(),
            actionButton("across", "Search"),
            textOutput("acrossFail"),
            br(),
            dataTableOutput("acrossInfo")), 
        
        div(id="mergedDataPanel", class="initiallyHidden",
            uiOutput("PreText"),
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