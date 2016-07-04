library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)

source("DatabaseCommunication.R")

con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
get_projects <- "SELECT project_code FROM project"
project_list <- dbGetQuery(con, get_projects)[["project_code"]]
get_variable_names <- "SELECT name_full FROM feature_name"
variable_list <- dbGetQuery(con, get_variable_names)

create_int_to_string <- "CREATE OR REPLACE FUNCTION convert_to_integer(v_input text)
RETURNS NUMERIC AS $$
DECLARE v_numeric_value NUMERIC DEFAULT NULL;
BEGIN
BEGIN
v_numeric_value := v_input::NUMERIC;
EXCEPTION WHEN OTHERS THEN
RETURN NULL;
END;
RETURN v_numeric_value;
END;
$$ LANGUAGE plpgsql;"

dbGetQuery(con, create_int_to_string)

dbDisconnect(con)

shinyUI(fluidPage(theme="bootstrap.css", shinyjs::useShinyjs(),
  strong(headerPanel(list(tags$head(tags$style("{background-color: black;}")),paste("Arithm","\U00F3","s", " v0.1",sep="")))),
    sidebarLayout(
      sidebarPanel(
          conditionalPanel(condition = "input.begin == false || input.back == true",
                           fileInput('file', 'Upload', multiple = T),
                           selectInput("projectChoice", "Choose a project", project_list, multiple=F, selectize=F),
                           selectInput("studyChoices", "Select studies", NULL, selected=NULL, multiple=T, selectize=F, size=6),
                           actionButton('load', 'Load'),
                           textOutput("loadSuccess"),
                           
                           br(),
                           
                           h3("Pre-Processing"),
                           numericInput("col_cutoff", label = "Remove columns that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                           numericInput("row_cutoff", label = "Remove rows that have more than (or equal to) ___ % missing values.", value = 50, min = 0, max = 100),
                           
                           actionButton('preProcess', 'Process'),
                           fluidRow(column(6,uiOutput("downloadB")),
                                    column(3,uiOutput("viewB"))),
                           uiOutput("proceed_text"),
                           
                           checkboxInput('begin', "Begin Analysis",value=F),
                           width = 3
                           ),
          
          conditionalPanel(condition = "input.begin == true",
                           checkboxInput('back', "Go Back",value=F),
                           
                           br(),
                           
                           radioButtons("Select_all", "Select all variables?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T),

                           br(),
                           
                           uiOutput("choose_var"),
                           uiOutput("help"),
                           uiOutput('select_group_var'),
                           uiOutput('select_func'),
                           uiOutput("help1"),
                           uiOutput('select_var'),
                           uiOutput("help2")
                           )
          ),
      
      mainPanel(
        conditionalPanel(condition = "input.begin == false || input.back == true",
                         tags$style(type="text/css", 
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                         br(),
                         textInput("acrossVariableSelect","Search for a variable across projects"),
                         actionButton("across", "Check Variable Across"),
                         textOutput("acrossFail"),
                         br(),
                         dataTableOutput("acrossInfo"),
                         br(),

                         hidden(tableOutput("merged"))
                         ),
        
        uiOutput("title1"),
        
        br(),
        
        uiOutput("select_subfunc"),
        
        br(),
        br(),
        
        uiOutput("output1")
        )
      )
))