
#the main function list
lst <- list()
lst[[1]] <- "Statistics"
lst[[2]] <- "Correlation"
lst[[3]] <- "Principal Component Analysis (PCA)"
lst[[4]] <- "Hierarchical Clustering"

lista <- list()
lista[[1]] <- tagList(h3("Select sub function"),
                      fluidRow(column(4,selectInput("sub_function", label = NULL,
                                                    choices = c("Characterization: Table" = 1,
                                                                "Visualization: Boxplot" = 2,
                                                                "Differential Analysis" = 3))),
                               column(4,uiOutput("subsubfunction")))
)



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

lista[[3]] <- NULL

lista[[4]] <- NULL

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

output$uiExample2 <- renderUI({
  tipify(bsButton("pB2", "Help", icon=icon("question-circle"),  size = "extra-small"),
         "Unlike pearson correlation, spearman correlation does not assume the variable follows a normal distribution.",
         placement = "right")
})

output$title1 <- renderUI({
  h1(lst[[as.numeric(selec_var()[[3]])]], class="smaller-margins")
})


