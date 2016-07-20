#Function to produce the significance table for differential analysis
makeTable1.3.1 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  var_name <- NULL
  type <- NULL
  rho <- NULL
  p_value <- NULL
  q_value <- NULL
  r_squared <- NULL
  n <- NULL
  method <- NULL
  
  if(input$var_interest %in% colnames(dataset)){
    varInterest <- dataset[,colnames(dataset) %in% input$var_interest, drop =F]
    
    #Y(Continuous) vs X(Continuous)
    for(i in input$choose_variable){
      if(i %in% colnames(dataset) & i != input$var_interest){
        v <- dataset[,colnames(dataset) %in% i, drop =F]
        
        #Correlation test
        a <- cor.test(varInterest[,1],v[,1],alternative = "two.sided",method = "spearman")
        
        var_name <- c(var_name,i)
        type <- c(type,"Continuous")
        n <- c(n, length(na.omit(cbind(varInterest,v))[,1]))
        rho <- c(rho, a$estimate)
        r_squared <- c(r_squared, (a$estimate)^2)
        p_value <- c(p_value, a$p.value)
        method <- c(method,"Spearman's rank correlation test")
      }
    }
    
    #Y(Continuous) vs X(Categorical)
    for(j in input$choose_variable){
      if(j %in% colnames(group) & j != input$var_interest){
        v <- group[,colnames(group) %in% j, drop =F]
        
        #Kruskal-Wallis test
        a <- kruskal.test(varInterest[,1],v[,1])
        
        var_name <- c(var_name,j)
        type <- c(type,"Categorical")
        
        #Give NA values for rho and r squared since kruskal wallis test does not produce correlation values
        rho <- c(rho, NA)
        r_squared <- c(r_squared, NA)
        
        p_value <- c(p_value, a$p.value)
        method <- c(method,"Kruskal-Wallis test")
        
        v_char <- as.character(v[,1])
        v_char[v_char == "NA"] <- NA
        varInterest_char <- as.character(varInterest[,1])
        varInterest_char[varInterest_char == "NA"] <- NA
        n <- c(n, length(na.omit(cbind(varInterest_char,v_char))[,1]))
      }
    }
    # if(max(p_value) < 0.7){
    #   q_value <- qvalue(p_value,lambda=seq(0,0.90,0.05))$qvalues
    # }
    # else if(max(p_value) >= 0.7){
    #   q_value <- qvalue(p_value)$qvalues
    # }
    
    df <- data.frame(Variable = var_name, Type = type, n = n, rho = rho, Rsquared = r_squared, PValue = p_value, Method = method, stringsAsFactors = F)
  }
  
  #Y(Categorical) vs X(Continuous)
  else if(input$var_interest %in% colnames(group)){
    varInterest <- group[,colnames(group) %in% input$var_interest, drop =F]
    for(i in input$choose_variable){
      if(i %in% colnames(dataset) & i != input$var_interest){
        v <- dataset[,colnames(dataset) %in% i, drop =F]
        
        #Multinomial logistic regression test, followed by likelihood ratio test
        test <- multinom(varInterest[,1] ~ v[,1])
        a <- Anova(test)
        
        var_name <- c(var_name,i)
        type <- c(type,"Continuous")
        p_value <- c(p_value, a$`Pr(>Chisq)`)
        method <- c(method,"Multinomial Logistic Regression & Likelihood Ratio Test")
        varInterest_char <- as.character(varInterest[,1])
        varInterest_char[varInterest_char == "NA"] <- NA
        n <- c(n, length(na.omit(cbind(varInterest_char,v))[,1]))
      }
    }
    
    #Y(Categorical) vs X(Categorical)
    for(j in input$choose_variable){
      if(j %in% colnames(group) & j != input$var_interest){
        v <- group[,colnames(group) %in% j, drop =F]
        
        #Fisher exact test
        a <- fisher.test(varInterest[,1],v[,1],workspace = 2000000)
        
        var_name <- c(var_name,j)
        type <- c(type,"Categorical")
        p_value <- c(p_value, a$p.value)
        method <- c(method,"Fisher's exact test")
        
        v_char <- as.character(v[,1])
        v_char[v_char == "NA"] <- NA
        n <- c(n, length(na.omit(cbind(varInterest,v_char))[,1]))
      }
    }
    # if(max(p_value) < 0.7){
    #   q_value <- qvalue(p_value,lambda=seq(0,0.90,0.05))$qvalues
    # }
    # else if(max(p_value) >= 0.7){
    #   q_value <- qvalue(p_value)$qvalues
    # }
    
    df <- data.frame(Variable = var_name, Type = type, n = n, PValue = p_value, Method = method, stringsAsFactors = F)
  }
  
  rownames(df) <- var_name
  
  #Order the table by the p values
  df <- df[order(df$PValue),]
  
  df
})

#Function to produce the results for differential analysis
makeResults1.3.1 <- reactive({
  dataset <- makeTable1.3.1()
  
  dataset <- dataset[order(dataset$PValue),]
  info1 <- paste("Most statistically significant variable: ",dataset[1,1])
  info2 <- paste("No. of variables with p < 0.05: ",sum(dataset$PValue < 0.05,na.rm=T))
  #info3 <- paste("No. of variables with q < 0.05: ",sum(dataset$QValue < 0.05))
  cat(sprintf(info1), "\n")
  cat(sprintf(info2), "\n")
  #cat(sprintf(info3), "\n")
})

#Function to produce the help text for differential analysis
helpText1.3.1 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  if(input$var_interest %in% colnames(dataset)){
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("Variable: Name of variables")
    info4 <- paste("Type: Variable type")
    info5 <- paste("n: Number of paired samples")
    info6 <- paste("rho (only continuous variables): Spearman's rank correlation value")
    info7 <- paste("RSquared (only continuous variables): rho^2")
    info8 <- paste("PValue: Significance level")
    #info9 <- paste("QValue: Adjusted P-Value")
    info10 <- paste("")   
    info11 <- paste("Method for continuous variables: Spearman's rank correlation test")
    info12 <- paste("Method for categorical variables: Kruskal-Wallis test")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
    cat(info5, "\n")
    cat(info6, "\n")
    cat(info7, "\n")
    cat(info8, "\n")
    #cat(info9, "\n")
    cat(info10, "\n")
    cat(info11, "\n")
    cat(info12, "\n")
  }
  
  else {
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("Variable: Name of variables")
    info4 <- paste("Type: Variable type")
    info5 <- paste("n: Number of paired samples")
    info6 <- paste("PValue: Significance level")
    #info7 <- paste("QValue: Adjusted P-Value")
    info8 <- paste("")   
    info9 <- paste("Method for continuous variable: Multinomial Logistic Regression & Likelihood Ratio Test")
    info10 <- paste("Method for categorical variable: Fisher's exact Test")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
    cat(info5, "\n")
    cat(info6, "\n")
    #cat(info7, "\n")
    cat(info8, "\n")
    cat(info9, "\n")
    cat(info10, "\n")
  }
})

listb[["1-3"]] <- tagList(fluidRow(
  column(4, uiOutput("varInterest")),
  column(4, uiOutput("select1_3_2"))
))

output$varInterest <- renderUI({
  selectInput("var_interest", label = "Select outcome of interest", choices = input$choose_variable, multiple = F)
})

listb[["1-3-1"]] <- tagList(h3("Significance Test"),
                            downloadButton('downloadData1.3.1', 'Download data'),
                            actionButton("help1.3.1","Help",icon=icon("question-circle")),
                            actionButton('get1.3.1', 'Get results'),
                            hidden(verbatimTextOutput("helptext1.3.1")),
                            hidden(verbatimTextOutput("getresults1.3.1")),
                            dataTableOutput("table1.3.1"))

output$downloadData1.3.1 <- downloadHandler(
  filename = function() {
    paste('Significance Table','.csv', sep='')},
  content = function(file) {
    df <- makeTable1.3.1()
    
    ########################################################################################################################
    #Insert some basic documentation in the first 7 rows of the data frame, such as title of project, data and time.
    d <- data.frame()
    size <- dim(df)
    d[1,] <-NA
    d[,1:(size[2])] <- NA
    d[1,1] <- "Name of Project:"
    d[1,2] <- input$projectChoice
    d[2,1] <- "Date & Time:"
    d[2,2] <- as.character(Sys.time())
    d[3,1] <- "Timezone:"
    d[3,2] <- as.character(Sys.timezone())
    d[4,1] <- "The data is based on the following files and variables :"
    
    nam <- "List of files:"
    for (i in input$studyChoices){
      nam <- paste(nam,"\n",i,sep = "")
    }
    d[4,2] <- nam
    
    nam1 <- "List of variables:"
    for (i in input$choose_variable){
      nam1 <- paste(nam1,"\n",i,sep = "")
    }
   
    d[4,3] <- nam1
    
    d[6,1] <- paste("This dataset is processed by removing rows that contain more than (and equal to) ",
                    input$row_cutoff, "% missing values and removing columns that contain more than (and equal to) ",
                    input$col_cutoff, "% missing values.", sep = "")
    ###########################################################################################################################################################
    
    d[8,1] <- paste("Title: Significance Table for differential analysis.")
    
    d[9,1] <- paste("Variable of interest:",input$var_interest)
    
    d[11,] <- colnames(df)
    df[,1] <- as.character(df[,1])
    d[12:(size[1]+11),] <- df
    
    colnames(d) <- rep("", length(colnames(d)))
    write.csv(d, file, row.names = F,na="")})

#Enable the "Get Results" button  to produce the results table when clicked
observeEvent(input$get1.3.1, {
  toggle("getresults1.3.1")
})

#Enable the "Help" button  to produce the results table when clicked
observeEvent(input$help1.3.1, {
  toggle("helptext1.3.1")
})

output$helptext1.3.1 <- renderPrint({
  helpText1.3.1()
})

output$getresults1.3.1 <- renderPrint({
  makeResults1.3.1()
})

output$table1.3.1 <- renderDataTable({
  makeTable1.3.1()
})