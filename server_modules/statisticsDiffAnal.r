values$sigtable <- NULL
values$sigtablename <- NULL

#Function for pairwise wilcox test
test.fun <- function(dat, col) { 
  
  c1 <- combn(unique(dat[,1]),2)
  sigs <- list()
  for(i in 1:ncol(c1)) {
    sigs[[i]] <- wilcox.test(
      dat[dat[,1] == c1[1,i],col],
      dat[dat[,1] == c1[2,i],col],
      alternative = "two.sided"
    )
  }
  names(sigs) <- paste(c1[1,],"by",c1[2,])
  
  tests <- data.frame(Group = names(sigs),
                      W=unlist(lapply(sigs,function(x) x$statistic)),
                      PValue=unlist(lapply(sigs,function(x) x$p.value)),row.names=NULL)
  
  return(tests)
}

#Function to produce the significance table for differential analysis
makeTable1.3.1 <- reactive({
  withProgress(message = 'Loading',value = 0, {
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
  
    n1 <- length(input$choose_variable) 
    if(input$var_interest %in% colnames(dataset)){
      varInterest <- dataset[,colnames(dataset) %in% input$var_interest, drop =F]
    
      for(i in input$choose_variable){
        #Y(Continuous) vs X(Continuous)
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
          #rho[is.na(rho)] <- 0
          #r_squared[is.na(r_squared)] <- 0
          incProgress(1/n1)
        }
        
        #Y(Continuous) vs X(Categorical)
        else if(i %in% colnames(group) & i != input$var_interest){
          v <- group[,colnames(group) %in% i, drop =F]
          
          a <- cbind(varInterest[,1],v[,1])
          
          a <- na.omit(a)
          if(length(unique(a[,2])) == 1){
            p_value <- c(p_value, NA)
          }
          if(length(unique(a[,2])) != 1){
            #Kruskal-Wallis test
            a <- kruskal.test(varInterest[,1],v[,1])
            p_value <- c(p_value, a$p.value)
          }
        
          var_name <- c(var_name,i)
          type <- c(type,"Categorical")
        
          #Give NA values for rho and r squared since kruskal wallis test does not produce correlation values
          rho <- c(rho, NA)
          r_squared <- c(r_squared, NA)
        
          method <- c(method,"Kruskal-Wallis test")
        
          v_char <- as.character(v[,1])
          v_char[v_char == "NA"] <- NA
          n <- c(n, length(na.omit(cbind(varInterest,v_char))[,1]))
          incProgress(1/n1)
        }
    
      q_value <- rep(NA,length(var_name))
    
      df <- data.frame(Variable = var_name, Type = type, n = n, rho = rho, Rsquared = r_squared, PValue = p_value, QValue = q_value, Method = method, stringsAsFactors = F)
      }
    }
  

    else if(input$var_interest %in% colnames(group)){
      varInterest <- group[,colnames(group) %in% input$var_interest, drop =F]
      for(i in input$choose_variable){
        #Y(Categorical) vs X(Continuous)
        if(i %in% colnames(dataset) & i != input$var_interest){
          v <- dataset[,colnames(dataset) %in% i, drop =F]
        
          #Multinomial logistic regression test, followed by likelihood ratio test
          if(length(unique(varInterest[,1])) == 2){
            varInterest[,1] <- relevel(varInterest[,1], ref = as.character(unique(varInterest[,1])[1]))
          }
          
          a <- cbind(varInterest[,1],v[,1])
          a <- na.omit(a)
          if(length(unique(a[,1])) == 1){
            p_value <- c(p_value, NA)
          }
          if(length(unique(a[,1])) != 1){
            test <- multinom(varInterest[,1] ~ v[,1], trace=F)
            a <- Anova(test)
            p_value <- c(p_value, a$`Pr(>Chisq)`)
          }
          var_name <- c(var_name,i)
          type <- c(type,"Continuous")
          method <- c(method,"Multinomial Logistic Regression & Likelihood Ratio Test")
          varInterest_char <- as.character(varInterest[,1])
          varInterest_char[varInterest_char == "NA"] <- NA
          n <- c(n, length(na.omit(cbind(varInterest_char,v))[,1]))
          incProgress(1/n1)
        }
    
        #Y(Categorical) vs X(Categorical)
        else if(i %in% colnames(group) & i != input$var_interest){
          v <- group[,colnames(group) %in% i, drop =F]
        
          #Fisher exact test
          a <- fisher.test(varInterest[,1],v[,1],workspace = 2000000)
        
          var_name <- c(var_name,i)
          type <- c(type,"Categorical")
          p_value <- c(p_value, a$p.value)
          method <- c(method,"Fisher's exact test")
        
          v_char <- as.character(v[,1])
          v_char[v_char == "NA"] <- NA
          varInterest_char <- as.character(varInterest[,1])
          varInterest_char[varInterest_char == "NA"] <- NA
          
          n <- c(n, length(na.omit(cbind(varInterest_char,v_char))[,1]))
          incProgress(1/n1)
        }
    
      q_value <- rep(NA,length(var_name))
    
      df <- data.frame(Variable = var_name, Type = type, n = n, PValue = p_value, QValue = q_value, Method = method, stringsAsFactors = F)
      }
    }
  
    #Order the table by the p values
    df <- df[order(df$PValue),]
  
    #Insert QValue
    p <- df$PValue[!is.na(df$PValue)]
    if(max(p) < 0.7){
      q_value <- qvalue(p,lambda=seq(0,0.90,0.05))$qvalues
    }
    else if(max(p) >= 0.7){
      q_value <- qvalue(p)$qvalues
    }
    df$QValue <- c(q_value,rep(NA,count_missing(df$PValue)))
    incProgress(1/n1)
    df
    
  })
})

#Function to produce the directionality table for differential analysis
makeTable1.3.2 <- reactive({
  withProgress(message = 'Loading',value = 0, {
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    direc <- NULL
    df <- makeTable1.3.1()
    if(input$var_interest %in% colnames(dataset)){
      for(i in 1:length(df[,1])){
        if(is.na(df$PValue[i])){
          direc <- c(direc,"No significance")
          incProgress(1/length(df[,1]))
        }
        else if(df$PValue[i] > 0.05){
          direc <- c(direc,"No significance")
          incProgress(1/length(df[,1]))
        }
        else{
          
          #Directionality for Y(Continuous) vs X(Continuous), Scatterplot
          if(df$Type[i] == "Continuous"){
            if(df$rho[i] < 0){
              direc <- c(direc,"-ve")
              incProgress(1/length(df[,1]))
            }
            else{
              direc <- c(direc,"+ve")
              incProgress(1/length(df[,1]))
            }
          }
          
          #Directionality for Y(Continuous) vs X(Categorical), Boxplot
          else{
            varX <- group[,colnames(group) %in% df$Variable[i]]
            varY <- dataset[,colnames(dataset) %in% input$var_interest]
            df1 <- data.frame(varX,varY)
            df1 <- na.omit(df1)
            
            #Pairwise U test
            top <- lapply(colnames(df1)[-1],function(x) test.fun(df1,x))[[1]]
            top <- na.omit(top)
            top <- top[top$PValue < 0.05,]
            
            #Provides a summary of statistics for each subgroup
            btm <- do.call(rbind,describeBy(df1[,2], group=df1[,1],  type=2))[c(1,2,3,4,5,8,9,10)]
            colnames(btm)[1] <- df$Variable[i]
            btm[,1] <- rownames(btm)
            btm <- data.frame(btm)
            
            top[,1] <- as.character(top[,1])
            
            test <- strsplit(top[,1]," ")
            text <- NULL
            for(i in 1:length(test)){
              xGroup <- NULL
              yGroup <- NULL
              for(j in 1: length(test[[i]])){
                n <- which(test[[1]] == "by")
                if(j < n){
                  yGroup <- paste(yGroup,test[[i]][j])
                }
                else if(j > n){
                  xGroup <- paste(xGroup,test[[i]][j])
                }
              }
              xGroup <- substring(xGroup, 2)
              yGroup <- substring(yGroup, 2)
              x_n <- btm[which(rownames(btm) %in% xGroup),2]
              y_n <- btm[which(rownames(btm) %in% yGroup),2]
              p <- round(top$PValue[1],3)
              text1 <- paste("(",xGroup,", n=",x_n,", ",yGroup,", n=",y_n," ,p=",p,")",sep="")
              text <- paste(text,text1,sep="\n")
            }
            direc <- c(direc,substring(text, 2))
            incProgress(1/length(df[,1]))
          }
        }
      }
      data.frame(Variable = df[,1], Type = df[,2], Direction = direc,stringsAsFactors = F)
    }
    else{
      for(i in 1:length(df[,1])){
        if(is.na(df$PValue[i])){
          direc <- c(direc,"No significance")
          incProgress(1/length(df[,1]))
        }
        else if(df$PValue[i] > 0.05){
          direc <- c(direc,"No significance")
          incProgress(1/length(df[,1]))
        }
        else{
          
          #Directionality for Y(Categorical) vs X(Continuous), Boxplot
          if(df$Type[i] == "Continuous"){
            varX <- dataset[,colnames(dataset) %in% df$Variable[i]]
            varY <- group[,colnames(group) %in% input$var_interest]
            
            df2 <- data.frame(varX,varY)
            df2 <- na.omit(df2)
            varX <- df2[,1]
            varY <- as.factor(df2[,2])
            
            #varY <- as.character(varY)
            if(length(unique(varY)) == 2){
              
              #Specify what is the reference level since multinomial logistics regression
              #does not show it when there are only 2 groups
              varY <- relevel(varY, ref = as.character(unique(varY)[1]))
            }
            model <- multinom(varY  ~ varX, trace=F)
            
            df1 <- coef(model)
            reference <- unique(varY)[!(unique(varY) %in% rownames(df1))]
            
            group_names <- NULL
            intercept_number <- NULL
            slope_number <- NULL
            n <- length(rownames(df1))
            
            if(n == 0){
              df1[2] <- round(df1[2],3)
              text1 <- paste("Reference group: ", unique(varY)[1], ". ", sep = "")
              text2 <- paste("(",unique(varY)[2],", ",df1[2], ")",sep="")
              direc <- c(direc,paste(text1, text2, sep=""))
              incProgress(1/length(df[,1]))
            }
            if(n != 0){
              df1[,2] <- round(df1[,2],3)
              text1 <- paste("Reference group: ", reference, ". ", sep = "")
              
              a <- paste("(",rownames(df1),", ",df1[,2], ")",sep="")
              text2 <- NULL
              for(i in a){
                text2 <- paste(text2,i,sep="\n")
              }
              text2 <- substring(text2, 2)
              direc <- c(direc,paste(text1, text2, sep=""))
              incProgress(1/length(df[,1]))
            }
            
            # text1 <- paste("Reference group: ", reference, ". ", sep = "")
            # text2 <- paste("The multinomial log-odds of being in group ",group_names,
            #                " relative to group ",reference," is(are) respectively ",intercept_number,
            #                " and the log-odds changes respectively by ",slope_number,
            #                " for each unit increase in ", df$Variable[i],".", sep = "")
            # direc <- c(direc,paste(text1, text2, sep=""))
          }
          else{
            #Directionality for Y(Categorical) vs X(Continuous), Boxplot
            varX <- group[,colnames(group) %in% df$Variable[i]]
            varY <- group[,colnames(group) %in% input$var_interest]
            varX <- as.character(varX)
            varY <- as.character(varY)
            df1 <- na.omit(cbind(varX,varY))
            
            df1 <- data.frame(df1)
            
            #Give the frequency table for each combination of categorical variable
            freq=table(col(df1), as.matrix(df1))
            counts <- ddply(df1, .(df1[,1], df1[,2]), nrow)
            a <- paste("(",counts[,1],"&",counts[,2],",","n=",counts[,3],")",sep="")
            
            text <- NULL
            for(i in a){
              text <- paste(text,i,sep="\n")
            }
            text <- substring(text, 2)
            direc <- c(direc,text)
            incProgress(1/length(df[,1]))
          }
        }
      }
      data.frame(Variable = df[,1], Type = df[,2], Direction = direc,stringsAsFactors = F)
    }
  })
})

#Function to produce the results for differential analysis
makeResults1.3.1 <- reactive({
  dataset <- makeTable1.3.1()
  
  dataset <- dataset[order(dataset$PValue),]
  info1 <- paste("Most statistically significant variable: ",dataset[1,1])
  info2 <- paste("No. of variables with p < 0.05: ",sum(dataset$PValue < 0.05,na.rm=T))
  info3 <- paste("No. of variables with q < 0.05: ",sum(dataset$QValue < 0.05,na.rm=T))
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
})

#Function to produce the help text for differential analysis
helpText1.3.1 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  if(values$sigtablename == "Significance Table"){
    if(input$var_interest %in% colnames(dataset)){
      info1 <- paste("The headers in the table below represents")
      info2 <- paste("")
      info3 <- paste("Variable: Name of variables")
      info4 <- paste("Type: Variable type")
      info5 <- paste("n: Number of paired samples")
      info6 <- paste("rho (only continuous variables): Spearman's rank correlation value")
      info7 <- paste("RSquared (only continuous variables): rho^2")
      info8 <- paste("PValue: Significance level")
      info9 <- paste("QValue: Adjusted P-Value")
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
      cat(info9, "\n")
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
      info7 <- paste("QValue: Adjusted P-Value")
      info8 <- paste("")   
      info9 <- paste("Method for continuous variable: Multinomial Logistic Regression & Likelihood Ratio Test")
      info10 <- paste("Method for categorical variable: Fisher's exact Test")
      
      cat(info1, "\n")
      cat(info2, "\n")
      cat(info3, "\n")
      cat(info4, "\n")
      cat(info5, "\n")
      cat(info6, "\n")
      cat(info7, "\n")
      cat(info8, "\n")
      cat(info9, "\n")
      cat(info10, "\n")
    }
  }
  if(values$sigtablename == "Directionality Table"){
    if(input$var_interest %in% colnames(dataset)){
      info1 <- paste("Method for continuous variables: Spearman's rank correlation test")
      info2 <- paste("Method for categorical variables: Kruskal-Wallis test")
      info3 <- paste("")
      info4 <- paste("For continuous variables: -ve/+v2 means rho < 0/ > 0 respectively")
      info5 <- paste("For categorical variables: the numbers represent the sample size in each subgroup")
      cat(info1, "\n")
      cat(info2, "\n")
      cat(info3, "\n")
      cat(info4, "\n")
      cat(info5, "\n")
    }
    
    else {
      info1 <- paste("Method for continuous variable: Multinomial Logistic Regression & Likelihood Ratio Test")
      info2 <- paste("Method for categorical variable: Fisher's exact Test")
      info3 <- paste("")
      info4 <- paste("For continuous variables: The numbers represent the change in log odds of being in that group ")
      info5 <- paste("relative to the control group for every unit increase in the continuous variable.")
      info6 <- paste("")
      info7 <- paste("For categorical variables: the numbers represent the sample size in each subgroup.")
      cat(info1, "\n")
      cat(info2, "\n")
      cat(info3, "\n")
      cat(info4, "\n")
      cat(info5, "\n")
      cat(info6, "\n")
      cat(info7, "\n")
    }
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
                            textOutput("text1.3.1.1"),
                            downloadButton('downloadData1.3.1', 'Download data'),
                            actionButton("help1.3.1","Help",icon=icon("question-circle")),
                            actionButton('get1.3.1', 'Get results'),
                            actionButton('display1.3.1.1', "Sig Table"),
                            actionButton('display1.3.1.2', "Display Directionality Table"),
                            hidden(verbatimTextOutput("helptext1.3.1")),
                            hidden(verbatimTextOutput("getresults1.3.1")),
                            uiOutput("text1.3.1.2"),
                            dataTableOutput("table1.3.1"))

output$text1.3.1.1 <- renderText({
  "Click on the button, Sig Table, to begin."
})

main_table <- observeEvent(input$display1.3.1.1,{
  output$table1.3.1 <- renderDataTable({main_table1()})
  output$text1.3.1.2 <- renderUI({
    textOutput("text1.3.1_2")
  })
  output$text1.3.1_2 <- renderText({
    NULL
  })
  values$sigtable <- main_table1()
  values$sigtablename <- "Significance Table"
})

direc_table <- observeEvent(input$display1.3.1.2,{
  output$table1.3.1 <- renderDataTable({direc_table1()})
  output$text1.3.1.2 <- renderUI({
    verbatimTextOutput("text1.3.1_2")
  })
  output$text1.3.1_2 <- renderPrint({
    info1 <- paste("**The numbers in the direction column for continuous variables represent the increase/decrease in the log-")
    info2 <- paste("odds of being in that group relative to the control group per unit increase in that continuous variable.**")
    cat(info1, "\n")
    cat(info2, "\n")
  })
  values$sigtable <- direc_table1()
  values$sigtablename <- "Directionality Table"
})

main_table1 <- eventReactive(input$display1.3.1.1,{
  makeTable1.3.1()
})

direc_table1 <- eventReactive(input$display1.3.1.2,{
  makeTable1.3.2()
})

output$select1.3.1 <- renderUI({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  if(input$var_interest %in% colnames(dataset) & input$type1.3.1 == 2){
    selectInput("choose_variable1.3.1", label = "Select Group Variable", choices = colnames(group), multiple = F)
  }
  else{
    return()
  }
})

output$downloadData1.3.1 <- downloadHandler(
  filename = function() {
    paste(values$sigtablename,'.csv', sep='')},
  content = function(file) {
    df <- values$sigtable
    
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
    
    d[8,1] <- paste("Title:", values$sigtablename, "for differential analysis.")
    
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
  NULL
})