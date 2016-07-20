#Function to produce the help text for the heatmap
helpText4 <- function(){
  info1 <- paste("The plot displays the HC dendrogram, which clusters similar samples and variables together.")
  info2 <- paste("")
  info3 <- paste("Selection of group variables determines the label colour of each sample in the dendrogram.")
  info4 <- paste("")
  info5 <- paste("The missing values are imputed using the Kth Nearest Neighbour Algorithm.")
  info6 <- paste("Distance: Euclidean, Cluster Method: Ward")
  info7 <- paste("Variables are automatically standardized for Hierarchical Clustering.")
  info8 <- paste("Only continuous variables are used for Hierarchical Clustering.")
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
  cat(info5, "\n")
  cat(info6, "\n")
  cat(info7, "\n")
  cat(info8, "\n")
}

#Function to produce the interactive heatmap
makePlot4.1 <- function(){
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  group1 <<- group[,which(colnames(group) %in% input$choose_group4)]
  if(class(group1) != "data.frame"){
    group1 <- data.frame(group1)
  }
  
  rownames(variable) <- values$data[,1]
  
  variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
  n_col <- length(colnames(variable))
  n_col <- n_col / 2
  variable <- variable[,1:n_col]
  
  variable <- scale(variable)
  
  p <- heatmaply(t(variable), scale='none', cexCol = 0, 
                 scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colors = rev(rainbow(20*10, start = 0/6, end = 4/6))),
                 distfun = function(x) dist(x,method = "euclidean"),
                 hclustfun = function(x) hclust(x,method = "ward"))
  
  layout(p,
         title = input$main4,
         autosize = FALSE,
         width = 1000,
         height = 1000,
         margin = list(l = 250, r = 50, b = 250, t = 50, pad = 4)
  )
}

##Function to produce the non-interactive heatmap
makePlot4.2 <- function(){
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  group1 <- group[,which(colnames(group) %in% input$choose_group4),drop=F]
  if(class(group1) != "data.frame"){
    group1 <- data.frame(group1)
  }
  
  rownames(variable) <- values$data[,1]
  
  variable <- kNN(variable, k = 5,numFun = weightedMean, weightDist=TRUE)
  n_col <- length(colnames(variable))
  n_col <- n_col / 2
  variable <- variable[,1:n_col]
  
  
  variable <- scale(variable)
  cols <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256))
  variable <- pmin(pmax(variable , -3), 3) 
  
  p <- heatmap.2(t(variable), Rowv = input$displayRow4, key = TRUE, scale = "none",
                 density.info="none", trace = "none", main = "title", 
                 cexRow = 1, margins = c(4,8), symkey = F, keysize = 1.0,
                 distfun = function(x) dist(x,method = "euclidean"),
                 hclustfun = function(x) hclust(x,method = "ward"))
  
  g_name <- values$data[,1,drop=F]
  g_colour <- rep(0,length(group1[,1]))
  
  col1 <- brewer.pal(11,"Spectral")[7]
  col2 <- brewer.pal(8,"Set1")[c(6,7,8)]
  col3 <- brewer.pal(8,"Accent")[8]
  col4 <- brewer.pal(11,"Spectral")[c(1,4)]
  col5 <- brewer.pal(9,"YlGnBu")[9]
  col <- c(col2,col1,col3,col4,col5)
  
  for (i in 1:length(unique(group1[,1]))){
    for (j in 1:length(group1[,1])){
      if(group1[,1][j] == unique(group1[,1])[i]){
        g_colour[j] <- col[i]
      }
    }
  }
  
  df <- data.frame(cbind(g_name,g_colour), stringsAsFactors = F) 
  df[,2] <- as.character(df[,2])
  
  df2 <- data.frame(rownames(p$carpet))
  group_col <- NULL
  for(i in 1:length(df2[,1])){
    for(j in 1:length(df[,1])){
      if(df2[,1][i] == df[,1][j])
        group_col <- c(group_col,df[,2][j])
    }
  }
  
  leg <- as.character(unique(group1[,1]))
  fil <- col[1:length(unique(group1[,1]))]
  
  my_palette <- rev(rainbow(20*10, start = 0/6, end = 4/6))
  
  if(input$displayRow4){
    a <- NULL
  }
  
  if(!input$displayRow4){
    a <- NA
  }
  
  heatmap.2(t(variable), Rowv = input$displayRow4, scale ='none', symkey = F, keysize = 1.0, col = cols,
            key = TRUE, key.xlab = "Row Z-Score", density.info="none", trace = "none", main = input$main4, 
            ColSideColors = group_col, labCol = NA, labRow = a, margins = c(4,14), cexRow = 1.5, cexCol = 1.5,
            distfun = function(x) dist(x,method = "euclidean"),
            hclustfun = function(x) hclust(x,method = "ward"))
  
  par(cex.main=1)
  legend("topright",
         title = input$choose_group4,
         legend = leg, 
         fill = fil, 
         bty="n", y.intersp = 1, cex=1)
}

listb[["4"]] <- tagList(uiOutput("chooseGroup4"),
                        checkboxInput("displayRow4", "Display Row Dendrogram", value = F),
                        uiOutput("uiExample4"),
                        
                        br(),
                        br(),
                        
                        textInput("main4", "Key in the title of Cluster Dendrogram"),
                        plotOutput("plot4",height = "800px",width = "1000px"),
                        downloadButton('downloadPlot4', 'Download the plot as png'),
                        actionButton("help4","Help",icon=icon("question-circle")),
                        hidden(verbatimTextOutput("helptext4")))
                        #plotlyOutput("plot4.1",height = "800px"))

output$chooseGroup4 <- renderUI({
  selectizeInput("choose_group4", "Select group variable (only categorical variables)",
                 choices = colnames(selec_var()[[2]]), multiple = F)
})

output$uiExample4 <- renderUI({
  tipify(bsButton("pB4", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Clusters similar variables together.")
})

#Enables the "Help" button to produce the help text for the heatmap when clicked
observeEvent(input$help4, {
  toggle("helptext4")
})

output$helptext4 <- renderPrint({
  helpText4()
})

output$downloadPlot4 <- downloadHandler(
  filename = function() {
    paste('Heatmap','.png', sep='')},
  content = function(file) {
    png(file,antialias = "cleartype", height = 700, width = 700)
    print(makePlot4.2())
    dev.off()})

output$plot4 <- renderPlot({
  makePlot4.2()
})