#Source code for PCA biplot
ggbiplot <- function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                      obs.scale = 1 - scale, var.scale = scale, groups = NULL, 
                      ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3, 
                      alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69, 
                      varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE, 
                      ...) 
{
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
                                                  1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
    ylab(u.axis.labs[2])
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                             sin(theta))
      g <- g + geom_path(data = circle, color = muted("white"), 
                         size = 1/2, alpha = 1/3)
    }
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
                                           xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 
                                                                                                  "picas")), color = muted("red"))
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    }
    else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                       mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    g <- g + geom_text(data = df.v, aes(label = varname, 
                                        x = xvar, y = yvar, angle = angle, hjust = hjust), 
                       color = "darkred", size = varname.size)
  }
  return(g)
}

#Function to produce the results table for PCA
makeText3.1 <- reactive({
  x <- makeText3.2()[1,1]
  x <- round((x*100),1) #Converts decimals to percentage
  y <- makeText3.2()[1,2]
  y <- round((y*100),1) #Converts decimals to percentage
  info1 <- paste("The first principal component explains", paste(x,"%", sep = ""), "of the total variation of the variables.")
  info2 <- paste("The second principal component explains", paste(y,"%", sep = ""),  "of the total variation of the variables.")
  
  group <- selec_var()[[2]]
  group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
  
  count <- 2
  info <- cbind(info1, info2)
  for (i in unique(group[,1])){
    count <- count + 1
    n <- length(which(group[,1] == i))
    text <- paste("Number of samples in group",i,":",n)
    info <- cbind(info,text)
  }
  
  cat(info, sep = "\n")
  
})

#Function to produce the output for PCA
makeText3.2 <- function(){
  variable <- selec_var()[[1]]
  
  rownames(variable) <- values$data[,1]
  if(length(colnames(variable)) >= 3){
    variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
  }
  
  var.pca <- prcomp(na.omit(variable), center = TRUE, scale. = TRUE) 
  
  info <- summary(var.pca)
  eigen <- info[[1]]^2
  newinfo <- rbind("Eigenvalues" = eigen, info$importance)
  newinfo[3:4,]
}

#Function to produce the help text for PCA
helpText3.1 <- function(){
  info1 <- paste("PCA computes the variables into principal components, which are sorted accordingly to the proportion of variance it explains.")
  info2 <- paste("")
  info3 <- paste("If there are more than 5 principal componetns, only the first 5 will be shown in the table.")
  info4 <- paste("Download the table to view all of them.")
  info5 <- paste("")
  info6 <- paste("Missing values are imputed using regularized iterative PCA algorithm.")
  info7 <- paste("Variables are automatically standardized for PCA.")
  info8 <- paste("Only continuous variables are used for PCA.")
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
  cat(info5, "\n")
  cat(info6, "\n")
  cat(info7, "\n")
  cat(info8, "\n")
}

#Function to produce PCA biplot
makePlot3 <- function(text_size){
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  group1 <<- group[,which(colnames(group) %in% input$choose_group3)]
  
  rownames(variable) <- values$data[,1]
  
  #Imputes missing values for PCA
  variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
  
  var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
  
  g <- ggbiplot(var.pca, varname.size = 3, obs.scale = 1, var.scale = 1,
                choices = c(as.numeric(str_sub(input$type3.2,3)),
                            as.numeric(str_sub(input$type3.3,3))),
                groups = group1, ellipse = TRUE,
                circle = F) +
    geom_point(aes(color=group1, size = 3)) + scale_size_identity() +
    theme(legend.direction = 'vertical', legend.position = 'right',
          legend.key.height = unit(2.5, "line"),
          text = element_text(size=text_size),
          axis.title.x = element_text(margin=margin(20,0,0,0)),
          axis.title.y = element_text(margin=margin(0,20,0,0)),
          axis.text.x = element_text(margin=margin(10,0,0,0)),
          axis.text.y = element_text(margin=margin(0,10,0,0)),
          axis.title = element_text(margin=margin(0,0,20,0))) +
    
    ggtitle(input$main3) + scale_colour_discrete(name = input$choose_group3)
  
  g
}

#Function to produce the help text for PCA biplot
helpText3.2 <- function(){
  info1 <- paste("The plot above displays the PCA biplot.")
  info2 <- paste("")
  info3 <- paste("Samples are coloured according to the group variable.")
  info4 <- paste("The arrows represent the position of the variables.")
  info5 <- paste("The samples are located with respect to its value on the variables.")
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
  cat(info5, "\n")
}

listb[["3"]] <- tagList(h3('Table of results explained by PC'),
                        br(),
                        downloadButton('downloadData3', 'Download data'),
                        actionButton("help3.1","Help",icon=icon("question-circle")),
                        actionButton('getPCA', 'Get results'),
                        hidden(verbatimTextOutput("text3.1")),
                        hidden(verbatimTextOutput("helptext3.1")),
                        verbatimTextOutput('text3.2'),
                        
                        br(),
                        
                        fluidRow(
                          column(4, uiOutput("Choice3.1")),
                          column(4, uiOutput("Choice3.2"))
                        ),
                        
                        fluidRow(
                          column(4,uiOutput("chooseGroup3"))
                        ),
                        
                        br(),
                        
                        textInput("main3", "Key in the title of PCA plot"),
                        plotOutput("plot3", height = 800, width = 800, hover = "plot1_hover3", brush = "plot1_brush3"),
                        br(),
                        downloadButton('downloadPlot3', 'Download plot as pdf'),
                        actionButton("inter3","Display interactivity"),
                        actionButton("help3.2","Help",icon=icon("question-circle")),
                        
                        br(), 
                        br(),
                        
                        fluidRow(
                          column(6, hidden(textOutput("hovered3")),
                                 hidden(uiOutput("uiExample3.1")),
                                 hidden(verbatimTextOutput("hover_info3"))),
                          column(6, hidden(textOutput("selected3")),
                                 hidden(uiOutput("uiExample3.2")),
                                 hidden(verbatimTextOutput("brush_info3")))
                        ),
                        
                        br(),
                        
                        hidden(verbatimTextOutput("helptext3.2"))
)

output$uiExample3.1 <- renderUI({
  tipify(bsButton("pB31", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Only categorical variables for group variables.")
})

output$downloadData3 <- downloadHandler(
  filename = function() {
    paste('PCA Result','.csv', sep='')},
  content = function(file) {
    df <- makeText3.2()
    df <- cbind(rownames(df),df)
    df[1,1] <- NA
    d <- data.frame()
    size <- dim(df)
    d[1:(size[1]+5),] <-NA
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
    
    d[8,1] <- paste("Title: PCA Results")
    
    d[10,] <- colnames(df)
    d[11:(size[1]+10),] <- df
    d[11:(size[1]+10),1] <- rownames(df)
    d[10,1] <- NA
    
    colnames(d) <- rep("", length(colnames(d)))
    write.csv(d, file,row.names=F,na="")})

#Enables the "Help" button to produce the results for PCA when clicked
observeEvent(input$getPCA, {
  toggle("text3.1")
})

output$text3.1 <- renderPrint({
  makeText3.1()
})

#Enables the "Help" button to produce the help text for PCA when clicked
observeEvent(input$help3.1, {
  toggle("helptext3.1")
})

output$helptext3.1 <- renderPrint({
  helpText3.1()
})

output$text3.2<- renderPrint(
  if(length(colnames(makeText3.2())) > 5){
    makeText3.2()[,1:5]
  }
  else{
    makeText3.2()
  }
)

output$Choice3.1 <- renderUI({
  selectizeInput("type3.2", "Select principal component on x axis", 
                 choices = colnames(makeText3.2()))
})

output$Choice3.2<- renderUI({
  selectizeInput("type3.3", "Select principal component on y axis", 
                 choices = colnames(makeText3.2())[-which(colnames(makeText3.2()) == input$type3.2)])
})

output$chooseGroup3 <- renderUI({
  selectizeInput("choose_group3", "Select group variable (only categorical variables)",
                 choices = colnames(selec_var()[[2]]), multiple = F)
})

#Enables the "Help" button to produce the help text for PCA biplot when clicked
observeEvent(input$help3.2, {
  toggle("helptext3.2")
})

output$helptext3.2 <- renderPrint({
  helpText3.2()
})

output$downloadPlot3 <- downloadHandler(
  filename = function() {
    paste('PCA Biplot','.pdf', sep='')},
  content = function(file) {
    ggsave(file, makePlot3(15), dpi = 300, height = 30, width = 30, units = "cm")})

output$plot3 <- renderPlot({
  makePlot3(15)
})

#Enables the "Display Interactivity" button to display the interaction table for PCA biplot when clicked
observeEvent(input$inter3, {
  toggle("uiExample3.1")
  toggle("uiExample3.2")
  toggle("hovered3")
  toggle("hover_info3")
  toggle("selected3")
  toggle("brush_info3")
})

output$uiExample3.1 <- renderUI({
  tipify(bsButton("pB31", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays the sample which the mouse is hovered on.")
})

output$uiExample3.2 <- renderUI({
  tipify(bsButton("pB32", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Displays samples which the mouse selects on the plot.")
})

output$hovered3 <- renderText({
  "Hovered Point"
})

output$hover_info3 <- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
  
  rownames(variable) <- values$data[,1]
  variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
  
  
  var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
  df <- cbind(values$data[,1],group,variable,var.pca$x)
  
  colnames(df)[1:2] <- c("Subject#", input$choose_group3)
  
  nearPoints(df, input$plot1_hover3, xvar = input$type3.2, yvar = input$type3.3, threshold = 10, maxpoints = 1)[,c(1:2)]
})

output$selected3 <- renderText({
  "Selected Points"
})

output$brush_info3 <- renderPrint({
  variable <- selec_var()[[1]]
  group <- selec_var()[[2]]
  group <- group[,which(colnames(group) %in% input$choose_group3),drop=F]
  
  rownames(variable) <- values$data[,1]
  variable <- imputePCA(variable, ncp = 2, scale = TRUE, method = "Regularized")$completeObs
  
  var.pca <- prcomp(variable, center = TRUE, scale. = TRUE) 
  
  df <- cbind(values$data[,1],group,variable,var.pca$x)
  
  colnames(df)[1:2] <- c("Subject#", input$choose_group3)
  
  
  brushedPoints(df, input$plot1_brush3, xvar = input$type3.2, yvar = input$type3.3)[,c(1:2)]
})