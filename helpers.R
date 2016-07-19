options(java.parameters = "-Xmx4g") 
library(XLConnectJars)
library(XLConnect)
library(psych)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(reshape2)
library(plotly)
library(heatmaply)
library(gplots)
library(RPostgreSQL)
library(tidyr)
library(missMDA)
library(VIM)
library(laeken)
library(qvalue)
library(nnet)
library(car)
library(plyr)

long <- function(file1, file2, common){
  form <- merge(file1, file2, by <- common, all = T)
  
  form
}

Long <- function(file, common){
  file[!sapply(file, function (x) all(is.na(x) | x == ""))]
}


Wide <- function(form, common){
  
  var_list <- data.frame(colnames(form), c(1:length(colnames(form))))
  var <- colnames(form)
  colnames(var_list) <- c("variable", "num")
  
  n1 <- length(form[,1])
  
  for (i in 1:n1){
    if (form[i,"Timepoint_Visit"] == " " | is.na(form[i,"Timepoint_Visit"])){
      form[i,"Timepoint_Visit"] <- 99   #Convert missing time.values to 5 first so that the reshape command in the next line will not ommit the missing values
    }
  }
  
  for (i in common){
    var <- var[-which(var == i)]
  }
  
  form_1 <- reshape(form, timevar = "Timepoint_Visit", idvar = "Subject", 
                    v.names = var, 
                    direction = "wide")
  form_1 <- form_1[,c(names(form_1)[1:2],sort(names(form_1)[3:length(names(form_1))]))]
  form_1[!sapply(form_1, function (x) all(is.na(x) | x == ""))]
  
}

is_greater <- function(dataset, number){
  dataset > number
}

strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

count_missing <- function(dataset){
  k <- 0
  for (i in dataset){
    if (is.na(i)){
      k <- k + 1
    }
    else if (i == 'nq' | i == 'NQ' | i == 'Nq' | i == "nQ" |
             i == 'NS' | i == 'ns' | i == 'Ns' | i == "nS" |
             i == 'BLD' | i == 'bld' | i == 'Bld' | i == ""  | 
             i == "n.q." | i =='NA'){
      k <- k + 1
    }
    else{
      k <- k
    }    
  }
  return (k)
}

all_missing <- function(dataset){
  if (count_missing(dataset) == length(dataset)){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

cor.mtest <- function(mat, conf.level = 0.95, u, met){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <-  matrix(NA, n, n)
  colnames(p.mat) = colnames(mat)
  rownames(p.mat) = colnames(mat)
  diag(p.mat) <- NA
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if (is.na(cor(mat[,i],mat[,j], use = u, method = met)) == F){
        tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level, use = u, method = met)
        p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      }
    }
  }
  return(p.mat)
}

