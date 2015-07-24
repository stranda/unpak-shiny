library(ggplot2)
xxx = matrix(nrow = 10, ncol = 8)
xxx = as.data.frame(xxx)

for (i in 1:8) {
  xxx[,i] = runif(10,1,100)
}

histogramList <- vector('list', ncol(xxx)/2)

for (i in 1:(ncol(xxx)/2)) {
  temp = ggplot  
}