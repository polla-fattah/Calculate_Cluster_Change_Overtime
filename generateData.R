require(mlbench)
#generates itemsets then changes them by jumping items from one cluster into another
#Parameters:
#n: number of items
#cl: number of clusters
#r: radius of cluster
#sd: standard deviation of items' distribution
#periods: number of time points
#nm: Number of items which must jump from one time point to another. 
#      if NULL then it will be random between 1 and 0
generateData <- function(n=500, cl=4, r=6, sd=1.5, periods=20, nm=NULL){
  set.seed(123)
  if (is.null(nm)){
    nm = as.integer(runif(periods - 1) * 20)
  }
  oldData <- initialData(n, cl, r, sd)
  allData <- oldData
  periods <- periods - 1
  for( i in 1:periods){
#    par(mar = c(3,3,1,1)) ## default is c(5,4,4,2) + 0.1
#    plot(oldData$x, oldData$y, col=oldData$idtyp, pch=oldData$idtyp, xlab = 'X', 
#        ylab = 'Y', mgp = c(2, 1, 0))
    
    newData <- mutateData(oldData, cl, nm[i])
    allData <- merge(allData, newData, all=TRUE, sort=FALSE)
    oldData <- newData
  }
#  par(mar = c(3,3,1,1)) ## default is c(5,4,4,2) + 0.1
#  plot(oldData$x, oldData$y, col=oldData$idtyp, pch=oldData$idtyp, xlab = 'X', 
#       ylab = 'Y', mgp = c(2, 1, 0))
  return (allData)
}
# creates next time point of data based on the previous data set and the number of items should be jumped
# data: previous time point dataset. it also introduce small randomness by changing the 
# possition of items in the same cluster.
#cl: number of clusters
#nm: number of jump
mutateData <- function(data,cl, nm){
  data$period <- data$period[1] + 1
  lenDat <- length(data[,1])
  sm <- sample(lenDat, nm)
  len <- length(sm)
  
  s1 <- sm[1:(len/2)]
  s2 <- sm[((len/2)+1):len]
  
  data$x[s1] <- -data$x[s1]
  data$y[s2] <- -data$y[s2]
  
  data$x <- data$x + data$x * runif(lenDat, 0.01, 0.02)
  data$y <- data$y + data$y * runif(lenDat, 0.01, 0.02)
  return (data)
}
# Creates the first dataset in a Cartesian  coordinate.
#n: number of items
#cl: number of clusters
#r: radius of cluster
#sd: standard deviation of items' distribution
initialData <- function(n, cl, r, sd){
  set.seed(138)
  p <- mlbench.2dnormals(n, cl, r, sd)

  idtyp <- as.integer(p$classes)
  idsubj <- 1:n
  period <- rep(1, n)
  x <- p$x[,1]
  y <- p$x[,2]
  return (data.frame(idtyp, idsubj, period, x, y))
}
