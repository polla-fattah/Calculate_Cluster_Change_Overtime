require(mlbench)

generateData <- function(n=500, cl=4, r=6, sd=1.5, periods=20, nm=NULL){
  set.seed(123)
  if (is.null(nm)){
    nm = as.integer(runif(periods - 1) * 20)
  }
  oldData <- initialData(n, cl, r, sd)
  allData <- oldData
  periods <- periods - 1
  for( i in 1:periods){
    newData <- mutateData(oldData, cl, nm[i])
    allData <- merge(allData, newData, all=TRUE, sort=FALSE)
    oldData <- newData
  }
  return (allData)
}
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
  plot(data$x, data$y, col=data$idtyp)
  return (data)
}
initialData <- function(n, cl, r, sd){
  set.seed(138)
  p <- mlbench.2dnormals(n, cl, r, sd)
  plot(p)
  idtyp <- as.integer(p$classes)
  idsubj <- 1:n
  period <- rep(1, n)
  x <- p$x[,1]
  y <- p$x[,2]
  return (data.frame(idtyp, idsubj, period, x, y))
}
