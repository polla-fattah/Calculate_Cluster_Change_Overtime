require(mcclust)
require(pROC)
require(clv)

#setwd('/path/to/generateData.R')
source('generateData.R')


Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)
AUC <- function(clust1, clust2) multiclass.roc(clust1, clust2)$auc[1]

main <- function(){
  set.seed(123)

  allData <<- generateData(500)#read.csv(file.choose(T))

  PERIODS <- max(allData$period)

  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()

  lables <<- allData[which(allData$period == 1), 1]
  data1 <- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- kmeans(data1, 4)$cluster
  
  for(period in 2:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters <<- kmeans(data, 4)$cluster
    
    resultRand[period-1] <<- Rand(clusters1, clusters)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters)
    resultFM[period-1] <<- FM(clusters1, clusters)
    resultVI[period-1] <<- VI(clusters1, clusters)
    resultAUC[period-1] <<- AUC(clusters1, clusters)
    
  }
  
  x = 1:(PERIODS - 1)
  print(resultRand)
  
  plot(x, resultRand, pch = 16, ylim=c(0.3, 1), xlab = 'time points', ylab = '', xaxt = "n")
  axis(side = 1, 
       at = c(1, 4, 9, 14, 19), 
       labels = c('2', '5', '10', '15', '20'),
       tck=-.05)
  lines(resultRand)
  
  points(x, resultJaccard, pch = 1)
  lines(resultJaccard)
  
  points(x, resultFM, pch = 2)
  lines(resultFM)
  
  resultVI = 1 - (resultVI / max(resultVI))
  points(x, resultAUC, pch = 4)
  lines(resultAUC)
  
  points(x, resultAUC, pch = 0)
  lines(resultAUC)
  abline(lm(resultAUC~x), lwd=2)
  
  legend('topright', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ),
          pch=c(16, 1, 2, 4, 0), bty='n', cex=.75)
  
}
main()