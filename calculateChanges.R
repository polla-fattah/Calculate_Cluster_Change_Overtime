require(mcclust)
require(pROC)
require(clv)

#setwd('/path/to/generateData.R')
source('generateData.R')

# External clustering validity measures function with single function call
Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)
AUC <- function(clust1, clust2) multiclass.roc(clust1, clust2)$auc[1]

main <- function(){
  set.seed(123)
  
  #Generates data alternativelycustom 
  #dataset can be used read.csv(file.choose(T))
  #custom data should have period attribute indicating time point 
  allData <<- generateData(500)

  PERIODS <- max(allData$period)
  CLUSTER_NO <- 4
  
  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()
  
  # Cluster the first time point to be compared with all other time points
  data1 <- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- kmeans(data1, CLUSTER_NO)$cluster
  
  #cluster all other time points and compare them with the first one.
  for(period in 2:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters <<- kmeans(data, CLUSTER_NO)$cluster
    
    resultRand[period-1] <<- Rand(clusters1, clusters)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters)
    resultFM[period-1] <<- FM(clusters1, clusters)
    resultVI[period-1] <<- VI(clusters1, clusters)
    resultAUC[period-1] <<- AUC(clusters1, clusters)
    
  }
  # Plotting results
  x = 1:(PERIODS - 1)

  cat('\n====Rand resluts====\n')
  cat(resultRand, '\n')
  plot(x, resultRand, pch = 16, ylim=c(0, 1), xlab = 'time points', ylab = '', xaxt = "n")
  axis(side = 1, 
       at = c(1, 4, 9, 14, 19), 
       labels = c('2', '5', '10', '15', '20'),
       tck=-.05)
  lines(resultRand)
  
  cat('\n====Jaccard resluts====\n')
  cat(resultJaccard, '\n')
  points(x, resultJaccard, pch = 1)
  lines(resultJaccard)
  
  cat('\n====FM resluts====\n')
  cat(resultFM, '\n')  
  points(x, resultFM, pch = 2)
  lines(resultFM)
  
  #change VI to fit with other metrics
  resultVI = 1 - (resultVI / max(resultVI))
  cat('\n====VI resluts====\n')
  cat(resultVI, '\n')
  points(x, resultVI, pch = 4)
  lines(resultVI)
  
  cat('\n====AUC resluts====\n')
  cat(resultAUC, '\n')
  points(x, resultAUC, pch = 0)
  lines(resultAUC)
  abline(lm(resultAUC~x), lwd=2)
  
  legend('bottomleft', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ),
          pch=c(16, 1, 2, 4, 0), bty='n', cex=.75)
  
}
main()