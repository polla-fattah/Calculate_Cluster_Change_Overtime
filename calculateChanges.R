require(mcclust)
require(pROC)
require(clv)
require(e1071)
library(stargazer)

#setwd('/path/to/generateData.R')
source('generateData.R')

# External clustering validity measures function with single function call
Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)
AUC <- function(clust1, clust2) multiclass.roc(clust1, clust2)$auc[1]

# Clustering Algorithms

kmeansClusters <- function(x, n)  kmeans(x, n)$cluster
cmeansClusters <- function(x, n)  cmeans(x, n)$cluster
pamClusters <- function(x, n)  pam(x, n)$clustering
hierClusters <- function(x, n) cutree(hclust(dist(x)), n)


getData <- function(type = 'test'){
  if (type == 'game10'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game/repeated.csv')
    data <- data[,1:5]
  }
  else if(type == 'game27'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game27/game27.csv')
    data <- data[,1:5]
  }
  else if(type == 'test'){
    data <- generateData(500)
  }
  else{
    return (NULL)
  }
  return (data)
}

measureChangesStart <- function(hierClusters, clusterFUNC = kmeansClusters){
  set.seed(123)
  
  PERIODS <- max(allData$period)
  CLUSTER_NO <- 4
  
  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()
  
  # Cluster the first time point to be compared with all other time points
  data1 <<- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- clusterFUNC(data1, CLUSTER_NO)
  
  #cluster all other time points and compare them with the first one.
  for(period in 2:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters <<- clusterFUNC(data, CLUSTER_NO)
    
    resultRand[period-1] <<- Rand(clusters1, clusters)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters)
    resultFM[period-1] <<- FM(clusters1, clusters)
    resultVI[period-1] <<- VI(clusters1, clusters)
    resultAUC[period-1] <<- AUC(clusters1, clusters)
    
  }
  
  data.frame(resultRand, resultJaccard, resultFM, resultVI, resultAUC)
}

measureChangesConsiquent <- function(hierClusters, clusterFUNC = kmeansClusters){
  set.seed(123)
  
  PERIODS <- max(allData$period)
  CLUSTER_NO <- 4
  
  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()
  
  # Cluster the first time point to be compared with all other time points
  data1 <<- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- clusterFUNC(data1, CLUSTER_NO)
  
  #cluster all other time points and compare them with the first one.
  for(period in 1:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters2 <<- clusterFUNC(data, CLUSTER_NO)
    
    resultRand[period-1] <<- Rand(clusters1, clusters2)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters2)
    resultFM[period-1] <<- FM(clusters1, clusters2)
    resultVI[period-1] <<- VI(clusters1, clusters2)
    resultAUC[period-1] <<- AUC(clusters1, clusters2)
    
    clusters1 <- clusters1
    
  }
  
  data.frame(resultRand, resultJaccard, resultFM, resultVI, resultAUC)
}
plotChanges <- function(f, x){
  # Plotting results
  #x = 1:(PERIODS - 1)
  
  par(mar = c(3, 2, 1, 1)) ## default is c(5,4,4,2) + 0.1
  
  plot(x, f$resultRand, pch = 16, ylim=c(0, 1), xlab = 'time points', 
       ylab = '', xaxt = "n", mgp = c(2, 1, 0))
  axis(side = 1, tck=-.05,
       at = c(1, 4, 9, 14, 19, 24), 
       labels = c('1-2', '4-5', '9-10', '14-15', '19-20', '24-25'))
  lines(resultRand)
  
  points(x, f$resultJaccard, pch = 1)
  lines(f$resultJaccard)
  
  points(x, f$resultFM, pch = 2)
  lines(f$resultFM)
  
  #change VI to fit with other metrics
  resultVI = 1 - (f$resultVI / max(f$resultVI))
  points(x, resultVI, pch = 4)
  lines(resultVI)
  
  points(x, f$resultAUC, pch = 0)
  lines(f$resultAUC)
  
  
  abline(lm(f$resultAUC~x), lwd=2)
  
  legend('topright', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ), #topright, bottomleft
         pch=c(16, 1, 2, 4, 0), bty='n', cex=.75)
}

dataTypes <- c('test', 'game10', 'game27')
clustFunc <- c(kmeansClusters, cmeansClusters, 
               pamClusters, hierClusters)
clustFuncStr <- c('kmeansClusters', 'cmeansClusters', 
               'pamClusters', 'hierClusters')

resultVecrors <- list()
resultFrame <- list()

for(dataType in dataTypes){
  allData <- getData(dataType)
  x <- 1:(max(allData$period)-1)
  i <- 1
  
  for(fun in clustFunc){
    index = paste0(dataType, '_', clustFuncStr[i])
    print(index)
    r <- measureChangesStart(allData , clusterFUNC = fun)

    png(file=paste0(dataType, '_', clustFuncStr[i], '.png'))
    plotChanges(r, x)
    dev.off()
    resultVecrors[[index]] <- as.vector(t(r))
    resultFrame[[index]] <- as.vector(r)
    i = i + 1
  }
}


# stargazer(summaryTable(brCa, 1:(ncol(brCa) - 2)), 
#           summary = F, title = 'Measures of centrality, 
#           dispersion, and number of missing values 
#           that each attribute has in the breast cancer data set.', 
#           label = 'tab:describeData', rownames = F)

