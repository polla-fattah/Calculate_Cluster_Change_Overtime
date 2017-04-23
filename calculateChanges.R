require(mcclust)
require(pROC)
require(clv)
require(e1071)
library(stargazer)

#setwd('/path/to/generateData.R')
#source('generateData.R')

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


getData <- function(dataset = 'test'){
  if (dataset == 'game10'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game/repeated.csv')
    data <- data[,1:5]
  }
  else if(dataset == 'game27'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game27/game27.csv')
    data <- data[,1:5]
  }
  else if(dataset == 'stock'){
    data = list()
    #prepare data frame from MainStocks4.R
    #d <- read.csv('C:/Users/pqf/Google Drive/PhD/Codes/Data/SP500 1-2015 to 7-2015 Normilized.csv')
    data$idtyp = DATA_FRAME$Symbol
    data$idsubj = DATA_FRAME$Symbol
    data$period = DATA_FRAME$Date
    data$close = DATA_FRAME$Close
    data$closeDiff = DATA_FRAME$closedDiff
    data <- as.data.frame(data)
  }
  else if(dataset == 'test'){
    data <- generateData(500)
  }
  else{
    return (NULL)
  }
  return (data)
}

measureChangesStart <- function(allData, clusterFUNC = kmeansClusters){
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

measureChangesConsiquent <- function(allData, clusterFUNC = kmeansClusters){
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
    
    clusters1 <<- clusters2
    
  }
  
  data.frame(resultRand, resultJaccard, resultFM, resultVI, resultAUC)
}

measureChangesClass <- function(allData, clusterFUNC = kmeansClusters){
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
  
  # This attribute is generated using attachClass(classify(c(1,3,5,2,4,2,6,5)))
  clusters1 <<- classify(ttr.varQuantileStock$classifierLimits)
  
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



plotChanges <- function(f, x, dataset){
  # Plotting results
  #x = 1:(PERIODS - 1)
  
  par(mar = c(3, 2, 1, 1)) ## default is c(5,4,4,2) + 0.1
  
  plot(x, f$resultRand, pch = 16, ylim=c(0, 1), xlab = 'time points', 
        mgp = c(2, 1, 0))
#  axis(side = 1, tck=-.05,
#       at = c(1, 4, 9, 14, 19, 24), 
#       labels = c('1-2', '4-5', '9-10', '14-15', '19-20', '24-25'))
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
  print('------------------------------------------')
  print(dataset)
  ff <<- lm(f$resultAUC~x)
  print(lm(f$resultAUC~x)$coefficients[2])
  print(lm(f$resultRand~x)$coefficients[2])
  
  abline(lm(f$resultAUC~x), lwd=2)
  if(dataset == 'test')
    legend('bottomleft', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ), #topright, bottomleft
           pch=c(16, 1, 2, 4, 0), bty='n', cex=1.3)
  else
    legend('topright', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ), #topright, bottomleft
           pch=c(16, 1, 2, 4, 0), bty='n', cex=1.3)
}

datasets <- c('stock') #c('test', 'game10', 'game27')
clustFunc <- c(kmeansClusters, cmeansClusters, 
               pamClusters, hierClusters)
clustFuncStr <- c('kmeansClusters', 'cmeansClusters', 
               'pamClusters', 'hierClusters')


#---------- 
resultVecrorsFirs <- list()
resultFrameFirs <- list()

for(dataset in datasets){
  allData <- getData(dataset)

  x <- 1:(max(allData$period)-1)
  i <- 1

  for(fun in clustFunc){
    index = paste0(dataset, '_', clustFuncStr[i])

    rFirs <- measureChangesStart(allData , clusterFUNC = fun)
    print('================================================================')
    print(fun)
    png(file=paste0(dataset, '_', clustFuncStr[i], '_Firs.png'))
    plotChanges(rFirs, x, dataset)
    dev.off()

    resultVecrorsFirs[[index]] <- as.vector(t(rFirs))
    resultFrameFirs[[index]] <- as.vector(rFirs)
    i = i + 1
  }
}
#
resultVectorsCons <- list()
resultFrameCons <- list()

for(dataset in datasets){
  allData <- getData(dataset)
  x <- 1:(max(allData$period)-1)
  i <- 1

  for(fun in clustFunc){
    index = paste0(dataset, '_', clustFuncStr[i])

    rCons <- measureChangesConsiquent(allData , clusterFUNC = fun)

    png(file=paste0(dataset, '_', clustFuncStr[i], '_Cons.png'))
    plotChanges(rCons, x, dataset)
    dev.off()

    resultVectorsCons[[index]] <- as.vector(t(rCons))
    resultFrameCons[[index]] <- as.vector(rCons)

    i = i + 1
  }
}

resultVecrorsClass <- list()
resultFrameClass <- list()


  #allData <- DATA_FRAME[, -43] # except class attribute
  allData <- getData('stock') # except class attribute
  
  x <- 1:(max(allData$period)-1)
  i <- 1

  for(fun in clustFunc){
    index = paste0('class_',  clustFuncStr[i])

    rClass <- measureChangesClass(allData , clusterFUNC = fun)
    print('================================================================')
    print(fun)
    png(file=paste0(dataset, '_', clustFuncStr[i], '_Class.png'))
    plotChanges(rClass, x, 'class')
    dev.off()

    resultVecrorsClass[[index]] <- as.vector(t(rClass))
    resultFrameClass[[index]] <- as.vector(rClass)
    i = i + 1
  }


# test dataset p-values



