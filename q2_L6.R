## Questao 2 

euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}


K_means2 <- function(x, centers, distFun, nItter) {
  Ein<-0
  clusterHistory <- list()
  centerHistory <- list()
  uni=F
  i=1
  while(uni==F){
    distsToCenters <- distFun(x, centers)
    clusters <- apply(distsToCenters, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    # Saving history
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
    if(i > 1 && centerHistory[[i-1]]==centers){
      uni=T
      break
    }
    i=i+1
  }
  dados<-list()
  erro<-list()
  for(i in 1:nrow(centers)){
    dados[[i]]<-ktest[which(clusters==i),]
    erro[[i]]<-apply(dados[[i]],1,function(x)norm(rbind(x,centers[i,])))
  }
  erros<-lapply(X = erro,FUN = sum)
  list(clusters=clusterHistory, centers=centerHistory,erros=erros)
}

y=rnorm(500,1.65)
x=rnorm(500,1.15)

data=cbind(x,y)
#X=matrix(rchisq(256,2),ncol=16)
test=data # A data.frame
ktest=as.matrix(test) # Turn into a matrix
idx<-sample(nrow(ktest), 4)
centers <- ktest[idx,] # Sample some centers, 5 for example
ktest<-ktest[-idx,]

res <- K_means2(ktest, centers, euclid, 100)

# dados<-list()
# erro<-list()
# for(i in 1:nrow(centers)){
# dados[[i]]<-ktest[which(res$clusters[[length(res$clusters)]]==i),]
# erro[[i]]<-apply(dados[[i]],1,function(x)norm(rbind(x,res$centers[[1]][i,])))
# }
