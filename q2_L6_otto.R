
funcao_distancia <- function(vetor1,vetor2){
  matriz_distancia <- matrix(NA,nrow = dim(vetor1)[1],ncol = dim(vetor2)[1])
  for(i in 1:nrow(vetor2)) {
    matriz_distancia[,i] <- sqrt(rowSums(t(t(vetor1)-vetor2[i,])^2))
  }
  matriz_distancia
}


lloyd_media <- function(dados, centers){
  evolucao_clusters <- list()
  evolucao_centers <- list()
  Ein <- list()
  centers_velho <- centers
  parar <- FALSE
  while(parar == FALSE){
    distancia_centros <- funcao_distancia(dados,centers)
    clusters <- apply(distancia_centros,1, which.min)
    centers <- apply(dados,2,tapply,clusters,mean)
    #tent_ein <- apply(centers,1,function(x) tapply(X = x,index = clusters,function(y) norm())
    #Ein[[length(Ein) + 1]] <- apply(dados,2,function(y) tapply(y,clusters,function(x) tapply(x,clusters,funcao_distancia(x,y))))
    #Ein[[length(Ein) + 1]] <- lapply(apply(distancia_centros,2,sum),sum)
    evolucao_clusters[[length(evolucao_clusters) + 1]] <- clusters
    evolucao_centers[[length(evolucao_centers) + 1]] <- centers
    if(all(centers %in% centers_velho) == TRUE){
      parar <- TRUE
    }
    centers_velho <- centers
    #Ein[[length(Ein) + 1]] <- norm(linha_cluster,evolucao_centers[[length(evolucao_centers)]][which(linha_cluster)])
  }
  dados_filtro <- list()
  erros <- list()
  for(i in 1:nrow(centers)){
    dados_filtro[[i]] <- dados[which(clusters==i),]
    erros[[i]] <- apply(dados_filtro[[i]],1,function(x)norm(rbind(x,centers[i,])))
  }
  Ein <- sum(unlist(lapply(X = erros,FUN = sum)))
  list(clusters = evolucao_clusters,centers = evolucao_centers, Ein = Ein)
}

#(b) Load the Nixon-Presley Meeting image or any other of your preference and then reshape it to create an m × 3 matrix of RBG pixel colors (where m = width × height).

#Vamos ler a imagem do musica de jazz Miles Davis e posteriormente separar os pigmentos de cor, para construção do array pedido no enunciado
miles <- readJPEG("miles_davis.jpeg", native = F)
miles_red <- as.vector(miles[,,1])
miles_green <- as.vector(miles[,,2])
miles_blue <- as.vector(miles[,,3])
miles_rgb <- cbind(miles_red,miles_green,miles_blue)

#(c) Call your K-means function on the data with K from 1 up to 16. Plot the Ein ×K.

#Aqui rodamos para os diferentes valores de k a função responsável por calcular o k-means.

miles_k_means <- list()
miles_Ein <- c()
for(k in 2:16){
saida <- lloyd_media(miles_rgb,miles_rgb[sample(x = nrow(miles_rgb),size = k),])
miles_Ein[length(miles_Ein) + 1]<- saida$Ein
miles_k_means[[length(miles_k_means) + 1]] <- saida
}

plot(x = 2:16, y = miles_Ein, xlab = "K", ylab = "Ein")








-----------------------------------------------------------

lloyd <- function(dados, clusters, limite){
  #greedy approach
  lin <- sample(x = 1:dim(dados)[1],size = clusters)
  centros <- t(dados[lin,])
  dados_sem_centros <- dados[-lin,]
  #creating clusters
  distancia <- apply(centros,2,function(y){apply(dados_sem_centros,1,function(x) dist(rbind(x,y)))})
  return(c(apply(distancia,1,min),which(dados ==apply(distancia,1,min))))
}


matriz_exemplo <- matrix(rnorm(16),ncol = 4)
