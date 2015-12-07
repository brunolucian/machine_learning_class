#Problem 6.14 Run the condensed nearest neighbor (CNN) algorithm for 3-NN on the digits data. Use all the data for classifying “1” versus “not 1”.

library(FNN)
library(unbalanced)

#(a) Set N = 500 and randomly split your data into a training set of size N and use the remaining data as a test/validation set.

treino_cnn <- read.csv("DIGIT/data/train.csv")
treino_cnn$label_novo <- as.numeric(treino_cnn$label == 1)
treino_cnn2 <- treino_cnn[500:1000,]
teste_cnn2 <- treino_cnn[-(500:1000),]



#(b) Use the 3-NN algorithm with all the training data and evaluate its per- formance: report Ein and Etest.

knn_ex4 <- (0:1)[knn(train = treino_cnn2[,-c(1,dim(treino_cnn2)[2])], test = teste_cnn2[,-c(1,dim(treino_cnn2)[2])],cl = treino_cnn2$label_novo, k = 3)]
E_teste <- sum(knn_ex4 != teste_cnn2$label_novo)/length(teste_cnn2$label_novo)
knn_cv_treino <- (0:1)[knn.cv(train = treino_cnn2[,-c(1,dim(treino_cnn2)[2])],cl = treino_cnn2$label_novo, k = 3)]
E_in <- sum(knn_cv_treino != treino_cnn2$label_novo)/length(treino_cnn2$label_novo)

#(c) Use the CNN algorithm to condense the data. Evaluate the performance of the 3-NN rule with the condensed data: report Ein and Eout.

cnn_ex4 <- ubCNN(X = treino_cnn2[,-c(1,dim(treino_cnn2)[2])],Y = treino_cnn2$label_novo, k = 3)
knn_cnn_ex4 <- (0:1)[knn(train = cnn_ex4$X, test = teste_cnn2[,-c(1,dim(treino_cnn2)[2])], cl = cnn_ex4$Y, k = 3)]
E_cnn_teste <- sum(knn_cnn_ex4 != teste_cnn2$label_novo)/length(teste_cnn2$label_novo)
knn_cnn_cv_treino <- (0:1)[knn.cv(train = cnn_ex4$X,cl = cnn_ex4$Y, k = 3)]
E_cnn_in <- sum(knn_cnn_cv_treino != cnn_ex4$Y)/length(cnn_ex4$Y)

------------------------------------------------------------------------------------------------------------------------


#(d) Repeat parts (b) and (c) using 1,000 random training-test splits and report the average Ein and Eout for the full versus the condensed data.


E_teste_parteb <- c()
E_in_parteb <- c()
E_teste_partec <- c()
E_in_partec <- c()
for(i in 1:1000)
{

treino_cnn2 <- treino_cnn[sample(1:(dim(treino_cnn)[1]),size = 500),]
teste_cnn2 <- treino_cnn[sample(1:(dim(treino_cnn)[1]),size = (dim(treino_cnn)[1]) - 500),]

#Parte (b)

knn_ex4 <- (0:1)[knn(train = treino_cnn2[,-c(1,dim(treino_cnn2)[2])], test = teste_cnn2[,-c(1,dim(treino_cnn2)[2])],cl = treino_cnn2$label_novo, k = 3)]
E_teste <- sum(knn_ex4 != teste_cnn2$label_novo)/length(teste_cnn2$label_novo)
knn_cv_treino <- (0:1)[knn.cv(train = treino_cnn2[,-c(1,dim(treino_cnn2)[2])],cl = treino_cnn2$label_novo, k = 3)]
E_in <- sum(knn_cv_treino != treino_cnn2$label_novo)/length(treino_cnn2$label_novo)

E_teste_parteb[1 + length(E_teste_parteb)] <- E_teste
E_in_parteb[1 + length(E_in_parteb)] <- E_in
#Parte (c)

cnn_ex4 <- ubCNN(X = treino_cnn2[,-c(1,dim(treino_cnn2)[2])],Y = treino_cnn2$label_novo, k = 3)
knn_cnn_ex4 <- (0:1)[knn(train = cnn_ex4$X, test = teste_cnn2[,-c(1,dim(treino_cnn2)[2])], cl = cnn_ex4$Y, k = 3)]
E_cnn_teste <- sum(knn_cnn_ex4 != teste_cnn2$label_novo)/length(teste_cnn2$label_novo)
knn_cnn_cv_treino <- (0:1)[knn.cv(train = cnn_ex4$X,cl = cnn_ex4$Y, k = 3)]
E_cnn_in <- sum(knn_cnn_cv_treino != cnn_ex4$Y)/length(cnn_ex4$Y)

E_teste_partec[1 + length(E_teste_partec)] <- E_cnn_teste
E_in_partec[1 + length(E_in_partec)] <- E_cnn_in
}

mean(E_teste_parteb)
mean(E_in_parteb)
mean(E_teste_partec)
mean(E_in_partec)
