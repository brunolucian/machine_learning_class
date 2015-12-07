#http://machinelearningmastery.com/linear-classification-in-r/
#http://openclassroom.stanford.edu/MainFolder/DocumentPage.php?course=MachineLearning&doc=exercises/ex9/ex9.html
require(FNN)
data(iris)

set.seed(321)
## Split in train + test set
idxs <- sample(1:nrow(iris),as.integer(0.8*nrow(iris)))

trainIris <- iris[idxs,]
y_train <- trainIris$Species
trainIris <- trainIris[,-5]
testIris <- iris[-idxs,]
y_test <- testIris$Species
testIris <- testIris[,-5]

## A k-nn escolhendo o melhor k
nn1 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 1)))
nn3 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 3)))
nn5 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 5)))
nn7 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 7)))
nn9 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 9)))
nn11 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 11)))
nn13 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 13)))
nn15 <- as.vector(as.array(knn.cv(train = trainIris,cl = y_train,k = 15)))

e1<-sum((nn1==y_train)==FALSE)/length(y_train)
e3<-sum((nn3==y_train)==FALSE)/length(y_train)
e5<-sum((nn5==y_train)==FALSE)/length(y_train)
e7<-sum((nn7==y_train)==FALSE)/length(y_train)
e9<-sum((nn9==y_train)==FALSE)/length(y_train)
e11<-sum((nn11==y_train)==FALSE)/length(y_train)
e13<-sum((nn13==y_train)==FALSE)/length(y_train)
e15<-sum((nn15==y_train)==FALSE)/length(y_train)


erros<-c(e1,e3,e5,e7,e9,e11,e13,e15)

plot(seq(1,15,by = 2),erros,type = "b",pch=16,xlab = "k vizinhos",ylab = "Erro de previsão")

## The resulting confusion matrix

knn7<-as.vector(as.array(knn(train = trainIris,test = testIris,cl = y_train,k = 7)))

table(y_test,knn7)


# load the package
library(VGAM)
# fit model
fit <- vglm(Species~., family = multinomial, data=iris)
# summarize the fit
summary(fit)
# make predictions
probabilities <- predict(fit, iris[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
# summarize accuracy
table(predictions, iris$Species)


# clustering
require(stats)
kmedia<-kmeans(x = iris[,-5],centers = 3,iter.max = 100)
clusters<-kmedia$cluster

table(clusters,iris[,5])
