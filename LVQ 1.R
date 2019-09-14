x <- iris
set.seed(123)

setosa <- subset(x, Species == "setosa")
virginica <- subset(x, Species == "virginica")
versicolor <- subset(x, Species == "versicolor")

ind <- sample(2, nrow(setosa), replace = TRUE, prob = c(0.7, 0.3))
#xtrain_sample <- x[ind == 1,]
#xtest_sample <- x[ind == 2,]
xtrain <- rbind(setosa[ind == 1,], virginica[ind == 1,], versicolor[ind == 1,])
xtest <- rbind(setosa[ind == 2,], virginica[ind == 2,], versicolor[ind == 2,])

#xtrain <- xtrain_sample
#xtest <- xtest_sample

L <- 3 #number of representative vector
alpha <- 0.1 #learning rate
betha <- 0.4
epoch <- 0
epochtot <- 50
minalpha <- 10^-9

#inisialisasi bobot
#Sepal.Length <- runif(3)+3
#Sepal.Width <- runif(3)+3
#Petal.Length <- runif(3)+3
#Petal.Width <- runif(3)+3
#v <- data.frame(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width) #inisialisasi bobot random
#v <- xtrain[sample(nrow(xtrain), 3),]
vc1 <- setosa[sample(nrow(setosa), 1),]
vc2 <- virginica[sample(nrow(virginica), 1),]
vc3 <- versicolor[sample(nrow(versicolor), 1),]
v <- rbind(vc1,vc2,vc3) 
v <- v[,-c(5)] 

d <- c()
classtrain <- c()

#TRAINING
while ((epoch < epochtot) && (alpha > minalpha)) {
  for (i in 1:nrow(xtrain)){
    for (l in 1:L){
      d[l] <- dist(rbind(xtrain[i,-c(5)], v[l,]), method = "euclidean")
    }
    indMin <- which(d == min(d))
    if (indMin == 1){
      classtrain <- "setosa"
    } else if (indMin == 2){
      classtrain <- "virginica"
    } else {
      classtrain <- "versicolor"
    }
    if (classtrain == xtrain$Species[i]){
      v[indMin,] <- v[indMin,] + alpha*(xtrain[i,-c(5)]-v[indMin,])
    } else {
      v[indMin,] <- v[indMin,] - alpha*(xtrain[i,-c(5)]-v[indMin,])
    }
  }
  alpha <- betha*alpha
  epoch <- epoch + 1
}

#TESTING
class <- c()
for (i in 1:nrow(xtest)) {
  for (l in 1:3) {
    d[l] <- dist(rbind(xtest[i,-c(5)], v[l,]), method = "euclidean")
  } 
  indMin <- which(d == min(d))
  class[i] <- indMin
}

xtest$class <- class
for (i in 1:nrow(xtest)){
  if (xtest$class[i] == 1){
    xtest$class[i] = "setosa"
  } else if (xtest$class[i] == 2){
    xtest$class[i] = "virginica"
  } else {
    xtest$class[i] = "versicolor"
  }
}

confusion_matrix <- table(xtest$class, class)
confusion_matrix_class <- table(xtest$Species, class)
confusion_matrix
confusion_matrix_class

#perhitungan kesalahan klasifikasi
error <- 0
for (i in 1:nrow(xtest)){
  if (xtest$class[i] != xtest$Species[i]){
  error <- error + 1
  }
}
akurasi <- (nrow(xtest)-error)/nrow(xtest)*100
akurasi
