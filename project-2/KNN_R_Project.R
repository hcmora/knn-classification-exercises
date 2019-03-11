setwd('R Studio/KNN Project')

project = read.csv('KNN_Project_Data',header = TRUE)
head(project,1)

# Still haven't worked out how to make the pairs plot differentiate on the target class, for some reason
# it only colors the first argument
# pairs(project, main = "Project Database", pch=21,bg = c("green3","blue")[unclass(project$TARGET.CLASS)])

# Workaround for colors
project.plot = project
project.plot[11] = project[11]+1
pairs(project.plot, main = "Project Database", pch=21,bg = c("blue","orange")[unclass(project.plot$TARGET.CLASS)])

# We proceed to scale the predictors
project2 = project[-11]
project2 = scale(project2)

set.seed(5)
train = sample(1:1000,700,replace = FALSE)

train.X = cbind(project2)[train,]
test.X = cbind(project2)[-train,]
train.TC = project$TARGET.CLASS[train]
test.TC = project$TARGET.CLASS[-train]

# KNN with k = 1
library(class)
knn.pred = knn(train.X,test.X,train.TC,k=1)
table(test.TC, knn.pred)
mean(test.TC == knn.pred)

# Now we iterate to determine the best k value
error_rate = rep(0,40)
k_range = 1:40
for (i in k_range) {
  knn.pred = knn(train.X,test.X,train.TC,k=i)
  error_rate[i] = mean(test.TC != knn.pred)
}
plot(error_rate~k_range, pch=21,bg = c("red"), xlab="K", ylab="Error Rate",main="Error Rate vs K")
lines(k_range,error_rate,lty='dashed', col="blue")
grid (NULL,NULL, lty = 6, col = "gray")

# We check that, in this case, for k = 13 we get the best results
# Special note: the results differ from the Python version, because of the randomness when selecting
# the data to train the KNN model!

knn.pred = knn(train.X,test.X,train.TC,k=13)
table(test.TC, knn.pred)
mean(test.TC == knn.pred)
