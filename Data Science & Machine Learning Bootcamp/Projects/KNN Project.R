library(ISLR)
str(iris)

head(iris)

species<- iris[, 5]

iris.standardised<- scale(iris[, -5])

#Joining the standardised data with response/target column
final.data<- cbind(iris.standardised, species)  
head(final.data)


########
## Train | Test Split
########
set.seed(101)
library(caTools)
sample<- sample.split(final.data$species, SplitRatio = 0.7)
df.train<- subset(final.data, sample == T)
df.test<- subset(final.data, sample == F)


#########
## USING KNN MODEL
#########
library(class)
predicted.species<- knn(df.train[1:4], df.test[1:4], df.train$species, k=1)
error<- mean(df.test$species != predicted.species)
error


#########
## CHOOSING K VALUE
#########
predicted.species<- NULL
error<- NULL

for (i in 1:10){
  set.seed(101)
  predicted.species<- knn(df.train[1:4], df.test[1:4], df.train$species, k=i)
  error[i]<- mean(df.test$species != predicted.species)
}

error

### VISUALISING K VALUES
k_values<- 1:10
df.error<- data.frame(k_values, error)

ggplot(df.error, aes(k_values, error)) + geom_point() + geom_line(lty= 'dotted', color= 'red')
