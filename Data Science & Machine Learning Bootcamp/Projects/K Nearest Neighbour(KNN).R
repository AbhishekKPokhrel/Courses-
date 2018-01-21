library(ISLR)
str(Caravan)

##A model to predict purchase
summary(Caravan$Purchase)
any(is.na(Caravan))

#########
#####Standardising all x variables except the purchase(being the variable which we want to predict)
#########
#Variance check to see whether standardisation is needed
var(Caravan[, 1]) #Variance of 1st column
var(Caravan[, 2]) #Variance of 2nd column
#you can see the variance is very difference so standardisation is needed

###Saving the purchase variable as a seperate variable
purchase<- Caravan[, 86]

#####Standardising using scale function
standardised.caravan<- scale(Caravan[ , -86])
#Checking whether standardised or not:
var(standardised.caravan[, 1])
var(standardised.caravan[, 2])


########
##Train Test Split
########
test.index<- 1:1000 #Testing the first 1000 rows only
test.data<- standardised.caravan[test.index,]
test.purchase<- purchase[test.index]

#Train
train.data<- standardised.caravan[-test.index,]
train.purchase<- purchase[-test.index]



###########
####  KNN MODEL
###########
library(class) #Contains the KNN function
set.seed(101)

predicted.purchase<- knn(train.data, test.data, train.purchase, k=1)

misclass.error<- mean(test.purchase != predicted.purchase)
misclass.error


######
## CHOOSING A K VALUE
######

#Creating a FOR LOOP to predict the k-value-> to implement the elbow method

predicted.purchase<- NULL #Not using NA instead of NULL because this is going to be the vector of all the predicted purchase
error.rate<- NULL

for (i in 1:20){
  set.seed(101)
  predicted.purchase<- knn(train.data, test.data, train.purchase, k=i)
  error.rate[i]<- mean(test.purchase != predicted.purchase)
}

error.rate

#####
##  VISUALISE K ELBOW METHOD
#####

library(ggplot2)
k.values<- 1:20
error.df<- data.frame(error.rate, k.values)

ggplot(error.df, aes(k.values, error.rate)) + geom_point()+ geom_line(lty= 'dotted', color= 'red')
#From this plot, we can see that once k=9, increasing k does not improve the misclassification rate.
#Hence the elbow is at k=9