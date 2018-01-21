
df.train<- read.csv('titanic_train.csv')
head(df.train)


library(Amelia) #To work with missing values
help("missmap")
#Visualising where missing data are present
missmap(df.train, main='Missing Map', col= c('yellow', 'black'), legend= FALSE) #col argument is the vector of 2colors, first(yellow) being that of missing cells & second(black) being that of observed cells


#Data Visualisations
library(ggplot2)
ggplot(df.train, aes(Survived)) + geom_bar() #TO see histogram of survived vs died
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill= factor(Pclass))) #To see count of people of each classes
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill= factor(Sex))) #To see the count of total males & females

#Histogram for ages on board, people with siblings, amount of fare 
ggplot(df.train, aes(Age)) + geom_histogram(bins= 20, alpha= 0.5, fill= 'blue')
ggplot(df.train, aes(SibSp)) + geom_histogram()
ggplot(df.train, aes(Fare)) + geom_histogram(fill= 'green', color='black', alpha= 0.5)


#FOr missing ages
#Trying to fill the missing data by avg of ages in each class as opposed to avg age
pl<- ggplot(df.train, aes(Pclass, Age))
pl<- pl+ geom_boxplot(aes(group= Pclass, fill= factor(Pclass), alpha= 0.4))
pl + scale_y_continuous(breaks= seq(min(0), max(80), by= 2)) + theme_bw()

#Imputation of age based on class
impute_age<- function(age, class){
  out<- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i]==1){
        out[i]<- 37
      }else if (class[i]==2){
        out[i]<- 29
      }else {
        out[i]<- 24
      }
    }else{
      out[i]<- age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)

df.train$Age<- fixed.ages

missmap(df.train,main= 'Imputation Check', col= c('yellow', 'black'), legend= FALSE)


library(dplyr)
df.train<- select(df.train, -PassengerId, -Name, -Ticket, -Cabin)
head(df.train,3)
df.train$Survived<- factor(df.train$Survived)
df.train$Pclass<- factor(df.train$Pclass)
df.train$SibSp<- factor(df.train$SibSp)
df.train$Parch<- factor(df.train$Parch)
str(df.train)

#MOdel building: Using Generalised Linear Models to perform Logistic Regression
log.model<- glm(Survived~. , family = binomial(link= 'logit'), data= df.train)
summary(log.model)


#Splitting this df.train into training & test sets
library(caTools)
set.seed(101)
split<- sample.split(df.train$Survived, SplitRatio = 0.7 )
final.train<- subset(df.train, split==TRUE)
final.test<- subset(df.train, split==FALSE)

final.log.model<- glm(Survived~., family = binomial(link= 'logit'), data= final.train)
fitted.probabilities<- predict(final.log.model, final.test, type = 'response') #response type because we're trying to predict 0 or 1
fitted.results<- ifelse(fitted.probabilities>0.5, 1, 0) #if prob>0.5 make the value=1 else vlaue=0

#Check for accuracy
misClassError<- mean(fitted.results != final.test$Survived)
accuracy<- 1- misClassError
accuracy

#Creating Confusion Matrix
table(final.test$Survived, fitted.results)

#Test.csv data
df.test<- read.csv('titanic_test.csv')
head(df.test)

missmap(df.test, main='Imputation Check', col= c('yellow', 'black'), legend = FALSE)

plt<- ggplot(df.test, aes(Pclass, Age)) + geom_boxplot(aes(group= Pclass, fill= factor(Pclass), alpha= 0.4))
plt + scale_y_continuous(breaks = seq(min(0), max(80), by=2)) + theme_bw()

