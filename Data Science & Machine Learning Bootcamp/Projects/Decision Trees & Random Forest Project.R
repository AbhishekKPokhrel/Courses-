## Model to predict whether the colleges are Public/Private

rm(list=ls())
library(ISLR)
df<- College

library(ggplot2)
ggplot(df, aes(Room.Board, Grad.Rate)) + geom_point(aes(color= df$Private))

#reorder fill variable
df$Private <- with(df, factor(Private, levels(Private)[2:1]))

## Creating Histograms
ggplot(df, aes(F.Undergrad)) +  geom_histogram(aes(fill=Private),color='black',bins=50)
ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50)
#As seen from the plot, one college has graduation rate above 100

#To see which one,
subset(df,Grad.Rate > 100)
#Setting its value to 100
df['Cazenovia College','Grad.Rate'] <- 100


######
## Test/Train Split
######
library(caTools)
set.seed(101)
sample<- sample.split(df$Private, SplitRatio = 0.7) #G3 used to know what is to be predicted i.e we want to predict G3; any column instead of G3 would work here
train<- subset(df, sample== TRUE) #70% data going to the training set
test<- subset(df, sample==FALSE) #30% data for testing

#####
## Using Decision Tree
####
library(rpart)

tree<- rpart(Private ~ ., method= 'class', data= train)
printcp(tree)

tree.pred<- predict(tree, test)


#Turning the two Yes/No column into one to match the original Yes/No label 
tree.pred<- as.data.frame(tree.pred)
joiner<- function(x){
  if (x>= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.pred$Private<- sapply(tree.pred$Yes, joiner)


### Creating a confusion matrix of the tree model:
table(tree.pred$Private, test$Private)


### Plotting the model:
library(rpart.plot)
prp(tree)




####################################
##########
### Random Forest Model
##########

library(randomForest)

rf.model<- randomForest(Private ~ ., data= train, importance= TRUE )

#Confusion matrix on its own training set
rf.model$confusion

#Grab the feature importance
rf.model$importance

p<- predict(rf.model, test)
table(p, test$Private)
