## model to predict the Flower Species

library(ISLR)
df<- iris


######
## Test|Train Split
######
library(caTools)
set.seed(101)
sample<- sample.split(df, SplitRatio = 0.7)
df.train<- subset(df, sample==T)
df.test<- subset(df, sample==F)

######
## Using Support Vector Machine model:
######
library(e1071)
model<- svm(Species~., data= df.train)
summary(model)

pred.values<- predict(model, df.test[-5])

cm<- table(df.test[, 5], pred.values)
cm

classifier<- svm(formula= Species~.,
                 data= df.train, type= 'C-classification',
                 kernel= 'linear')

y_pred<- predict(classifier, newdata = df.test[-5])

cm<- table(df.test[, 5], y_pred)
cm


