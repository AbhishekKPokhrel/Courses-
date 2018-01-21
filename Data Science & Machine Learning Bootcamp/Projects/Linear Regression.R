
df<- read.csv('student-mat.csv', sep = ';')
head(df)
summary(df)

any(is.na(df)) #To check for NA values

# Checking Categorical variables have a factor set to them
str(df) #CHeck that the categories you want are as factos

library(ggplot2)
library(dplyr)
library(ggthemes)
library(corrplot) #all values need to be numeric values to plot correlation
library(corrgram) #all values need not be numeric, so df can be passed directly for correlation
#Numeric columns only
num_cols<- sapply(df, is.numeric) #Returns only the numeric columns
#Filter Numeric columns for correlation
cor.data<- cor(df[ ,num_cols ]) #Comma used to get all the Numeric columns; gives the correlation matrix
cor.data

corr_plt<- corrplot(cor.data, method= 'color')
corr_gm<- corrgram(df)

corrgram(df, order= TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
ggplot(df, aes(x=G3)) + geom_histogram(bins=20, alpha= 0.5, fill= 'blue')

#Split data into training and test sets
library(caTools)
set.seed(101)
sample<- sample.split(df$G3, SplitRatio = 0.7) #G3 used to know what is to be predicted i.e we want to predict G3; any column instead of G3 would work here
train<- subset(df, sample== TRUE) #70% data going to the training set
test<- subset(df, sample==FALSE) #30% data for testing

# #The general model of building a Linear Regression in R 
# #Looks like this:
# model<- lm(y~ x1 + x2, data)
# 
# #or to use all the features in your data:
# model<- lm(y~ .,data )

#Train and Build Model
model<- lm(G3~ ., train)
summary(model)

#Plotting the residuals
res<- residuals(model)
class(res)
res<- as.data.frame(res) #Conversion to dataframe needed for ggplot
ggplot(res, aes(res)) + geom_histogram(fill= 'blue', alpha= 0.5)
head(res)

#Prediction
G3_prediction<- predict(model, test)

results<- cbind(G3_prediction, test$G3) #To check the actual vs predicted values
colnames(results)<- c('Predicted','Actual')
results<- as.data.frame(results)
head(results)

#Taking care of Negative values as the lowest a person can score is zero but the model goes to negative values
 to_zero<- function(x){
   if(x<0){
     return(0)
   }else{
     return(x)
   }
 }

#Apply to_zero funtion
results$Predicted<- sapply(results$Predicted, to_zero) #applied only to predicted column because the min of actual is zero
min(results)

#Mean Squared Error
mse<- mean((results$Predicted - results$Actual)^2)
mse

#Root MSE
rmse<- mse^0.5
rmse

#Calculating R^2
SSE<- sum((results$Predicted - results$Actual)^2)
SST<- sum((mean(df$G3) - results$Actual)^2)

R2<- 1- SSE/SST
R2







