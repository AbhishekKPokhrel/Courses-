rm(list=ls())
library(ggplot2)
library(ggthemes)

bike<- read.csv('bikeshare.csv')
head(bike)

countVtemp<- ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()
countVtemp

bike$datetime<- as.POSIXct(bike$datetime, format= "%Y-%m-%d %H:%M:%S") #Converting datetime column from factor to POSIXct
class(bike$datetime)

countVdatetime<- ggplot(bike, aes(datetime, count)) + geom_point(alpha= 0.5, aes(color= temp)) + theme_bw() + scale_color_continuous(low='#55D8CE',high='#FF6E2E')
countVdatetime

cor(bike$temp, bike$count) #To get correlation value
cor(bike[ , c('temp', 'count')]) #To get correlation matrix

#Boxplot
ggplot(bike, aes(x= factor(season), y=count)) + geom_boxplot(aes(color= factor(season))) + theme_bw()


#Feature Engineering: Creating Hour column that takes the hour from the datetime column
bike$hour<- sapply(bike$datetime, function(x){
  format(x, "%H")
})

#Scatterplot of CountVHour, only using bike date where working day=1
bike_1<- subset(bike, workingday==1)
ggplot(bike_1, aes(x= hour, y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)  + theme_bw()+ scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))

#Scatterplot for nonworking days i.e working day=0
bike_0<- subset(bike, workingday==0)
ggplot(bike_0, aes(x= hour, y=count)) + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)  + theme_bw()+ scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))

#Model Building
temp.model<- lm(count~ temp, data= bike)
summary(temp.model)
#predicting count with temp=25
temp.test<- data.frame(temp=c(25))
predict(temp.model, temp.test)

#Converting hour from character to numeric
bike$hour<- sapply(bike$hour, as.numeric)
str(bike)

model<- lm(count~ . -casual -registered -datetime -atemp, bike)
summary(model)


#Subsetting date: Using date to split data into train & test sets
bike$date<- sapply(bike$datetime, function(x){
  format(x, "%Y-%m-%d")
})
head(bike$date)
train<- subset(bike, date< "2012-07-01")
test<- subset(bike,date>="2012-07-01")


