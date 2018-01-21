

adult <- read.csv('adult_sal.csv')

library(dplyr)
adult <- select(adult,-X)

#####
###Data Cleaning
####

###Unemployed
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

#applying the function
adult$type_employer <- sapply(adult$type_employer,unemp)


###Govt job as Self empl
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}


adult$type_employer <- sapply(adult$type_employer,group_emp)  



###Married, Not Married & Never Married
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)


##
####FOr Country
##

#First check the names of country present by
levels(adult$country)

#Then create matrix of countries
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

#Group them using the following function
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)





######
####For Missing Values
######

library(Amelia)

#Transforming ? values to NA values
adult[adult== '?']<- NA

##Setting character to factor for the columns of data cleaning
adult$type_employer<- sapply(adult$type_employer, factor)
adult$marital<- sapply(adult$marital, factor)
adult$country<- sapply(adult$country, factor)
adult$occupation <- sapply(adult$occupation,factor)
##Creating Missing values plot with missmap
missmap(adult, y.at = c(1), y.labels = c(''), col= c('yellow', 'black'))


#Drop missing values
adult<- na.omit(adult)



######
###Visualization
######
library(ggplot2)
library(dplyr)
ggplot(adult, aes(age)) + geom_histogram(aes(fill= income), color= 'black', binwidth = 1) + theme_bw()

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
# +guides(fill = guide_legend(reverse=TRUE))

#reorder fill variable
adult$income <- with(adult, factor(income, levels(income)[2:1]))

#plot
library(ggplot2)
ggplot(adult, aes(age)) + geom_histogram(aes(fill= income), color= 'black', binwidth = 1) + theme_bw()
##Rename Country to Region
adult<- rename(adult, region= country)

ggplot(adult, aes(region)) + geom_bar(aes(fill= income), color= 'black') + theme_bw()




#################
###LOGISTIC REGRESSION MODEL ####
################

#Train Test Split
library(caTools)
set.seed(101)

sample<- sample.split(adult$income, SplitRatio = 0.7)
train<- subset(adult, sample== T)
test<- subset(adult, sample==F)

model<- glm(income ~. , family = binomial(link= 'logit'), data= train)

summary(model)

###Choosing variables in Stepwise Algorith
#new.step.model<- step(model) 


test$predicted.income<- predict(model, newdata = test, type= 'response')

table(test$income, test$predicted.income > 0.5)

####Metrics of performance
#accuracy
accuracy<- (6372+1423)/ (6372+1423+548+872)
accuracy

#Recall:
6372/(6372+548)

#Precision:
6372/(6372+872)
