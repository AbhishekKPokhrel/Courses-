
library(ggplot2)

batting<- read.csv('Batting.csv')
head(batting)
str(batting)
head(batting$AB)
head(batting$X2B)

#Adding columns of Batting Avg(BA), On Base Percentage(OBP), Slugging Percentage(SP)
batting$BA<- batting$H/ batting$AB #Hits(H)/At Base(AB)
tail(batting$BA, 5)

batting$OBP<- (batting$H + batting$BB + batting$HBP)/ (batting$AB + batting$BB + batting$HBP + batting$SF)

#For Slugging Percentage(SLG); first calculate 1B; 1B= H-2B-3B-HR
batting$X1B<- batting$H - batting$X2B - batting$X3B - batting$HR
Total_Bases= batting$X1B + (2* batting$X2B) + (3* batting$X3B) + (4* batting$HR)
batting$SLG<- Total_Bases/ batting$AB
tail(batting$SLG, 5)

str(batting)


#Merging Salary Data with Batting Data:
sal<- read.csv('Salaries.csv')
summary(batting)
min(batting$yearID)
min(sal$yearID) 

#Removing the batting data that occured before 1985 to match the salary data
batting_85<- subset(batting, yearID >=1985)
summary(batting_85)

combo<- merge(batting_85, sal, by= c('playerID', 'yearID') )
head(combo)
summary(combo)

lost_players<- subset(combo,playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))
lost_players_01<- subset(lost_players, yearID == 2001)
lost_players_01<- lost_players_01[ ,c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'SLG', 'BA', 'AB')]
lost_players_01

#Finding Replacement Players
                              #Constraints:
                            #Salary 15million
mean(lost_players_01$OBP) # i.e mean OBP>= 0.363867
sum(lost_players_01$AB) #sum of At Bats(AB)>= 1469 or >=490 each player


combo_2001<- subset(combo, yearID==2001)
combo_2001<- combo_2001[ , c('playerID', 'H', 'X2B', 'X3B', 'HR', 'OBP', 'SLG', 'BA', 'AB', 'salary')]
head(combo_2001)

#To represent values as float as opposed to exponential
help(option) #find scipen; +ve value gives float values while -ve gives scientific(exp) value
options(scipen = 999) #options(scipen= -999) reverts back to exp

ggplot(combo_2001, aes(x= OBP, y= salary)) + geom_point(size=2)

players<- subset(combo_2001, salary< 8000000 & OBP >= 0.37)
players<- subset(players, AB>= 490 )

library(dplyr)
arr<- head(arrange(players, desc(OBP)), 10)
get<- arr[2:4, ]
get

sum(get$salary) # ~ 10million
mean(get$OBP) # ~0.43
sum(get$AB) # = 1773 or avg 591 eacg player
mean(get$AB)
