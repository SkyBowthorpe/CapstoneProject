#Skyler Bowthorpe Data Science Capstone 2 of 2
#Introduction
#can we predict sales using other attributes?

#load libraries
options(warn =-1)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(caret)
library(plyr)

#load data from csv
dat<-read.csv(file="edx/Capstone/Video_Games_Sales_as_at_22_Dec_2016.csv",header = FALSE,stringsAsFactors=FALSE)

#clean up data
#change data types to more usable classes
dat<-na.omit(dat)
dat$Platform <- as.factor(as.character(dat$Platform))
dat$Genre <- as.factor(as.character(dat$Genre))
dat$Publisher <- as.factor(as.character(dat$Publisher))
dat$Name <- as.factor(as.character(dat$Name))
str(dat)
#Ok I can see there is already a bit of data wrangling I will need to do before we get started with this dataset. 
#I am going to focus on global sales for this analysis and we will round it to the nearest million.
dat <- dat %>% mutate(Global_Sales=round(Global_Sales))
dat <- dat[!(names(dat) %in% c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Year","Rank"))]
attach(dat)
head(dat)
#Plan Going Forward
#What can we do to predict sales? Are are there specific attributes of games that sell more games?  Are there attributes that sell to specific niches e.g. popular in only japan
#first I will make some assumptions and explore their validity to start forming a hypothesis.  Then I can build a model that will predict sales based on attribues of games that are significant.

#there are many games that are missing ratings.
#First I want to know if the critic score is correlated with global sales.  
#The logic is that high critic scores will influence people to go buy a game therefore higher global sales.
cor(Global_Sales,Critic_Score)
#This is a lower correlation than I was expecting, Lets visualize the relationship between sales and critic scores.
ggplot()+
  geom_point(aes(Critic_Score,Global_Sales))+
  scale_y_continuous(trans = "log10")+# using log scale so that the few games with very high sales make the data hard to visualize
  xlab("Average Critic Score")+
  ylab("Global Sales in millions (log scale)")+
  ggtitle("The correlation between critic scores and global sales")

dat <- dat[!(names(dat) %in% c("Critic_Score", "Critic_Count", "User_Score", "User_Count", "Developer", "Year_of_Release"))]
#This is a good start let's explore other variables that may help predict global sales

#What are the most popular Genres?  Do games in those genres have high sales individually?
ggplot(dat, aes(Genre,Global_Sales)) +
  geom_bar(fill="red", stat="identity")+
  coord_flip()+
  xlab("Game Genre")+
  ylab("Global Sales")+
  ggtitle("Global Sales by Genre")

#The data shows that action games have the highest collective sales but also a very high number of action titles.
average_rev_by_genre <- aggregate(Global_Sales~Genre,dat,mean)
arrange_by_rev2 <- arrange(average_rev_by_genre,desc(Global_Sales))
arrange_by_rev2$Genre=factor(arrange_by_rev2$Genre,levels=arrange_by_rev2$Genre)
ggplot(arrange_by_rev2,aes(Genre,Global_Sales))+
  geom_bar(fill="blue", stat="identity")+
  coord_flip()+
  xlab("Game Genre")+
  ylab("Global Sales")+
  ggtitle("Average Game Sales by Game Genre")

#While the Adventure Genre had the most sales; Miscellaneous, Platforming, and Shooter games had the higher per-game performance.
#What are some of the games that are in the Miscellaneous category?

head(dat %>% filter(Genre == "Misc"), n = 15)
#It looks like Misc covers party, music and learning games

#graphs comparing platfoms
ggplot(dat, aes(Platform,Global_Sales,fill=Platform))+
  geom_bar(stat="identity")+
  xlab("Platform")+
  ylab("Global Sales")+
  ggtitle("Global Sales by Platform")

# For more clarity I will categorize the platforms into major companies
dat$Platform <- as.character(dat$Platform)
dat$Platform[dat$Platform %in% c("PS","PS2","PS3","PS4","PSP","PSV")] <- "Sony"
dat$Platform[dat$Platform %in% c("XB","XOne","X360")] <- "Microsoft"
dat$Platform[dat$Platform %in% c("Wii","NES","GB","DS","SNES","GBA","3DS","N64","GC","WiiU")] <- "Nintendo"
dat$Platform[!(dat$Platform  %in% c("Nintendo","Sony","Microsoft"))] <- "Other"
dat$Platform <- as.factor(dat$Platform)

#Much cleaner chart comparing platforms.
ggplot(dat, aes(Platform,Global_Sales,fill=Platform))+
  geom_bar(stat="identity")+
  xlab("Platform")+
  ylab("Global Sales")+
  ggtitle("Global Sales by Major Plaform Companies")

#There are too many publishers to plot all of them. I will show just the top ten.
top_publishers <- aggregate(Global_Sales~Publisher,dat,mean)
arrange_by_rev3 <- arrange(top_publishers,desc(Global_Sales))
arrange_by_rev3$Publisher = factor(arrange_by_rev3$Publisher, levels = arrange_by_rev3$Publisher)
ggplot(head(arrange_by_rev3,10),aes(Publisher,Global_Sales,fill=Publisher))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="Platform",y="Average Game sales in Millions")+#Flipping the axis to it is more readable
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none")+ 
  ggtitle("Top Game Publishers")
  
#Publishers vs highest sales games.
top_games_by_publisher = dat %>% select(Name,Global_Sales,Publisher) %>% arrange(desc(Global_Sales)) 
ggplot(head(top_games_by_publisher,15),aes(Name,Global_Sales,fill=Publisher))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="Platform",y="Average Game sales in Millions")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5),legend.position="none")+ 
  ggtitle("Highest Selling Games")+
  labs(x="Game",y="Sales")

#What effect the parent safety ratings have on sales?
ggplot(subset(dat,Rating %in% c("E","E10+","T","M")),
  aes(Rating,Global_Sales,fill=Rating))+
  geom_bar(stat="identity")+
  theme(legend.position="none")+
  xlab("ESRB Rating")+
  ylab("Global Sales")+
  ggtitle("Global Sales by Rating (color by publisher)")

#Create training and test partitions 
index <- createDataPartition(y=dat$Global_Sales, p=0.8, list=FALSE) 
train <- dat[index,]
test <- dat[-index,]

# Create a na?ve set for comparison
RMSE <- function(true_sales, predicted_sales){
  sqrt(mean((true_sales - predicted_sales)^2))}

mu_hat <- mean(train$Global_Sales)
naive_rmse <- RMSE(test$Global_Sales, mu_hat)
predictions <- rep(2.5, nrow(test))
rmse_results <- data_frame(method="Just the average",RMSE=naive_rmse)

#Rating effect
mu <- mean(train$Global_Sales)

genre_avgs <- train %>%
  group_by(Genre) %>%
  mutate(genre_effect = mean(Global_Sales - mu))

predicted_sales <- mu + test %>%
  left_join(genre_avgs, by="Genre") %>%
  .$genre_effect
model_1_rmse <- RMSE(predicted_sales, test$Global_Sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="ESRB Rating Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()
#Wow ok that does not work at all :(

#Add platform effect to the model
#platform_avgs <- train %>%
#group_by(Platform) %>%
#mutate(platform_effect = mean(Global_Sales - mu))

#predicted_sales <- test %>%
  #full_join(genre_avgs,test, by = "Genre") %>%
  #full_join(platform_avgs,test, by = "Genre") %>%
  #mutate(pred = mu + genre_effect + platform_effect) %>%
  #.$pred
#model_2_rmse <- RMSE(predicted_sales, test$Global_Sales)
#rmse_results <- bind_rows(rmse_results,
                        #data_frame(method="Rating + Platform Effects Model",
                                     #RMSE = model_2_rmse ))
#rmse_results %>% knitr::kable()
#This is how I would have continued to test and tune the model but I received this error: Error: Evaluation error: cannot allocate vector of size 3.4 Gb. and it crashed my computer.

