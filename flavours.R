
#STAT 515 Final Project
#Swathi Pollishetty
#loading required libraries
library(tidyverse)
library(useful)
library(dplyr)
library(cluster)
library(randomForest)
library(GGally)
library(corrplot)
library(DMwR2)
library(factoextra)
#read csv file
flavours <- read.csv("C:\\Users\\sravan\\Documents\\STAT515\\Final\\flavors_of_cacao.csv")
names(flavours)
#examining the data
str(flavours)
dim(flavours)
#remove spaces and line brakes from column names
names(flavours) <- tolower(gsub(pattern = '[[:space:]+]', '_', names(flavours)))
names(flavours)

#renaming columns for easier legibility
flavours<-dplyr::rename(flavours,Maker=company ,REF=ref,Rating=rating,BeanOrigin=specific.bean.origin.or.bar.name,ReviewDate=review.date,CompanyLocation=company.location,BroadBeanOrigin=broad.bean.origin,BeanType=bean.type,CocoaPercentage=cocoa.percent)
  
#displaying column names
names(flavours)

#checking for null values for each column
lapply(flavours,function(x) { length(which(is.na(x)))})

#Dropping Bean Type since it has a lot of missing values
#We do not use Broad Bean Origin,Bean Type in our Analysis
flavours <- subset(flavours,select=c(-BeanType,-BroadBeanOrigin))

#checking for null values again
lapply(flavours,function(x) { length(which(is.na(x)))})

#checking size of flavours
dim(flavours)

#removing null and missing values
#flavours<- na.omit(flavours)


#remove % from CocoaPercentage and change it to numeric type
flavours$CocoaPercentage <- lapply(flavours$CocoaPercentage, function(x) gsub("%", "", x))
flavours$CocoaPercentage <- as.numeric(flavours$CocoaPercentage)

#checking summary of the data
summary(flavours)

#checking new column names
colnames(flavours)
#factor
flavours$Maker <- as.factor(flavours$Maker)
#Adding Rating level variable
RatingLevel=ifelse(flavours$Rating<=3.5,"Low","High")
flavours <- data.frame(flavours,RatingLevel)
#write.csv(flavours,"C:\\Users\\sravan\\Documents\\STAT515\\Final\\finalflavours.csv",row.names=FALSE)


# get the mean chocolate rating by year 
meanRatingByYear <- flavours %>%
  group_by(ReviewDate) %>%
  summarise(Rating = mean(Rating))
print(meanRatingByYear)

# line chart showing change in reviews over years
ggplot(data = meanRatingByYear, aes(x = ReviewDate, y = Rating)) +
  geom_line() +
  scale_x_continuous(breaks = c(2006:2017)) + 
  labs(title = "Mean Rating of Chocolate Bars over Years",
       x = "Review Year",
       y = "Mean Rating")

#Histogram showing Count of Ratings
ggplot(flavours, aes(x=Rating, fill=Rating,color=Blue)) +
  geom_bar(color="black") +
  scale_x_continuous(breaks=seq(0,5,0.5))+
  labs (x = "Rating",
        y = "Count",
        title = "Count of Ratings") 
#-------------------------------------------------------------

#fitting linear regression model for Rating given ReviewDate
model1 <- lm(Rating~ReviewDate,data=flavours)
sm1<-summary(model1)
sm1
#printing RMSE for model 1
mean(sm1$residuals^2)
#plotting residual plots for model 1
par(mfrow=c(2,2))
plot(model1)

#Residuals vs Fitted values plot for predicting Rating given ReviewDate
ggplot(data=model1,
       aes(x=.fitted, y=.resid)) +
  geom_point( ) +
  geom_hline(yintercept=0) +
  geom_smooth(se=TRUE, method="loess",
              method.args=list(degree=1, family="symmetric")) +
  labs(x="fitted Values", y="Residuals",title="Residuals vs Fitted plot for predicting Rating given ReviewDate")


#-------------------------------------------------

#Linear Regression Model for predicting Rating given CocoaPercentage
model2 <- lm(Rating~CocoaPercentage,data=flavours)
sm2<-summary(model2)
sm2
#plotting Residual plots for model 2
par(mfrow=c(2,2))
plot(model2)
#printing RMSE for model 2
mean(sm2$residuals^2)
#Residuals vs Fitted values plot for predicting Rating given CocoaPercentage
ggplot(data=model2,
       aes(x=.fitted, y=.resid)) +
  geom_point( ) +
  geom_hline(yintercept=0) +
  geom_smooth(se=TRUE, method="loess",
              method.args=list(degree=1, family="symmetric")) +
  labs(x="fitted Values", y="Residuals",title="Residuals vs Fitted plot for predicting Rating given CocoaPercentage")
#fitting using polynomial of CocoaPerecentage
model3 <- lm(Rating~CocoaPercentage+I(CocoaPercentage^2),data=flavours)
sm3 <- summary(model3)
sm3
#plotting residual plots for Model 3
par(mfrow=c(2,2))
plot(model3)
#printing RMSE for Model 3
mean(sm3$residuals^2)

ggplot(data=model3,
       aes(x=.fitted, y=.resid)) +
  geom_point( ) +
  geom_hline(yintercept=0) +
  geom_smooth(se=TRUE, method="loess",
              method.args=list(degree=1, family="symmetric")) +
  labs(x="fitted Values", y="Residuals",title="Residuals vs Fitted plot for predicting Rating with polynomial of CocoaPercentage")

#Plot for CocoaPercentage vs Rating
ggplot(aes(x=CocoaPercentage, y=Rating), data=flavours) +
  geom_point() +
  geom_smooth(se=TRUE, method="loess",
              method.args=list(degree=1, family="symmetric")) +
  labs( x= "CocoaPercentage", y="Rating")
#-------------------------------------------------
#perform k-means 
flavours_kmeans <- dplyr::select(flavours,Rating,CocoaPercentage,ReviewDate,REF)


#Optimal number of clusters using silhouette method
fviz_nbclust(flavours_kmeans, kmeans, method = "silhouette")


#perform k-means
CompanyK2N5<- kmeans(x=flavours_kmeans, centers=2, nstart=30)
#centers
CompanyK2N5$centers
#plotting k means plot
plot.kmeans(CompanyK2N5, data=flavours_kmeans)
fviz_cluster(CompanyK2N5,data=flavours_kmeans)

#Random Forests
set.seed(599)

flavours_rf <- select(flavours,c(BeanOrigin,Rating,CocoaPercentage,REF,ReviewDate,CompanyLocation))
head(flavours_rf)
train = sample(1:nrow(flavours_rf), nrow(flavours_rf)/2)
head(train)
set.seed(111)
bag.flavours=randomForest(Rating~., data=flavours_rf, subset=train,mtry=2,
                          importance=TRUE,ntrees=500)

bag.flavours
# plot OOB-estimated MSE vs # trees
plot(bag.flavours, main="Bagged trees", mtry=2)

# Compute test MSE for  bagged trees
flavours_rf.test=flavours[-train,"Rating"]
yhat.bag = predict(bag.flavours,newdata=flavours_rf[-train,],na.action = na.pass)
#RMSE for Random Forest model
mean((yhat.bag-flavours_rf.test)^2)
#plotting test set rating vs predicted rating
ggplot(data.frame(yhat.bag, flavours_rf.test),
       aes(x=yhat.bag ,y=flavours_rf.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="predicted Rating",
       y="test-set Rating",
       title="Plot for test set rating vs predicted rating")
#finding important variables using importance function
importance(bag.flavours)
varImpPlot(bag.flavours,main = 'Variable Importance Plot for Predicting Rating')

#printing Confusion matrix
table(flavours_rf.test,yhat.bag)





