mtcars
dim(mtcars)
head(mtcars)
str(mtcars)
View(mtcars)
help(lm)
model1 <- lm(mpg ~cyl+wt,data=mtcars)
model1
summary(model1)
plot(model1,pch=18,col="red",which=c(4))

cooks.distance(model1)
cooksDistance <- cooks.distance(model1)
cooksDistance
sort(round(cooksDistance,5))

library(ISLR)
library(dplyr)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData<-na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
head(HittersData)
SalaryPredictModel<-lm(Salary~.,data=HittersData)
summary(SalaryPredictModel)
cooksD<-cooks.distance(SalaryPredictModel)
influential<-cooksD[(cooksD>(3*mean(cooksD,na.rm=TRUE)))]
influential
name_of_influential<-names(influential)
name_of_influential
outlier<-HittersData[name_of_influential,]
Hitters_Without_outlier<-HittersData%>%anti_join(outlier)
SalaryPredictModel2<-lm(Salary~.,data=Hitters_Without_outlier)
summary(SalaryPredictModel2)

plot(SalaryPredictModel,pch=18,col="red",which=c(4))
plot(SalaryPredictModel2,pch=18,col="red",which=c(4))
