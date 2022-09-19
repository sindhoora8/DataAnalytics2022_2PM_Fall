EPI_data<-read.csv("Downloads/EPI/2010EPI_data.csv")
View(EPI_data)
names(EPI_data)
names(EPI_data)<-EPI_data[1,]
EPI_data<-EPI_data[-1,]
names(EPI_data)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
View(tf)
E <- !is.na(EPI)
numeric(EPI)
summary(EPI)
class(EPI)
EPI<-type.convert(EPI,"numeric")
class(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,plot=TRUE)
hist(EPI, seq(30., 95., 1.0), plot=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))  
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); 
qqline(EPI)
qqplot(qt(ppoints(250), df = 5),EPI, xlab = "Q-Q plot for t
dsn")
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)
class(DALY)
DALY<-type.convert(DALY,"numeric")
boxplot(EPI,DALY)
qqplot(EPI,DALY)
EPILand<-EPI[!Landlock]
class(Landlock)
Landlock<-type.convert(Landlock,"numeric")
Eland <- EPILand[!is.na(EPILand)]
class(Eland)
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPI_South_Asia <- EPI[EPI_regions]
class(EPI_South_Asia)
Eregion <- EPI_South_Asia[!is.na(EPI_South_Asia)]
EPI_data<-read.csv("Downloads/EPI/2010EPI_data.csv")
View(EPI_data)
names(EPI_data)
names(EPI_data)<-EPI_data[1,]
EPI_data<-EPI_data[-1,]
names(EPI_data)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
View(tf)
E <- !is.na(EPI)
numeric(EPI)
summary(EPI)
class(EPI)
EPI<-type.convert(EPI,"numeric")
class(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,plot=TRUE)
hist(EPI, seq(30., 95., 1.0), plot=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))  
rug(EPI)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); 
qqline(EPI)
qqplot(qt(ppoints(250), df = 5),EPI, xlab = "Q-Q plot for t
dsn")
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)
class(DALY)
DALY<-type.convert(DALY,"numeric")
boxplot(EPI,DALY)
qqplot(EPI,DALY)
EPILand<-EPI[!Landlock]
class(Landlock)
Landlock<-type.convert(Landlock,"numeric")
Eland <- EPILand[!is.na(EPILand)]
class(Eland)
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPI_South_Asia <- EPI[EPI_regions]
class(EPI_South_Asia)
Eregion <- EPI_South_Asia[!is.na(EPI_South_Asia)]
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
help("qqnorm")
par(pty="s")
qqnorm(EPI); 
qqline(EPI)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)
plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI)
qqline(EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)
plot(ecdf(EPI_data$DALY),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
class(DALY)
qqnorm(DALY)
qqline(DALY)
plot(ecdf(EPI_data$WATER_H),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$WATER_H),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
class(WATER_H)
WATER_H<-type.convert(WATER_H,"numeric")
qqnorm(WATER_H)
qqline(WATER_H)
class(DALY)
class(EPI)
boxplot(EPI,WATER_H)
install.packages("readxl")
library("readxl")
multivariate<-read_excel("Downloads/multivariate.xls")
head(multivariate)
summary(multivariate)
View(multivariate)
attach(multivariate)
Ho<-!is.na(Homeowners)
Im<-!is.na(Immigrants)
mm <-lm(Ho ~ Im)
summary(mm)$coef
plot(Ho,Im)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrants = c(0, 20))
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
mm %>% predict(newImmigrantdata)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients
