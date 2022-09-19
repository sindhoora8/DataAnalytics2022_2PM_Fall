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

