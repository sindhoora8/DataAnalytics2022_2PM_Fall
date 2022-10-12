library(gdata) 
#faster xls reader but requires perl!
bronx1<-read.xls("/Users/sindhooramandadi/Downloads/rollingsales_bronx.xls",sheet=1,verbose=FALSE,pattern="BOROUGH",stringsAsFactors=FALSE,perl="perl") 
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xls("/Users/sindhooramandadi/Downloads/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
 
View(SALE.PRICE)

help(gsub)
SALE.PRICE<-(gsub(",","", bronx1$SALE.PRICE)) 
SALE.PRICE<-sub("\\$","",SALE.PRICE)
SALE.PRICE
SALE.PRICE<-as.numeric(SALE.PRICE)
View(SALE.PRICE)
#GROSS.SQUARE.FEET_NA<-is.na(GROSS.SQUARE.FEET)
#View(GROSS.SQUARE.FEET_NA)
#GROSS.SQUARE.FEET<-GROSS.SQUARE.FEET[!GROSS.SQUARE.FEET_NA]
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$GROSS.SQUARE.FEET)) 
View(GROSS.SQUARE.FEET)

#LAND.SQUARE.FEET_NA<-is.na(bronx1$LAND.SQUARE.FEET)
#View(LAND.SQUARE.FEET_NA)
#LAND.SQUARE.FEET<-LAND.SQUARE.FEET[!LAND.SQUARE.FEET_NA]
LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$LAND.SQUARE.FEET)) 
#SALE.PRICE<-type.convert(SALE.PRICE,"numeric")
#GROSS.SQUARE.FEET<-type.convert(GROSS.SQUARE.FEET,"numeric")
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)+factor(BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)*factor(BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#

