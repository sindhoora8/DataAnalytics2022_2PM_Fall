install.packages("titanic")
data(Titanic)
library(rpart)
titanic <- rpart(Survived ~ ., data=Titanic)
titanic
titanic$frame
plot(titanic)
text(titanic)
