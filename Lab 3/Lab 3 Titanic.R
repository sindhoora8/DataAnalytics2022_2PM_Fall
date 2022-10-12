install.packages("titanic")
data(Titanic)
titanic <- tree(Survived ~ ., data=Titanic)
titanic
titanic$frame
plot(titanic)
text(titanic)
