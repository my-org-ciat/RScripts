#link - https://www.geeksforgeeks.org/implement-machine-learning-with-caret-in-r/

library(ggplot2)
library(ggpubr) 
library(reshape)
library(caret)

#Dataset
data("iris")

# To display the first five 
# rows of our data
head(iris)

# To see the last five rows 
# of our data
tail(iris)

# Summary statistics of data
summary(iris)


# Visualizing the outliers by using boxplot
# As we use ggplot2 we will take numerical 
# variables by subsetting the entire of it
df <- subset(iris, select = c(Sepal.Length, 
                              Sepal.Width, 
                              Petal.Length, 
                              Petal.Width))
df

# plot and see the box plot of each variable
ggplot(data = melt(df), 
       aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=variable))

# To plot the barplot for our 
# categorical variable
ggplot(data = iris, 
       aes(x = Species, fill = Species)) +
  geom_bar()


# In this we visualize the histogram of all 
# numerical variables of the dataset
a <- ggplot(data = iris, aes(x = Petal.Length)) +
  geom_histogram( color = "red", 
                  fill = "blue", 
                  alpha = 0.01) + geom_density()

b <- ggplot(data = iris, aes(x = Petal.Width)) +
  geom_histogram( color = "red", 
                  fill = "blue", 
                  alpha = 0.1) + geom_density()
c <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram( color = "red", 
                  fill = "blue", 
                  alpha = 0.1) + geom_density()

d <- ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_histogram( color = "red", 
                  fill = "blue", 
                  alpha = 0.1) + geom_density()

ggarrange(a, b, c, d + rremove("x.text"), 
          labels = c("a", "b", "c", "d"),
          ncol = 2, nrow = 2)

# Create train-test split of the data
limits <- createDataPartition(iris$Species,
                              p=0.80,
                              list=FALSE)

# select 20% of the data for validation
testiris <- iris[-limits,]

# use the remaining to training
# and testing the models
trainiris <- iris[limits,]


Q <- quantile(trainiris$Sepal.Width,
              probs=c(.25, .75),
              na.rm = FALSE)

# Code to calculate the IQR, 
# upper and lower bounds
iqr <- IQR(trainiris$Sepal.Width)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr

# Elimination of outliers by using of iqr
normal <- subset(trainiris,
                 trainiris$Sepal.Width > (Q[1] - 1.5*iqr)
                 & trainiris$Sepal.Width < (Q[2]+1.5*iqr))
normal

# boxplot using cleaned dataset
boxes <- subset(normal,
                select = c(Sepal.Length,
                           Sepal.Width,
                           Petal.Length,
                           Petal.Width))
ggplot(data = melt(boxes),
       aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=variable))

# crossvalidation set to 10
crossfold <- trainControl(method="cv",
                          number=10,
                          savePredictions = TRUE)
metric <- "Accuracy"

# Set a random seed to 42
set.seed(42)
fit.lda <- train(Species~.,
                 data=trainiris,
                 method="lda",
                 metric=metric,
                 trControl=crossfold)
print(fit.lda)

set.seed(42)
fit.svm <- train(Species~.,
                 data=trainiris,
                 method="svmRadial",
                 metric=metric,
                 trControl=crossfold)
print(fit.svm)

# prediction on test data
predictions <- predict(fit.lda, testiris)
confusionMatrix(predictions, testiris$Species)
