# This is my attempt to replicate my DataCamp solution but on my local machine

# Index of solutions
# my_forest: just a quick try of a random forest with pretty much default settings.
# my_tree_two:  DT, Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked
# my_tree_three: DT, Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, control = rpart.control(minsplit = 50, cp = 0)
# my_tree_four: DT, Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size
# my_tree_five: DT including my new variable "Deck"
# my_tree_six: my_tree_five, with cp=0 and minbucket=5.
# my_tree_eight: my_tree_five, with cp=0 and minbucket=30.
# my_tree_eight: my_tree_eight with Title added back in.

# Utilities **************

# my own utility to convert the factor back to its proper numeric value (instead of the label of the level!)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Now try writing a function to calculate the accuracy rate on the train dataframe
calc.accuracy <- function(myprediction, the_answers) {
  mistakes <- abs(myprediction-the_answers)
  accuracy <- (length(myprediction)-sum(mistakes))/length(myprediction)
  return(accuracy)
}

# Now improve the function to calculate the accuracy rate on bootstrap samples from train
calc.bootstrap.accuracy <- function(myfile,m,n){
  # myfile should have first column with your prediction and second column with the answer key
  # pull m bootstrap samples of size n
  list.of.results <- vector(mode = "numeric", length = m)
  for (i in 1:m){
    y <- randomRows(myfile,n)
    list.of.results[i] <- calc.accuracy(y[,1], y[,2])
  }
  print(paste("Iteration results ",list.of.results))
  bootstrap_accuracy <- mean(list.of.results)
  print(paste("The bootstrap accuracy (mean) is ", bootstrap_accuracy))
  return(bootstrap_accuracy)
}

# create helper function to random sample rows from a dataframe
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

# end Utilities ***************

# Import Data ****************

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url, header = TRUE)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url, header = TRUE)

# Print train and test to the console
train
test

str(train)
str(test)

# end Import Data ****************

# Data Exploration ***************

# Survival rates in absolute numbers
table(train$Survived)

# Survival rates in proportions
prop.table(table(train$Survived))

# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)

# Two-way comparison: row-wise proportions
prop.table(table(train$Sex,train$Survived), margin = 1)

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Two-way comparison
prop.table(table(train$Child,train$Survived),1)

# end Data Exploration ****************

# Variable Generation ***************

# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0

# Set Survived to 1 if Sex equals "female" (this is basically the AllMenDie model...)
test_one$Survived[test_one$Sex=="female"] <- 1


# end Variable Generation ***************

# Load in the R package for decision trees
library("rpart")

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
#install.packages('rattle')
library(rattle)
#install.packages('rpart.plot')
library("rpart.plot")
#install.packages('RColorBrewer')
library("RColorBrewer")

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Build output in Kaggle format
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# make the tree bigger by relaxing the complexity parameter
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# New DT including new variable
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

# More Variable Generation ****************

test$Survived <- NA
# drop the Child column before merging train and test
train <- train[,-13]
all_data <- rbind(train, test)

all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(all_data$Embarked)

all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Create Title variable by stripping from Name field
all_data$Name <- as.character(all_data$Name)
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_data$Title <- sub(' ', '', all_data$Title)
all_data$Title[all_data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
all_data$Title[all_data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
all_data$Title <- factor(all_data$Title)

# Create Deck variable from Cabin
# Cabin has a lot of missings. Let's set those to S for Steerage.
all_data$Cabin <- as.character(all_data$Cabin)
all_data$Cabin[all_data$Cabin==""] <- "S"
all_data$Cabin[is.na(all_data$Cabin)] <- "S"
# Cabins seem to have a leading value of either "A" "B" "C" "D" "E" "F" "G" "S" (that's the one I added), or "T".  Let's strip that out into a new variable.  I'm not sure if that is really corresponding to the deck of the ship, but I'll call it Deck anyway.
all_data$Deck <- sapply(all_data$Cabin, FUN=function(x) {strsplit(x, split="")[[1]][1]})
all_data$Deck <- factor(all_data$Deck)

# Create Family Size variable by adding siblings and parent/child
all_data$family_size <- all_data$SibSp + all_data$Parch +1

# Deal with missing Ages
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split back into Train and Test
train <- all_data[1:891,]
test <- all_data[892:1309,]

# end More Variable Generation ****************

set.seed(111) 

# Apply the Random Forest Algorithm

#install.packages('randomForest')
library("randomForest")

my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Title, data=train, importance = TRUE, ntree=1000) 

my_prediction <- predict(my_forest, test, "class") 

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction) 

write.csv(my_solution,file="my_solution.csv" , row.names=FALSE) 

varImpPlot(my_forest)

# Now time to investigate if Cabin can improve the model...
# Now try the best Decision Tree I had so far but with Deck as well.
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck, data = train, method = "class")
fancyRpartPlot(my_tree_five)
my_prediction <- predict(my_tree_five, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
# performance on Kaggle dropped.  Now I'm going to try controlling the tree more.
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck, data = train, method = "class",control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(my_tree_five)
my_prediction <- predict(my_tree_five, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# calculate accuracy rate of my_tree_five
my_trainprediction <- predict(my_tree_five, train, type = "class")
my_testprediction <- predict(my_tree_five, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score))
# [1] 0.8271605

# calculate accuracy rate of my_tree_four
my_trainprediction <- predict(my_tree_four, train, type = "class")
my_testprediction <- predict(my_tree_four, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score))
# [1] 0.8395062

# calculate bootstrap accuracy rate of my_tree_five
my_trainprediction <- predict(my_tree_five, train, type = "class")
my_testprediction <- predict(my_tree_five, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300)
# [1] 0.819

# calculate bootstrap accuracy rate of my_tree_four
my_trainprediction <- predict(my_tree_four, train, type = "class")
my_testprediction <- predict(my_tree_four, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300)
# [1] 0.8416667

# calculate bootstrap accuracy rate of my_forest (which should be wildly overfit...)
my_trainprediction <- predict(my_forest, train, type = "class")
my_testprediction <- predict(my_forest, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300)
# [1] 0.9386667 Crap!  I thought for sure this performance would be lower.  I guess though that if it is accurate on train then it is accurate on subsets of train...

# my_tree_six. Same as my_tree_five but changing the control parameters.
my_tree_six <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck, data = train, method = "class",control = rpart.control(cp = 0, minbucket=5))
fancyRpartPlot(my_tree_six)
my_prediction <- predict(my_tree_six, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE) # Kaggle performance = 0.75598
# calculate accuracy rate of my_tree_six
my_trainprediction <- predict(my_tree_six, train, type = "class")
my_testprediction <- predict(my_tree_six, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score)) # accuracy on train = 0.8810325
# calculate bootstrap accuracy rate of my_tree_six
my_trainprediction <- predict(my_tree_six, train, type = "class")
my_testprediction <- predict(my_tree_six, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300) # bootstrap accuracy on train = 0.8793333

# my_tree_seven. Same as my_tree_six but changing the control parameters to be more chunky.
my_tree_seven <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck, data = train, method = "class",control = rpart.control(cp = 0, minbucket=30))
fancyRpartPlot(my_tree_seven)
my_prediction <- predict(my_tree_seven, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE) # Kaggle performance = 0.77990
# calculate accuracy rate of my_tree_seven
my_trainprediction <- predict(my_tree_seven, train, type = "class")
my_testprediction <- predict(my_tree_seven, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score)) # accuracy on train = 0.8170595
# calculate bootstrap accuracy rate of my_tree_seven
my_trainprediction <- predict(my_tree_seven, train, type = "class")
my_testprediction <- predict(my_tree_seven, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300) # bootstrap accuracy on train = 0.814

# my_tree_eight: my_tree_seven with Title added back in.
my_tree_eight <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck + Title, data = train, method = "class",control = rpart.control(cp = 0, minbucket=20))
fancyRpartPlot(my_tree_eight)
my_prediction <- predict(my_tree_eight, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE) # Kaggle performance = 0.76555
# calculate accuracy rate of my_tree_eight
my_trainprediction <- predict(my_tree_eight, train, type = "class")
my_testprediction <- predict(my_tree_eight, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score)) # accuracy on train = 0.8428732
# calculate bootstrap accuracy rate of my_tree_eight
my_trainprediction <- predict(my_tree_eight, train, type = "class")
my_testprediction <- predict(my_tree_eight, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300) # bootstrap accuracy on train = 0.849

# rerunning DT4 to see if that was the one that got .80383 in Kaggle perf...
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")
fancyRpartPlot(my_tree_four)
my_prediction <- predict(my_tree_four, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE) # Kaggle performance = 0.78469
# calculate accuracy rate of my_tree_four
my_trainprediction <- predict(my_tree_four, train, type = "class")
my_testprediction <- predict(my_tree_four, test, type = "class")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
calc.accuracy(my_scoredtrain$Survived, as.numeric.factor(my_scoredtrain$Score)) # accuracy on train = 0.8395062
# calculate bootstrap accuracy rate of my_tree_four
my_trainprediction <- predict(my_tree_four, train, type = "class")
my_testprediction <- predict(my_tree_four, test, type = "class")
my_scoredtrain <- data.frame(Score = as.numeric.factor(my_trainprediction), Survived = as.numeric(train$Survived))
calc.bootstrap.accuracy(my_scoredtrain,10,300) # bootstrap accuracy on train = 0.8436667

# OK, now moving on to experiment with logistic regression. ********************

# first trying to get a logistic regression with glm function.
my_logit_one <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Deck + Title, data=train, family = "binomial")
summary(my_logit_one)
my_trainprediction <- predict(my_logit_one, train, type = "response")
my_testprediction <- predict(my_logit_one, test, type = "response")
my_scoredtrain <- data.frame(Survived = train$Survived, Score = my_trainprediction)
my_scoredtrain$Score <- round(my_scoredtrain$Score)
calc.accuracy(my_scoredtrain$Survived, as.integer(my_scoredtrain$Score)) # accuracy on train = 0.8406285
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = as.integer(round(my_testprediction)))
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE) # Kaggle performance = 0.77512

# end Logistic Regression section. ***********************

# cool little helper re missing values
install.packages("Amelia")
library('Amelia')
missmap(train, main = "Missing values vs observed")
