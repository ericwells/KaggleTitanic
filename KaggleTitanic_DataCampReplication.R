# This is my attempt to replicate my DataCamp solution but on my local machine

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

# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0

# Set Survived to 1 if Sex equals "female" (this is basically the AllMenDie model...)
test_one$Survived[test_one$Sex=="female"] <- 1

# Load in the R package
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

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# make the tree bigger

my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)



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

