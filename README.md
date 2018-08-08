kaggletitanic
Titanic - ML

#library(randomForest)
# library(ggplot2)
# library(ggthemes)
# library(scales)
# library(e1071)

train <- read.csv("C:/Users/Harish Dhanarajan/Downloads/Titanic/train.csv", 
                  na.strings=c(""), stringsAsFactors = T)

test <- read.csv("C:/Users/Harish Dhanarajan/Downloads/Titanic/test.csv", 
                 na.strings=c(""), stringsAsFactors = T)

sample <- read.csv("C:/Users/Harish Dhanarajan/Downloads/Titanic/gender_submission.csv", 
                   na.strings=c(""), stringsAsFactors = T)

#Look for missing values
sapply(train,function(x) sum(is.na(x)))
sapply(test,function(x) sum(is.na(x)))

#Handling Age and Port of Embarkation
#paste("PassengerId: ", train[is.na(train$Embarked), 1], 
" Missing!!!")

# Visualize embarkment, passenger class, & median fare to determine the missing values
#ggplot(embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
#  geom_boxplot() +
#  geom_hline(aes(yintercept=80), #<--- passenger price
#             colour='red', linetype='dashed', lwd=2) +
#  scale_y_continuous(labels=dollar_format()) +
#  theme_few()

#Above Plot can be used to determine Port of Embarkment which is 'C'
#As a result
train$Embarked[c(62, 830)] <- 'C'
sum(is.na(train$Embarked))

#Missing Ages
modelling <- mice(train[, !names(train) %in%
                          c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
model_output <- complete(modelling)
train$Age <- model_output$Age

modelling1 <- mice(test[, !names(test) %in%
               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
model_output1 <- complete(modelling1)
test$Age <- model_output1$Age

#Add Child Feature in test and train
train$Child[train$Age < 18] <- "Yes"
train$Child[train$Age >= 18] <- "No"
train$Child <- factor(train$Child)

test$Child[test$Age < 18] <- "Yes"
test$Child[test$Age >= 18] <- "No"
test$Child <- factor(test$Child)

#Feature Engineering 'Title' in train and test
raretitles <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)

train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title %in% raretitles]  <- 'Rare Title'
train$Title <- as.factor(train$Title)

test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)
test$Title[test$Title == 'Mlle']        <- 'Miss' 
test$Title[test$Title == 'Ms']          <- 'Miss'
test$Title[test$Title == 'Mme']         <- 'Mrs' 
test$Title[test$Title %in% raretitles]  <- 'Rare Title'
test$Title <- as.factor(test$Title)

#Few More
train$FamilySize <- train$SibSp + train$Parch + 1
test$FamilySize <- test$SibSp + test$Parch + 1

train$FamilySizeDescription[train$FamilySize == 1] <- 'singleton'
train$FamilySizeDescription[train$FamilySize < 5 & train$FamilySize > 1] <- 'small'
train$FamilySizeDescription[train$FamilySize > 4] <- 'large'
train$FamilySizeDescription <- as.factor(train$FamilySizeDescription)

test$FamilySizeDescription[test$FamilySize == 1] <- 'singleton'
test$FamilySizeDescription[test$FamilySize < 5 & test$FamilySize > 1] <- 'small'
test$FamilySizeDescription[test$FamilySize > 4] <- 'large'
test$FamilySizeDescription <- as.factor(test$FamilySizeDescription)


#Data Conversion and Cleaning
#Converting Survival Level to Y/N
train$Survived <- factor(train$Survived, levels = c(0,1), 
                         labels = c("No", "Yes"))
sample$Survived <- factor(sample$Survived, levels = c(0,1), 
                          labels = c("No", "Yes"))
#Coverting PClass into Factor
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)

#RandomForest Modelling

Model <- randomForest(Survived ~ Sex+Title+Pclass+Fare+FamilySizeDescription
                      +SibSp+Parch+Child+Embarked, data=train, importance=TRUE, ntree=2000)

#varImpPlot(Model)

Prediction <-  predict(Model, test, type = "class")

confusionMatrix(Prediction, sample$Survived, positive = "Yes")

write.csv(Prediction, file = "C:/Users/Harish Dhanarajan/Downloads/Titanic/submission.csv", row.names = FALSE)
