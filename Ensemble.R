#Load Relevant Library if Required.
library(ggplot2)
library(ggthemes)
library(scales)
library(mice)
library(Amelia)
library(data.table)

#Load Data
train <- read.csv("C:/Users/Harish Dhanarajan/Downloads/Kaggle/Titanic/train.csv", 
                  na.strings=c(""), stringsAsFactors = T)

test <- read.csv("C:/Users/Harish Dhanarajan/Downloads/Kaggle/Titanic/test.csv", 
                 na.strings=c(""), stringsAsFactors = T)

#Exploratory Data Analysis and Cleaning
#Find Duplicates
dup<-function(x){if(length(unique(colnames(x))==ncol(x))){print('No')}else{print('Yes')}}
cat('Is there any duplicate column in train data:', dup(train), 
    '\nIs there any duplicate column in test data:', dup(test), sep = ' ')

#Checking NA's
cat('Number of NAs in train:', sum(is.na(train)), 
    '\nNo of NAs in test:', sum(is.na(test)), sep = ' ') 

#Checking for Factors and Numerics
cat('Number of factor columns in train dataset:',
    length(which(sapply(train, is.factor))),
    '\nNumber of numeric columns in train dataset:',
    length(which(sapply(train, is.numeric)))) 

cat('Number of factor columns in test dataset:',
    length(which(sapply(test, is.factor))),
    '\nNumber of numeric columns in test dataset:',
    length(which(sapply(test, is.numeric))))

#No.of Missing Values
#sapply(train,function(x) sum(is.na(x)))
#sapply(test,function(x) sum(is.na(x)))

#Combine the data for cleaning
full <- bind_rows(train, test)
full$Survived <- ifelse(full$Survived == 1, "one", "zero")

#Data Visualisation (Excluding Survived)
#Missing Data
missmap(full[-2], main = "Missing values vs observed")

#Feature Engineering for Name, Titles
length(unique(full$Name))

#Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
#Show title counts by sex
table(full$Sex, full$Title)

#Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#Reassign mlle, ms,full and mme accordingly
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Fare
full$Fare[is.na(full$Fare)] <- median(full[full$Pclass=='3' & full$Embarked=='S',]$Fare, na.rm=TRUE)

# Age
age_features <- c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked", "Title")
age_frml <- as.formula(paste("Age ~ ", paste(age_features, collapse= "+")))
age_fit <- rpart(age_frml, data = full[-which(is.na(full$Age)), ], cp = 0.001)
full$Age[is.na(full$Age)] <- round(predict(age_fit, full[is.na(full$Age), ]), 2)

# Adding Family Size feature
full$FSize <- full$SibSp + full$Parch + 1

# Adding Child feature
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Adding FsizeD
full$FSizeD[full$FSize == 1] <- 'Alone'
full$FSizeD[full$FSize < 5 & full$FSize > 1] <- 'Small'
full$FSizeD[full$FSize > 4] <- 'Big'

# Visualize embarkment, passenger class, & median fare to determine the missing values
ggplot(embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), #<--- passenger price
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

#Above graph says Port of Embarkment is 'C'
full$Embarked[c(62, 830)] <- 'C'

colSums(is.na(full)|full=='')

features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "FSizeD", "Child")
frml <- as.formula(paste("Survived ~ ", paste(features, collapse= "+")))


# prepare data for modeling
char_features <- names(full)[sapply(full, is.character)]
full[, char_features] <- lapply(full[, char_features], as.factor)
train <- full[!is.na(full$Survived), ]
test <- full[is.na(full$Survived), ]; test$Survived <- NULL


my_control <- trainControl(
  method="boot",
  repeats=5,
  number=25,
  verboseIter = F,
  savePredictions=TRUE,
  summaryFunction=twoClassSummary,
  classProbs = T,
  index=createResample(train$Survived, 25)
)

set.seed(121)
model_list <- caretList(
  frml, 
  data=train,
  metric="ROC",
  trControl=my_control,
  tuneList = list(
    rf = caretModelSpec(method = "rf", tuneGrid = data.frame(mtry = round(sqrt(length(features))))),
    nnet = caretModelSpec(method = "nnet")
  ))

ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=20,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

preds <- round(predict(ensemble, newdata = test, type = "prob"))
submission <- data.frame(PassengerId = test$PassengerId, Survived = preds)

head(submission)

write.csv(submission, "C:/Users/Harish Dhanarajan/Downloads/Titanic/Ensemble.csv")
