#The logic behind this code was from the blog of Mr. Trevor Stephens. It was merely improved upon from that
#Link: http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r

#Installing the necessary packages
#install.packages(c("rpart", "rattle", "rpart.plot", "RColorBrewer", "party", "partykit","plyr","randomForest"))
library(rpart)		#Decision tree package			        
library(rattle)		#Fancy plot for Decision tree			
library(rpart.plot)	#Package for plotting decision tree	
library(RColorBrewer)				
library(party)					
library(partykit)				
library(randomForest)	#Random forest package

#Reading both train and test data
dat_train <- read.csv("train.csv")
dat_test <- read.csv("test.csv")

#Creating the "to be predicted" column for Test data
dat_test$Survived <- NA

#Merging train and test data for preprocessing
all_dat <- rbind(dat_train,dat_test)

#Coverting names into characters from factor
all_dat$Name <- as.character(all_dat$Name)

#Extracting title from names (Titles related to higher status have higher chances of survival)
all_dat$Title <- sapply(all_dat$Name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][2]})
all_dat$Title <- sub(' ', '', all_dat$Title)

#Combining Titles of similar nature
all_dat$Title[all_dat$Title %in% c('MMe', 'Mlle')] <- 'Mlle'
all_dat$Title[all_dat$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
all_dat$Title[all_dat$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonhkeer')] <- 'Lady'

#Converting all titles into factors
all_dat$Title <- factor(all_dat$Title)

#Creating a new variable for family size (Bigger the family lesser the chances of survival)
all_dat$FamilySize <- all_dat$SibSp + all_dat$Parch + 1

#Creating a variable for family ID (extracted from Name) from surnames and converting it to factors
all_dat$Surname <- sapply(all_dat$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
all_dat$FamilyID <- paste(as.character(all_dat$FamilySize), all_dat$Surname, sep="")

#Categorizing family ID based on size
all_dat$FamilyID[all_dat$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(all_dat$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
all_dat$FamilyID[all_dat$FamilyID %in% famIDs$Var1] <- 'Small'
all_dat$FamilyID <- factor(all_dat$FamilyID)

#Filling the missing age values by predicting using regression trees
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=all_dat[!is.na(all_dat$Age),], method="anova")
all_dat$Age[is.na(all_dat$Age)] <- predict(Agefit, all_dat[is.na(all_dat$Age),])

#Filling missing embarked values with the 'mode' of the variable and coverting it into a factor
all_dat$Embarked[all_dat$Embarked == ''] = "S"
all_dat$Embarked <- factor(all_dat$Embarked)

#Filling missing fare values with median of the same
all_dat$Fare[is.na(all_dat$Fare)] <- median(all_dat$Fare, na.rm=TRUE)

#Splitting train and test data after preprocessing
new_train<- all_dat[1:nrow(dat_train),]
new_test<- all_dat[(nrow(dat_train)+1):nrow(all_dat),]

#Detaching party kit to run cforest()
detach("package:partykit", unload=TRUE)

#Building a model based on c-forest function with 2000 trees
titanicFit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID,
                      data = new_train, controls=cforest_unbiased(mtry=3, ntree=2000))

#Predicting using test data
Prediction <- predict(titanicFit, new_test, OOB=TRUE, type = "response")

#Creating dataframe according to submission format
sub_final <- data.frame(PassengerId = dat_test$PassengerId, Survived = Prediction)

#Creating the submission file
write.csv(sub_final, file = "sub_final.csv", row.names = FALSE)
