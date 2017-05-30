#Data loading
data.train<-read.csv("train.csv")
data.test <- read.csv("test.csv")
data.train<-train
data.test<-test
data.test$Survived <- NA #Merging both Test and train to predict missing value in age etc
full.data <- rbind(data.train, data.test)
dim(data.train)
dim(full.data)
names(data.train)

## Id, Name, Ticket, and Cabin are not relevent information,so we are ignore these variables
data <- data.train[,c(2,3,5,6,7,8,10,12)]#Variables which are needed
names(data)
table(train$Survived)#To check from train data how many people survived
summary(train$Sex)
table(train$Sex, train$Survived)#To check how many female and male survived

colSums(is.na(train))
colSums(is.na(test))
#Missing value of age
age_na <- which(is.na(train$Age))
length(age_na)
age <-train$Age
age
for(i in 1:length(age_na))
{
  age[age_na[i]]=sample(train$Age[-age_na],1)
}
table(age)
length(age)
which(is.na(age))
#Missing value train fare
which(is.na(train$Fare))
fare <-train$Fare
#missing value embarked
which(is.na(train$Embarked))
table(train$Embarked)
#Family
familyno <- train$SibSp+train$Parch

#New data set
trainnew <-data.frame(survived,pclass,sex,age,familyno,fare,embarked)
trainnew
#split data into two
data = sample(nrow(data.train), 0.5*nrow(data.train), replace = FALSE)
train1 = trainnew[data,]
test1 = trainnew[-data,]
summary(train1)
summary(test1)
str(train1)
str(test1)
#logistic regression
logit <- glm(survived ~.-(sex) ,data = train1,family = binomial)
summary(logit)

pbtr<- predict(logit, type = "response",data = train1)  
pbte <- predict(logit, type = "response", data = test1)  
pbtr
pbte

pdtr<- rep("0", nrow(train1))
pdtr[pbtr > 0.5] = "1"


pdte <- rep("0", nrow(test1))
pdte[pbte > 0.5] = "1"


# Confusion matrix 
table(train1$survived, pdtr)     
table(test1$survived, pdte)      

# Classification accuracy
mean(pdtr == train1$survived)
mean(pdte == test1$survived) 

#ROC plot

library(ROCR)
pbtr <- predict(logit, type = "response") 
pdtr <- prediction(pbtr, train$survived)
pftr <- performance(pdtr, measure = "tpr", x.measure = "fpr")
plot(perfTr, main = "ROC Curve for Train Set", colorize = TRUE)
train1

##Gradient

library("gbm")
gbmFit <- gbm(survived ~ . ,                            
              data = train1,                        
              distribution = "multinomial",             
              n.trees = 5000,                           
              interaction.depth = 3,                    
              shrinkage = 0.001,                        
              cv.folds = 10,                            
              n.cores = 2)                             

gbmFit                  # the final optimized model after k-fold cross validation
summary(gbmFit)         # importance of the variables in the model (values and visual)

probData <- predict(gbmFit, Tita_train, type = "response")  # probabilities of classes
predData <- apply(probData, 1, which.max)                # prediction (majority class)
mean(predData == as.numeric(Tita_train$Survived))          # classification accuracy

probDataT <- predict(gbmFit, Tita_valid, type = "response")  # probabilities of classes
predDataT <- apply(probDataT, 1, which.max)                # prediction (majority class)
mean(predDataT == as.numeric(Tita_valid$Survived)) 