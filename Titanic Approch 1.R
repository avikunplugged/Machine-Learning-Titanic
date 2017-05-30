HTitanic_data=read.csv("train.csv")




## Finding out titles as names are not really important
Titanic_data$Title <- ifelse(grepl('Mr ',Titanic_data$Name),'Mr',
                          ifelse(grepl('Mrs ',Titanic_data$Name),'Mrs',
                                 ifelse(grepl('Miss',Titanic_data$Name),'Miss',
                                        ifelse(grepl('Master',Titanic_data$Name),'Master',
                                        'Others'))))

Titanic_data = Titanic_data[,-c(4),drop=FALSE]  



Titanic_data = Titanic_data[,-c(10),drop=FALSE]  
### Models
print(str(Titanic_data))
Titanic_data$Title=as.factor(Titanic_data$Title)
Titanic_data$Survived=as.factor(Titanic_data$Survived)
Titanic_data$Pclass=as.factor(Titanic_data$Pclass)
print(str(Titanic_data))

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)

train = sample(nrow(Titanic_data), 0.5*nrow(Titanic_data), replace = FALSE)
Tita_train = Titanic_data[train,]
Tita_valid = Titanic_data[-train,]
summary(Tita_valid)
summary(Tita_train)
## save the passenger id elsewhere where we would cbind later
train_pass_id=Tita_train$PassengerId
Tita_train=Tita_train[,-c(1),drop=TRUE]


library(randomForest)
print(str(Tita_train))


Tita_train
# Find the optimal value of mtry
# Experiment with various values
# Number of trees is fixed (500)
oobError <- double(8)
for (mt in 1:8) {
  rfFit <- randomForest(Survived ~ ., data = Tita_train, ntree = 500, mtry = mt)
  oobError[mt] <- rfFit$err.rate[500]     # OOB MSE after fitting all 500 trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
cat("Building RandomForest on train data using ", which.min(oobError) ," variables")

##we see out of bag error is least for mtry=3 AFTER TRYING 50 TIMES 
##HENCE WITH 50 DIFFERNT RANDOM SAMPLING OF THE DATASET

## random forest
titanic_survival_train_rf = randomForest(Survived ~ ., data=Tita_train,ntree=5000,mtry=which.min(oobError), importance=TRUE)
##visualizing important  factors
summary(titanic_survival_train_rf)
plot(titanic_survival_train_rf)
importance(titanic_survival_train_rf)
varImpPlot(titanic_survival_train_rf)
titanic_survival_train_rf$confusion
#Prediction & accuracy on training set

predTrain <- predict(titanic_survival_train_rf, Tita_train, type = "class") 
mean(predTrain == Tita_train$Survived)                   

## save the passenger id elsewhere where we would cbind later
valid_pass_id=Tita_valid$PassengerId
Tita_valid=Tita_valid[,-c(1),drop=TRUE]
titanic_survival_train_rf

#prediction and accuracy on the validity set
predValid <- predict(titanic_survival_train_rf, Tita_valid,oobError=TRUE, type = "class") 
mean(predValid == Tita_valid$Survived)      


##Gradient Boosting

library("gbm")
gbmFit <- gbm(Survived ~ . ,                            # formula
              data = Tita_train,                        # train data set
              distribution = "multinomial",             # distribution of response
              n.trees = 5000,                           # number of trees
              interaction.depth = 3,                    # depth of the trees
              shrinkage = 0.001,                        # shrinkage parameter
              cv.folds = 10,                            # k-fold cross validation
              n.cores = 2)                             # number of cores to use

gbmFit                  # the final optimized model after k-fold cross validation
summary(gbmFit)         # importance of the variables in the model (values and visual)

probData <- predict(gbmFit, Tita_train, type = "response")  # probabilities of classes
predData <- apply(probData, 1, which.max)                # prediction (majority class)
mean(predData == as.numeric(Tita_train$Survived))          # classification accuracy

probDataT <- predict(gbmFit, Tita_valid, type = "response")  # probabilities of classes
predDataT <- apply(probDataT, 1, which.max)                # prediction (majority class)
mean(predDataT == as.numeric(Tita_valid$Survived))          # classification accuracy

## WE USE GBM FIM GRADIENT BOOSTING AS IT GIVES SLIGHTLY BETTER GUESS OF SURVIVAL
##and also it gives us option for tuning better making our model  much morre FLEXIBLE


##OVER  the tEST dataset given
Tst=read.csv("test.csv")
result=read.csv("gender_submission TITANIC.csv")
Tst[Tst$PassengerId %in% result$PassengerId, "Survived"] = result$Survived
for_age_Tst=Tst[is.na(Tst$Age),]
for_age_Tst$Age = predict(lm_Age, newdata = for_age_Tst)
Tst[Tst$PassengerId %in% for_age_Tst$PassengerId, "Age"] = for_age_Tst$Age 
Tst$Title <- ifelse(grepl('Mr ',Tst$Name),'Mr',
                    ifelse(grepl('Mrs ',Tst$Name),'Mrs',
                           ifelse(grepl('Miss',Tst$Name),'Miss',
                                  ifelse(grepl('Master',Tst$Name),'Master',
                                         'Others'))))
Tst = Tst[,-c(3),drop=FALSE]  
Tst = Tst[,-c(9),drop=FALSE]  

Tst_id=Tst$PassengerId
Tst=Tst[,-c(1)]
Tst=Tst[,-c(6)]



Tst$Title=as.factor(Tst$Title)
Tst$Pclass=as.factor(Tst$Pclass)
Tst$Survived=as.factor(Tst$Survived)
print(str(Tst))
print(str(Tita_train))
Tst=Tst[,c(8,1,2,3,4,5,6,7,9)]



probData1 <- predict(gbmFit, Tst, type = "response")  # probabilities of classes
predData1 <- apply(probData1, 1, which.max)                # prediction (majority class)
mean(predData1 == as.numeric(Tst$Survived))          # classification accuracy

