TRAIN<-read.csv("train.csv",stringsAsFactors = FALSE)
TEST <-read.csv("test.csv")


#########################################################
table(TRAIN$Age)
s=which(is.na(TRAIN$Age))
length(s)
hist(TRAIN$Age)
age <-TRAIN$Age
age
for(i in 1:length(s))
{
  age[s[i]]=sample(TRAIN$Age[-s],1)
}
hist(age)
table(age)
which(is.na(age))

######################################################
table(TRAIN$Cabin)
cabin <-TRAIN$Cabin
for(i in 1:nrow(TRAIN))
{
  if(nchar(cabin[i]) == 0)
  {  cabin[i]=0
  }else{
    str=strsplit(cabin[i]," ")
    cabin[i]=length(str[[1]])
    
  }
}
table(cabin)
######################################################
which(is.na(TRAIN$Fare))
######################################################
which(is.na(TRAIN$Embarked))
table(TRAIN$Embarked)
######################################################
which(is.na(TRAIN$Name))
table(TRAIN$Name)
midname <-array("NA",nrow(TRAIN))
for (i in 1:nrow(TRAIN)){
  l = strsplit(TRAIN$Name[i],", ")[[1]][2]
  midname[i] = strsplit(l,". ")[[1]][1]
}
table(midname)
midname[midname != 'Mr' & midname != 'Miss' & midname != 'Mrs' & midname != 'Master'] <-'unusual'
table(midname)
######################################################
which(is.na(TRAIN$Ticket))
ticket <-TRAIN$Ticket
for(i in 1:nrow(TRAIN))
    {
      ticket[i]=nchar(ticket[i])
}
table(ticket)
######################################################
familystr <-TRAIN$SibSp+TRAIN$Parch
table(familystr)
survived <-TRAIN$Survived
pclass <-TRAIN$Pclass
sex <-TRAIN$Sex
embarked <-TRAIN$Embarked

######################################################
which(is.na(TRAIN$Fare))
fare <-TRAIN$Fare
train1 <-data.frame(survived,pclass,
                   midname,sex,age,
                   familystr,ticket,fare,cabin,embarked)
train1
set.seed(10)
data <- sample(nrow(TRAIN), 0.5*nrow(TRAIN), replace = FALSE)


Train <-train1[data,]
Test <-train1[-data,]
str(Train)
str(Test)
Train$midname <-as.factor(Train$midname)
Test$midname <-as.factor(Test$midname)
######################################################
logit <- glm(survived ~.-(sex+ticket) ,data = Train,family = binomial)
summary(logit)

probTr<- predict(logit, type = "response")  
probTe <- predict(logit, type = "response", 
                     data = Test)  


contrasts(default)

predTr<- rep("0", nrow(Train))
predTr[probTr > 0.5] = "1"
#predTr <- as.factor(predTrain)

predTe <- rep("0", nrow(Test))
predTe[probTe > 0.5] = "1"
#predTe <- as.factor(predTest)

# Confusion matrix for predictions
table(Train$survived, predTr)      # on train set
table(Test$survived, predTe)      # on validation set

# Classification accuracy
mean(predTr == Train$survived)
mean(predTe == Test$survived)     


library(ROCR)
probTr <- predict(logit, type = "response")  # on train set
predTr <- prediction(probTr, train$survived)
perfTr <- performance(predTr, measure = "tpr", x.measure = "fpr")
plot(perfTr, main = "ROC Curve for Train Set", colorize = TRUE)

###########################################################
library("tree")
treefit <- tree(as.factor(survived) ~ ., data =Train)
plot(treefit)
text(treefit, pretty = FALSE)

# Predict using the model
predTr <- predict(treefit, Train,type="class")  # on train set
predTe <- predict(treefit, Test,type="class")  # on validation set

# Confusion matrix for predictions
#table(irisTrain$Species, predTrain)      # on train set
#table(irisValid$Species, predValid)      # on validation set
Train$survived
predTr
# Classification accuracy
mean(predTr == Train$survived)     # on train set
mean(predTe == Test$survived)     # on validation set

ltreefit <- tree(as.factor(survived) ~ ., data = Train, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(Train),  # number of sample points
                                        mincut = 1,             # minimum points in each child
                                        minsize = 2,            # minimum points in each parent
                                        mindev = 0))            # minimum information gain to split
plot(ltreefit)

predTr <- predict(ltreefit, Train, type = "class")  # prediction on train set
mean(predTr == Train$survived)                     # classification accuracy
predTe <- predict(ltreefit, Test, type = "class")  # prediction on validation set
mean(predTe == Test$survived) # classification accuracy


ltreefit <- tree(as.factor(survived) ~ ., data = Train, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(Train),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreefit)

cvtree <- cv.tree(ltreefit, FUN = prune.misclass, K = 10) # K-fold Cross-Validation
cbind(cvtree$size, cvtree$dev, cvtree$k)                  # check cvTree output
plot(cvtree$size, cvtree$dev, type="b")                   # plot deviance vs size
plot(cvtree$k, cvtree$dev, type="b")                      # plot deviance vs alpha

bestSize <- 20 
# choose this parameter carefully, based on the cvTree output
ptreefit <- prune.misclass(ltreefit, best = bestSize)     # prune tree to best size
plot(ptreefit)
text(ptreefit, pretty = FALSE)

predTr <- predict(ptreefit, Train, type = "class")  # prediction on train set
mean(predTr == Train$survived)                     # classification accuracy
predTe <- predict(ptreefit, Test, type = "class")  # prediction on validation set
mean(predTe == Test$survived)                     # classification accuracy



###########################################################
bagfit <- randomForest(as.factor(survived) ~. ,                    # formula
                       data = Train,                  # data set
                       ntree = 500,                      # number of trees
                       mtry = 9,                         # variables for split
                       importance = TRUE)                # importance recorded
bagfit
predTr <- predict(bagfit, Train, type = "class")   # prediction on train set
mean(predTr == Train$survived)                    # classification accuracy
predTe <- predict(bagfit, Test, type = "class")   # prediction on validation set
mean(predTe == Test$survived)                    # classification accuracy

#############################################################
rffit <- randomForest(as.factor(survived) ~ .,                     # formula
                      data = Train,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 2,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rffit

predTr <- predict(rffit, Train, type = "class")    # prediction on train set
mean(predTr == Train$survived)                    # classification accuracy
predTe <- predict(rffit, Test, type = "class")    # prediction on validation set
mean(predTe == Test$survived)                    # classification accuracy

importance(rfFit)         # importance of the variables in the model (values)
varImpPlot(rfFit)         # importance of the variables in the model (visual)






