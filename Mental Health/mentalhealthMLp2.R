teds = read.csv(file = 'C:/Users/nehap/Documents/Courses/Semester 2/big data/finalProj/tedsd_puf_2019.csv')
nrows(teds)
ted1<- subset(tedsd_pat, select = -c( 1,	2,	3,	4,	11,	15,	36,	37,	38,	45,	46,	47,	54,	73,	74,	9,	21,	23,	24,	25))																																																									
head(ted1)

#replace -9 with NA
ted1[ted1 ==-9]<- NULL
ted2 = sapply(ted1, function(x) x <- replace(x, x==-9, NA))
head(ted2)
ted3 = as.data.frame(ted2)
head(ted3)

#impute
imputed_ted3 <- mice(ted3, m=2, maxit =2 , method = 'cart', seed = 500)
ted4 = complete(imputed_ted3,1)
ted5 = write.csv(ted4,'C:/Users/nehap/Documents/Courses/Semester 2/big data/finalProj/ted4.csv',row.names = TRUE)

library(mice)



ted4 = read.csv(file = 'C:/Users/nehap/Documents/Courses/Semester 2/big data/finalProj/ted4.csv')



install.packages("doParallel")
library(caret)
library(randomForest)
library(doParallel)
install.packages("miceadds")
library(miceadds)

library(dplyr)
library(ranger)
library(car)
library(ROCR)
library(pROC)
library(ROCR)



#dichotomise completion
##Creating binary variable indicating whether or not a patient completed treatment
ted4[['COMPLETION']] <- ifelse(ted4$REASON == 1, 1, 0)

##Creating a binary variable indicating pregnant or not pregnant
ted4[['PREG']] <- ifelse(ted4$PREG== 1, 1, 0)


##Binary variable that indicates any use of opioids
ted4[['OUD']] <- ifelse(ted4$SUB1 == 5 | ted4$SUB1 == 6 | ted4$SUB1 == 7 | ted4$SUB2 == 5 | ted4$SUB2 == 6 | ted4$SUB2 == 7 | ted4$SUB3 == 5 | ted4$SUB3 == 6 | ted4$SUB3 == 7, 1, 0)

##Renaming no drug reported to also be no IV drug reported
ted4[['IDU']] <- ifelse(ted4$IDU == -9, 0, ifelse(ted4$IDU == 0, 0, 1))





#removing reasons from the list of predictors, and also sl.no

ted6 = subset(ted4, select = -c(1,15))
head(ted6,2)
##Generate a random number that is 80% of the total number of rows in dataset.
ran <- sample(1:nrow(ted6), 0.8 * nrow(ted6))

##extract training set
ted_train <- ted6[ran,] 
##extract testing set
ted_test <- ted6[-ran,]





#random forest is faster with ranger

rf <- ranger(COMPLETION ~ .,data=ted_train,mtry=7,importance='impurity',classification = T)
pred <- predict(rf, data=ted_train, type='response')

##Create test labels
test.Y = ted_test$COMPLETION

##Calculate error rate
1-mean(pred$predictions==test.Y)

##Variables in order of importance based on gini impurity
x<-rf$variable.importance[order(rf$variable.importance, decreasing=T)]
x

###Create a model with top thirty predictors###

impTrain <- ted_train[,c(names(x[1:30]), 'COMPLETION')] ##top thirty predictors + response variable
testImp <-ted_test[,c(names(x[1:30]), 'COMPLETION')]
imp.Y = testImp$COMPLETION ## response
set.seed(1)

rf.imp <- ranger(COMPLETION~.,mtry=floor(sqrt(30)), data=impTrain,importance = 'impurity', classification = T)
##Predict with test set
pred.imp <- predict(rf.imp, data=testImp, type='response')
mean(pred.imp$predictions==imp.Y)
1 - mean(pred.imp$predictions==imp.Y)

## top 30 Variables in order of importance based on gini impurity
x<-rf.imp$variable.importance[order(rf.imp$variable.importance, decreasing=T)]
x

#confusion matrix

cm_imp = rf.imp$confusion.matrix
cm_imp

#ROC and AOC
pred_object <- prediction(pred.imp$predictions, ted_test$COMPLETION)

per_measure <- performance(pred_object, "tnr", "fnr")

roc.imp = plot(per_measure, col="red", lwd=1)

abline(a=0,b=1,lwd=1,lty=1,col="gray")

roc1 = roc(pred.imp$predictions, ted_test$COMPLETION)
auc(roc1)
roc1

#variable importance plot
library(abcrf)
install.packages("abcrf")

variableImpPlot(rf.imp)
# Prepare for plotting
df <- data.frame("Feature" = names(rf.imp$variable.importance),
                 "Gini.importance" = rf.imp$variable.importance)


par(mfrow=c(1,3))
par(mar=c(5,6,4,0)+.1)
p1 <- barplot(df$Gini.importance, horiz=TRUE, names.arg = df$Feature, las=2, xlab = "Gini importance", col = 'green')
legend("bottomright",inset=-.025,legend =paste("runtime",round(t1-t0,2),"s"), bty = "n")
text(0, p1, round(df$Gini.importance,2),cex=1,pos=4) 


###Logistic regression
install.packages("aod")
library(aod)
library(ggplot2)
library(caret)
#chisq.test(head(ted_train,100))

ted7 = as.data.frame(head(ted6,1000))
log_ted <- glm(COMPLETION ~ ., data = ted_train, family = "binomial")
summary(log_ted)
anova(log_ted,test='Chisq')
lod_ted2 =  glm(COMPLETION ~  AGE  +	ALCDRUG  + ALCFLG    + ARRESTS  + COKEFLG +DAYWAIT   +	DETCRIM +DETNLF   +DETNLF_D+ EDUC+ EMPLOY +	ETHNIC +FREQ1  +GENDER +HERFLG  +HLTHINS   + LIVARAG  +	MARFLG         +	MARSTAT             +	METHUSE            +	MTHAMFLG         +	OPSYNFLG     +	PRIMINC       +	PSOURCE           +	RACE          +	ROUTE1           +	SEDHPFLG          +	SERVICES , data = ted_train, family = "binomial")
lod_ted2
ted_test$COMPLETION
pred_log_ted2 = predict(lod_ted2, newdata = ted_test)
tabmlog = table(pred_log_ted2,test.Y)
tabmlog
accuracy = function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(tabmultlog)

head(pred_log_ted2)

probabilities <- lod_ted2 %>% predict(ted_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
mean(predicted.classes == ted_test$COMPLETION)
table(predicted.classes,ted_test$COMPLETION)

##ROC object
roclog <- prediction(predicted.classes, ted_test$COMPLETION)
##Performance object (tpr vs fpr)
perflog <- performance(roclog, measure='tpr', x.measure='fpr')
##plot with
plot(perflog)
abline(a=0,b=1)
Metrics::logLoss(ted_test$COMPLETION,predicted.classes)
Metrics::auc(ted_test$COMPLETION,predicted.classes)



#boost
install.packages("gbm")
library(gbm)
##Boosting using gbm package, computationally intensive, hard to test all hyperparameters
set.seed(1)
##train model
boost.16 <- gbm(COMPLETION ~.,data = ted_train,
                interaction.depth = 6,
                n.trees = 1000,
                shrinkage = 0.01,
                distribution = 'bernoulli',
                verbose=T)

##Predicting on test set
pred.boost <- predict(boost.16, newdata=ted_test,n.trees=1000,type='response')
##Translating probabilities to binary class
boost.class<- ifelse(pred.boost>0.5,1,0)
##Accuracy = 0.686
mean(boost.class==ted_test$COMPLETION)
table(boost.class,ted_test$COMPLETION)
##ROC object
rocBOOST <- prediction(pred.boost, ted_test$COMPLETION)
##Performance object (tpr vs fpr)
perfBOOST <- performance(rocBOOST, measure='tpr', x.measure='fpr')
##plot with
plot(perfBOOST)
abline(a=0,b=1)
Metrics::logLoss(ted_test$COMPLETION,pred.boost)
Metrics::auc(ted_test$COMPLETION,pred.boost)

performance(rocBOOST, measure='auc') ##AUC = 0.717

#demographics

#Demographics of the participants

median(ted4$AGE)

males = subset(ted4$GENDER, GENDER == 1 )
sum(males$GENDER)

insu = subset(ted4, HLTHINS == 1|HLTHINS == 2|HLTHINS == 3 )
nrow(insu)

employ = subset(ted4, EMPLOY == 3 )
nrow(employ)
