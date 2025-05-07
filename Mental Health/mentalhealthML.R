

library(magrittr)
library(classifierplots)
library(rpart)
library(rpart.plot)
library(Boruta)
library(caret)
library(InformationValue)
library(MLmetrics)
library(randomForest)
library(ROCR)
library(tidyverse)
library(tree)
library(xgboost)


teds = read.csv(file = 'C:/Users/nehap/Documents/Courses/Semester 2/big data/finalProj/tedsd_puf_2019.csv')
nrows(teds)
#impute
library(mice)

imputed_teds <- mice(teds, m=9, maxit = 50, method = 'pmm', seed = 500)

tedsd = complete(imputed_teds,1)
head(tedsd)

#selected rows with co-occuring mental health issues and no prior admissions
tedsd_pat = subset(teds, NOPRIOR == 0 & PSYPROB ==1)

#replaced all -9 with NA for imputation
 tedsd_pat2 = sapply(tedsd_pat, function(x) x[x == -9])
head(tedsd_pat2)
tedsd_pat_df = as.data.frame(tedsd_pat2)
head(tedsd_pat_df)

tedsd_pat_df[tedsd_pat_df=="NA"]<- NULL
#imputation on the final subset

imputed_tedsd_pat_df <- mice(tedsd_pat_df, m=2, maxit =2 , method = 'cart', seed = 500)

ted = complete(imputed_tedsd_pat_df,1)
head(ted)
head(ted[,2])

#select the subset without geographical indicators and those measured at discharge
ted1<- subset(ted, select = -c( 1,	2,	3,	4,	11,	15,	36,	37,	38,	45,	46,	47,	54,	73,	74,	9,	21,	23,	24,	25))																																																									
 
head(ted1)

###doing again
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

#Demographics of the participants

median(ted4$AGE)
rowsum(ted4$GENDER)
#complete




#partitioning data into training and testing set

##Generate a random number that is 80% of the total number of rows in dataset.
ran <- sample(1:nrow(ted4), 0.8 * nrow(ted4))

##extract training set
ted_train <- ted4[ran,] 
##extract testing set
ted_test <- ted4[-ran,]

#RandomForest

forest_fit <- randomForest(REASON ~., data = ted_train)            
which.min(forest_fit$err.rate[, 1])