library(readxl)
library(reshape2)
library(mltools)
library(data.table)
library(leaps)
library(randomForest)
require(caTools)
#install.packages('Boruta')
#library(Boruta)
library(corrplot)
library(tidyverse)
library(Hmisc)#
#library(caret)
source('C:/Users/natasha.raymond/Downloads/hw.R')


## Step 1 Read and Prep Data
df <- read_excel("C:/Users/natasha.raymond/Downloads/Airfare Data_Train.xlsx")
#orig <- read_excel("C:/Users/natasha.raymond/Downloads/Data_Train.xlsx")

# Drop redundant fields
df <- subset(df, select= -c(Duration_Hour,Duration_Min,Dep_Hour,Dep_Min,Arrival_Hour,Arrival_Min))

# Summaries 
head(df)
summary(df)
str(df)

# Handle Missing Values
sum(is.na(df))
df = na.omit(df)
sum(is.na(df))

## Step 2 Data Exploration

hist.data.frame(df, mtitl = "Histogram of all variables")

# Drop fields
df <- subset(df, select= -c(Additional_Info,Route))

splom(df, as.matrix = TRUE,
      cex = .5,varname.cex = 0.56,
      varname.col = "red", pscale = 0 )

# Separate categorical variables 
df_id <- cbind(ID = 1:nrow(df),df)
#head(df_id)
cat <- subset(df_id, select=c("ID","Airline","Source","Destination"))
#head(cat)

# Convert to factor data types
cat <- cat %>% mutate(across(c(2,3,4),as.factor))
str(cat)
# One-hot coding
cat_oh <- one_hot(as.data.table(cat))
str(cat_oh)


#head(cat_oh)

# Merge back and drop derived fields
final <- cbind(df_id,cat_oh)
#str(final)
final <- subset(final, select= -c(ID,Airline,Source,Destination))
final <- subset(final, select= -c(ID))

## Remove Spaces in field names
names(final) <- make.names(names(final))

# Reorder y
final <- final[, c(7, 1,2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]

str(final)
## correlation
corr <- cor(final)
#head(corr)

corrplot(corr, method="circle")
mtext("Correlation Heat Map", at=-5, line=1, cex=2)

## final dataset
final <- subset(final, select= -c(Destination_Banglore,Destination_Cochin,Destination_Hyderabad,Destination_Kolkata))

str(final)

## Backward Stepwise
regfit.full = regsubsets(Price~.,data = final,
                         nvmax = 22)  # all subsets by default
#summary(regfit.full)

regfit.fwd = regsubsets(Price~.,data = final,
                        nvmax = 22,method = "forward")
#summary(regfit.fwd)

regfit.bwd = regsubsets(Price~.,data = final,
                        nvmax = 22,method = "backward")
#summary(regfit.bwd)

coef(regfit.full,22)
coef(regfit.fwd,22)
coef(regfit.bwd,22)


namFull <- names(coef(regfit.full,22))
namBwd <- names(coef(regfit.bwd,22))
namFwd <- names(coef(regfit.fwd,22))

match(namBwd,namFull) # 3 differ
match(namFwd,namFull) # 3 differ
match(namBwd,namFwd)  # 1 differs

namFull <- replace(namFull,1,'Price')

namFull

final <- final[,which((names(final) %in% namFull) == TRUE)]

str(final)

# Change data type for categorical variables
final <- final %>% mutate(across(-c(Price,Duration,ArrivalTime,DepartureTime),as.factor))

##Split test and train
sample = sample.split(final$Price, SplitRatio = .75)
train = subset(final, sample == TRUE)
test  = subset(final, sample == FALSE)
#dim(train)
#dim(test)

str(train)
#summary(train)
#str(test)
#train = na.omit(train)

x.train <- as.matrix(train[, 2:23])
y.train <- train[, 1]

head(x.train)
head(y.train)


x.test <- as.matrix(test[, 2:23])
y.test <- test[, 1]

#head(x.train)
#head(y.train)


#test <- subset(test, select= -c(Price))


## Random Forest

set.seed(4543)
rftrain <- randomForest(                    
  Price ~ .,
  data=train,
  importance=TRUE,  #proximity=FALSE, ntree=500, keepForest=TRUE
  do.trace = FALSE
)


set.seed(4543)
rftest <- randomForest(
                   Price ~ .,
                   data=test,
                   importance=TRUE,  #proximity=FALSE, ntree=500, keepForest=TRUE
                   do.trace = FALSE
)

rftrain
rftest
pred = predict(rftrain, newdata=test)

RMSE <- sqrt(mean((test$Price - pred)^2))
RMSE


#VarImp

imp <- importance(rftrain)
imp
varImpPlot(rftrain,cex=.8)

pred

plot(rftrain)

table(pred,test$Price)


# Plot results

results <- cbind(pred,test$Price)

colnames(results)<-c('pred','act')

results<-as.data.frame(results)

#View(results)
head(results)


ggplot(results,aes(x = pred, y = act)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  labs(x = "Predicted Price",
       y = "Actual Price",title = "Actual vs Model")

hist(results$act, breaks=30, freq= FALSE, main="Actual v Model", xlab="Price", xlim=c(0,100000),ylim=c(0,.00015),col=rgb(0,0,1,1/4))
#dens <- density(pred)
lines(density(results$pred))
legend("topright", c("Actual"), fill=rgb(0,0,1,1/4))


str(results)

# Trying out less variables
rtrain21 <- subset(final, select= -c(16))
str(rftrain21)
set.seed(1)
rftrain21 <- randomForest(Price ~ .,data=rtrain21,importance=TRUE,do.trace = FALSE)
rftrain21
pred21 = predict(rftrain21, newdata=test)

RMSE21 <- sqrt(mean((test$Price - pred21)^2))
RMSE21


# 200 trees
set.seed(1)
rftrain21tree <- randomForest(Price ~ .,data=rtrain21,ntree=200,do.trace = FALSE)
rftrain21tree
pred21tree = predict(rftrain21tree, newdata=test)
RMSE21tree <- sqrt(mean((test$Price - pred21tree)^2))
RMSE21tree

# bagging
set.seed(1)
rftrain21treebag <- randomForest(Price ~ .,data=rtrain21,mtree=200,ntry=21,do.trace = FALSE)
rftrain21treebag
pred21treebag = predict(rftrain21treebag, newdata=test)
RMSE21treebag <- sqrt(mean((test$Price - pred21treebag)^2))
RMSE21treebag
