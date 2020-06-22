#CSV compilation--------------------------------------------------

d3=read.csv('G:/Transportation Analytics/project/trip_data/JC-201903-citibike-tripdata.csv', header=TRUE)
d4=read.csv('G:/Transportation Analytics/project/trip_data/JC-201904-citibike-tripdata.csv', header=TRUE)
d5=read.csv('G:/Transportation Analytics/project/trip_data/JC-201905-citibike-tripdata.csv', header=TRUE)
d6=read.csv('G:/Transportation Analytics/project/trip_data/JC-201906-citibike-tripdata.csv', header=TRUE)
d7=read.csv('G:/Transportation Analytics/project/trip_data/JC-201907-citibike-tripdata.csv', header=TRUE)
d8=read.csv('G:/Transportation Analytics/project/trip_data/JC-201908-citibike-tripdata.csv', header=TRUE)
d9=read.csv('G:/Transportation Analytics/project/trip_data/JC-201909-citibike-tripdata.csv', header=TRUE)
d10=read.csv('G:/Transportation Analytics/project/trip_data/JC-201910-citibike-tripdata.csv', header=TRUE)
d11=read.csv('G:/Transportation Analytics/project/trip_data/JC-201911-citibike-tripdata.csv', header=TRUE)
d12=read.csv('G:/Transportation Analytics/project/trip_data/JC-201912-citibike-tripdata.csv', header=TRUE)
d13=read.csv('G:/Transportation Analytics/project/trip_data/JC-202001-citibike-tripdata.csv', header=TRUE)
d14=read.csv('G:/Transportation Analytics/project/trip_data/JC-202002-citibike-tripdata.csv', header=TRUE)
d3$date = as.Date(d3$starttime, format = "%Y-%m-%d")
d4$date = as.Date(d4$starttime, format = "%Y-%m-%d")
d5$date = as.Date(d5$starttime, format = "%Y-%m-%d")
d6$date = as.Date(d6$starttime, format = "%Y-%m-%d")
d7$date = as.Date(d7$starttime, format = "%Y-%m-%d")
d8$date = as.Date(d8$starttime, format = "%Y-%m-%d")
d9$date = as.Date(d9$starttime, format = "%Y-%m-%d")
d10$date = as.Date(d10$starttime, format = "%Y-%m-%d")
d11$date = as.Date(d11$starttime, format = "%Y-%m-%d")
d12$date = as.Date(d12$starttime, format = "%Y-%m-%d")
d13$date = as.Date(d13$starttime, format = "%Y-%m-%d")
d14$date = as.Date(d14$starttime, format = "%Y-%m-%d")
zfull=rbind(d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)
sum(is.na(zfull))
write.csv(zfull,'G:\\Transportation Analytics\\project\\trip_data\\zfull.csv',row.names = FALSE)

# Part1 (station extract)-------------------------------------------------------------------
full=read.csv('G:/Transportation Analytics/project/trip_data/zfull.csv', header=TRUE)
library(dplyr)
sum(is.na(full))
colnames(full)
#unique checkout count with freq
length(unique(full$start.station.id))
outtable=table(full$start.station.id)
bike_out=as.data.frame(outtable)

#unique checkin count with freq
intable=table(full$end.station.id)
bike_in=as.data.frame(intable)

#merge checkin/checkout and add total
stationcount=merge(bike_out, bike_in, by="Var1", all=TRUE)
colnames(stationcount) <- c("station","checkout","checkin")
stationcount[is.na(stationcount)] <- 0
stationcount$total=stationcount$checkout+stationcount$checkin
#descending total
stationcount=stationcount[with(stationcount,order(-total)),]

#station with total above 20000
newstat=subset(stationcount,checkout>10000)
barplot(newstat$checkout,main = "Checkout Distribution in Stations",xlab = "Station ID"
        ,ylab = "Frequency",names.arg=newstat$station)
fulln=subset(full, start.station.id %in% newstat$station)
percent = round(nrow(fulln)/nrow(full), 3) * 100
#print("Of the "" total trips, {nrow(fulln)} involve one of our 26 stations. That's {}%")

# Part2 (weather merge)-------------------------------------------------------------------

darksky=read.csv('G:/Transportation Analytics/project/dark_sky.csv', header=TRUE)
fulln_model=fulln[,c("starttime","stoptime","start.station.id","end.station.id","date")]

#extracting date,month,year,time
fulln_model$month = months(as.Date(fulln_model$date))
fulln_model$day = weekdays(as.Date(fulln_model$date))
tm1.lt <- as.POSIXlt(fulln$starttime)
fulln_model$hour=tm1.lt$hour
str(darksky)
x#extracting date,month,year,time for weather
darksky$date = as.Date(darksky$time, format = "%Y-%m-%d")
tm2.lt <- as.POSIXlt(darksky$time)
darksky$hour=tm2.lt$hour
colnames(darksky)
#remove unwanted columns weather
darksky=subset(darksky, select=-c(summary,icon,precipIntensity,
                                  precipProbability,precipType,precipAccumulation,
                                  ozone,uvIndex,windGust,windBearing,cloudCover,apparentTemperature))
darksky=darksky[!duplicated(darksky$time), ]

#Merging weather with model
darksky$dthr=paste(darksky$date,darksky$hour, sep="")
fulln_model$dthr=paste(fulln_model$date,fulln_model$hour, sep="")
fi=merge(fulln_model,darksky,by.x = "dthr",by.y = "dthr")

colnames(fi)
names(fi)[6] <- "date"
names(fi)[9] <- "hour"
fi=fi[,-c(17,18)]
sum(is.na(fi))

# Part3 (data grouping)-------------------------------------------------------------------
library(dplyr)
fi_from_counts=fi %>% group_by(start.station.id,day,hour) %>% count
colnames(fi_from_counts)[4] <- "checkout_count"

checkouts_model=fi %>% group_by(start.station.id,dthr) %>% count
colnames(checkouts_model)[3] <- "checkout_count_hr"
###
colnames(checkouts_model)[3] <- "Checkout Count"
colnames(checkouts_model)[1] <- "Station ID"
darksky$month = months(as.Date(darksky$date))
darksky=darksky[,c(4,1,2,3,5,6)]
darksky=darksky[1427,]
checkouts_model=checkouts_model[1,]
checkouts_model=merge(checkouts_model,darksky,by.x = "dthr",by.y = "dthr")
###
checkouts_model=merge(checkouts_model,darksky,by.x = "dthr",by.y = "dthr")

checkouts_model$month = months(as.Date(checkouts_model$date))
checkouts_model$day = weekdays(as.Date(checkouts_model$date))
colnames(checkouts_model)
#removing unwanted col
checkouts_model=subset(checkouts_model, select=-c(dthr,time,date))
sum(is.na(checkouts_model))
#rearanging col
checkouts_model=checkouts_model[,c(2,1,10,9,11,3:8)]
length(unique(checkouts_model$start.station.id))
#changing cols to factor
str(checkouts_model)
checkouts_model$start.station.id <- as.factor(checkouts_model$start.station.id)
checkouts_model$month <- as.factor(checkouts_model$month)
checkouts_model$hour <- as.factor(checkouts_model$hour)
checkouts_model$day <- as.factor(checkouts_model$day)
write.csv(checkouts_model,'G:\\Transportation Analytics\\project\\checkouts_model.csv',row.names = FALSE)
# Part4 (data transformation) --------------------------------------------
checkouts_model=read.csv('G:/Transportation Analytics/project/checkouts_model.csv', header=TRUE)
library(moments)
barplot(table(checkouts_model$count_log),xlab="Checkout count per hour",ylab="Frequency",main="Frequency of Checkouts")
#skewness check
skew=skewness(checkouts_model$checkout_count_hr)
data_log=skewness(log(checkouts_model$checkout_count_hr))
data_sqrt=skewness((checkouts_model$checkout_count_hr)**0.5)
data_cube=skewness((checkouts_model$checkout_count_hr)**(1/3))

checkouts_model$count_log=log(checkouts_model$checkout_count_hr)

hist(checkouts_model$checkout_count_hr)
# Modeling ----------------------------------------------------------------
#test, train split
# # 80% of the sample size

#before removing outliers
checkouts_model=read.csv('checkouts_model.csv', header=TRUE)
str(checkouts_model)
hist(checkouts_model$checkout_count_hr)
colnames(checkouts_model)
#reaaranging and removing cols
checkouts_model=checkouts_model[,c("checkout_count_hr" ,"start.station.id",
                                   "hour","day","month","temperature")]

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}
###
smp_size <- floor(0.80 * nrow(checkouts_model))
checkouts_model$start.station.id <- as.factor(checkouts_model$start.station.id)
checkouts_model$hour <- as.factor(checkouts_model$hour)
str(checkouts_model)
train_ind <- sample(seq_len(nrow(checkouts_model)), size = smp_size, replace=FALSE)
train <- checkouts_model[train_ind, ]
test <- checkouts_model[-train_ind, ]
colnames(train)
str(train)

# Linear reg --------------------------------------------------------------

linreg = lm(checkout_count_hr~.,data=train)
summary(linreg)
pred_lr=round(predict(linreg,train))
eval_results(train$checkout_count_hr,pred_lr,train)


# bosting -----------------------------------------------------------------

library(gbm)
gbmboosting <- gbm(checkout_count_hr~.,data =  train, n.trees=500, interaction.depth = 3,shrinkage = 0.05)
gbmpred <- round(predict(gbmboosting, newdata = train, n.trees = 500))

#mean(gbmpred==train$checkout_count_hr)
summary(gbmboosting)
eval_results(train$checkout_count_hr,gbmpred,train)

# desion tree -------------------------------------------------------------

library(rpart)
fit_tree<- rpart(checkout_count_hr~., data = train, method = 'anova')
plot(fit_tree)
text(fit_tree)
q=as.data.frame(sort(fit_tree$variable.importance,decreasing = T))
treepred=round(predict(fit_tree,train))
eval_results(train$checkout_count_hr,treepred,train)


# random forest -----------------------------------------------------------

library(randomForest)
rfx <- train[,2:6]
rfy <- train[,1]
rfmodel <- randomForest(rfx, rfy)
rfpred=predict(rfmodel,test)
rfpred=round(rfpred)
sst <- sum((rfpred - mean(train$checkout_count_hr))^2)
sse <- sum((rfpred - train$checkout_count_hr)^2)
rsq <- 1 - sse / sst
rsq
sort(importance(rfmodel),decreasing = T)
varImpPlot(rfmodel)
mean(rfpred == train$checkout_count_hr)
eval_results(train$checkout_count_hr,rfpred,test)


# ridge reg ---------------------------------------------------------------

library(tidyverse)
library(broom)
library(glmnet)
lambdas <- 10^seq(3, -2, by = -.1)
x <- train %>% select(start.station.id,month,
                      hour,day,temperature) %>% data.matrix()
y=train$checkout_count_hr
ridmod <- glmnet(x, y, alpha = 0, lambda = lambdas)
summary(ridmod)
cv_ridmod <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_ridmod)
opt_lambda <- cv_ridmod$lambda.min
opt_lambda
coef(cv.lasso, cv.lasso$lambda.min)
ridfit <- cv_ridmod$glmnet.fit
summary(ridfit)
x1 <- test %>% select(start.station.id,month,
                      hour,day,dayNight,icon,temperature,humidity) %>% data.matrix()
y1= test$checkout_count_hr
ridge_predicted <- round(predict(ridfit, s = opt_lambda, newx = x))

ridge_predicted[75]
train$checkout_count_hr[75]

eval_results(train$checkout_count_hr,ridge_predicted,train)
#Lasso----
library(tidyverse)
library(broom)
library(glmnet)
x <- train %>% select(start.station.id,month,
                      hour,day,temperature) %>% data.matrix()
y=train$checkout_count_hr
cv.lasso <- cv.glmnet(x, y, alpha = 1)
lasmod <- glmnet(x, y, alpha = 1,lambda = cv.lasso$lambda.min)
coef(lasmod,cv.lasso$lambda.min)
#cv.lasso$lambda.1se
x1 <- test %>% select(start.station.id,month,
                      hour,day,temperature) %>% data.matrix()
y1= test$checkout_count_hr
lasso_predicted <- round(predict(lasmod, s = cv.lasso$lambda.min, newx = x1))

lasso_predicted[75]
test$checkout_count_hr[75]

eval_results(test$checkout_count_hr,lasso_predicted,test)

#SVM----
library(e1071) 
classifier = svm(formula = checkout_count_hr ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 
y_pred = as.integer(predict(classifier, newdata = test[-1]))
y_pred=as.integer(y_pred)
eval_results(test$checkout_count_hr,y_pred,test)
table(y_pred, test$checkout_count_hr)
str(test)
train[4,6] = scale(train[4,6])
str(train)
plot(test$checkout_count_hr,y_pred)

#after outliers removed
checkouts_model=read.csv('checkouts_outlier.csv', header=TRUE)
colnames(checkouts_model)
#reaaranging and removing cols
checkouts_model=checkouts_model[,c("checkout_count_hr" ,"start.station.id",
                                   "hour","day","month","temperature")]

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}
###
smp_size <- floor(0.80 * nrow(checkouts_model))
checkouts_model$start.station.id <- as.factor(checkouts_model$start.station.id)
checkouts_model$hour <- as.factor(checkouts_model$hour)
str(checkouts_model)
train_ind <- sample(seq_len(nrow(checkouts_model)), size = smp_size, replace=FALSE)
train <- checkouts_model[train_ind, ]
test <- checkouts_model[-train_ind, ]
colnames(train)
str(train)

# Linear reg --------------------------------------------------------------

linreg = lm(checkout_count_hr~.,data=train)
summary(linreg)
pred_lr=round(predict(linreg,train))
eval_results(train$checkout_count_hr,pred_lr,train)


# bosting -----------------------------------------------------------------

library(gbm)
gbmboosting <- gbm(checkout_count_hr~.,data =  train, n.trees=2000,cv.folds = 5,
                   distribution="gaussian",interaction.depth = 9,shrinkage=0.09,bag.fraction = 1.15)
best.iter <- gbm.perf(gbmboosting, method = "cv")
gbmpred <- round(predict(gbmboosting, newdata = test, n.trees = best.iter))

#mean(gbmpred==train$checkout_count_hr)
summary(gbmboosting)
eval_results(test$checkout_count_hr,gbmpred,test)

best.iter <- gbm.perf(gbmboosting, method = "cv")
print(best.iter)

# Plot relative influence of each variable
par(mfrow = c(1, 1))
summary(gbmboosting, n.trees = 1)          # using first tree
summary(gbmboosting, n.trees = best.iter)  # using estimated best number of trees
print(pretty.gbm.tree(gbmboosting, i.tree = 1))
print(pretty.gbm.tree(gbmboosting, i.tree = gbmboosting$n.trees))

# Construct univariate partial dependence plots
p1 <- plot(gbmboosting, i.var = 1, n.trees = best.iter)
p2 <- plot(gbmboosting, i.var = 2, n.trees = best.iter)
grid.arrange(p1, p2, ncol = 2)
# desion tree -------------------------------------------------------------

library(rpart)
fit_tree<- rpart(checkout_count_hr~., data = train, method = 'anova')
plot(fit_tree)
text(fit_tree)
q=as.data.frame(sort(fit_tree$variable.importance,decreasing = T))
treepred=round(predict(fit_tree,train))
eval_results(train$checkout_count_hr,treepred,train)


# random forest -----------------------------------------------------------

library(randomForest)
rfx <- train[,2:6]
rfy <- train[,1]
rfmodel <- randomForest(rfx, rfy)
rfpred=predict(rfmodel,test)
rfpred=round(rfpred)
sst <- sum((rfpred - mean(train$checkout_count_hr))^2)
sse <- sum((rfpred - train$checkout_count_hr)^2)
rsq <- 1 - sse / sst
rsq
sort(importance(rfmodel),decreasing = T)
varImpPlot(rfmodel)
mean(rfpred == train$checkout_count_hr)
eval_results(train$checkout_count_hr,rfpred,test)


#Ridge ---------------------------------------------------------------

library(tidyverse)
library(broom)
library(glmnet)
lambdas <- 10^seq(3, -2, by = -.1)
x <- train %>% select(start.station.id,hour,
                      day,month,temperature) %>% data.matrix()
y=train$checkout_count_hr
cv_ridmod <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_ridmod)
ridfit <- cv_ridmod$glmnet.fit
summary(ridfit)
x1r <- test %>% select(start.station.id,hour,
                       day,month,temperature) %>% data.matrix()
y1r= test$checkout_count_hr
ridge_predicted <- round(predict(ridfit, s = cv_ridmod$lambda.min, newx = x1r))

ridge_predicted[75]
test$checkout_count_hr[75]

eval_results(test$checkout_count_hr,ridge_predicted,test)
#Lasso----
library(tidyverse)
library(broom)
library(glmnet)
x <- train %>% select(start.station.id,month,
                      hour,day,temperature) %>% data.matrix()
y=train$checkout_count_hr
cv.lasso <- cv.glmnet(x, y, alpha = 1)
lasmod <- glmnet(x, y, alpha = 1,lambda = cv.lasso$lambda.min)
coef(lasmod,cv.lasso$lambda.min)
cv.lasso$lambda.min
x1 <- test %>% select(start.station.id,month,
                      hour,day,temperature) %>% data.matrix()
y1= test$checkout_count_hr
lasso_predicted <- round(predict(lasmod, s = cv.lasso$lambda.min, newx = x1))

eval_results(test$checkout_count_hr,lasso_predicted,test)

