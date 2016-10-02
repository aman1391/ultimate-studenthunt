################
X_train<- read.csv("Train.csv")
X_test<- read.csv("Test.csv")
###########3
X_All<- plyr::rbind.fill(X_train,X_test)
############
X_All$Date<- lubridate::dmy(X_All$Date)
X_All$wday<- lubridate::wday(X_All$Date)
X_All$month<- lubridate::month(X_All$Date)
X_All$day<- lubridate::day(X_All$Date)
################
X_All$Date<- NULL
X_All[is.na(X_All)]<- 0
#######
X_All$ID<- NULL
############3
c<- cor(X_All[,-16])
High<- caret::findCorrelation(c,cutoff = 0.99)
High
X_All<- X_All[,-c(High+1)]
boxplot(X_All$Average_Breeze_Speed)
summary(X_All$Average_Breeze_Speed)
X_All$Average_Breeze_Speed<- X_All$Average_Breeze_Speed/7.6
##
X_All$Average_Breeze_Speed[X_All$Average_Breeze_Speed>=10]<- mean(X_All$Average_Breeze_Speed)
###########
hist(X_All$Average_Breeze_Speed)
boxplot(X_All$Max_Breeze_Speed)
X_All$Max_Breeze_Speed<- X_All$Max_Breeze_Speed/7.6
X_All$Max_Breeze_Speed[X_All$Max_Breeze_Speed>=10]<- mean(X_All$Max_Breeze_Speed)
#################
X_All$Min_Breeze_Speed<- X_All$Min_Breeze_Speed/7.6
boxplot(X_All$Min_Breeze_Speed)
X_All$Min_Breeze_Speed[X_All$Min_Breeze_Speed>=5]<- mean(X_All$Min_Breeze_Speed)
boxplot(X_All$Var1)
X_All$Var1<- X_All$Var1/0.83
###########linear line fitting of var1
boxplot(X_All$Average_Atmospheric_Pressure)
summary(X_All$Average_Atmospheric_Pressure)
X_All$Average_Atmospheric_Pressure<- NULL
X_All$Min_Ambient_Pollution<- X_All$Min_Ambient_Pollution/4
X_All$Max_Ambient_Pollution<- X_All$Max_Ambient_Pollution/4
boxplot(X_All$Min_Ambient_Pollution)
boxplot(X_All$Max_Ambient_Pollution)
X_All$ave_1<- (X_All$Max_Ambient_Pollution+X_All$Min_Ambient_Pollution)/2
#X_All$Min_Ambient_Pollution<- NULL
#X_All$Max_Ambient_Pollution<- NULL
######
boxplot(X_All$Average_Moisture_In_Park)
X_All$Average_Moisture_In_Park<- X_All$Average_Moisture_In_Park/3
X_All$Average_Moisture_In_Park[X_All$Average_Moisture_In_Park<=60]<- mean(X_All$Average_Moisture_In_Park)
boxplot(X_All$Max_Moisture_In_Park)
X_All$Max_Moisture_In_Park<- X_All$Max_Moisture_In_Park/3
X_All$Max_Moisture_In_Park[X_All$Max_Moisture_In_Park<80]<- mean(X_All$Max_Moisture_In_Park)
######

X_All$Min_Moisture_In_Park<- X_All$Min_Moisture_In_Park/3
X_All$Min_Moisture_In_Park[X_All$Min_Moisture_In_Park<20]<- mean(X_All$Min_Moisture_In_Park)
boxplot(X_All$Min_Moisture_In_Park)
boxplot(X_All$Var1)
summary(X_All$Var1)
##
#X_All$Var1[X_All$Var1>=0]<- mean(X_All$Var1)
hist(X_All$Var1)
#########
X_train_1<- X_All[1:nrow(X_train),]
##########
L_1<- lm(Var1~.-(Footfall),data = X_train_1)
summary(L_1)
X_train_1$Var1<- predict(L_1,X_train_1)
X_test_1<- X_All[-(1:nrow(X_train)),]
X_test_1$Var1<- predict(L_1,X_test_1)


boxplot(X_train_1$Var1)
X_train_1$Var1[X_train_1$Var1<0]<- mean(X_train_1$Var1)
##############
boxplot(X_test_1$Var1)
X_test_1$Var1[X_test_1$Var1<0]<- mean(X_test_1$Var1)

X_test_1$Var1[X_test_1$Var1>=40]<- mean(X_test_1$Var1)

L_11<- rpart::rpart(Footfall~.,data = X_train_1)
summary(L_11)
caret::RMSE(predict(L_11,X_train_1),X_train_1$Footfall)
hist(X_All$Average_Breeze_Speed)
hist(X_All$Max_Breeze_Speed)
hist(X_All$Min_Breeze_Speed)
#############xgboost
library(xgboost)
y<-X_train_1$Footfall
X_train_1$Footfall<- NULL
#X_train_1$month<- as.factor(X_train_1$month)
set.seed(767)
model_xgb_cv <- xgb.cv(data=data.matrix(X_train_1), label=data.matrix(y), objective="reg:linear", nfold=5, nrounds=130, eta=0.06, max_depth=8, subsample=0.950, colsample_bytree=0.98, min_child_weight=1, eval_metric="rmse")
model_xgb <- xgboost(data=data.matrix(X_train_1), label=data.matrix(y), objective="reg:linear", nrounds=400, eta=0.06585, max_depth=5, subsample=0.99989, colsample_bytree=0.99991, min_child_weight=400, eval_metric="rmse")
X_test_1$Footfall<- NULL
# model scoring

pred_2 <- predict(model_xgb, data.matrix(X_test_1))
write.csv(data.frame(ID=X_test$ID , Footfall=pred_2),'se_49.csv',row.names = F)
X_train_1$Footfall<- y
X_test_1$Footfall<- 0
#########
X_train_1$pre_l<- NULL
X_test_1$pre_l<- NULL
L_1<- lm(Footfall~.,data = X_train_1)
summary(L_1)
boxplot(X_All$Direction_Of_Wind)
############
X_train_1$pre_l<- predict(L_11,X_train_1)
X_test_1$pre_l<- predict(L_11,X_test_1)
############
importance_matrix <- xgb.importance(names(X_train_1), model = model_xgb)
# Nice graph
xgb.plot.importance(importance_matrix)
importance_matrix
#######
X_All<- rbind(X_train_1,X_test_1)
############
boxplot(X_All$pre_l)
colnames(X_All)
X_All_D<- dummies::dummy.data.frame(X_All,names = c("month","day","wday"),sep="_")
###############
X_train_1<- X_All_D[1:nrow(X_train_1),]
X_test_1<- X_All_D[-(1:nrow(X_train_1)),]
boxplot(X_All$Average_Breeze_Speed)
X_All$Average_Breeze_Speed[X_All$Average_Breeze_Speed>8.5]<- mean(X_All$Average_Breeze_Speed)
boxplot(X_All$Max_Breeze_Speed)
X_All$Max_Breeze_Speed[X_All$Max_Breeze_Speed<2]<- mean(X_All$Max_Breeze_Speed)
boxplot(X_All$Min_Breeze_Speed)
boxplot(X_All$Min_Ambient_Pollution)
boxplot(X_All$Max_Ambient_Pollution)
boxplot(X_All$Average_Moisture_In_Park)
boxplot(X_All$Max_Moisture_In_Park)
X_All$Max_Moisture_In_Park[X_All$Max_Moisture_In_Park<87.5]<- mean(X_All$Max_Moisture_In_Park)
boxplot(X_All$Min_Moisture_In_Park)
X_All$Min_Moisture_In_Park[X_All$Min_Moisture_In_Park<37.5]<- mean(X_All$Min_Moisture_In_Park)
boxplot(X_All$wday)
##########
s<- read.csv("se_49_F.csv")
s_1<- read.csv("se_49.csv")
s_3<- data.frame(ID=X_test$ID,Footfall=(s$Footfall*s_1$Footfall)^0.5)
write.csv(s_3,'trial_49.csv',row.names = F)
