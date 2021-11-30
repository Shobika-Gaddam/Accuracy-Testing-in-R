library(ISLR)
dim(Auto)
Auto[1:392,]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
plot(cylinders,mpg)
plot(Auto$cylinders,Auto$mpg)
attach(Auto) #directly masked to the global environment. No need to give $ attachments.
cylinders=as.factor(cylinders)
plot(cylinders,mpg)
plot(cylinders,mpg,col="red")
plot(cylinders,mpg,col="red",varwidth=T)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T,ylab="MPG")
plot(cylinders,mpg,col="red",varwidth=T,ylab="MPG",xlab="CYLINDERS")
hist(mpg)
hist(mpg,col=3)
hist(mpg,col=3,breaks=15)
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,Auto)
plot(horsepower,mpg)
summary(Auto)

library(caret)
data("Auto")
head(Auto)
nrow(Auto)
ncol(Auto)

df<-data("Auto")
df<-Auto
dim(df)
names(df)

validation_index<-createDataPartition(df$horsepower,p=0.70,list=FALSE)
validation<-df[-validation_index,]
df<-df[validation_index,]
dim(df)

sapply(df,class)
head(df)
levels(df$horsepower)
percentage<-prop.table(table(df$horsepower))*100
cbind(freq=table(df$horsepower),percentage=percentage)
summary(df)

x<-df[,1:4]
y<-df[,9]
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(x[,i],main=names(Auto)[i])
}

plot(y) 
#featurePlot(x=x,y=y,plot="ellipse")

featurePlot(x=x,y=y,plot = "box")

scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)
control<-trainControl(method="cv",number=10)
metric="none"

#Linear Regression
set.seed(7)
fit.lm <- train(horsepower~., data=df, method="lm", metric=metric, trControl=control)
print(fit.lm)
predictions <- predict(fit.lm, validation)
r21 = R2(validation$horsepower, predictions, form = "traditional")
print(r21)

#SVM regression
library(e1071)
model_reg = svm(horsepower~., data=df)
print(model_reg)
pred = predict(model_reg, validation)
print(pred)
r2 = R2(validation$horsepower, pred, form = "traditional")
print(r2)

#Random forest
fit.rf <- train(horsepower~., data=df, method="rf", metric=metric, trControl=control)
predictions1 <- predict(fit.rf, validation)
r22 = R2(validation$horsepower, predictions1, form = "traditional")
print(r22)

#Decision Tree Regression
library(rpart)
fit.rpart <- train(horsepower~., data=df, method="rpart", metric=metric, trControl=control)
predictions2 <- predict(fit.rpart, validation)
r23 = R2(validation$horsepower, predictions2, form = "traditional")
print(r23)

cat("Accuracy_SVM",r2,"\n","Accuracy_linear_regression",r21,"\n","Accuracy_random_forest_regression",r22,"\n","Accuracy_decision_regression",r23)

