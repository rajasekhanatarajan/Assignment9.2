# Assignment9.2
Use the below given data set

DataSet

Perform the below given activities:

a. Create classification model using different decision trees.

b. Verify model goodness of fit.

c. Apply all the model validation techniques.

d. Make conclusions

Ans 2 a -> library(caret)

build <- read.csv("weightlifting.csv")

test <- read.csv("weightlifting.csv")

dim(build)

dim(test)

build[,1:100] <- sapply(build[,1:100],as.numeric)

test[,1:100] <- sapply(test[,1:100], as.numeric)

build <- build[2:101]

test <- test[2:101]

library(Amelia)

missmap(test, main = "Missingness Map Test")

nas <- is.na(apply(test,2,sum))

test <- test[,!nas]

dim(test)

build <- build[,!nas]

inTrain <- createDataPartition(y=build$classe, p=0.10, list=FALSE)

train <- build[inTrain,]

val <- build[-inTrain,]

rm(inTrain,nas,build)

nas <- is.na(apply(test,2,sum))

test <- test[,!nas]

dim(test)

build <- build[,!nas]

inTrain <- createDataPartition(y=build$classe, p=0.10, list=FALSE)

train <- build[inTrain,]

val <- build[-inTrain,]

rm(inTrain,nas,build)

pred <- predict(Mod0, val)

cm <- confusionMatrix(pred, val$classe)

cm$table

library(knitr)

set.seed(123)

random forest model

system.time(Mod1 <- train(classe ~ ., method = "rf",

data = train, importance = T,

trControl = trainControl(method = "cv", number = 3)))

save(Mod1,file="Mod1.RData")

load("Mod1.RData")

Mod1$finalModel

vi <- varImp(Mod1)

vi$importance[1:10,]

pred1 <- predict(Mod1, val)

cm1 <- confusionMatrix(pred1, val$classe)

library(pROC)

pred1.prob <- predict(Mod1, val, type="prob")

pred1.prob$

roc1 <- roc(val$total_accel_belt, pred1.prob$E)

plot(roc1, print.thres="best", print.thres.best.method="closest.topleft")

coord1 <- coords(roc1, "best", best.method="closest.topleft", ret=c("threshold", "accuracy")) coord1

plot(Mod1)

plot(varImp(Mod1), top = 10)

load("Mod2.RData")

pred2 <- predict(Mod2, val)

cm2 <- confusionMatrix(pred2, val$classe)

cm2$overall

load("gmbFit.RData")

plot(gmbFit)

plot(gmbFit,plotType = "level")

resampleHist((gmbFit))

predgmb <- predict(gmbFit, val)

cmgmb <- confusionMatrix(pred2, val$classe)

cmgmb$overall

load("Mod3.RData")

pred3 <- predict(Mod3, val)

cm3 <- confusionMatrix(pred3, val$classe)

cm3$overall

varImp(Mod3)

plot(varImp(Mod3), top = 10)

re <- data.frame(Tree=cm0$overall[1], rf=cm1$overall[1], boosting=cm2$overall[1], bagging=cm3$overall[1]) library(knitr) re par(mfrow=c(2,2))

plot(cm0$byClass, main="classification tree", xlim=c(0.4, 1.005), ylim=c(0.7,1))

text(cm0$byClass[,1]+0.04, cm0$byClass[,2], labels=LETTERS[1:5], cex= 0.7)

plot(cm1$byClass, main="random forest", xlim=c(0.96, 1.005))

text(cm1$byClass[,1]+0.003, cm1$byClass[,2], labels=LETTERS[1:5], cex= 0.7)

plot(cm2$byClass, main="boosting", xlim=c(0.93, 1.001))

text(cm2$byClass[,1]+0.005, cm2$byClass[,2], labels=LETTERS[1:5], cex= 0.7)

plot(cm3$byClass, main="bagging", xlim=c(0.97, 1.005))

text(cm3$byClass[,1]+0.003, cm3$byClass[,2], labels=LETTERS[1:5], cex= 0.7)

test$classe <- as.character(predict(Mod1, test))

weightlifting_write_files = function(x){ n = length(x) for(i in 1:n){ filename = paste0("./predict/problem_id_", i, ".txt") write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE) } }

Mod1$finalModel #In conclusion, we compare all four methods: classification trees, random forest, booting trees and bagging. So, we found random forest as our prediction model due to better accuracy.
