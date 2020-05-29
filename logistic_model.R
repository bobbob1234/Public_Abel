library(DescTools)
library(mefa)
library(splitstackshape)
library(glmnet)
library(plotmo) # for plotres
library(fastDummies)
library(FeatureHashing)
library(caret)
library(e1071)
library(neuralnet)
library(GGally)
library(data.table)
setwd("C:/Users/White/abel")
train <- list.files()
temp = list.files(pattern  =  "train")
myfiles = lapply(temp, fread)
myfiles <- do.call(rbind.data.frame,myfiles)

train <- myfiles
train$rules <- gsub(".y","",train$rules)
x <- 1:nrow(train)
train2 <- cbind(train,x)
colnames(train2) <- c("rules","support","confidence","lift","count","FLAG","id")
train3 <- train2[,c(1,5,6,7)]
head(train3)
tail(train3)
train3 <- train3[order(-count),]

#train3$count <- NULL
head(train3)
#### Process Rules into readable format ####
w_hiddenlayers <- train3
w_hiddenlayers$rules <- as.character(w_hiddenlayers$rules)
w_hiddenlayers$rules2 <- gsub("*V","", w_hiddenlayers$rules) 
w_hiddenlayers$rules2 <- gsub('[0-9]+', '', w_hiddenlayers$rules2)
w_hiddenlayers2 <- w_hiddenlayers[,c(1,2,3,4)]
w_hiddenlayers2$blank <- ""
w_hiddenlayers2$blank <- NULL
w_hiddenlayers2 <- as.data.frame(w_hiddenlayers2)
w_hiddenlayers3 <- w_hiddenlayers2
train_processed <- w_hiddenlayers3
train_processed$id <- NULL
micro_dby <- train_processed
rule_id <- "rule-"
number_id <- 1:nrow(micro_dby)
id_merger <- paste(rule_id,number_id)
id_merger <- gsub(" ","",id_merger)
micro_dby<- cbind(micro_dby,id_merger)
micro_dby2 <- micro_dby
micro_dby$id_merger <- NULL
micro_dby<- micro_dby[!duplicated(micro_dby[c("rules","FLAG")]),]
micro_dby$FLAG <- as.factor(micro_dby$FLAG) 
micro_dby_model_train <- micro_dby
#micro_dby_model_train <- stratified(micro_dby,"FLAG",0.001)
rm(train,train2,train3,w_hiddenlayers,w_hiddenlayers2,w_hiddenlayers3,id_merger,number_id,x,train_processed,myfiles)

df.expanded <- micro_dby_model_train[rep(seq(nrow(micro_dby_model_train)), micro_dby_model_train$count),]
df.expanded$count <- NULL
micro_dby <- df.expanded %>% as.data.frame()
table(micro_dby$FLAG)
flag_tables <- table(micro_dby$FLAG) %>% as.vector()
total <- sum(flag_tables)
high_value_percent <- flag_tables[2] / total * 100 %>% round()
low_value_percent <- 100 - high_value_percent
macro_start

global_db
micro_dby$FLAG <- ifelse(micro_dby$FLAG == "Y",1,0)
Index <- sample(1:nrow(micro_dby), 0.7*nrow(micro_dby))  # row indices for training data
micro_dby_train <- micro_dby[Index, ]  # model training data
micro_dby_test <- micro_dby[-Index, ]   # model test data
test_flags <- micro_dby_test$FLAG
db_hash <- micro_dby %>% as.data.frame()
predictorNames <- setdiff(names(db_hash),"FLAG")
predictor_ensemble <- "FLAG"
# change all NAs to 0

objTrain <-db_hash[Index,] %>% as.data.frame()
objTrain2 <- objTrain[,predictorNames] %>% as.data.frame()

objTest <- db_hash[-Index,] %>% as.data.frame()
objTest2 <- objTest[,predictorNames] %>% as.data.frame()

objTrain_hashed = hashed.model.matrix(~., data=objTrain2, hash.size=2^12, transpose=FALSE)
objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
objTest_hashed = hashed.model.matrix("~.", data=objTest2, hash.size=2^12, transpose=FALSE)
objTest_hashed = as(objTest_hashed, "dgCMatrix")





## Builiding Logistic Model
glmnetModel <- cv.glmnet(objTrain_hashed, objTrain[,"FLAG"], 
                         family = "binomial", type.measure = "class")

glmnetPredict <- predict(glmnetModel, objTest_hashed, s="lambda.min",type = "class")
glmnetPredict <- ifelse(glmnetPredict == "Y",1,0)

test_table <- 1:nrow(objTest) %>% as.data.frame()
test_table$Predictions <- glmnetPredict
test_table$Actuals <- test_flags
test_table$difference <- if_else(test_table$Predictions == test_table$Actuals,"Y","N")
misClasificError <- mean(glmnetPredict != test_flags)
print(paste('Accuracy',1-misClasificError))
summary(glmnetModel)

## Examine Base Model (Bias,Variance,Overfitting,Underfitting,Heteredoskedacity,Predictive Power)
## IF Low Results , try ensembling


if(ENESEMBLE_FLAG == 1)
{
# Random forests
# Bayesian networks
# Support vector machines
# Probit model
ctrl <- trainControl(
    method = "cv",
    p = 0.70,
    returnData = TRUE,
    savePredictions = TRUE,
    allowParallel = TRUE
  )

dtree_fit <- train(FLAG~., data = df.expanded, method = "rpart",
                   trControl = ctrl)

test_table_dt <- dtree_fit$pred
test_table_dt$Predictions <- test_table_dt$pred
test_table_dt$Actuals <- test_table_dt$obs
test_table_dt$difference <- if_else(test_table_dt$Predictions == test_table_dt$Actuals,0,1)
bias <- mean(test_table_dt$Actuals) - test_table_dt$Predictions

misClasificError <- mean(test_table_dt$difference)
print(paste('Accuracy',1-misClasificError))


## Neural Networks

data_nn <- df.expanded
dmy <- dummyVars("~ .", data = data_nn, fullRank = T)
data_nn <- data.frame(predict(dmy, newdata = data_nn))
Index <- sample(1:nrow(data_nn), 0.7*nrow(data_nn))  # row indices for training data
nn_train <- data_nn[Index, ]  # model training data
nn_test <- data_nn[-Index, ] 
last_col  <- ncol(nn_train)
colnames(nn_train)[last_col] <- "FLAG"
colnames(nn_test)[last_col] <- "FLAG"

feature_names <- names(nn_train)
f <- as.formula(paste("FLAG ~", paste(feature_names[!feature_names %in% "FLAG"], collapse = " + ")))
nn <- neuralnet(f,data = data_nn,hidden = c(1,1),linear.output = F)
results <- nn$result.matrix %>% as.data.frame()
nn_weights <- nn$generalized.weights

pr.nn <- compute(nn,nn_test)
nn_test_table <- data.frame(actual = nn_test$FLAG,prediction = pr.nn$net.result)
rounded_nn_table <- sapply(nn_test_table,round,digits = 0) %>% as.data.frame()
rounded_nn_table$difference <- ifelse(rounded_nn_table$actual ==  rounded_nn_table$prediction,0,1)
misClasificError <- mean(rounded_nn_table$difference)
print(paste('Accuracy',1-misClasificError))


## Support Vector Machine
data_svm <- df.expanded
Index <- sample(1:nrow(data_svm), 0.7*nrow(data_svm))  # row indices for training data
svm_train <- data_svm[Index, ]  # model training data
svm_test <- data_svm[-Index, ] 
svm.model <- svm(svm_train$FLAG ~.,data = svm_train)
print(svm.model)
pred <- predict(svm.model,svm_test)
svm_table <- table(pred,svm_test$FLAG) %>% as.data.frame()
colnames(svm_table) <-  c("Predictions","Actuals","Count")
svm_table$difference <- ifelse(svm_table$Predictions == svm_table$Actuals,1,0)
svm_table <- aggregate(svm_table$Count,by = list(svm_table$difference),sum)
total_sum <- sum(svm_table$x)
correct_sum <- sum(svm_table[2,2])
svm_accuracy <- correct_sum/total_sum
}





## Model Validation
#anova(mylogit, test="Chisq")
r2_1 <- glmnetModel$glmnet.fit$dev.ratio[which(glmnetModel$glmnet.fit$lambda == glmnetModel$lambda.min)] %>% round(digits = 2)
r2_2 <- glmnetModel$glmnet.fit$dev.ratio[which(glmnetModel$glmnet.fit$lambda == glmnetModel$lambda.1se)] %>% round(digits = 2)
r2 <- ave(r2_1,r2_2)
plot.cv.glmnet(glmnetModel)
a <- coef(glmnetModel, s = "lambda.min")



labels = as.numeric(glmnetPredict > 0.5) 
labels[1:10] = abs(labels[1:10] - 1) # randomly make some labels not match predictions

labels_reordered = labels[order(glmnetPredict, decreasing=TRUE)]
roc_dat = data.frame(TPR=cumsum(labels_reordered)/sum(labels_reordered), FPR=cumsum(!labels_reordered)/sum(!labels_reordered))

# plot the roc curve
plot(roc_dat$FPR, roc_dat$TPR)
