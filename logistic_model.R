library(DescTools)
library(mefa)
library(splitstackshape)
setwd("C:/Users/cebojo01/Desktop/abel/")
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
library(fastDummies)
micro_dby$FLAG <- ifelse(micro_dby$FLAG == "Y",1,0)
Index <- sample(1:nrow(micro_dby), 0.7*nrow(micro_dby))  # row indices for training data
micro_dby_train <- micro_dby[Index, ]  # model training data
micro_dby_test <- micro_dby[-Index, ]   # model test data
test_flags <- micro_dby_test$FLAG
db_hash <- micro_dby %>% as.data.frame()
predictorNames <- setdiff(names(db_hash),"FLAG")

# change all NAs to 0

objTrain <-db_hash[Index,] %>% as.data.frame()
objTrain2 <- objTrain[,predictorNames] %>% as.data.frame()

objTest <- db_hash[-Index,] %>% as.data.frame()
objTest2 <- objTest[,predictorNames] %>% as.data.frame()

library(FeatureHashing)
objTrain_hashed = hashed.model.matrix(~., data=objTrain2, hash.size=2^12, transpose=FALSE)
objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
objTest_hashed = hashed.model.matrix("~.", data=objTest2, hash.size=2^12, transpose=FALSE)
objTest_hashed = as(objTest_hashed, "dgCMatrix")

library(glmnet)
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
#anova(mylogit, test="Chisq")
r2_1 <- glmnetModel$glmnet.fit$dev.ratio[which(glmnetModel$glmnet.fit$lambda == glmnetModel$lambda.min)] %>% round(digits = 2)
r2_2 <- glmnetModel$glmnet.fit$dev.ratio[which(glmnetModel$glmnet.fit$lambda == glmnetModel$lambda.1se)] %>% round(digits = 2)
r2 <- ave(r2_1,r2_2)
library(plotmo) # for plotres
plot.cv.glmnet(glmnetModel)
a <- coef(glmnetModel, s = "lambda.min")



labels = as.numeric(glmnetPredict > 0.5) 
labels[1:10] = abs(labels[1:10] - 1) # randomly make some labels not match predictions

labels_reordered = labels[order(glmnetPredict, decreasing=TRUE)]
roc_dat = data.frame(TPR=cumsum(labels_reordered)/sum(labels_reordered), FPR=cumsum(!labels_reordered)/sum(!labels_reordered))

# plot the roc curve
plot(roc_dat$FPR, roc_dat$TPR)
