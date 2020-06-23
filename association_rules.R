### Rules    The likelihood of a user having x transaction therefore having another transaction (y) ####
library(arules)
library(arulesViz)

#Support -  This rule occurs in 70% of transactions
#Confidence - #70% of Transactions that have this rule bought Y
#Lift - 1.0 = Indepdent, > 1 - rules are depdent on each other , lift < 1  negatively depdent on each other (antecedent, consequent)
#rulesabfecalt <- eclat (abf, parameter = list(supp = 0.001))
#rulesabfweecalt <- weclat(abf, parameter = list(supp = 0.001,conf = 0.5, maxlen = 15))


support_combinations <- seq(0.2,1,0.2)
confidence_combinations <-seq(0.2,1,0.2)
rule_length_combinations <- seq(2,4,1)
ar_parameter_combinations <-  expand.grid(support = support_combinations,confidence = confidence_combinations,rule_length = rule_length_combinations)


holding_frames_ar <- list()
high_rules <- list()
low_rules <- list()
holding_frames_ar[[1]] <- abf_y
holding_frames_ar[[2]] <- abf_n
for(ar_frames in 1:length(holding_frames_ar))
{
  abf <- holding_frames_ar[[ar_frames]]
  abf <- abf %>% mutate_all(as.character)
  ## Remove NA'S in all the columns
  loop_count <- 1
  for(loop_count in 1:length(abf)){
    abf[loop_count] <- ifelse(abf[loop_count] == "",NA,abf[loop_count]) 
    loop_count = loop_count + 1
  }
unique_rule_names <- colnames(abf)
abf$sum <- 1
abf_agg <- abf %>% group_by_at(setdiff(unique_rule_names,"sum")) %>% summarise(sum = sum(sum)) %>% as.data.frame()

unique_trans <- abf_agg
abf<- dplyr::left_join(abf,unique_trans,by = unique_rule_names)
abf$pasted <- apply(abf,1,paste,collapse = "",sep = "-")
id_table <- table(abf$pasted) %>% as.data.frame()
id_table$Freq <- 1:nrow(id_table) 
colnames(id_table) <- c("pasted","ID")

abf2 <- dplyr::left_join(abf,id_table,by = c("pasted"))
abf2$sum.x <- NULL
abf2$sum.y <- NULL
abf2$pasted <- NULL
ruleset <- stratified(abf2,"ID",0.5) %>% as.data.frame()
ruleset$ID <- NULL
ruleset <- ruleset %>% mutate_all(as.factor)



rule_frames <- list()
rule_complete_frame <- list()
for(i in 1:nrow(ar_parameter_combinations))
{
support <- ar_parameter_combinations[i,"support"]
confidence <- ar_parameter_combinations[i,"confidence"]
maxlen <- ar_parameter_combinations[i,"rule_length"]

rule_frames[[i]] <- apriori (ruleset, parameter = list(supp = ar_parameter_combinations[i,"support"],confidence = ar_parameter_combinations[i,"confidence"],maxlen = ar_parameter_combinations[i,"rule_length"]))
rule_frames[[i]]<- as(rule_frames[[i]],"data.frame")
dmy <- dummyVars(" ~ .", data = rule_frames[[i]])
rule_frames[[i]] <- data.frame(predict(dmy, newdata = rule_frames[[i]]))
}


if(ar_frames == 1)
{
  for(i in 1:length(rule_frames))
  {
  rule_frames[[i]]$FLAG <- "Y"
  rule_frames[[i]]$support_level <- ar_parameter_combinations[i,"support"]
  rule_frames[[i]]$confidence_level <- ar_parameter_combinations[i,"confidence"]
  rule_frames[[i]]$maxlen_level <- ar_parameter_combinations[i,"rule_length"]
  }
  high_rules <- rule_frames
  rm(rule_frames)
}
if(ar_frames == 2)
{
  for(i in 1:length(rule_frames))
  {
  rule_frames[[i]]$FLAG <- "N"
  rule_frames[[i]]$support_level <- ar_parameter_combinations[i,"support"]
  rule_frames[[i]]$confidence_level <- ar_parameter_combinations[i,"confidence"]
  rule_frames[[i]]$maxlen_level <- ar_parameter_combinations[i,"rule_length"]
  
  }
  low_rules <-  rule_frames
}
}
gc()
all_rules <- list()
for(i in 1:length(high_rules))
{
  all_rules[[i]] <- high_rules[[i]]
  all_rules[[i]] <- dplyr::bind_rows(all_rules[[i]],low_rules[[i]])
  all_rules[[i]] <-  all_rules[[i]] %>%  mutate_all(funs(ifelse(is.na(.), 0, .)))
}
combination_store <- list()
for(i in 1:length(all_rules))
{
  combination_store[[i]] <- data.frame(support = all_rules[[i]]$support_level,confidence = all_rules[[i]]$confidence_level,maxl = all_rules[[i]]$maxlen_level)
  combination_store[[i]]$ID <- i
}
combination_store <- do.call(rbind.data.frame,combination_store)
combination_store <- unique(combination_store)
for(i in 1:length(all_rules))
{
  all_rules[[i]]$support_level <- NULL
  all_rules[[i]]$confidence_level <- NULL
  all_rules[[i]]$maxlen_level <- NULL
  all_rules[[i]]$support <- NULL
  all_rules[[i]]$lift <- NULL
  all_rules[[i]]$confidence <- NULL

}
gc()
rm(rule_frames,high_rules,low_rules)
gc()
getwd()
if(shiny_running() == T){
  dir <- getwd()
  dir2 <<- paste(dir,"Model Outputs")
  dir3 <<- paste(dir,"Rule Outputs")
  
  model_outputs <- dir3
  
  dir.create(dir2)
  dir.create(dir3)
} else {
    setwd("C:/Users/White/May 2020 Abel/2020 Abel/Model Outputs")
  }


for(i in 1:50)
{
  id <- i
  Index <- sample(1:nrow(all_rules[[i]]), 0.7*nrow(all_rules[[i]]))  # row indices for training data
  all_rules[[i]]$FLAG <- ifelse(all_rules[[i]]$FLAG == "Y",1,0)
  model_train <- all_rules[[i]][Index, ]  # model training data
  model_test <- all_rules[[i]][-Index, ]   # model test data
  
  glm_model_stack <- glm(FLAG ~ ., data = model_train, family = binomial(),maxit = 100)
  glm_predictions <- predict(glm_model_stack,model_test,type = "response")
  glm_predictions <- ifelse(glm_predictions > 0.5,1,0)

  
  test_flags <- model_test$FLAG
  test_table <- 1:nrow(model_test) %>% as.data.frame()
  test_table$Predictions <- glm_predictions
  test_table$Actuals <- test_flags
  test_table$difference <- if_else(test_table$Predictions == test_table$Actuals,"Y","N")
  test_table$difference2 <- if_else(test_table$difference == "Y",1,0)
  
  Accuracy  <- (mean(test_table$difference2))
  model <- data.frame(ID = i,accuracy = Accuracy)
  model <- dplyr::left_join(model,combination_store,by = c("ID"))

  output_name <- paste("model",i,".csv")
  output_name <- gsub(" ","",output_name)
  if(shiny_running() == T)
  {
    fwrite(model,dir2)
  }
  else{
    fwrite(model,output_name)
  }

}

temp = list.files(pattern  =  "*.csv")
myfiles = lapply(temp, fread)
thresh_frame  <- do.call(rbind.data.frame,myfiles)
thresh_frame <- subset(thresh_frame,thresh_frame$accuracy != 1)
max_row <- thresh_frame[which.max(thresh_frame$accuracy),]
support <- max_row$support
confidence <- max_row$confidence
max_length <- max_row$maxl




  rule_set_y <- apriori (abf_y, parameter = list(supp = support,confidence = confidence,maxlen = max_length))
  rulesabf <- (as(rule_set_y,"data.frame"))
  
  ## Finding Relevant Rules
  redundand_rules <- is.redundant(rule_set_y)
  relevant_rules <- rulesabf[redundand_rules == FALSE]
  relevant_rules2 <- as(relevant_rules,"data.frame")
  
  TIME_RUN2 <- as.POSIXct(TIME_RUN)
  TIME_RUN2 <- gsub(":","-",TIME_RUN2)
  csv_path <- paste("C:/Users/White/abel/", "train-high-values--",TIME_RUN2,".csv")
  csv_path <- gsub(" ","",csv_path)
  relevant_rules2$FLAG <- "Y"
  if(shiny_running() ==  T)
  {
    fwrite(relevant_rules2,dir3)
  }  else{
    fwrite(relevant_rules2,csv_path)
  }

  rule_set_y <- apriori (abf_n, parameter = list(supp = support,confidence = confidence,maxlen = max_length))
  rulesabf <- rule_set_y
  
  redundand_rules <- is.redundant(rule_set_y)
  relevant_rules <- rulesabf[redundand_rules == FALSE]
  relevant_rules2 <- as(relevant_rules,"data.frame")
  
  TIME_RUN2 <- as.POSIXct(TIME_RUN)
  TIME_RUN2 <- gsub(":","-",TIME_RUN2)
  csv_path <- paste("C:/Users/White/abel/", "train-low-values--",TIME_RUN2,".csv")
  csv_path <- gsub(" ","",csv_path)
  relevant_rules2$FLAG <- "N"
  if(shiny_running() ==  T)
  {
  fwrite(relevant_rules2,dir3)
  } else{
    fwrite(relevant_rules2,csv_path)
  }
  
 rm(all_rules)