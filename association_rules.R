### Rules    The likelihood of a user having x transaction therefore having another transaction (y) ####
library(arules)
library(arulesViz)

#Support -  This rule occurs in 70% of transactions
#Confidence - #70% of Transactions that have this rule bought Y
#Lift - 1.0 = Indepdent, > 1 - rules are depdent on each other , lift < 1  negatively depdent on each other (antecedent, consequent)
#rulesabfecalt <- eclat (abf, parameter = list(supp = 0.001))
#rulesabfweecalt <- weclat(abf, parameter = list(supp = 0.001,conf = 0.5, maxlen = 15))


support_combinations <- seq(0.01,1,0.01)
confidence_combinations <-seq(0.01,1,0.01)
rule_length_combinations <- seq(1,14,1)
ar_parameter_combinations <-  expand.grid(support = support_combinations,confidence = confidence_combinations,rule_length = rule_length_combinations)


holding_frames_ar <- list()
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
ruleset <- stratified(abf2,"ID",1) %>% as.data.frame()
ruleset$ID <- NULL
ruleset <- ruleset %>% mutate_all(as.factor)



rule_frames <- list()
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
  rule_frames[[i]]$FLAG <- "Y"
  rule_frames[[i]]$support_level <- support
  rule_frames[[i]]$confidence_level <- confidence
  rule_frames[[i]]$maxlen_level <- maxlen
  # high_train$FLAG <- "Y"
}
if(ar_frames == 2)
{
  rule_frames[[i]]$FLAG <- "N"
  rule_frames[[i]]$support_level <- support
  rule_frames[[i]]$confidence_level <- confidence
  rule_frames[[i]]$maxlen_level <- maxlen

}
}




# ## Finding Relevant Rules
# redundand_rules <- is.redundant(rulesabfrules)
# relevant_rules <- rulesabfrules[redundand_rules == FALSE]
# relevant_rules2 <- as(relevant_rules,"data.frame")
# 
# 


#relevant_rules2 <- rbind(relevant_rules2,negative_rules)
#plot(relevant_rules,jitter = 0)
# Find Significant rules (WIP)
#significant_rules <- is.significant(rulesabfrules)
if(SAMPLE_RUN == "Y")
{
  TIME_RUN2 <- as.POSIXct(TIME_RUN)
  TIME_RUN2 <- gsub(":","-",TIME_RUN2)
  csv_path <- paste("C:/Users/White/abel/", "train-high-values--",TIME_RUN2,".csv")
  csv_path <- gsub(" ","",csv_path)
  relevant_rules2$FLAG <- "Y"
  fwrite(relevant_rules2,csv_path)
  SAMPLE_RUN <- "N"
}
if(SAMPLE_RUN == "N")
{
  TIME_RUN2 <- as.POSIXct(TIME_RUN)
  TIME_RUN2 <- gsub(":","-",TIME_RUN2)
  csv_path <- paste("C:/Users/White/abel/", "train-low-values--",TIME_RUN2,".csv")
  csv_path <- gsub(" ","",csv_path)
  relevant_rules2$FLAG <- "N"
  fwrite(relevant_rules2,csv_path)
}
rm(abf,channel_agg,df8,df9,listtchs,no,p,prob_table,relevant_rules,relevant_rules2,rulesabf,tchs_above_thresh,unique_channels,ys,redundand_rules,filter2,rulesabfrules)
csv_path
