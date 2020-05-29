#### IMPORT ALL LIBRARIES + FUNCTION DEFINITIONS ####
devtools::install_github("tidyverse/multidplyr")
setwd("C:/Users/White/May 2020 Abel/2020 Abel")
source("library_wrapper.R")
attach(iris)
TIME_RUN <- Sys.time()
MIS_FLAG <- ""
local_data <- fread("C:/Users/White/OneDrive/1D Downloads/nba-playbyplay-data-20182019/NBA-PBP_2016-2017.csv") %>% as.data.frame()

combination_table <- list()

for(i in 1:ncol(local_data))
{
  combination_table[[i]] <- table(local_data[,i]) %>% as.data.frame()
  combination_table[[i]]$data_type <- class(local_data[,i])
}


local_data2 <- local_data[,c("URL","Shooter","ShotType","Assister","ShotOutcome","Date","WinningTeam","ShotDist","Rebounder","ReboundType")]

local_data2$combination <- paste(local_data2$Assister,local_data2$Shooter,local_data2$Rebounder,local_data2$ReboundType)
local_data2$Date <- format(mdy(local_data2$Date),"%Y-%m-%d")
game_table <- unique(local_data$URL) %>% as.data.frame()
game_table$encode <- ""


loop_count <- 1
for(loop_count in 1:nrow(game_table)){
  game_table[loop_count,2] <- loop_count
  loop_count = loop_count + 1
}


# Encode Channels
game_table$encode <- as.numeric(game_table$encode)

local_data2$game_id <- qdapTools::lookup(local_data2$URL,game_table)
local_data2$combination <- paste(local_data2$combination,local_data2$game_id)
local_data2$game_id <- NULL
local3_table <- getanID(data = local_data2,id.vars = "combination")
prob_table <- table(local3_table$WinningTeam) %>% as.data.frame() 
prob_table$probaility <- prob_table$Freq / sum(prob_table$Freq)
prob_table$negativeprobs <- 1 - prob_table$probaility
prob_table2 <- prob_table
prob_table2$Var1 <- NULL
prob_table2 <- as.matrix(prob_table2)
a <- colprods(prob_table[,4])
negative_tch <- c(NA,a)
prob_table$Freq <- NULL
prob_table$negativeprobs <- NULL
prob_table <- rbind(prob_table,a)
local3_table$prob <- qdapTools::lookup(local3_table$WinningTeam,prob_table)
local3_table$revenue <- 2*(runif(1:nrow(local3_table),max=1) * (local3_table$prob * 1/(local3_table$prob^(1/local3_table$ShotDist+2))))
local3_table$revenue <- ifelse(local3_table$ShotOutcome == "make",local3_table$revenue,0)
max_rev <- max(local3_table$revenue[local3_table$revenue != Inf]) + 100
local3_table$revenue <- ifelse(local3_table$revenue == Inf,max_rev,local3_table$revenue)
local3_table_1_split <- subset(local3_table,local3_table$revenue == 0)
local3_table_1_split$.id <- NULL
local3_table_1_split_list <- split(local3_table_1_split,local3_table_1_split$Date)
local3_table_1_split_list <- split(local3_table_1_split,local3_table_1_split$Date)
for(i in 1:length(local3_table_1_split_list))
{
  local3_table_1_split_list[[i]] <- local3_table_1_split_list[[i]] %>% group_by(local3_table_1_split_list[[i]]$combination) %>% mutate(id = 1:n())
  local3_table_1_split_list[[i]]$id <- local3_table_1_split_list[[i]]$id + 1
}
local3_table_1_split2 <- do.call(rbind.data.frame,local3_table_1_split_list)
local3_table_1_split2$.id <- local3_table_1_split2$id + 1
local3_table_1_split2$`local3_table_1_split$combination` <- NULL
local3_table_1_split2$id <- NULL
local3_table_2_split <- subset(local3_table,local3_table$revenue > 0)
local3_table_2_split$.id <- 1
local4_append <- rbind(local3_table_1_split2,local3_table_2_split)
local4 <- local3_table[,c("combination",".id","revenue","Date","ShotType")]
local4 <- subset(local4,local4$ShotType != "")
ALL_FLAGS <- local4
colnames(ALL_FLAGS) <- c("id","rank","revenue","start_time_utc","channel")
source("overview.R")
#source("C:/Users/White/abel/bigquery_pull_ga_reports.R")

library(multidplyr)
count <- 0
SAMPLE_RUN <- "Y"
repeat{
  if(count == 2)
  {
    break
  }
  source("propensity_calculations.R")
  
  gc(TRUE)
  library(multidplyr)
  library(Rfast)
  
  ALL_FLAGS2 <- ALL_FLAGS
  ## Subsetting for columns that we need for processing
  ALL_FLAGS <- ALL_FLAGS[,c("id","revenue","channel","rank","start_time_utc")]
  
  source("summary_data.R")
  
  
  
  
  listtchs <- table(ALL_FLAGS$rank)%>% as.data.frame()
  
  tchs_above_thresh <- subset(listtchs,listtchs$Freq > 100) ## was 1000
  tchs_above_thresh$Var1 <- as.numeric(tchs_above_thresh$Var1)
  maxtchs_above_tresh <- max(tchs_above_thresh$Var1)
  unique_channels <- unique(ALL_FLAGS$channel) %>% as.data.frame()
  unique_channels$encode <- ""
  
  
  loop_count <- 1
  for(loop_count in 1:nrow(unique_channels)){
    unique_channels[loop_count,2] <- loop_count
    loop_count = loop_count + 1
  }
  
  
  # Encode Channels
  unique_channels$encode <- as.numeric(unique_channels$encode)
  #source("C:/Users/White/abel/markov_chain_attribution.R")
  source("tranpose_prob.R")
  minsupp <- runif(1,0.10,0.3) %>% round(digits = 2)
  minsupp <- 0.5
  source("association_rules.R")
  count = count + 1
}
## Adjust Support Threshold -> redo support
csv_path
csv_path2 <- gsub("low","high",csv_path)
low_rules <- fread(csv_path)

high_rules <- fread(csv_path2)
all_rules <- rbind(high_rules,low_rules)
fwrite(all_rules,)

source("logistic_model.R")
saveRDS(glmnetModel, "./logistic_model_v2.rds")

## Import New Trainning Data####
local_data <- read.csv("C:/Users/White/OneDrive/1D Downloads/nba-playbyplay-data-20182019/NBA-PBP-2017-2018.csv")
local_data2 <- local_data[,c("URL","Shooter","ShotType","Assister","ShotOutcome","Date","WinningTeam","ShotDist")]
local_data2$combination <- paste(local_data2$Shooter,local_data2$Assister,sep = ",")
local_data2$Date <- format(mdy(local_data2$Date),"%Y-%m-%d")
game_table <- unique(local_data$URL) %>% as.data.frame()
game_table$encode <- ""


loop_count <- 1
for(loop_count in 1:nrow(game_table)){
  game_table[loop_count,2] <- loop_count
  loop_count = loop_count + 1
}


# Encode Channels
game_table$encode <- as.numeric(game_table$encode)

local_data2$game_id <- qdapTools::lookup(local_data2$URL,game_table)
local_data2$combination <- paste(local_data2$combination,local_data2$game_id)
local_data2$game_id <- NULL

local3_table <- getanID(data = local_data2,id.vars = "combination")
prob_table <- table(local3_table$WinningTeam) %>% as.data.frame() 
prob_table$probaility <- prob_table$Freq / sum(prob_table$Freq)
prob_table$negativeprobs <- 1 - prob_table$probaility
prob_table2 <- prob_table
prob_table2$Var1 <- NULL
prob_table2 <- as.matrix(prob_table2)
a <- colprods(prob_table[,4])
negative_tch <- c(NA,a)
prob_table$Freq <- NULL
prob_table$negativeprobs <- NULL
prob_table <- rbind(prob_table,a)
local3_table$prob <- qdapTools::lookup(local3_table$WinningTeam,prob_table)
local3_table$revenue <- 2*(runif(1:nrow(local3_table),max=1) * (local3_table$prob * 1/(local3_table$prob^(1/local3_table$ShotDist+2))))
local3_table$revenue <- ifelse(local3_table$ShotOutcome == "make",local3_table$revenue,0)
local3_table_1_split <- subset(local3_table,local3_table$revenue == 0)
local3_table_1_split_list <- split(local3_table_1_split,local3_table_1_split$Date)
for(i in 1:length(local3_table_1_split_list))
{
  local3_table_1_split_list[[i]] <- local3_table_1_split_list[[i]] %>% group_by(local3_table_1_split_list[[i]]$combination) %>% mutate(id = 1:n())
  local3_table_1_split_list[[i]]$id <- local3_table_1_split_list[[i]]$id + 1
}

local3_table_1_split2 <- do.call(rbind.data.frame,local3_table_1_split_list)
local3_table_1_split2$.id <- local3_table_1_split2$id + 1
local3_table_1_split2$`local3_table_1_split$combination` <- NULL
local3_table_1_split2$id <- NULL
local3_table_2_split <- subset(local3_table,local3_table$revenue > 0)
local3_table_2_split$.id <- 1
local4_append <- rbind(local3_table_1_split2,local3_table_2_split)
local4 <- local3_table[,c("combination",".id","revenue","Date","ShotType")]
local4 <- subset(local4,local4$ShotType != "")
ALL_FLAGS <- local4
colnames(ALL_FLAGS) <- c("id","rank","revenue","start_time_utc","channel")
fwrite(ALL_FLAGS,"test.csv")
delete_meta <- function()
{
  rm(db_hash,micro_dby,micro_dby_test,micro_dby_train,glmnetPredict,objTest,objTrain,objTest_hashed,objTrain_hashed,objTest_hashed,myfiles)
  rm(test_table,train_processed,Index,test_flags,micro_dby2,glmnetModel)
  gc(TRUE)
}
delete_meta()
ALL_FLAGS <- fread("test.csv")

source("C:/Users/White/abel/logistic_test.R")
source("C:/Users/White/abel/traceback.R")

