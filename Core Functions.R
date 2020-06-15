library(Rcpp)
library(inline)


#########  CORRELATION FUNCTION #########
#1. Instead of base R version, uses formula to output correlation metric for each x and y
#2 . Defines numeric vectors ( I assume these are data types that enable "communication" with C++ and R")
#3. First Line defines a variable of the size of the second colummn, 
#4. Uses a for loop to iterate and correlate for each binary pair(x,y)
library(Rcpp)
{
  cppFunction('NumericVector corr(NumericVector X , NumericVector Y,NumericVector xavg, NumericVector yavg)
  {
    int n = Y.size();
    NumericVector Corr(n);
    
    for(int i = 0; i < n; ++i)
    {
 Corr[i] = sum(X[i]-xavg) * sum(Y[i]- yavg) / sqrt(sum(pow(X[i]-xavg,2))) * sqrt(sum(pow(Y[i]-yavg,2)));
    }
return Corr;
  }'
  )
}

data_transformation_function <- function(local_data)
{
  # pressure_table <- local_data[,c("URL","SecLeft","HomePlay","HomeScore","AwayPlay","AwayScore","ShotOutcome")]
  # pressure_table$ShotOutcome2 <- pressure_table$ShotOutcome
  # pressure_table$ShotOutcome2 <- ifelse(pressure_table$ShotOutcome2 == "","miss",pressure_table$ShotOutcome2)
  # 
  # ## Defensive and Offensive Pressure Calculations
  # 
  # defense_counter <- 0
  # pressure_table$defensive_pressure <- ""
  # pressure_table$offensive_pressure <- sequence(rle(as.character(pressure_table$ShotOutcome2))$lengths)
  # for(i in 1:nrow(pressure_table))
  # {
  #   
  #   
  #   ## Defensive Pressure Calculations
  #   if(pressure_table[i,"HomePlay"] == "" || pressure_table[i,"AwayPlay"] == "")
  #   {
  #     defense_counter <- defense_counter + 1
  #     
  #   }
  #   if(pressure_table[i,"HomePlay"]!= "" || pressure_table[i,"AwayPlay"] != "")
  #   {
  #     if(i == 1)
  #     {
  #       
  #     }
  #     if(i > 1)
  #       if(pressure_table[i,"HomeScore"] != pressure_table[(i-1),"HomeScore"] || pressure_table[i,"AwayScore"] != pressure_table[(i-1),"AwayScore"])
  #       {
  #         if(i > 3)
  #           defense_counter <- mean(pressure_table[i:i+3,"HomeScore"])
  #         if(defense_counter < pressure_table[])
  #       }
  #     
  #   }
  #   pressure_table$defensive_pressure[i] <- defense_counter
  #   
  # }
  # 
  # pressure_table$price <- pressure_table$offensive_pressure * as.numeric(pressure_table$defensive_pressure)
  # 
  # for(i in 1:ncol(local_data))
  # {
  #   combination_table[[i]] <- table(local_data[,i]) %>% as.data.frame()
  #   combination_table[[i]]$data_type <- class(local_data[,i])
  # }
  
  
  
  
  
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
  return(ALL_FLAGS)
}
delete_tranpose<- function()
{
  rm(ALL_FLAGS,ALL_FLAGS2,holding_frames,local_data,local_data2,local3_table,local3_table_1_split,local3_table_1_split_list,local3_table_1_split2,local3_table_2_split,local4,local4_append,pressure_table,no,ys)
  gc(TRUE)
}
delete_meta <- function()
{
  rm(db_hash,micro_dby,micro_dby_test,micro_dby_train,glmnetPredict,objTest,objTrain,objTest_hashed,objTrain_hashed,objTest_hashed,myfiles)
  rm(test_table,train_processed,Index,test_flags,micro_dby2,glmnetModel)
  gc(TRUE)
}

exeucte_function_first <- function()
{
  source("overview.R")
  source("propensity_calculations.R")
  holding_frames <- list()
  holding_frames[[1]] <- ys
  holding_frames[[2]] <- no
  
  
  for(data_frame_count in 1:length(holding_frames))
  {
    ALL_FLAGS2 <- holding_frames[[data_frame_count]] %>% as.data.frame()
    ## Subsetting for columns that we need for processing
    ALL_FLAGS <- ALL_FLAGS2[,c("id","revenue","channel","rank","start_time_utc")]
    
    
    
    
    listtchs <- table(ALL_FLAGS$rank)%>% as.data.frame()
    
    tchs_above_thresh <- subset(listtchs,listtchs$Freq > 900) ## was 1000
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
  }
  delete_transpose()
  source("association_rules.R")
  
}