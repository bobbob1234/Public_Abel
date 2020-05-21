## Traceback ##
rule_log_output <- fread("C:/Users/White/abel/logistic_predictions_unique.csv")
flags_storage <- rule_log_output$FLAG
rule_log_output <- rule_log_output$. %>% as.data.frame()

trace_dataset <- fread("C:/Users/White//abel/trace_r_sets.csv")
trace_dataset <- fread("C:/Users/White/abel/traceback_dataset.csv")
trace_lookup <- fread("C:/Users/White/abel/traceback_lookup_table.csv")
trace_lookup <- trace_lookup[,c(2,1)] %>% as.data.frame()
# loop_count <- 1
# ncol <- ncol(trace_dataset) - 1
# length <- 1:ncol
# for(loop_count in length){
#   trace_dataset[[loop_count]] <- qdapTools::lookup(trace_dataset[[loop_count]],trace_lookup) 
#   loop_count = loop_count + 1
#   
# }
trace_master_table <- trace_dataset
trace_master_table$FLAG <- ifelse(trace_master_table$V2 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1)
trace_master_table$FLAG <- ifelse(trace_master_table$V3 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V4 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V5 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V6 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V7 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V8 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V9 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )
trace_master_table$FLAG <- ifelse(trace_master_table$V10 != "",1,0)
trace_master_table <- subset(trace_master_table,trace_master_table$FLAG == 1 )

trace_master_table <- trace_master_table[1:1000,]


ID <- trace_master_table$id
trace_master_table$id <- NULL
rule_log_output$Cond <- ifelse(rule_log_output$. %like% ",",1,0)

single_condition_ar_output <- subset(rule_log_output,rule_log_output$Cond == 0)
splits <- str_split_fixed(single_condition_ar_output$.,"=>",2) %>% as.data.frame()
splits$V1 <- as.character(splits$V1)
splits$V1 <- gsub(".y","",splits$V1)
splits$V1 <- gsub("=","",splits$V1)
splits$V1 <- gsub("\\{|\\}", "", splits$V1)
splits$V2 <- as.character(splits$V2)
splits$V2 <- gsub(".y","",splits$V2)
splits$V2 <- gsub("=","",splits$V2)
splits$V2 <- gsub("\\{|\\}", "", splits$V2)
splits$V1 <- gsub(" ","",splits$V1)
splits$V2 <- gsub(" ","",splits$V2)
# splits2 <- apply(splits,1,as.character) %>% as.data.frame()
splits$FLAG <- ifelse(splits$V1 == "",1,0)
splits  <- subset(splits,splits$FLAG == 0)
ar_rule_squish <- list()
for( i in 1:nrow(splits))
{
  ar_rule_squish[[i]] <- splits[i,]
  ar_rule_squish[[i]] <- as.data.frame(ar_rule_squish[[i]])
  a  <- str_extract(ar_rule_squish[[i]][[1]],"^[V][0-9]{1,5}")
  b  <- str_extract(ar_rule_squish[[i]][[2]],"^[V][0-9]")
  c  <- "FLAG"
  ar_rule_squish[[i]][[1]] <- gsub("^[V][0-9]","",ar_rule_squish[[i]][[1]])
  ar_rule_squish[[i]][[2]] <- gsub("^[V][0-9]","",ar_rule_squish[[i]][[2]])

  refreshed_col_names <- c(a,b,c)
  colnames(ar_rule_squish[[i]]) <- refreshed_col_names
  loop_count = loop_count + 1
  }
output_table <- list()
single_cond_function <- function(origin_data,ar_convert_data)
{
  ## To Improve recode below from line 58-62 into lapply
  for(i in 1:nrow(origin_data))
  {
    output_table[[i]] <- origin_data[i,]
  }
  for(a in 1:length(ar_convert_data))
  {
    colnames <- colnames(ar_convert_data[[a]])
    touchpoint_record <- ar_convert_data[[a]]
    rule_vector <- paste("rule",a)
    #rule_id <- converted_from_associtaion_rules[i]
  for(i in 1:length(output_table))
  {
  output_table[[i]][,rule_vector] <- ifelse(output_table[[i]][[colnames[1]]] == touchpoint_record[1] & output_table[[i]][[colnames[[2]]]] == touchpoint_record[2]
                             ,rule_vector,"")
  }
  }
  output_table
}


single_condition_traceback_table <- single_cond_function(trace_master_table,ar_rule_squish)
single_condition_tb_master <- do.call(rbind.data.frame,single_condition_traceback_table)
single_condition_tb_master$ID <- ID
rule_sum2 <- single_condition_tb_master[, grep("rule", names(single_condition_tb_master)), with = FALSE]
rule_sum$sum <- apply(rule_sum,1,sum)
single_condition_tb_master <- cbind(single_condition_tb_master,rule_sum$sum)
a <- colnames(single_condition_tb_master)
length_a <- length(a) -1
a <- a[1:length_a]
length2 <- length(a) + 1
a[length2] <- c("total")
colnames(single_condition_tb_master) <- a

trace_single_cond_output <- single_condition_tb_master

multi_condition_ar_output <- subset(rule_log_output,rule_log_output$Cond == 1)
splits <- str_split_fixed(multi_condition_ar_output$.,"=>",Inf) %>% as.data.frame()
x_table <- splits$V1 %>% as.data.frame()
y_table <- splits$V2 %>% as.data.frame()
count_y <- str_count(x_table$.,"y")
count_y <- max(count_y)
x_table2 <- str_split_fixed(x_table$.,",",count_y) %>% as.data.frame()
for(i in 1:ncol(x_table2))
{
x_table2[,i] <- as.character(x_table2[,i])
x_table2[,i] <- gsub(".y","",x_table2[,i])
x_table2[,i] <- gsub("=","",x_table2[,i])
x_table2[,i] <- gsub("\\{|\\}", "", x_table2[,i])
x_table2[,i] <- gsub(" ","",x_table2[,i])
}
x_rule_squish <- list()
for( i in 1:nrow(x_table2))
{
  x_rule_squish[[i]] <- x_table2[i,]
  x_rule_squish[[i]] <- as.data.frame(x_rule_squish[[i]])
  col_length <- ncol(x_rule_squish[[i]])
  a <- vector()
  for ( ai in 1:col_length)
  {
    a[ai] <- str_extract(x_rule_squish[[1]][[ai]],"^[V][0-9]{1,5}")
  }

  refreshed_col_names <- a
  colnames(x_rule_squish[[i]]) <- refreshed_col_names
}
y_table <- splits$V2 %>% as.data.frame()
count_y <- str_count(y_table$.,"y")
count_y <- max(count_y)
y_table2 <- str_split_fixed(y_table$.,",",count_y) %>% as.data.frame()
for(i in 1:ncol(y_table2))
{
  y_table2[,i] <- as.character(y_table2[,i])
  y_table2[,i] <- gsub(".y","",y_table2[,i])
  y_table2[,i] <- gsub("=","",y_table2[,i])
  y_table2[,i] <- gsub("\\{|\\}", "", y_table2[,i])
  y_table2[,i] <- gsub(" ","",y_table2[,i])
}
y_rule_squish <- list()
for( i in 1:nrow(y_table2))
{
  y_rule_squish[[i]] <- y_table2[i,]
  y_rule_squish[[i]] <- as.data.frame(y_rule_squish[[i]])
  col_length <- ncol(y_rule_squish[[i]])
  a <- vector()
  for ( ai in 1:col_length)
  {
    a[ai] <- str_extract(y_rule_squish[[1]][[ai]],"^[V][0-9]{1,5}")
  }
  
  refreshed_col_names <- a
  colnames(y_rule_squish[[i]]) <- refreshed_col_names
}

multi_rule_squish <- list()
for(i in 1:length(x_rule_squish) )
{
  multi_rule_squish[[i]] <- cbind(x_rule_squish[[i]],y_rule_squish[[i]])
}

multi_cond_function <- function(origin_data,ar_convert_data)
{
  ## To Improve recode below from line 58-62 into lapply
  for(i in 1:nrow(origin_data))
  {
    output_table[[i]] <- origin_data[i,]
  }
  for(a in 1:length(ar_convert_data))
  {
    colnames <- colnames(ar_convert_data[[a]])
    touchpoint_record <- ar_convert_data[[a]]
    rule_vector <- paste("multi-rule",a)
    #rule_id <- converted_from_associtaion_rules[i]
    for(i in 1:length(output_table))
    {
      output_table[[i]][,rule_vector] <- ifelse(output_table[[i]][[colnames[1]]] == touchpoint_record[1] & output_table[[i]][[colnames[[2]]]] == touchpoint_record[2]
                                                ,rule_vector,"")
    }
  }
  output_table
}
multi_condition_traceback_table <- multi_cond_function(trace_master_table,multi_rule_squish)
multi_condition_tb_master <- do.call(rbind.data.frame,multi_condition_traceback_table)
multi_condition_tb_master$ID <- ID
rule_sum <- multi_condition_tb_master[, grep("rule", names(multi_condition_tb_master)), with = FALSE]
rule_sum$sum <- apply(rule_sum,1,sum)
multi_condition_tb_master <- cbind(multi_condition_tb_master,rule_sum$sum)
a <- colnames(multi_condition_tb_master)
length_a <- length(a) -1
a <- a[1:length_a]
length2 <- length(a) + 1
a[length2] <- c("total")
colnames(multi_condition_tb_master) <- a
rule_vector <- "a"
multi_rule_vector <- "a"
for(a in 1:nrow(single_condition_ar_output))
{
  rule_vector[a] <- paste("rule",a)
}
for(a in 1:nrow(multi_condition_ar_output))
{
  multi_rule_vector[a] <- paste("multi-rule",a)
}
rule_lookup <- cbind(rule_log_output,flags_storage) %>% as.data.frame()
rule_lookup$Cond <- ifelse(rule_lookup$. %like% ",",1,0)
single_rule_lookup <- subset(rule_lookup,rule_lookup$Cond == 0)
single_rule_lookup$. <- rule_vector
multi_rule_lookup <- subset(rule_lookup,rule_lookup$Cond == 1)
multi_rule_lookup$. <- multi_rule_vector
rule_lookup_combined <- rbind(single_rule_lookup,multi_rule_lookup) %>% as.data.frame()
rule_lookup_combined$Cond <- NULL
rule_lookup_combined$flags_storage <- ifelse(rule_lookup$flags_storage == "Y",1,0)
blank <- c("",0)
rule_lookup_combined <- rbind(rule_lookup_combined,blank)

multi_condition_tb_master <- multi_condition_tb_master[, grepl(c("rule"), names(multi_condition_tb_master)), with = FALSE]
multi_condition_tb_master2 <- multi_condition_tb_master
for(i in 1:ncol(multi_condition_tb_master2))
{
  multi_condition_tb_master2[[i]] <- qdapTools::lookup(multi_condition_tb_master2[[i]],rule_lookup_combined) 
  
  
}
multi_condition_tb_master$total2 <- apply(multi_condition_tb_master,1,sum)
trace_multi_cond_output <- multi_condition_tb_master  

master_output_table <- cbind(single_condition_tb_master,multi_condition_tb_master)
master_output_table$combined_total <- master_output_table$total + master_output_table$total2

for( i in 1:ncol(rule_sum))
{
  rule_sum[i] <- qdapTools::lookup(rule_sum[i],rule_lookup_combined)
}
final_output <- master_output_table[,c("V1","V2","V3","V4","V5","V5","V6","V7","V8","V9","V10")]

final_output <- cbind(final_output,multi_condition_tb_master2,rule_sum2)
trace_dataset_output <- master_output_table
fwrite(trace_dataset_output,"C:/Users/cebojo01/Desktop/abel/ml_datasets/traced_with_ar.csv")
fwrite(final_output,"C:/Users/cebojo01/Desktop/abel/ml_datasets/ar_flags.csv")
rm(list = ls())
gc(TRUE)
