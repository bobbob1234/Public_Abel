library(Rfast)
## Stripping out sesssion number out of the id column
#ALL_FLAGS$id <-gsub("-.*","",ALL_FLAGS$id)
ALL_FLAGS$start_time_utc <- as.Date(ALL_FLAGS$start_time_utc)
###

## Ordering the dataframe by id, start_time, rank
ALL_FLAGS <- ALL_FLAGS %>% arrange(id,start_time_utc,rank)

cluster <- new_cluster(4)
ALL_FLAGS2 <- ALL_FLAGS %>% group_by(ALL_FLAGS$id)  %>% partition(cluster)
df <- ALL_FLAGS2 %>% group_by(id) %>% summarise(channel = toString(sort(channel))) %>% collect()

## Splitting the column into n columns by delimiter
df2 <- str_split_fixed(df$channel,",", n = maxtchs_above_tresh+1) %>% as.data.frame()
df2[maxtchs_above_tresh+1] <- NULL


## Generate unique row number id ## 
df2$row_num <-""
i <- 1
for(i in  1:nrow(df2)){
  df2$row_num[i] <- i  
  i = i + 1 }
df2$row_num <- as.numeric(df2$row_num)
df3 <- df2



## Calculate Probability Distribution by Frequency
## Calculate Probabilites + multiply negative probabilities into a vector

prob_table <- table(ALL_FLAGS$channel) %>% as.data.frame() 
list_probs <- list()
df3 <- as.data.frame(df3)
for( i in 1:(ncol(df3)-1))
{
  prob_table <- table(df3[i]) %>% as.data.frame()
  prob_table$probaility <- prob_table$Freq / sum(prob_table$Freq)
  prob_table$negativeprobs <- 1 - prob_table$probaility
  prob_table2 <- prob_table
  prob_table2$Var1 <- NULL
  prob_table2 <- as.matrix(prob_table2)
  prob_tch <- colprods(prob_table[,4]) # 4
  
  negative_tch <- c("NA",prob_tch)
  prob_table$Freq <- NULL
  prob_table$negativeprobs <- NULL
  prob_table <- rbind(prob_table,negative_tch)
  prob_table$Var1 <- gsub(" ","",prob_table$Var1)
  list_probs[[i]] <- prob_table
}

## Remove Datasets Not Using
rm(ALL_FLAGS)
rm(ALL_FLAGS2)
rm(cluster)
gc(TRUE)

## Removes double spaces in all columns
loop_count <- 2
length2 <- ncol(df3) - 1
length <- 1:length2
for(loop_count in length){
  df3[[loop_count]] <- gsub(" ","",df3[[loop_count]]) 
  loop_count = loop_count + 1
}


## Remove NA'S in all the columns
loop_count <- 1
for(loop_count in length){
  df3[[loop_count]] <- ifelse(df3[[loop_count]] == "",NA,df3[[loop_count]]) 
  loop_count = loop_count + 1
}

## Lookups probability in prob_table & returns probability
head(prob_table)


loop_count <- 1
for(loop_count in length){
  df3[[loop_count]] <- qdapTools::lookup(df3[[loop_count]],list_probs[[loop_count]]) 
  loop_count = loop_count + 1
  
}
df4 <- df3
## Converts all columns into a numeric data type 
df4<- apply(df4,2,as.numeric) %>% as.data.frame()
df4[is.na(df4)] <- negative_tch[2] %>% as.numeric()



## Applies row-wise multiplication of all rows so that we get the probability of a sequence
df5<- apply(df4,1,prod) 
df6 <- cbind(df4,df5)
df6 <- as.data.frame(df6)

## Plot probability 
plot_normal_dist <- unique(df6$df5)
plot2 <- plot_normal_dist * 100
hist(df6$df5)

# Log probabilites  + mulitply log by -1 -> to get positive range
df6$logged_prob_seq <- log10(df6$df5) * -1
hist(df6$logged_prob_seq)
plot(df6$logged_prob_seq)

## Generate filter from logged_probs, filter out probabilities that are greater than average
filter2 = ave(df6$logged_prob_seq)
df7 <- df6
df7 <- subset(df7,df7$logged_prob_seq < filter2) ###is this right ?
df7$df5 <- NULL
df7$logged_prob_seq <- NULL
df8 <- left_join(df7,df2,by = "row_num")
df9 <- select_if(df8,is.factor)

## Calculate amount of datapoints
datapoints <- nrow(df8) * ncol(df8)
abf <- df9
rm(df7,df6,df5,df4,df3,df2,df)
abf[abf == ""] <- NA
abf <- apply(abf,2,as.factor) %>% as.data.frame()
