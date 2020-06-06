#ALL_FLAGS <- fread(w_d3)



ALL_FLAGS$id <-gsub("-.*","",ALL_FLAGS$id)
ALL_FLAGS2 <- ALL_FLAGS
### High/Value Customer Flag Setting
#ALL_FLAGS2$id <-gsub("-.*","",ALL_FLAGS2$id)
ALL_FLAGS2 <- ALL_FLAGS2[,c("id","revenue","rank")]
ALL_FLAGS2 <- subset(ALL_FLAGS2,ALL_FLAGS$rank == 1)
ALL_FLAGS2$id <-gsub("-.*","",ALL_FLAGS2$id)
## Ordering the dataframe by id, revenue
ALL_FLAGS2 <- ALL_FLAGS2 %>% arrange(id,revenue)
ALL_FLAGS2$rank <- NULL
ALL_FLAGS2 <- ALL_FLAGS2 %>% group_by(ALL_FLAGS2$id)
df <- ALL_FLAGS2 %>% group_by(id) %>% summarise(revenue = toString(sort(revenue)))

#source("C:\Users\cebojo01\Desktop\abel\markov_chain_attribution.R")

## Get Contributing Revenue Threshold ##
# Split by the data by n
df2 <- str_split_fixed(df$revenue,",", n = Inf) %>% as.data.frame()
df2$id <- df$id
xcols <- ncol(df2) - 1
# Convert all columns to numeric data type
df3 <- apply(df2[1:xcols],2,as.numeric) %>% as.data.frame()
df3[is.na(df3)] <- 0 
df3$total <- apply(df3,1,sum)
df3$id <- df2$id

df3 <- subset(df3,df3$total > 0)
summary(df3$total)
quartile_stats <- summary(df3$total)
## Create High/Low Flags
df3$FLAG <- ""
### total > than median AND less than Quartile 3
df3$FLAG <- ifelse(df3$total > quartile_stats[3] & df3$total < quartile_stats[5],"N",df3$FLAG)
# Total > than Q3
df3$FLAG <- ifelse(df3$total > quartile_stats[5],"Y",df3$FLAG)
# Total < than Median 
df3$FLAG <- ifelse(df3$total < quartile_stats[3],0,df3$FLAG)


df4 <- subset(df3,df3$FLAG != 0)

## Join back to original dataset
df5 <- left_join(ALL_FLAGS,df4,by = "id")
df6 <- subset(df5,df5$FLAG == "Y")
df7 <- subset(df5,df5$FLAG == "N")
ALL_FLAGS3 <- rbind(df6,df7)
ys <- df6
no <- df7
if(SAMPLE_RUN == "Y")
{
  ALL_FLAGS <- ys
}
if(SAMPLE_RUN == "N")
{
  ALL_FLAGS <- no
}
rm(df,df2,df3,ALL_FLAGS2,ALL_FLAGS3,df,df4,df5,xcols,quartile_stats)
