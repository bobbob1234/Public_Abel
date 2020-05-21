library(bigrquery)
set_service_token(NULL)
user_id <- bq_user()

## Import New Trainning Data####
sql_string2 <- "SELECT id ,channel,rank,revenue,start_time_utc FROM tablex"
query_results <- query_exec(sql_string2,project_ID,use_legacy_sql = F,max_pages = Inf)
TIME_RUN2 <- as.POSIXct(TIME_RUN)
TIME_RUN2 <- gsub(":","-",TIME_RUN2)
w_d2 <- substring(sql_string2,97)
w_d3 <- paste("C:/Users/cebojo01/Desktop/abel/ml_datasets/",w_d2,TIME_RUN2,user_id,"ori_data.csv")
w_d3 <- gsub(" ","",w_d3)
w_d3 <-gsub("`","",w_d3)
fwrite(query_results,w_d3)
rm(query_results)

