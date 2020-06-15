#### IMPORT ALL LIBRARIES + FUNCTION DEFINITIONS ####
devtools::install_github("tidyverse/multidplyr")
getwd()
source("library_wrapper.R")
source("Core Functions.R")
install_version("dplyr", version = "0.8.4", repos = "http://cran.us.r-project.org")


local_data <- fread("C:/Users/White/OneDrive/1D Downloads/nba-playbyplay-data-20182019/NBA-PBP_2016-2017.csv") %>% as.data.frame()
ALL_FlAGS <- data_transformation_function(local_data)
exeucte_function_first(local_data)

source("logistic_model.R")
saveRDS(glmnetModel, "./logistic_model_v2.rds")
saveRDS(glm_model_stack, "./ensemble_model.rds")


## Import New Trainning Data####
local_data <- read.csv("C:/Users/White/OneDrive/1D Downloads/nba-playbyplay-data-20182019/NBA-PBP-2017-2018.csv")
ALL_FLAGS <- data_transformation_function(local_data)
fwrite(ALL_FLAGS,"test.csv")
delete_meta()
ALL_FLAGS <- fread("test.csv")

source("C:/Users/White/abel/logistic_test.R")
source("C:/Users/White/abel/traceback.R")

