### Rules    The likelihood of a user having x transaction therefore having another transaction (y) ####
library(arules)
library(arulesViz)

#Support -  This rule occurs in 70% of transactions
#Confidence - #70% of Transactions that have this rule bought Y
#Lift - 1.0 = Indepdent, > 1 - rules are depdent on each other , lift < 1  negatively depdent on each other (antecedent, consequent)
#rulesabfecalt <- eclat (abf, parameter = list(supp = 0.001))
#rulesabfweecalt <- weclat(abf, parameter = list(supp = 0.001,conf = 0.5, maxlen = 15))
data_frame_class_abf <- class(abf)
rules_2 <- as(abf,"transactions")
purchases <- rbind(rules_2)
MIS_FUNCTION <- function(transactions,)
function (data, parameter = NULL, appearance = NULL, control = NULL) 
{
  data <- as(data, "transactions")
  items <- data@data
  if (is(appearance, "list")) 
    appearance <- as(c(appearance, list(labels = itemLabels(data))), 
                     "APappearance")
  appearance <- as(appearance, "APappearance")
  control <- as(control, "APcontrol")
  parameter <- as(parameter, "APparameter")
  if (control@verbose) {
    cat("Apriori\n")
    cat("\nParameter specification:\n")
    show(parameter)
    cat("\nAlgorithmic control:\n")
    show(control)
  }
  abs_supp <- as.integer(parameter@support * length(data))
  if (control@verbose) {
    cat("\nAbsolute minimum support count:", abs_supp, "\n\n")
  }
  result <- .Call(R_rapriori, items@p, items@i, items@Dim, 
                  parameter, control, appearance, data@itemInfo)
  call <- match.call()
  result@info <- list(data = call$data, ntransactions = length(data), 
                      support = parameter@support, confidence = parameter@confidence)
  quality(result)$count <- as.integer(round(quality(result)$support * 
                                              length(data)))
  if (is(result, "rules")) {
    validObject(result@lhs@data)
    validObject(result@rhs@data)
  }
  else {
    validObject(result@items@data)
  }
  result
}

rulesabfrules <- apriori (abf, parameter = list(supp = 0.9, conf = 0.8,maxlen = 4))

rulesabf <- (as(rulesabfrules,"data.frame"))

## Finding Relevant Rules
redundand_rules <- is.redundant(rulesabfrules)
relevant_rules <- rulesabfrules[redundand_rules == FALSE]
relevant_rules2 <- as(relevant_rules,"data.frame")
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
