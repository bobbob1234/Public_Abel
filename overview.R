#OVERVIEW OF THIS CODE
#1. Create High-Value + Low-Value Customer Datasets + Download Consumer Journeys From BQ


#2. Apply Markov_Chain Attribution ( because I wanted to learn something new  lol...)
#3. Tranpose Consumer Journey From Wide To Flattened
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
ids <-c("Bob", "John") ## ideally I would have about 100k  unique ids ( 100,000)
d <- data.frame(x = ids, y = 1:10, fac = fac)
colnames(d) <- c("id","touchpoint1","touchpoint2")
d2 <- split(d, d[1])
d2
rm(d2)

#4. Apply Associaton Rules  & Logit Regression + Auto_Update Training Sets 
sample_ar <- fread("file:///C:/Users/cebojo01/Desktop/abel/testnrules.csv")
head(sample_ar)
tail(sample_ar)
#5. Compute List of ID's + Best Touchpoint

#6. Output report(ID + touchpoints to serve if  can be controlled, otherwise output ID's)
# a <- data.frame(a = c("1","2","3"), b = c("OTHER%NOT_SET","PPC%,"DIRECT%OTHER"), c = c("Y","Y","Y"))
# colnames(a) <- c("id","touchpoint","High Value")
# head(a)
