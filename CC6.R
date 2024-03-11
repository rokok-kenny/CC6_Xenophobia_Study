library(data.table)
library(dplyr)
dt <- fread("WVS_Cross-National_Wave_7_csv_v5_0.csv", stringsAsFactors = T)
View(dt)

#drop all other countries
sg <- dt[B_COUNTRY == 702]
summary(sg)
dim(sg)
View(sg) #2012 entries, 606 columns

#####CHECKS FOR DATASET VALIDITY#####

#check for duplicated interviews
anyDuplicated(sg$D_INTERVIEW) #0

#check for any potential error in mismatch in "country code" and "COW country code", no error
all(sg$B_COUNTRY == 702)
all(sg$B_COUNTRY_ALPHA == "SGP")

#####PREPARATION OF VARIABLES#####

sg$Q21 <- factor(sg$Q21) #dependent variable
summary(sg$Q21) #count of respondents by willingness to have immigrants/foreign workers as neighbours

sg$Q123 <- factor(sg$Q123)
summary(sg$Q123) #count of respondents by CULTURAL OPENNESS #11 erroneous rows where value == -2

sg$Q263 <- factor(sg$Q263)
summary(sg$Q263) #count of respondents by IMMIGRATION STATUS

sg$Q63 <- factor(sg$Q63)
summary(sg$Q63) #count of respondents by TRUST TOWARDS FOREIGNERS #50 erroneous rows where value == -2

sg$Q34 <- factor(sg$Q34)
summary(sg$Q34) #count of respondents by PERCEIVED GROUP THREAT #3 erroneous rows where value == -2

#removal of erroneous rows
sg <- sg[sg$Q123 != -2, ]
sg <- sg[sg$Q63 != -2, ]
sg <- sg[sg$Q34 != -2, ]
dim(sg) #1953 observations left to work with

#####DESCRIPTIVE STATISTICS#####
summary(sg$Q21)
summary(sg$Q123)
summary(sg$Q263)
summary(sg$Q63)
summary(sg$Q34)

#####RELEVELING THE BASELINES#####
sg$Q21 <- relevel(sg$Q21, ref = 1)
sg$Q123 <- relevel(sg$Q123, ref = 0)
sg$Q63 <- relevel(sg$Q63, ref = 4)
sg$Q34 <- relevel(sg$Q34, ref = 1)
sg$Q263 <- relevel(sg$Q263, ref = 1)

#####LOGISTIC REGRESSION#####

# Fit logistic regression model

m1 <- glm(Q21 ~ Q123 + Q263 + Q63 + Q34, family = binomial, data = sg)
summary(m1)

# Predicting the test results - probability of new test set
prob_pred = predict(m1, type = "response", newdata = test_set)
prob_pred
plot(density(prob_pred))
