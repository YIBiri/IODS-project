# Ylva Biri 
# Analysis of longitudinal data

library(dplyr)
library(tidyr)
setwd("C:/Users/Ylva/Documents/GitHub/IODS-project")

# 1. Import data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="\t", header = T)

str(BPRS)
str(RATS)

summary(BPRS)
summary(RATS)
# Data is in wide form: one row represents one week, columns represents individual observations. 
# This allows for examining e.g. the mean and max for a week, but not the tracking of an individual. 


#2. Change categorical variables to factors
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATS$Group <- factor(RATS$Group)
RATS$ID <- factor(RATS$ID)


# 3. Convert data from wide to long form 
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(BPRSL$weeks, 5, 5)))

RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 


# 4. Look at new data 
glimpse(BPRSL)
head(BPRSL)
summary(BPRSL)

glimpse(RATSL)
head(RATSL)
summary(RATSL)
# In the long data, each observation (individual subject, one test from a specific time) is given in its own row. 
# The modified long data has values for both individual (ID/subject) and time of testing (Time/week). 
# This means we can easily collect observations for a specific week (as in wide data) but also for specific individuals to conduct a longitudinal analysis.


# Save data:
write.table(RATSL, file = "data/rats.txt", sep = "\t")
write.table(BPRSL, file = "data/bprs.txt", sep = "\t")