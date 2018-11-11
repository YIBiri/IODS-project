#Ylva Biri, 5.11.2018
#Open Data Science 2018 Exercise 2
library(dplyr)
setwd("C:/Users/Ylva/Documents/GitHub/IODS-project")

##DATA WRANGLING

full_data <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t", header = T)

str(full_data)
dim(full_data)
#There are 183 observations and 60 variables. Apart from gender, all columns of the data are integers.

#Combine related questions
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(full_data, one_of(deep_questions))
full_data$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(full_data, one_of(surface_questions))
full_data$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(full_data, one_of(strategic_questions))
full_data$stra <- rowMeans(strategic_columns)

#Combine into dataset
data <- select(full_data, one_of(c("gender","Age","Attitude", "deep", "stra", "surf", "Points")))
#Exclude where points are 0
data <- subset(data, (Points != 0))
dim(data)
str(data)

#Saving output to .txt
?write.table
write.table(data, file = "data/lrn14.txt", sep = "\t")

#Re-reading data from newly created .txt
dt <- read.table("data/lrn14.txt")
str(dt)
head(dt, n= 5)


###ANALYSIS (for more, see Chapter 2)
install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(dt, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

?lm()
plot <- qplot(Attitude, Points, data = dt) + geom_smooth(method = "lm")

lmmodel <- lm(Points ~ Attitude, data = dt)
summary(lmmodel)
lmmodel2 <- lm(Points ~ Attitude + surf + stra, data = dt)
summary(lmmodel2)
lmmodel3 <- lm(Points ~ Attitude + surf + stra + gender, data = dt)
summary(lmmodel3)