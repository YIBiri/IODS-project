#Ylva Biri
#Data wrangling 

# 1. Creation & setting up
setwd("C:/Users/Ylva/Documents/GitHub/IODS-project")
library(dplyr)


# 2. Reading data
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


# 3. Exploring the data
str(hd)
dim(hd)

str(gii)
dim(gii)
# hd = 195 obs, 8 var
# gii = 195 obs, 10 var


# 4. Renaming variables
colnames(hd) <- c("hdi_rank", "country", "hdi", "lifexp", "exped", "yrsed", "gni_cap", "gni_hdi")
colnames(gii) <- c("gii_rank", "country", "gii", "matmor", "adbirth", "parlperc", "secedF", "secedM", "laborF", "laborM")


# 5. Mutate data

# ratio of Female and Male populations with secondary education in each country
gii <- mutate(gii, eduratio = secedF / secedM)

# ratio of labour force participation of females and males in each country
gii <- mutate(gii, labratio = laborF / laborM)

# checking everything is ok
str(gii)


# 6. Joining datasets using "country"
human <- inner_join(hd, gii, by = "country", suffix = c(".hd", ".gii"))

# checking...
str(human)

# saving into data folder
write.table(human, file = "data/human.txt", sep = "\t")

# reading it again, just to make sure :)
dt <- read.table("data/human.txt")
