#Ylva Biri
#Data wrangling 

# 1. Creation & setting up
setwd("C:/Users/Ylva/Documents/GitHub/IODS-project")
library(dplyr)
library(stringr)


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
colnames(hd) <- c("hdi_rank", "country", "hdi", "lifexp", "exped", "yrsed", "gni", "gni_hdi")
colnames(gii) <- c("gii_rank", "country", "gii", "matmor", "adbirth", "parlperc", "secedF", "secedM", "laborF", "laborM")


# 5. Mutate data

# ratio of Female and Male populations with secondary education in each country
gii <- mutate(gii, eduratio = secedF / secedM)

# ratio of labour force participation of females and males in each country
gii <- mutate(gii, labratio = laborF / laborM)

# checking everything is ok
head(gii)


# 6. Joining datasets using "country"
human <- inner_join(hd, gii, by = "country", suffix = c(".hd", ".gii"))

# Saving data
write.table(human, file = "data/human.txt", sep = "\t")

# Check updated data
humantest <- read.table("data/human.txt")

# checking...
str(human)




# EXERCISE 5

# GNI to numeric
str(human)
human$gni <- str_replace(human$gni, pattern=",", replace ="") %>% as.numeric()
str(human)

# Exclude unnecessary variables
keep <- c("country", "eduratio", "labratio", "exped", "lifexp", "gni", "matmor", "adbirth", "parlperc")
human <- select(human, one_of(keep))
dim(human2)

# Remove rows with NA values
human <- filter(human, complete.cases(human))

# Remove regions
human$country
# i.e. remove the last 7 rows
last <- nrow(human) - 7
human <- human[1:last, ]

# Define rows by country, remove country
rownames(human) <- human$country
human <- select(human, -country)

#Check output
summary(human)

# Save and overwrite old data
write.table(human, file = "data/human.txt", sep = "\t")

# Check updated data
humantest <- read.table("data/human.txt")
