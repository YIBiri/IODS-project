# Ylva Biri 
#IODS-project: Logistic regression
# Data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance 
library(dplyr)
setwd("C:/Users/Ylva/Documents/GitHub/IODS-project")

#### DATA WRANGLING

# Reading in data
matdt <- read.table("data/student-mat.csv", sep = ";", header = T)
pordt <- read.table("data/student-por.csv", sep = ";", header = T)

# Examining structures 
glimpse(matdt) # Matdt = 295 observations, 33 variables
glimpse(pordt) # Pordt = 649 observations, 33 variables


# Joining the two data sets
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

mathpor <- inner_join(matdt, pordt, by = join_by, suffix = c(".math", ".por"))
glimpse(mathpor)

# Combine "duplicated" answers...

# 1. create a new data frame with only the joined columns
alc <- select(mathpor, one_of(join_by))

# 2. columns that were not used for joining the data
notjoined_columns <- colnames(matdt)[!colnames(matdt) %in% join_by]

# 3. for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(mathpor, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# New column for average wwekday and weekend consumption
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
# ... and high consumption
alc <- mutate(alc, high_use = alc_use > 2)

glimpse(alc)

# Save output
write.table(alc, file = "data/alc.txt", sep = "\t")



### ANALYSIS

# Read data
alc <- read.table("data/alc.txt")
colnames(alc)


library(tidyr); library(ggplot2)

# Plotting fun

table(high_use = alc$high_use, sex = alc$sex) %>% prop.table(2) %>% addmargins %>% round(digits = 2)

# Sex

countf <- length(which(alc$sex == "F"))
countm <- length(which(alc$sex == "M"))


ggplot(alc, aes(sex, ..count..)) + 
  ylab("number of students") +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(aes(fill = high_use), position = "fill") +
  ggtitle("Proportion of high consumption by sex")  
  
# Absences
ggplot(alc, aes(x = high_use, y = absences, col = sex)) + 
  geom_boxplot() + 
  ylab("Absences") +
  xlab("High consumption") +
  ggtitle("Student absences by alcohol consumption and sex")

#Going out
ggplot(alc, aes(x = high_use, y = goout, col = sex)) + 
  geom_boxplot() + 
  ylab("Going out with friends") +
  xlab("High consumption") +
  ggtitle("Impact of going out by sex")

ggplot(alc, aes(sex, ..count..)) + 
  ylab("number of students") +
  face_wrap(~sex) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(aes(fill = high_use), position = "fill") +
  ggtitle("Proportion of high consumption by sex")

ggplot(alc, aes(studytime, ..count..)) + 
  geom_bar(stat = "identity", aes(fill = high_use), position = "dodge") + 
  facet_wrap(~sex) +
  ggtitle("Study-time and consumption by sex")

ggplot(alc, aes(studytime, ..count..)) + 
  ylab("number of students") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~sex) +
  geom_bar(aes(fill = high_use), position = "fill") +
  ggtitle("Study time and consumption by sex")  


ggplot(alc, aes(x = sex, y = absences, col = high_use)) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),  width = .75, linetype = "dashed") +
  ylab("Absences") +
  xlab("Sex") +
  ggtitle("Student absences by alcohol consumption and sex")



## Logistic regression model 
m <- glm(high_use ~ studytime + goout + absences + sex, data = alc, family = "binomial")
summary(m)
coef(m)

## Predictive power
# create predictions 
probabilities <- predict(m, type = "response")
probabilities
# create new column with predictions
alc <- mutate(alc, probability = probabilities)

# new column for whether prediction was accurate
alc <- mutate(alc, prediction = probability > 0.5)

tail(alc)

#confusion matrix
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% round(digits = 2) %>% addmargins

# Same as above in plot form
ggplot(alc, aes(x = probability, y = high_use, col = prediction)) +
  geom_point() +
  ylab("High Use") +
  xlab("Prediction") +
  ggtitle("Prediction accuracy")