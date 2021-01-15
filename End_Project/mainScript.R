# imports
install.packages('dplyr')
install.packages('rstudioapi')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('psych')
install.packages('relaimpo')
install.packages('tidyverse')
install.packages('hrbrthemes')
install.packages('viridis')

library(rstudioapi)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)

library(tidyverse)
library(hrbrthemes)
library(viridis)

# Reading data from .csv
data = (
  read.csv(
    "https://raw.githubusercontent.com/dimamik/AGH_Statistics/master/End_Project/insurance.csv",
    header = TRUE
  )
)

# Discovering data structure
View(data)
names(data)
str(data)
summary(data)

# --------------------------------------------

# Cleaning the data and writing to new file

# Also we don't want to have Smoker as Yes/No, but 1/0
data <- data %>%
  mutate(smoker = ifelse(smoker == "no", 0, 1))

# It is better to use 0/1 <->Male/Female
data <- data %>%
  mutate(sex = ifelse(sex == "male", 0, 1))

# We don't need a region
drops <- c("region")
data<-data[,!(names
        (data) %in% drops)]

# Check for Nulls in Data 
if (anyNA(data)){
  print("Data is not valid")
}else{
  print("Data is valid")
}

# Writing to new csv file
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
write.csv(data,"./out/data.csv",row.names = FALSE)

#-----------------------------------
# EDA (Exploratory data analysis)

# Describe numeric data by groups
pairs.panels(data[c("age","bmi", "children", "charges")])


ggplot(data = data,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")




#Srednia
mean(wzrost)
#Median
median(wzrost)
#Odchylenie przecietne
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(wzrost))
# (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(wzrost)
# Histograma
hist(wzrost)
# Boxplot :)
boxplot(wzrost)

