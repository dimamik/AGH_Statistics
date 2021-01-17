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
install.packages('GGally')
install.packages('vcd')
install.packages('e1071')
install.packages("ggpubr")
install.packages("fitdistrplus")

library(rstudioapi)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(GGally)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(vcd)
library(e1071)
library(ggpubr)
library(fitdistrplus)
# Reading data from .csv
data = (
  read.csv(
    "https://raw.githubusercontent.com/dimamik/AGH_Statistics/master/End_Project/src/insurance.csv",
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
#data <- data %>%
#  mutate(smoker = ifelse(smoker == "no", 0, 1))

# It is better to use 0/1 <-> Male/Female
#data <- data %>%
#  mutate(sex = ifelse(sex == "male", 0, 1))

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

# Build Correlogram to find out correlation dependencies

ggpairs(data, title="correlogram with ggpairs()") 

ggcorr(data, method = c("everything", "pearson")) 

# As we can see, the  age has the highest correlation with charges (0.299),
# Also BMI has (0.198) correlation, which can give us some expectancies to 
# Future

# Now I suggest analysing single data columns to verify that data is 
# cleared and is well distributed 

# Start with smoking
summary(data$smoker)

ggplot(data) + 
  geom_bar(mapping = aes(x = smoker))


# Looks like we have more smokers than non-smokers, lets have a look
# How it affects out charge statistics

ggplot(data = data,aes(smoker,charges,fill=smoker)) +
geom_boxplot() +
scale_fill_viridis(discrete = TRUE, alpha=0.6) +
geom_jitter(color="black", size=0.4, alpha=0.9) +
theme_ipsum() +
theme(
  legend.position="none",
  plot.title = element_text(size=11)
) +
ggtitle("Smokers and non-smokers BoxPlots") +
xlab("")

# Looks like more data about non-smokers doesn't change the picture, 
# but we can trust it more
# And we can visually divide data from smokers into 2 groups, lets find out 
# why

ggplot(data,aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker, size = bmi, alpha = .05)) +
  ggtitle("Charges vs Age")

# From that we can find out that smokers with high BMI pay more than smokers with low
# Also there looks like a linear dependency between age and charges, but we will find 
# it out later


# Summing up on Smoking
# So based on the analysis we can say, that smokers in average pay more for treatment
# then non-smokers, smokers can be divided into two groups - those with normal BMI (~30)
# and those with high BMI, the second group is more affected by deceases and in
# the end pays more for treatment. (Or insurance is more in our case)


# Age
summary(data$age)

# Basic properties

# Mean
mean(data$age)
# Median
median(data$age)
# Kurtoza
kurtosis(data$age)
# Average deviation
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(data$age))


# Odchylenie standardowe
sd(data$age)
# Rozstep
range(data$age)
# Rozstęp kwartylowy
IQR(data$age)
# (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$age)
#Skosnosc
skewness(data$age)
#Momenty centralne, nie absolutne
moment(data$age,0.25)
moment(data$age,0.5)
moment(data$age,0.75)
moment(data$age,1)

# Kwantyle
quantile(data$age, c(0.1, 0.25, 0.5, 0.75, 0.9))
# Odchylenie medianowe
mad(data$age)

#Boxplot
ggplot(data, aes("",y=age)) + 
  geom_boxplot(fill="slateblue") +
  theme_classic()
# Histogram
ggplot(data, aes(x=age)) + 
  geom_histogram()+
  scale_x_continuous(breaks = seq(0, 155, 2))+
  labs(title="Histogram for age", x="age", y="Count")
# Probability distribution
ggplot( data, aes(x=age)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Probability distribution of age") +
  theme(plot.title = element_text(hjust = 0.5))

# As we can see, we've got evenly uniformly distributed variable, 
# So next analysis can pretend to be accurate

# So lets plot the age and the charges for non-smokers

ggplot(subset(data,smoker =='no') , aes(x=age, y=charges)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# As we can see, that visually dependency is almost linear, which we will 
# More formally discuss later

#If we want plot for smokers, we get something like that:
ggplot(subset(data,smoker =='yes') , aes(x=age, y=charges)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# So we can see that charges are greater, as a line-dependency is visually rising faster

#Now lets for our interest find out at what age is the most 'smoker' age
ggplot(subset(data,smoker =='yes'), aes(x=age)) + 
  geom_histogram()+
  scale_x_continuous(breaks = seq(0, 155, 2))+
  labs(title="Histogram for most 'smoker' age", x="Age", y="Count")

# So around 19 is the most smoking age (based on our data)




# Summing up we have 
ggplot(data,aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker)) + 
  ggtitle("Charges vs Age")

#TODO Rozklad zmiennej

# Sex

#Let's see is sex connected with charges
ggplot(data,aes(x= sex, y = charges)) + 
  geom_boxplot() +
  ggtitle("Charges") +
  theme_classic()

# So it's not correlated, and gender doesn't affect charges


# BMI
summary(data$bmi)

# Basic properties

# Mean
mean(data$bmi)
# Median
median(data$bmi)
# Kurtoza
kurtosis(data$bmi)
# Average deviation
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(data$bmi))
# (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$bmi)
#Skosnosc
skewness(data$bmi)
#Momenty centralne, nie absolutne
moment(data$bmi,0.25)
moment(data$bmi,0.5)
moment(data$bmi,0.75)
moment(data$bmi,1)
#Boxplot
ggplot(data, aes("",y=bmi)) + 
  geom_boxplot(fill="slateblue") +
  theme_classic()
# Histogram
ggplot(subset(data,smoker =='yes'), aes(x=bmi)) + 
  geom_histogram()+
  labs(title="Histogram for bmi", x="BMI", y="Count")

# Probability distribution
ggplot( data, aes(x=bmi)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Probability distribution of bmi") +
  theme(plot.title = element_text(hjust = 0.5))

# As we can see, we've got something that looks like normal distribution,
# more formally we will discuss it later

# Summing up we have 
ggplot(data,aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker, size = bmi, alpha = .05)) +
  ggtitle("Charges vs Age")

# Correlation is 0.198 which is ...
cor(data$bmi,data$charges)

#TODO Rozklad zmiennej

plot(density(data$bmi),main="Density estimate of data") 


#TODO Divide into less than 30 and more than 30 and analize

#Children
ggplot(data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Medical charges by children")

# How we can see the charges doesn't depend on number of children



# Regression model
attach(data)
plot(age,charges,col=smoker)
summary(charges[smoker=="no"])
summary(charges[smoker=="yes"])










