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
install.packages("uniftest")
isntall.packages("rcompanion")

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
library(stats4)
library(car)
library(uniftest)
library(rcompanion)

#----------------------------------------------

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

# ----------------------------------------------

# Cleaning the data and writing to new file

# Also we can want to have Smoker as Yes/No, but 1/0
#data <- data %>%
#  mutate(smoker = ifelse(smoker == "no", 0, 1))

# It can be better to use 0/1 <-> Male/Female
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

# In our case we don't need to perform data cleaning, 
# Because our data is already cleaned

# Writing to new csv file

current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
write.csv(data,"./out/data.csv",row.names = FALSE)

# ----------------------------------------------

# EDA (Exploratory data analysis)


# ----------------------------------------------

# Analysis of single variables to discover their properties
# To verify that data is well distributed and makes sens

# Smoking
summary(data$smoker)

ggplot(data) + 
  theme_calc() +
  geom_bar(mapping = aes(x = smoker))


# Summary: We have more non-smokers than smokers

# Point Estimation

# Point Estimation of Population Proportion
f = sum(data$smoker=='yes')
n = length(data$smoker)
f/n

# The point estimate of smoking people proportion in survey is 20%.

# ----------------

# Gender

ggplot(data) + 
  theme_calc() +
  geom_bar(mapping = aes(x = sex))

# As we can see we have almost equal distribution of man and women in survey

# Point Estimate of Gender Proportion
f = sum(data$sex=='male')
n = length(data$sex)
f/n

# The point estimate of male gender proportion in survey is 50%.
# ----------------

# Age

# Basic number properties
# Return parameters: 
# item name item number number of valid cases mean 
# standard deviation median mad: median absolute deviation (from the median) minimum maximum skew standard error
describeBy(data$age)

# Average deviation
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(data$age))

# Kwantyle (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$age)

# Rozstep kwartylowy (upper-hinge - lower-hinge)
IQR(data$age)

#Momenty centralne, nie absolutne
moment(data$age,0.25)
moment(data$age,0.5)
moment(data$age,0.75)
moment(data$age,1)


# ------------

#Boxplot
ggplot(data, aes("",y=age)) + 
  geom_boxplot(fill="slateblue") +
  theme_classic()

# Histogram
hist <- ggplot(data, aes(x=age)) + 
  geom_histogram()+
  scale_x_continuous(breaks = seq(0, 155, 2))+
  scale_y_continuous(breaks = seq(0, 80, 5))+
  labs(title="Histogram for age", x="age", y="Number of people")

# Probability distribution
ggplot( data, aes(x=age)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Probability distribution of age") +
  theme(plot.title = element_text(hjust = 0.5))

# As we can see, we've got evenly uniformly distributed variable, 
# So next analysis can pretend to be accurate

# -------------
# Point Estimators

# Mean 
mean(data$age)

# A point estimate of the mean age is 39.2 years

# ----------------
# BMI

# Basic number properties
# Return parameters: 
# item name item number number of valid cases mean 
# standard deviation median mad: median absolute deviation (from the median) minimum maximum skew standard error
describeBy(data$bmi)

# Average deviation
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(data$bmi))

# Kwantyle (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$bmi)

# Rozstęp kwartylowy (upper-hinge - lower-hinge)
IQR(data$bmi)

#Momenty centralne, nie absolutne
momentnt(data$bmi,0.25)
moment(data$bmi,0.5)
moment(data$bmi,0.75)
moment(data$bmi,1)

# ------------
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


# ----------------

# Let's discuss the distribution of BMI

# Visual methods -> Density plot of BMI
ggplot( data, aes(x=bmi)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Probability distribution of bmi") +
  theme(plot.title = element_text(hjust = 0.5))

ggqqplot(data$bmi)

t.test(data$bmi)

# normality test
shapiro.test(data$bmi)

# The output p<0.05 tells us that  that the distribution of the data are significantly 
# different from normal distribution. In other words, we can't assume the normality.


# So summing up -> we are dealing with not normal distribution, but close to normal
#W = 0.99389, p-value = 2.605e-05
# So as we can see, BMI is not normally distributed


plot(density(data$bmi),main="Density estimate of data") 

# -------------
# Inteval estimators

# We will use The One Sample t Test -> determines whether the sample mean is statistically different from a known or hypothesized population mean. 

# Check data requirements:
# Test variable that is continuous (i.e., interval or ratio level) OK
# Scores on the test variable are independent (i.e., independence of observations) OK
# Random sample of data from the population OK
# Normal distribution (approximately) of the sample and population on the test variable OK
# Homogeneity of variances (i.e., variances approximately equal in both the sample and population) OK
# No outliers (OK)

t.test(data$bmi)

# Or by hands
# Interval Estimators FOR MEAN!

n <- length(data$bmi)

mu <- mean(data$bmi)

s <- sd(data$bmi)

# Standard error
error <- s/sqrt(n)

# critical value z
z <- qnorm(0.025, lower.tail = F) 

# the confidence interval
left <- mu - z*error
right <- mu + z*error

intr_estimation <- c("estimate" = mu, "lower95%" = left, "upper95%" = right)

round(intr_estimation, digits = 5)

# So we can evaluate 95% confidence interval, which is a range of values that you can be 95% certain contains the true mean of the population.
# In our case it is [30.33635 , 30.99045]

# Interval Estimators for MEDIAN!

x       = data$bmi
msQuantile = apply(matrix(sample(x, rep=TRUE, 10^4*length(x)), nrow=10^4), 1, median)
quantile(msQuantile, c(.025, 0.975))

# So we can evaluate 95% confidence interval, which is a range of values that you can be 95% certain contains the true median of the population.
# In our case it is [30.01987 , 30.80000 ]

# -------------------

#Children

ggplot(data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Medical charges by children")

# Now we can see the charges doesn't depend on number of children

# -------------------

# Charges

# Basic number properties
# Return parameters: 
# item name item number number of valid cases mean 
# standard deviation median mad: median absolute deviation (from the median) minimum maximum skew standard error
describeBy(data$charges)

# Average deviation
avg.dev <- function(x)
  mean(abs(x - mean(x)))
c(avg.dev(data$charges))

# Kwantyle (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$charges)

# Rozstep kwartylowy (upper-hinge - lower-hinge)
IQR(data$charges)

#Momenty centralne, nie absolutne
momentnt(data$charges,0.25)
moment(data$charges,0.5)
moment(data$charges,0.75)
moment(data$charges,1)

# ------------
#Boxplot
ggplot(data, aes("",y=charges)) + 
  geom_boxplot(fill="slateblue") +
  theme_classic()
# Histogram
ggplot(subset(data,smoker =='yes'), aes(x=charges)) + 
  geom_histogram()+
  labs(title="Histogram for charges", x="BMI", y="Count")

# Probability distribution
ggplot( data, aes(x=charges)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Probability distribution of charges") +
  theme(plot.title = element_text(hjust = 0.5))


# Doesn't look like normally-distributed, so there is no sens in testing

#------------------------------------
# So Summing up we have
describeBy(data)

# -----------------------------------------------------------------------------------

# Now let's check for dependencies

# Build Correlogram to find out correlation dependencies

ggpairs(data, title="correlogram with ggpairs()") 

ggcorr(data, method = c("everything", "pearson"))

# As we can see, the  age has the highest correlation with charges (0.299)***,
# Also BMI has (0.198)*** correlation, which can give us some expectancies to 
# Future, where *** means Pr(>|t|) close to 0

#----------------------------------------------
# Dependency analysis between data

# Analise smoking and charges

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
  geom_smooth(method=lm , color="red", se=FALSE)

# So we can see that charges are greater, as a line-dependency is visually rising faster

# Now lets for our interest find out at what age is the most 'smoker' age
ggplot(subset(data,smoker =='yes'), aes(x=age)) + 
  geom_histogram()+
  scale_x_continuous(breaks = seq(0, 155, 2))+
  labs(title="Histogram for most 'smoker' age", x="Age", y="Count")

# So around 19 is the most smoking age (based on our data)

# Summing up we have 
ggplot(data,aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker)) + 
  ggtitle("Charges vs Age")

# Summing up on Smoking
# So based on the analysis we can say, that smokers in average pay more for treatment
# then non-smokers, smokers can be divided into two groups - those with normal BMI (~30)
# and those with high BMI, the second group is more affected by deceases and in
# the end pays more for treatment. (Or insurance is more in our case)

#-------------------------------------

# Sex

#Let's see is gender connected with charges
ggplot(data,aes(x= sex, y = charges)) + 
  geom_boxplot() +
  ggtitle("Charges")

# So it's not correlated, and gender doesn't affect charges

#-------------------------------------

# BMI

# Summing up we have 
ggplot(data,aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker, size = bmi, alpha = .05)) +
  ggtitle("Charges vs Age")

# Correlation is 0.198 which means that there is a positive correlation between two variables, 
# but it is weak and likely unimportant.
cor(data$bmi,data$charges)


# Let's divide bmi data into obese and not (More than 30 bmi is obese)
bmiMoreOrLessThan30 <- ifelse(data$bmi>=30,"yes","no")

ggplot(data = data,aes(bmiMoreOrLessThan30,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Obesity")

# As we can see there is no big difference in insurance costs, but 
# those with high BMI has more outliers, which means hard deseases etc.

#--------------------------------------------

# Regression model

# Build linear model using all possible variables and analyse the result
model_all <- lm(charges ~ age + sex + bmi + children + smoker,data)
summary(model_all)

# Analyse the results for group of data to figure out which needs to be removed
# And which relationships are the strongest 

# Standard Error -> measures the average amount 
# that the coefficient estimates vary from the actual average value of our response variable
# Expectations -> lower number relative to estimation coefficients
# Reality -> For age and bmi this values are low enough 

# T Value -> is a measure of how many standard deviations our coefficient estimate is far away from 0.
# Expectations -> if it is far away from 0, we can reject null-hypothesis (declare that relationship)
# exists
# Reality -> For age,bmi and smokers it seems that some kind of relationship exists, but
# The strongest one for a first look is with smokers (Need to mention that we are talking about
# Relationship with Charges)

# Pr(> |t|) -> relates to the probability of observing any value equal or larger than t.
# In other words, indicates, the possibility of value/relation been observed by chance
# Expectations -> less than 0.05
# Reality -> for all except gender is less than 0.05, which satisfies us

# Residual standard error -> measure of quality of linear regression fit
# In other words it is the average amount that the response (age/bmi etc.) 
# will deviate from the true regression line.
# Expectations -> Lower, comparing with estimate, better.
# Also expecting it to be normal
# Reality -> Only for smokers we can see low difference, or about 25% error on guessing

# Multiple R-squared -> a measure of how well the model is fitting the actual data.
# Expectations -> Close to 1
# Reality -> 74% , not bad, but can be better

# Adjusted R-squared -> it adjusts Multiple R-squared for the number of variables considered.
# Expectations -> Close to 1
# Reality -> 0.7488 = 74%

# F-statistics -> indicator of whether there is a relationship 
# between our predictor and the response variables.
# Expectations -> The further from 1 the better (comparing with data size and predictors)
# Reality -> 798

# So to start with, I'd remove not really suitable sex and children, and check weather Residual standard error is 
# Following normal distribution

model<- lm(charges ~ age + bmi + smoker, data)
summary(model)

res <- resid(model)
qqnorm(res)
qqline(res)

# As we can see, Residual standard error is not following normal distribution, 
# and R-squared can still be better

# Now I suggest looking at Smoking closely, because it affects charges the most,
# But as we saw in previous analysis, smokers can be devided into two categories
# Those with low and high BMI
# I suggest us doing that, by adding new variable -> SmokerWithHighBMI

data$SmokerWithHighBMI <- ifelse(data$bmi>30 & data$smoker=="yes","yes","no")

# Check what we got there
describeBy(data$charges,data$SmokerWithHighBMI)

# Now build the model
model<- lm (charges ~ age  + smoker+ bmi + SmokerWithHighBMI,data)
summary(model)

# Analysing as in previous model, we can come to 
# Adjusted R-squared is 85,8%, which is higher than in previous model
# which gives us the possibility to say that data is fitting the model
# Errors are lower, t-values are greater, Pr( >|t|) is slightly bigger 
# on average, which, makes our results more random, but for all
# except BMI it's still less than 0.05, which is normal

# Now lets build Residual standard error and check if is 
# Following normal distribution

res <- resid(model)
qqnorm(res)
qqline(res)
AIC(model)
BIC(model)

# let's also check for plots of our model parameters
plot(model,2)

# Residuals vs Fitted ->  shows if residuals have non-linear patterns. 
# In our case we have a bit clustered left side which means that there are 
# Some outliers not covered by model

# Normal Q-Q -> Shows if residuals are normally distributed
# As we can see there is a part when data stops following the normal distribution
# Which means that our model doesn't cover all the outliers, and 
# Error while defining them can be significant !!! Which means that we need to work
# More on model

# Scale-Location ->  the assumption of equal variance
# Data is not spread randomly on the line, which is not quite good

# Residuals vs Leverage ->   This plot helps us to find influential cases (i.e., subjects) if any. 
# Watch if Cook's distance is high

# Let's search more


# let's consider more age impact

# Now build the model
model<- lm (log(charges) ~ age  + ade^2 + smoker+ bmi + SmokerWithHighBMI + children,data)
summary(model)
plot(model)

# As we can see, data on graphics is better distributed, but we still have problems
# With Normal Q-Q, which is not normal :)
# Residuals are essentially the difference between the actual observed response values
# So we need them to be normally distributed across 0


#-------------------------------------

# Student's t-test
# Let's show that charge doesn't depend on sex

mans_charge <- subset(data,sex=="male" & smoker=='no')
females_charge <- subset(data,sex=="female"& smoker=='no')

# As we see on boxplots below, visually they have the same distribution, and median
ggplot(data = data,aes(sex,charges,fill=sex)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Smokers and non-smokers BoxPlots") +
  xlab("")

test <- t.test( mans_charge$charges, females_charge$charges
)
test
# So because p-value is < 0.07 we are assuming right that they are from same population
# Which means that it doesn't matter which gender is the person, whose charge we
# are analysing