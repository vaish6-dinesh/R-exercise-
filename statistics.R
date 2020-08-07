View(faithful)
#Fin mean
mean(faithful$eruptions) #Mean is 3.487783

#Find the median
median(faithful$eruptions) # Median is 4

#Find the standard deviation
sd(faithful$eruptions) # standard deviation is 1.141371

#Find the data range
range(faithful$eruptions) # Range is 1.6 5.1

#Plot histogram to see the spread
hist(faithful$eruptions)


# FIND the correlation between eruption and waiting
cor(faithful$eruptions, faithful$waiting) #0.9008112

#Plot the data to see the coorelation
plot(faithful$eruptions, faithful$waiting)

# FIND the covariance between eruption and waiting
cov(faithful$eruptions, faithful$waiting) #13.97781

# Understand Data Distribution
hist(faithful$eruptions, col="blue")
#do we have outlier in eruptions data?
boxplot(faithful$eruptions, col="green") # No outlier
#do we have outlier in waiting data?
boxplot(faithful$waiting, col="yellow") # No outlier

# Lets identify the outlier in the vector age below
age<-c(5,6,7,8,9,10,11,12,18)
boxplot(age) # 18 is the outlier

#load e1071 package to use skewness function
# Find skewness
# install.packages("e1071")
library(e1071)
skewness(faithful$eruptions) # skewness is -0.4135498. Negative means, left skewed

# Find kurtosis
kurtosis(faithful$eruptions) # kurtosis is -1.511605. The normal distribution has zero kurtosis 


