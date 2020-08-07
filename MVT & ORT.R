fix(carMPG)
#missing value treatement
#checking for missing values
summary(carMPG)
#command to replace the missing values with mean
carMPG$Horsepower[is.na(carMPG$Horsepower)]<-mean(carMPG$Horsepower,na.rm = T)
#command to check for outliers
boxplot(carMPG$Acceleration)
#command to assign the third quater value
Q3<-quantile(carMPG$Horsepower,0.75)
#command to assign the first quater value
Q1 <- quantile(carMPG$Horsepower, 0.25)
#assigning IQR
IQR<-Q3-Q1
#Assigning Smax and Smin values
SMAX<-Q3+1.5*IQR
SMIN<-Q1-1.5*IQR
#Replacing the specific columns with Smax and Smin values
carMPG$Horsepower[carMPG$Horsepower>SMAX]<-SMAX
carMPG$Horsepower[carMPG$Horsepower<SMIN]<-SMIN
#Finally viewing the result
boxplot(carMPG$Horsepower)
