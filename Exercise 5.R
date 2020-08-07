summary(carMPG)
boxplot(carMPG$Origin)
#command to assign the third quater value
Q3<-quantile(carMPG$Acceleration,0.75)
#command to assign the first quater value
Q1 <- quantile(carMPG$Acceleration, 0.25)
#assigning IQR
IQR<-Q3-Q1
#Assigning Smax and Smin values
SMAX<-Q3+1.5*IQR
SMIN<-Q1-1.5*IQR
#Replacing the specific columns with Smax and Smin values
carMPG$Acceleration[carMPG$Acceleration>SMAX]<-SMAX
carMPG$Acceleration[carMPG$Acceleration<SMIN]<-SMIN
#Finally viewing the result
boxplot(carMPG$Acceleration)