summary(carMPG)
#command to remove the seperators between strings
carMPG$Car_Name<-gsub("\\ .*","",carMPG$Car_Name)
carMPG$Cylinders<-as.factor(carMPG$Cylinders)
carMPG$Origin<-as.factor(carMPG$Origin)
str(carMPG)
fix(carMPG)
#command to display the level in a factor column
levels(carMPG$Car_Name)
#command to assign the value to the different levels
levels(carMPG$Car_Name)[4]<-"toyota"
str(carMPG)
#create the matrix model of the levels of the column
dummy1<-data.frame(model.matrix(~(Car_Name-1),data = carMPG))
dummy2<-data.frame(model.matrix(~Cylinders-1,data = carMPG))
dummy3<-data.frame(model.matrix(~Origin-1,data = carMPG))
#binding the columns to different dataframe
car_1<-cbind(carMPG[,c(1,3,4,5,6)],dummy2,dummy3,dummy1)
#command to create a stack 
set.seed(10)
indices<-sample(1:nrow(car_1),0.7*(nrow(car_1)))
train<-car_1[indices,]
test<-car_1[-indices,]

