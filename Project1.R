data1<-data.frame(rent_rooms_dist %>% filter(Rent <= 10000, Distance <= 5))

data1$Rent<-as.factor(data1$Rent)
data1$Distance<-as.factor(data1$Distance)

model1<-lm(Rooms ~ Rent + Distance,data = data1)

step<-stepAIC(model1,direction = "both")

summary(model1)

data1$predicted_mpg<-predict(model1, data1[ , -3 ])

R<-cor(data1$Rooms, data1$predicted_mpg)
r2<-R^2 #adjusted correlation
r2
round(r2,4)


data2<-data.frame(rent_rooms_dist %>% filter(Rooms == 2, Distance <= 3))
data3<-data.frame(rent_rooms_dist %>% filter(Rent <= 15000, Rooms == 3))