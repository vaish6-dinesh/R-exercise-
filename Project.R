model1<-lm(MPG ~. ,data=train)
step<-stepAIC(model1,direction = "both")
summary(model1)
step
vif(model1)
model2<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namechevrolet + Car_Namefiat + 
             Car_Namevolkswagon + Car_Namecapri + Car_Namedatsun + Car_Nameoldsmobile + 
             Car_Namecadillac + Origin3, data = train)
summary(model2)
vif(model2)
model3<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namechevrolet + Car_Namefiat + 
             Car_Namevolkswagon + Car_Namecapri + Car_Namedatsun + Car_Nameoldsmobile + 
             Car_Namecadillac, data = train)
summary(model3)
vif(model3)
model4<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namechevrolet + Car_Namefiat + 
             Car_Namevolkswagon + Car_Namecapri + Car_Namedatsun + Car_Nameoldsmobile, data = train)
summary(model4)vif(model1)
model5<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namechevrolet + Car_Namefiat + 
             Car_Namevolkswagon + Car_Namecapri + Car_Namedatsun, data = train)
summary(model5)
vif(model5)
model6<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namechevrolet + Car_Namevolkswagon 
           + Car_Namecapri + Car_Namedatsun, data = train)
summary(model6)
vif(model6)
model7<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameford + Car_Namechevrolet + Car_Namevolkswagon 
           + Car_Namecapri + Car_Namedatsun, data = train)
summary(model7)
vif(model7)
model8<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameamc + Car_Nameford + Car_Namevolkswagon 
           + Car_Namecapri + Car_Namedatsun, data = train)
summary(model8)
vif(model8)
model9<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameford + Car_Namevolkswagon 
           + Car_Namecapri + Car_Namedatsun, data = train)
summary(model9)
vif(model9)
model10<-lm(MPG ~ Horsepower + Weight + Acceleration + Cylinders3 + Cylinders6 + 
             Car_Nameford
           + Car_Namecapri + Car_Namedatsun, data = train)
summary(model10)
vif(model10)
model11<-lm(MPG ~ Horsepower + Weight + Cylinders3 + Cylinders6 + 
              Car_Nameford
            + Car_Namecapri + Car_Namedatsun, data = train)
summary(model11)
vif(model11)
model12<-lm(MPG ~ Horsepower + Weight + Cylinders3 + Cylinders6 + 
              Car_Nameford + Car_Namedatsun, data = train)
summary(model12)
vif(model12)
model13<-lm(MPG ~ Horsepower + Weight + Cylinders6 + 
              Car_Nameford + Car_Namedatsun, data = train)
summary(model13)
vif(model13)
model14<-lm(MPG ~ Horsepower + Weight + Cylinders6 + Car_Namedatsun, data = train)
summary(model14)
vif(model14)

test$predicted_mpg<-predict(model14, test[ , -1 ])
View(test)

R<-cor(test$MPG, test$predicted_mpg) #value between 0.8 and 0.9 good model
r2<-R^2 #adjusted correlation
r2
#0.7 to 0.8 decent model
#0.8 to 0.9 good model
#0.9 to 1 is fit model
#0.5 to 0.7 dependent model(good or bad)
#less than 0.5 unfit model
