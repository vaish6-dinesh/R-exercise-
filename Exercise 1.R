#Exercise

#Command to create columns based on a filter
#"==" for assignment operator
#"<,> for other oprators
#"&" for using more filters

emp$GenderCatagory[emp$Gender=="M"]<-"MALE"
emp$GenderCatagory[emp$Gender=="F"]<-"FEMALE"
emp$SalaryBand[emp$Salary>8000]<-"HIGH INCOME"

#Command to create columns with 2 filters 
emp$SalaryBand[emp$Salary>3000 & emp$Salary<5000]<-"Lower MIDDLE INCOME"
emp$SalaryBand[emp$Salary>5000 & emp$Salary<8000]<-"Higher MIDDLE INCOME"
emp$SalaryBand[emp$Salary<3000]<-"LOW INCOME"
View(titanic3)
titanic3$SurvivedStatus[titanic3$survived==0]<-"DEAD"
titanic3$SurvivedStatus[titanic3$survived==1]<-"SURVIVED"
titanic3$Siblings[titanic3$sibsp==0]<-"NO SIBLINGS"
titanic3$Siblings[titanic3$sibsp==1]<-"1 SIBLING"
titanic3$Siblings[titanic3$sibsp==2]<-"2 SIBLINGS"
titanic3$Siblings[titanic3$sibsp>2]<-"MORE THAN 2 SIBLINGS"
View(titanic3)
