id<-c(1,2,3,4)
name<-c("Archie","Jughead","betty","Veronica")
age<-c(20,30,40,60)
emp2<-data.frame(cbind(id,name,age))
emp1<-cbind(emp1,age)
str(emp2)
View(emp1)
emp1[2,2]<-"Nagesh"
emp2$name<-as.character(emp2$name)
emp2$id<-as.numeric(emp2$id)
emp2$age<-as.numeric(emp2$age)
emp2$name[2]<-"Nagesh"
View(emp2)
emp1<-emp1[,-4]
w<-c(5,"Skandam",10)
emp1<-rbind(emp1,w)
emp2<-rbind(emp2,w)
