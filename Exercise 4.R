id<-c(5,6,7,8)
name<-c("Rahul","Hari","Suresh","Ramesh")
age<-c(20,30,40,50)
emp<-data.frame(cbind(id,name,age))
View(emp)
rm(emp)
emp_cbin <-rbind(emp2, emp)
emp_rbin<-cbind(emp2,emp)

#Creating character vector x with 3 elements
x<-c("John", "Skanda", "Gomes")

#create a numeric vector y with 3 elements
y<-c(1,2,3)

#Create a vector
z<-cbind(x,y)

#To dispay the matrix in the console window
print (z)

class (x)
class (y)
class (z)

# create dataframe from x and y. Add a third column s
d<-data.frame("name"=x, "age"=y)
View(d)
id<-c("A1", "B1", "C1")
e<-cbind(d,id)
View(e)

#how to work with rbind
w<-c("Farhan", 5, "D1")
g<-rbind(e, w)

d$name<-as.character(d$name)
e$name<-as.character(e$name)
e$id<-as.character(e$id)

#Create a vector with five elements
h<-c("N", "S", "E", "W", "SE")
str(h)
#For converting to factor
h<-as.factor(h)
h<-as.numeric(h)
h<-as.character(h)
t<-c(h, "NW")
print (t)
t<-cbind(h, "NW")
t<-rbind(h, "NW")