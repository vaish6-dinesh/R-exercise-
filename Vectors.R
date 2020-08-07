#Precidence Rule
#logical<integer<double<complex<character
#command to create a vector
v1=c(1,2,3,4)
#command to check the class
class(v1)
#command to check the datatype
typeof(v1)
#command to change the the datatype
v1=as.integer(v1)
#command to check whether double it is double datatype 
is.double(v1)
v2=c(1,2,3,4,"Jack","Archie")
class(v2)
v2
v3=c(1,2,3,4,TRUE,FALSE)
typeof(v3)
#command to accessing the 4th element in the vector
v1[4]
#modifying the vector elements
v1[4]=50
v1
#command to insert an element in the prefix
v1<-c(30,v1)
#command to remove the element
v1[-2]
#command to insert an element in the suffix
v1=c(v1,50)
v1
v1[-2]
#command to insert an element in the middle
v1=c(v1[1:3],70,v1[4:6])