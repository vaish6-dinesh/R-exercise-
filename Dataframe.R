#Dataframe is a collection of hectrogenous elements
#The only difference between list and dataframe is that 
#you need create any type of address for the vecor declared
#Command to create the vectors
eid<-c(1,2,3,4)
ename<-c("Ram","Raj","Scanda","Archie")
eage<-c(20,30,40,50)
eloc<-c("Mumbai","Chennai","Bangalore","Delhi")
esal<-c(20000,30000,40000,50000)
#Command to create the dataframe
e<-data.frame(
  eid,ename,eage,eloc,esal
)
#Command to display the structure of the dataframe
str(e)
#Command to display the dataframe in graphical form
fix(e)
e