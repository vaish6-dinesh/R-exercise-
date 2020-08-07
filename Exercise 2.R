#To display the top 6 rows
head(emp)
#To display the last 6 rows 
tail(emp)
emp[,c(1,2)]
View(emp[,c(1,2)])

#To display the information such as:
#mean,median,1st quater,max,min,2nd quater and no of rows displaying "NA"
summary(emp)

#To create an independent variable
x<-mean(emp$Age)
#To convert the value to absolute
x<-floor(x)
#To check and replace the columns with "NA" 
emp$Age[is.na(emp$Age)]<-x
View(emp)

data("USPersonalExpenditure")
View(USPersonalExpenditure)
library(graphics)
barplot(USPersonalExpenditure,
        main="Personal expenditure for a year",
        xlab="year",
        ylab="expenditures",
        col=c("red","green","yellow","blue","violet"),
        legend=T)

#displays the subset of the particular year specified 
dfsub<-subset(USPersonalExpenditure, colnames=1940)
dfsub
wv=dfsub[ ,1]
pie(c(wv), main="1940 US Expenditure", col=c("red","green", "blue"))
wv

#Displays the row data data based on different condition

dfsub<-subset(USPersonalExpenditure, colnames=1950)
dfsub1<-subset(df, revenue < 1000)
dfsub2<-subset(df, population > 100 & revenue < 1000)
wv=dfsub[1:5]
dfsub1
dfsub2
wv

#Displays the column names of the perticular dataset
colnames(USPersonalExpenditure)

data()
data(iris)
View(iris)
