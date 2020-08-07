#Simple Pie chart
Cloth<-c(20,15,30,20,15)
labls<-c("Jeans","Cotton","Chinos","Corduroy","Linen")
pie(Cloth,labels = labls,main = "User Preference of Pants")

# Pie Chart with Percentages
Cloth<-c(20,15,30,20,15)
lbls<-c("Jeans","Cotton","Chinos","Corduroy","Linen")
pct<-round(Cloth/sum(Cloth)*100)
lbls<-paste(lbls,pct)
lbls<-paste(lbls,"%",sep = "")
pie(Cloth,labels = lbls, col=rainbow(length(lbls)),main = "User Preference of Pants")