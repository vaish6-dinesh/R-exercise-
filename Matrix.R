#To create the matrix
v1<-matrix(1:6,nrow = 4,ncol = 2)
#To create matrix with random elements
v2<-matrix(c(100,1,4,20,10,30,40,50),ncol = 4)
#To create matrix with a sequence, be precise with rows and colomns
v3<-matrix(seq(1,10.5,by=0.5),nrow = 5, ncol = 4)
#To access the matrix elements
v1[4,2]
v2[1,4]
#To change an specific element in the matrix
v1[4,2]<-50
#To display only the row or colomn
v1[1,]
v1[,1]
#To remove the elements in the matrix
v1<-NULL
#To remove the matrix
rm[v1]