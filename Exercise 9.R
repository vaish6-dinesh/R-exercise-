### Finding the maximum and minimum number ###

x <- c(5,8,3,9,2,7,4,6,10)

which.min(x)

which.max(x)

x[which.max(x)]

x[which.min(x)]


### Finding sum of natural numbers upto n using recursion ###

calculate_sum<-function(n)
{
  if(n<=1) {
    return(n)
  }else {
    return(n+calculate_sum(n-1))
  }
}
calculate_sum(5)

### Function to find the factors of a number ###

print_factors <- function(x) {
  print(paste("The factors of",x,"are;"))
  for(i in 1:x) {
    if ((x %% i) == 0) {
      print(i)
    }
  }
}

print_factors(18)

### 

num=as.integer(readline(prompt = "enter a number: ")

               
if((num %% 2)==0) {
  print(paste(num,"is Even"))
} else {
  print(paste(num,"is odd"))
}


x<-seq(1,100,by = 1)


for(i in 1:100){
if((i %% 2)==0) {
  print("Even")
  y<-c(y,i)
}
else
{
  print("Odd")
  z<-c(z,i)
}
}
print(paste("The even sequence is:",y))
print(paste("The odd sequence is:",z))

a<-data.frame(z,y)
View(a)