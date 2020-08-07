year = as.integer(readline(prompt = "Enter a year: "))

if((year %% 4) == 0) {
  if((year %% 100) == 0) {
    if((year %% 400) == 0) {
      print(paste(year, "is a leap year"))
    } else {
      print(paste(year, "is not a leap year"))
    }
  } else {
    print(paste(year, "is a leap year"))
  }
} else {
  print(paste(year, "is not a leap year" ))
}



num = as.double(readline(prompt = "Enter a number:"))

if (num > 0) {
  print("Positive number")
} else {
  if (num==0) {
    print("Zero")
  
} else {
  print("Negative number")
}
}



num = as.integer(readline(prompt = "Enter a number:"))

for (i in 10) {
  
  print(paste(num,'x',i,'=',num*i))
}

library(svDialogs)
user <- as.numeric(dlgInput("Enter a number:", Sys.info()[""])$res)
sum = 0

temp = user
while (temp > 0) {
  digit = temp %% 10
  
  sum = sum + (digit ^ 3)
  temp = floor(temp/10)
}
if (user == sum){
  print(paste(user, "is an armstrong number"))
} else {
  print(paste(user, "is not an armstrong number"))
}

num= as.integer(readline(prompt = "Enter the number"))

