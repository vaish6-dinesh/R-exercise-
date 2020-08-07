### Program to find the factorial of a number imputed by user ###

# take input from the user

num = as.integer(readline(prompt = "Enter a number: "))

factorial = 1

if(num < 0) {
  print("Sorry, Factorial does not exist for -ve numbers")
} else if(num==0) {
  print("The factorial of 0 is 1")
}
else if{
  for(i in 1:num)
    factorial = factorial * i
}
print(paste("The factorial of", num ,"is", factorial))
