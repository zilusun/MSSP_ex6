#ex6.1
#Create a function to calculate the area of a circle
func1 <- function(x){x = (x/2)^2*pi;return(x)}
#Test the function by finding the area of a circle with a diameter of 3.4 cm.
x=3.4
y=func1(x)
#use it on a vector of data
diameter <- c(1,2,3,4,5) #create a vector of data "diameter"
sapply(diameter,func1)


#ex6.2 
#Write a function to convert farenheit to centegrade
FtoC <- function(oF){oC = (oF-32)*(5/9) ; return(oC)}
oF=50 #test with oF=50
oC=FtoC(oF)
# print out your result in the following format: “Farenheit : value of oF is equivalent to value oC centigrade.”
cat("Farenheit: value of ",oF ,"is equivalent to value" ,oC ,"centigrade")


#ex6.3
#Create a vector of normally distributed data, of length 100, mean 35 and standard deviation of 15.
summary_var <- function(x){
  cat("mean : ",mean(x),"\n")
  cat("median:",median(x),"\n")
  cat("range:between",range(x)[1],"and",range(x)[2],"\n")
  dens <- density(x)
  hist(x, main = "", freq = FALSE)
  lines(dens, lty = 1, col = "red")
}
summary_var(rnorm(100,35,15))


#ex6.4
#Write a function to calculate the median value of a vector of numbers
func2 <- function(x){
  sorted_x <- sort(x) 
  n <- length(x)
  if (n %% 2 == 0){
    median_x <- 0.5*(sorted_x[n/2]+sorted_x[n/2 + 1])}
  else{
    median_x <- sorted_x[(n+1)/2]
  }
  return(median_x)}
#test odds
x <- c(1,2,3,4,5)
result <- func2(x)
print(result)
#test evens
x <- c(1,2,3,4,5,6)
result <- func2(x)
print(result)

#ex6.5
#Write a function to simulate this model
riker <- function(nzero,r,time,K=100){
  N <- c()
  N[1] <- nzero
  for (t in 1:time) {
    N[t+1] <- N[t]*exp(r * (1 - N[t] / K))
  }
  return(N)
}
#test
nzero <- 1
r <- 0.5
time <- 15
K=100
population <- riker(nzero,r,time,K=100)
print(population)
plot(population, type = 'l', xlab = "Time", ylab = "Population", main = "Ricker Model Simulation")



