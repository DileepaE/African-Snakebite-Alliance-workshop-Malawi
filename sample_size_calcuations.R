#Sampling Size of Population Proportion-----
#Using a 50% planned proportion estimate, 
#find the sample size needed to achieve 5% margin of error for the female student survey at 95% confidence level.
zstar = qnorm(.975) 
zstar 
p = 0.50 
E = 0.05 
n<-zstar^2*p*(1-p)/E^2 
n


#power for a sample with 300
pnorm(((0.05 * sqrt(300)) - (qnorm(0.975) * sqrt(0.5 * 0.5))) / sqrt(0.5 * 0.5))


#Finite Population Correction (FPC)
N <- 170
n*((N-n)/(N-1))^0.5
371/(1+n/170)


#Sampling Size of Population Mean----
#Assume the population standard deviation σ of the student height in survey is 9.48. 
#Find the sample size needed to achieve a 1.2 centimeters margin of error at 95% confidence level.
zstar = qnorm(.975) 
sigma = 9.48 
E = 1.2 
zstar^2 * sigma^2/ E^2 


#power for a sample with 200 
pnorm((1.2 * sqrt(200) / 9.48) - qnorm(0.975))


#proportion comparison----
qnorm(0.8) #80% power
qnorm(0.95) #95% power one tail
qnorm(0.975) #95% power two tail

r <- 1
p1 <- 0.2
p2 <- 0.33 
p.bar <- (p1+p2)/2

(((0.84+1.96)^2)*(p1*(1-p1) + p2*(1-p2)))/((p1-p2)^2) 

#power for a sample with 100
pnorm(sqrt((100* (p1 - p2)^2) / (p1 * (1 - p1) + p2 * (1 - p2))) - 1.96)


#mean comparison-----
r <-1
sigma1 <- 16
sigma2 <- 10
difference <- 92 - 85

((r + 1) * (0.84 + 1.96)^2 * (sigma1^2 + (sigma2^2 / r))) / (2 * (difference)^2)

#power for a sample with 25 
pnorm(sqrt((2 * 25 * difference^2) / ((r + 1) * (sigma1^2 + (sigma2^2 / r)))) - 1.96)





