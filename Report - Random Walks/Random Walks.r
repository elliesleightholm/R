# Computer Practical - Code - Markov Processes

#Qu1)
#Creating a sample of 1’s and -1’s with p = 0.6 and q = 0.4:

Z <- sample(c(1, -1), 10, replace = TRUE, prob = c(0.6, 0.4))
Z

#Altogether, the code to produce a random walk is:
p <- 0.6
q <- 1 - p
N <- 10
# Add your code for increments
Z <- sample(c(1, -1), N, replace = TRUE, prob = c(p, q))
# Should produce a string of 10 plus-or-minus 1s
Z    
# Turn increments into a random walk
X <- cumsum(Z) 
# Should produce something that looks like a random walk
X    

#Qu2)
#Plotting the graphs:
#For N = 10
p <- 0.6
q <- 1 - p
N <- 10
Z <- sample(c(1, -1), N, replace = TRUE, prob = c(p, q))
X <- cumsum(Z) 
plot(1:N, X,
      type = "b",
      col  = "red",
      ylim = c(-10,10),
      xlab = "Number of Steps Taken",
      ylab = "Increments of the random walk",
      main = "Random Walk for N = 10")

#For N = 10,000
p <- 0.6
q <- 1 - p
N <- 10000
Z <- sample(c(1, -1), N, replace = TRUE, prob = c(p, q))
X <- cumsum(Z) 
plot(1:N, X,
      type = "b",
      col  = "blue",
      xlab = "Number of Steps Taken",
      ylab = "Increments of the random walk",
      main = "Random Walk for N = 10000")
      
#Qu 3) 

RandomWalk <- function(N, p) {
  Z <- sample(c(1, -1), N, replace = TRUE, prob = c(p, 1-p))
  cumsum(Z)
}

#Plots:

#plot(RandomWalk(20,0.3),
#      type = "b",
#      col  = "purple",
#      ylim = c(-20,20),
#      xlab = "Number of Steps Taken",
#      ylab = "Increments of the random walk",
#      main = "Random Walk for N = 20 and Probability 0.3")


#Qu 4)
#This is the code but you can vary the value of p
N <- 100
p <- 0.5
trials <- 10000  # Or some other appropriate large number
samples <- replicate(trials, RandomWalk(N, p)[N])
exp = sum(samples)/trials
exp
# Now add code to output the average of samples


