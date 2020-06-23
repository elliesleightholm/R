#MATH2750 Practical

# Code needed before attempting questions:
# Produces a transition matrix for a Markov chain based on a parameter eps
TransMat <- function(eps) {
  matrix(c(0.1, 0.9,   0,       0,       0,   0,
           0.4, 0.2, 0.4,       0,       0,   0,
             0, 0.5,   0,     0.5,       0,   0,
             0,   0, 0.5, 0.5-eps,     eps,   0,
             0,   0,   0,     eps, 0.3-eps, 0.7,
             0,   0,   0,       0,     0.5, 0.5),
         nrow = 6, ncol = 6, byrow = TRUE)
}


# Simulates a Markov chain with transition matrix P for N steps from X0
MarkovChain <- function(N, X0, P) {
  X <- numeric(N)   # empty vector of length N
  space <- nrow(P)  # number of points in sample space
  
  now <- X0
  for (n in 1:N) {
    now <- sample(space, 1, prob = P[now, ])
    X[n] <- now
  }
  
  X
}


# Finds the stationary distribution of a Markov chain
StatDist <- function(P) {
  LeftEigen <- eigen(t(P))
  statmeas <- LeftEigen$vectors[, 1]  # Unnormalised stationary measure
  statmeas / sum(statmeas)            # Normalised stationary distribution
}




# Question 1 

# Set up variables and create the Markov Chain vector:
eps <- 0.2
P   <- TransMat(eps)
N   <- 50
X0  <- 1
X   <- MarkovChain(N, X0, P)

# Plot the Markov Chain for N and X defined above:
plot(0:N, c(1, X),
     type = "b",
     col  = "blue",
     xlab = "Time Step, n",
     ylim = c(1, 6),
     ylab = bquote("Markov Chain, " ~ X[n]), 
     main = bquote("Markov Chain " ~ .(N) ~ "steps, transition matrix with epsilon =" ~ .(eps)))


# Question 2 

# Create a table for the proportion of time spent in particular state (1 to 6):
num_visits <- table(factor(X, 1:6))
proportion_time <- num_visits/N

# Plot our proportion time against the Stationary Distribution:
stationary <- StatDist(P)
rbind(proportion_time, stationary)
barplot(rbind(proportion_time, stationary), beside = TRUE,
     col  = c("red","blue"),
     xlab = "States",
     ylab = "Overall Time Spent at Particular State",
     main = bquote("Proportion of Time Spent in Each State for" ~ .(N) ~ "Steps”),
     legend.text = c("Proportion of Time", "Stationary Distribution"))