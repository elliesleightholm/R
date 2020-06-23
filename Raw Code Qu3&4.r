# Question 3

# We create a function that returns the expected return time for a specific value:
expected_return <- function(vector, value){
	visit <- which(vector == value)
	return_time <- diff(visit)
	mean(return_time)
}

# We can extend this further to return the expected return time for all states for a given vector:
expected_return_times <- sapply(1:6, expected_return, vector = X)
expected_return_times

# Now we observe the relationship between the expected return times, µ, and the stationary distribution, π. We know that the two are related as follows: 1/µi = πi. We can observe whether this is true for our values by inputting the following:
barplot(rbind(1/expected_return_times,stationary), beside = TRUE,
         col = c("red","blue"),
         xlab = "States",
         ylab = "Expected Return Times",
         main = bquote("Expected Return Time vs Stationary Distribution for" ~.(N)~ "Steps"),
         legend.text = c("1/µ", "Stationary Distribution")
 )

# Again, we can alter the value of N (as defined above) and observe the behaviour as N is increased. We observe that as N is increased, the relation defined above is satisfied. 

# Question 4

# No code is required for this question but we will input some values to observe the behaviour of the Markov chain when we choose an epsilon value close to 0. 
eps <- 0.0001
P   <- TransMat(eps)
N   <- 50
X0  <- 1
X   <- MarkovChain(N, X0, P)
table(factor(X, 1:6))

# We observe that choosing an epsilon value close to 0 lowers the probability of landing on a 5 or a 6. This is intuitive after visualising the probability diagram given for this particular matrix. 