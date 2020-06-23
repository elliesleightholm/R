#Question 1 Code:
#This code sets the graph height and width:
options(repr.plot.width = 8, repr.plot.height = 8)

#We now input code that calculates the mean for our sample size, finds the fit for the Normal Distribution and then plots our Poisson Distribution agaist the Normal for our particular sample size, n. We do this with the following code:

# Define our function, outputplotCLT:
outputplotCLT <- function(input){
#We define the number of sample means
  samp = 1000
#We now define the sample size as follows
  n = input$rvs
#Now we sample according to the Poisson Distribution. In the code below the number of rows in the matrix is the number of sample means (defined above). The number of columns in the matrix is the sample size (also defined above). We then need to calculate the mean and standard deviation of the approximate normal. For a Poisson Distribution with lambda=2 we have, mean = 2 and variance = 2/n. Putting this together, we get the following code:
 cases=c("[1] Pois(2)")
  if(input$select == 1){
    matrix = matrix(rpois(n*samp, 2), nrow = samp, ncol = n)
    mu = 2
    sd = sqrt(2)/sqrt(n)
  }
#Now we calculate the means from each sample with the following code
  means = rowMeans(matrix)
#Next, we need to find a fit for the normal (estimate parameters)
  xfit = seq(from = min(means), to = max(means), by = .001)
  yfit = dnorm(xfit, mean = mu, sd = sd)
#Now we plot the means and overlay this over the Normal Distribution. We are able to do this with the following code
#We also give the graph a title, labels and a key as follows:
  plot(density(means), main = "", xlab = "",
       ylab = "Density", col = rgb(0, 1, 0, 3/4),
       lwd = 8, ylim = c(0, max(density(means)$y, yfit)))
  lines(xfit, yfit, col = rgb(0, 0, 1, 3/4), lwd = 8)
# Title and Key:
  title(main = paste(c("n=",input$rvs," Case=",cases[input$select]),collapse=''))
      legend("topright", legend = c("Sample Mean Density", "Estimated Normal Density"),
         lty=c(1,1), lwd=c(2.5,2.5),
         col=c(rgb(0, 1, 0, 3/4), rgb(0, 0, 1, 3/4)))
}

#Now we have the function define above, we can plot graphs with different sample sizes. First, since we have used the 'cases' command we must now set up code that allows the reader to select the Poisson Distribution. We do this as follows:
select=as.integer(readline(prompt=paste(c("Enter 1 for Pois(2)\n"),collapse='')))

#Once the reader selects the Poisson Distribution we can now set up a sample size input. The following command allows the reader to input their desired sample size:
rvs=as.integer(readline(prompt = "Enter a sample size\n"))

#Now we have all the code we need, all that is left is to plot the graph. We do this by the following command:
input=NULL
input$select=select
input$rvs=rvs
outputplotCLT(input)

#The command above plots one single graph. However, we can tell R to plot a sequence of graphs. The following code tells R to plot graphs with sample sizes from n = 1 to n = 70001 with n increasing by 10000 each time. This allows us to produce multiple graphs in one simple command and means we are able to compare any differences quickly and efficiently: 
input=NULL
input$select=1
for (i in seq(1,80000,10000)){
    input$rvs=i
    outputplotCLT(input)
}
# After plotting this, we observe that the sample mean of the Poisson Distribution approaches the Normal Distribution for very large n. 

#Question 2 Code

#First, we put specify the data range we want to use when optimising. Since we are implementing MLE for the exponential distribution, our data range must be random data from an exponential distribution. Therefore, we define our data range as follows:
data=rexp(100)
# We can callback our data to observe the types of data we get
data
# Further, we can check that the length of the data is indeed 100.
length(data)

#Now we have specified our data range, we can create the log-likelihood function for the exponential distribution. Since the exponential distribution only has one parameter, we only need to find the maximum likelihood for one parameter, we'll call this theta. We do this as follows
exponential.lik<-function(y,par){
# This function requires knowledge of the sample size, we use the next line to find the sample size of our data
	n<-length(y)
	theta = par
# Define the log likelihood for the exponential distribution
	log1<-n*log(theta)-theta*sum(y)
# We need to return -1 times the log likelihood because the optim command minimises the function and we require the maximum.
	return(-log1)
}

#We must optimise our function with our given data values. We are able to use the optim command with our data included to optimise. We also use the BFGS method as defined in our code below. We do this as follows:
resExp = optim(par=1,exponential.lik,y=data,method="BFGS")

#We can observe values given by the optim function by recalling resExp. This gives us 'par' which is the best set of parameters found (our optimal value). It also produces 'value' which gives the corresponding value of our function, 'exponential.lik' to par. Furthermore, it also gives 'counts' which is the number of calls to the function and its gradient respectively. 
resExp

#Since we are considering the exponential distribution, we know that our direct estimation must be 1 divided by the mean of the data. We define theta as follows: 
theta = 1/mean(data)

#Finally, we can create a table to compare the direct estimation with our MLE. Here, we use the 'cbind' command to produce a table with the values we have found above. This allows us to observe any similarities or differences between the direct estimation and the MLE. 
cbind('direct'=c('theta'=1/mean(data)),
'optim'=resExp$par)

