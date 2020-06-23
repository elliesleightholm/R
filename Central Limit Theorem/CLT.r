#Question 1 Code:
#Graph height and width
options(repr.plot.width = 8, repr.plot.height = 8)

#Creating the Function
outputplotCLT <- function(input){
  samp = 1000
  n = input$rvs
 cases=c("[1] Pois(2)")
  if(input$select == 1){
    matrix = matrix(rpois(n*samp, 2), nrow = samp, ncol = n)
    mu = 2
    sd = sqrt(2)/sqrt(n)
  }
   
  means = rowMeans(matrix)
  xfit = seq(from = min(means), to = max(means), by = .001)
  yfit = dnorm(xfit, mean = mu, sd = sd)
  plot(density(means), main = "", xlab = "",
       ylab = "Density", col = rgb(0, 1, 0, 3/4),
       lwd = 8, ylim = c(0, max(density(means)$y, yfit)))
  lines(xfit, yfit, col = rgb(0, 0, 1, 3/4), lwd = 8)
  title(main = paste(c("n=",input$rvs," Case=",cases[input$select]),collapse=''))
      legend("topright", legend = c("Sample Mean Density", "Estimated Normal Density"),
         lty=c(1,1), lwd=c(2.5,2.5),
         col=c(rgb(0, 1, 0, 3/4), rgb(0, 0, 1, 3/4)))
}

#Choosing the Poisson Distribution
select=as.integer(readline(prompt=paste(c("Enter 1 for Pois(2)\n"),collapse='')))

#Inputting required sample size
rvs=as.integer(readline(prompt = "Enter a sample size\n"))

#Plotting the graph
input=NULL
input$select=select
input$rvs=rvs
outputplotCLT(input)

#Plotting a sequence of graphs from n = 1 to n = 80001 with n increasing by 10000 each time
input=NULL
input$select=1
for (i in seq(1,100000,10000)){
    input$rvs=i
    outputplotCLT(input)
}

#Question 2

#Inputting random data range for exponential distribution
data=rexp(100)
data
length(data)

#Creating the log-likelihood function
exponential.lik<-function(y,par){
	n<-length(y)
	theta = par
	log1<-n*log(theta)-theta*sum(y)
	return(-log1)
}

#Optimising the function
resExp = optim(par=1,exponential.lik,y=data,method="BFGS")
resExp

#Stating the parameters
theta = 1/mean(data)

#Creating a table to compare the direct estimation with the MLE
cbind('direct'=c('theta'=1/mean(data)),
'optim'=resExp$par)

