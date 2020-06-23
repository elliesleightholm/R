data=rexp(100)
data
length(data)

exponential.lik<-function(y,par){
	n<-length(y)
	theta = par
	log1<-n*log(theta)-theta*sum(y)
	return(-log1)
}


resExp = optim(par=1,exponential.lik,y=data,method="BFGS")
resExp
theta = 1/mean(data)
cbind('direct'=c('theta'=1/mean(data)),
'optim'=resExp$par)

