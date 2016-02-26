#' Confidence Interval Simulation Program
#' 
#' This program simulates random samples from which it constructs confidence
#' intervals for one of the parameters mean (Mu), variance (Sigma), or
#' proportion of successes (Pi).
#' 
#' Default is to construct confidence intervals for the population mean.
#' Simulated confidence intervals for the population variance or population
#' proportion of successes are possible by selecting the appropriate value in
#' the type argument.
#' 
#' @param samples the number of samples desired.
#' @param n the size of each sample.
#' @param mu if constructing confidence intervals for the population mean or
#' the population variance, mu is the population mean (i.e., type is one of
#' either \code{"Mean"}, or \code{"Var"}). If constructing confidence intervals
#' for the poulation proportion of successes, the value entered for mu
#' represents the population proportion of successes \code{(Pi)}, and as such,
#' must be a number between 0 and 1.
#' @param sigma the population standard deviation. \code{sigma} is not required
#' if confidence intervals are of type \code{"Pi"}.
#' @param conf.level confidence level for the graphed confidence intervals,
#' restricted to lie between zero and one.
#' @param type character string, one of \code{"Mean"}, \code{"Var"} or
#' \code{"Pi"}, or just the initial letter of each, indicating the type of
#' confidence interval simulation to perform.
#' @return Graph depicts simulated confidence intervals. The number of
#' confidence intervals that do not contain the parameter of interest are
#' counted and reported in the commands window.
#' @author Alan T. Arnholt
#' @keywords distribution
#' @examples
#' 
#' CIsim(100, 30, 100, 10)
#'     # Simulates 100 samples of size 30 from 
#'     # a normal distribution with mean 100
#'     # and standard deviation 10.  From the
#'     # 100 simulated samples, 95% confidence
#'     # intervals for the Mean are constructed 
#'     # and depicted in the graph. 
#' 
#' CIsim(100, 30, 100, 10, type="Var")
#'     # Simulates 100 samples of size 30 from 
#'     # a normal distribution with mean 100
#'     # and standard deviation 10.  From the
#'     # 100 simulated samples, 95% confidence
#'     # intervals for the variance are constructed 
#'     # and depicted in the graph.
#'     
#' CIsim(100, 50, .5, type="Pi", conf.level=.90)     
#'     # Simulates 100 samples of size 50 from 
#'     # a binomial distribution where the population
#'     # proportion of successes is 0.5.  From the
#'     # 100 simulated samples, 90% confidence
#'     # intervals for Pi are constructed 
#'     # and depicted in the graph.  
#' 
#' @export CIsim
CIsim <-
function(samples=100, n=30, mu=0, sigma=1, conf.level = 0.95, type ="Mean")
{
Adkblue <- "#0080FF"
Aorange <- "#FF4C0C"
alpha <-1-conf.level
CL<-conf.level*100
N <-samples
choices <- c("Mean", "Var", "Pi")
alt <- pmatch(type, choices)
type <- choices[alt]
if (length(type) > 1 || is.na(type))
stop("alternative must be one \"Mean\", \"Var\", \"Pi\"")
if (type == "Pi" && (mu <=0 |mu >= 1))
stop("Value for Pi (mu) must be between 0 and 1.")
if (N <= 0 ||  n <= 0)
stop("Number of random CIs (samples) and sample size (n) must both be at least 1")
if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1))
stop("'conf.level' must be a single number between 0 and 1")
if (sigma <= 0 &&  (type=="Var" || type=="Mean") )
stop("Variance must be a positive value")

if (type == "Mean")
   {
	 junk <- rnorm(N*n, mu, sigma)
	 jmat <- matrix(junk, N, n)
	 xbar <- apply(jmat, 1, mean)
	 ll <- xbar - qnorm(1 - alpha/2)*sigma/sqrt(n)
	 ul <- xbar + qnorm(1 - alpha/2)*sigma/sqrt(n)
	 notin <- sum((ll > mu) + (ul < mu))
	 percentage <- round((notin/N) * 100,2)
plot(ll, type = "n", ylim = c(min(ll), max(ul)), xlab = " ", ylab = " ")
title(sub=bquote(paste("Note: ",.(percentage),"% of the random confidence intervals do not contain ", mu ,"=", .(mu))))
title(main=bquote(paste(.(N), " random ", .(CL), "% confidence intervals where ", mu,  " = ", .(mu) )))
        for(i in 1:N)
    	  {
      		low<-ll[i];
      		high<-ul[i];
      		if(low < mu & high > mu)
      		{
          segments(i,low,i,high)
          }
      		else if(low > mu & high > mu )
      		{
          segments(i,low,i,high, col=Aorange, lwd=5)
          }
      		else
          {
          segments(i,low,i,high, col=Adkblue, lwd=5)
          }
    	  }
        abline(h = mu)
        cat(percentage,"% of the random confidence intervals do not contain Mu =", mu,".", "\n")
    }
else if (type == "Var")
    {
    junk <- rnorm(N*n, mu, sigma)
    jmat <- matrix(junk, N, n)
    s2 <- apply(jmat, 1, var)
    ll <- ((n - 1)*s2)/qchisq(1 - alpha/2, (n - 1))
    ul <- ((n - 1)*s2)/qchisq(alpha/2, (n -1))
    variance <- sigma^2
    notin <- sum((ll > variance) + (ul < variance))
    percentage <- round((notin/samples) * 100,2)
plot(ll, type = "n", ylim = c(min(ll), max(ul)), xlab = " ", ylab = " " )
title(sub=bquote(paste("Note: ",.(percentage),"% of the random confidence intervals do not contain ", sigma^2 ,"=", .(variance))))
title(main=bquote(paste(.(N), " random ", .(CL), "% confidence intervals where ", sigma^2,  " = ", .(variance) )))
	    for(i in 1:N)
	    {
		    low<-ll[i]
		    high<-ul[i]
		    if(low < variance & high > variance)
		    {
        segments(i,low,i,high)
        }
		    else if( low > variance & high > variance )
		    {
        segments(i,low,i,high, col=Aorange, lwd=5)
        }
		    else
        {
        segments(i,low,i,high, col=Adkblue, lwd=5)
        }
	    }
      abline(h = variance)
      cat(percentage,"% of the random confidence intervals do not contain Var =", sigma^2,".", "\n")
    }
else if (type == "Pi")
    {
    X <- rbinom(samples, n, mu)
    p <- X/n
    ll <- p - qnorm(1 - alpha/2)*sqrt((p * (1 - p))/n)
    ul <- p + qnorm(1 - alpha/2)*sqrt((p * (1 - p))/n)
    notin <- sum((ll > mu) + (ul < mu) )
    percentage <- round((notin/samples)*100,2)
plot(ll, type = "n", ylim = c(min(ll), max(ul)), xlab = " ", ylab = " " )
title(sub=bquote(paste("Note: ",.(percentage),"% of the random confidence intervals do not contain ",pi,"=",.(mu))))
title(main=bquote(paste(.(N), " random ", .(CL), "% confidence intervals where ", pi,  "=", .(mu) )))
	    for(i in 1:N)
	    {
  		low<-ll[i]
  		high<-ul[i]
  		if( low < mu & high > mu)
  		  {
        segments(i,low,i,high)
        }
  		  else if( low > mu & high > mu )
  		  {
        segments(i,low,i,high, col=Aorange, lwd=5)
        }
  		  else
        {
        segments(i,low,i,high, col=Adkblue, lwd=5)
        }
	    }
      abline(h = mu)
      cat(percentage,"% of the random confidence intervals do not contain Pi =", mu,".", "\n")
    }
}
