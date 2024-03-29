% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Soccer}
\alias{Soccer}
\title{Weights of 25 soccer players}
\format{
A data frame/tibble with 25 observations on one variable
\describe{ 
\item{weight}{soccer players weight (in pounds)} 
}
}
\usage{
Soccer
}
\description{
Data for Exercise 1.46
}
\examples{

stem(Soccer$weight, scale = 2)
hist(Soccer$weight, breaks = seq(110, 210, 10), col = "orange",
     main = "Problem 1.46 \n Weights of Soccer Players", 
     xlab = "weight (lbs)", right = FALSE)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
