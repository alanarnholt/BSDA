% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Drink}
\alias{Drink}
\title{Opinion on referendum by view on moral issue of selling alcoholic beverages}
\format{
A data frame/tibble with 472 observations on two variables
\describe{ 
\item{drinking}{a factor with levels \code{ok},
\code{tolerated}, and \code{immoral}} 
\item{referendum}{a factor with levels \code{for}, \code{against}, and \code{undecided}} 
}
}
\usage{
Drink
}
\description{
Data for Exercise 8.53
}
\examples{

T1 <- xtabs(~drinking + referendum, data = Drink)
T1
chisq.test(T1)
rm(T1)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
