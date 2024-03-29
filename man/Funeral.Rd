% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Funeral}
\alias{Funeral}
\title{Cost of funeral by region of country}
\format{
A data frame/tibble with 400 observations on two variables
\describe{ 
\item{region}{a factor with levels \code{Central},
\code{East,} \code{South}, and \code{West}} 
\item{cost}{a factor with levels \code{less than expected}, \code{about what expected}, 
and \code{more than expected}}
}
}
\usage{
Funeral
}
\description{
Data for Exercise 8.54
}
\examples{

T1 <- xtabs(~region + cost, data = Funeral)
T1
chisq.test(T1)  
rm(T1)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
