% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Rehab}
\alias{Rehab}
\title{Rehabilitative potential of 20 prison inmates as judged by two psychiatrists}
\format{
A data frame/tibble with 20 observations on four variables
\describe{ 
\item{inmate}{inmate identification number} 
\item{psych1}{rating from first psychiatrist on the inmates rehabilative potential} 
\item{psych2}{rating from second psychiatrist on the inmates rehabilative potential} 
\item{differ}{\code{psych1} - \code{psych2}}
}
}
\usage{
Rehab
}
\description{
Data for Exercise 7.61
}
\examples{

boxplot(Rehab$differ)
qqnorm(Rehab$differ)
qqline(Rehab$differ)
t.test(Rehab$differ)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
