% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Autogear}
\alias{Autogear}
\title{Number of defective auto gears produced by two manufacturers}
\format{
A data frame/tibble  with 20 observations on two variables
\describe{ 
\item{defectives}{number of defective gears in the production of 100 gears per day} 
\item{manufacturer}{a factor with levels \code{A} and \code{B}} 
}
}
\usage{
Autogear
}
\description{
Data for Exercise 7.46
}
\examples{

t.test(defectives ~ manufacturer, data = Autogear)
wilcox.test(defectives ~ manufacturer, data = Autogear)
t.test(defectives ~ manufacturer, var.equal = TRUE, data = Autogear)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
