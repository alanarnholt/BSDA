% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Pearson}
\alias{Pearson}
\title{Karl Pearson's data on heights of brothers and sisters}
\format{
A data frame/tibble with 11 observations on three variables
\describe{ 
\item{family}{number indicating family of brother and sister pair}
\item{brother}{height of brother (in inches)} 
\item{sister}{height of sister (in inches)} 
}
}
\source{
Pearson, K. and Lee, A. (1902-3), On the Laws of Inheritance in Man, 
\emph{Biometrika, 2}, 357.
}
\usage{
Pearson
}
\description{
Data for Exercise 2.20
}
\examples{

plot(brother ~ sister, data = Pearson, col = "lightblue")
cor(Pearson$brother, Pearson$sister)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
