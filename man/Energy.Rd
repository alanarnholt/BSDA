% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Energy}
\alias{Energy}
\title{Amount of energy consumed by homes versus their sizes}
\format{
A data frame/tibble with 12 observations on two variables
\describe{ 
\item{size}{size of home (in square feet)} 
\item{kilowatt}{killowatt-hours per month} 
}
}
\usage{
Energy
}
\description{
Data for Exercises 2.5, 2.24, and 2.55
}
\examples{

plot(kilowatt ~ size, data = Energy)
with(data = Energy, cor(size, kilowatt))
model <- lm(kilowatt ~ size, data = Energy)
plot(Energy$size, resid(model), xlab = "size")

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
