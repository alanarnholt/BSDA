% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Golf}
\alias{Golf}
\title{Distances a golf ball can be driven by 20 professional golfers}
\format{
A data frame/tibble with 20 observations on one variable
\describe{ 
\item{yards}{distance a golf ball is driven in yards} 
}
}
\usage{
Golf
}
\description{
Data for Exercise 5.24
}
\examples{

stem(Golf$yards)
qqnorm(Golf$yards)
qqline(Golf$yards)

\dontrun{
library(ggplot2)
ggplot2::ggplot(data = Golf, aes(sample = yards)) + 
           geom_qq() + 
           theme_bw()
}

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
