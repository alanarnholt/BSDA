% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Weather94}
\alias{Weather94}
\title{Weather casualties in 1994}
\format{
A data frame/tibble with 388 observations on one variable
\describe{ 
\item{type}{factor with levels \code{Extreme Temp}, \code{Flash Flood}, 
\code{Fog}, \code{High Wind}, \code{Hurricane}, \code{Lighting}, \code{Other}, 
\code{River Flood}, \code{Thunderstorm}, \code{Tornado}, and \code{Winter Weather}} 
}
}
\usage{
Weather94
}
\description{
Data for Exercise 1.30
}
\examples{

T1 <- xtabs(~type, data = Weather94)
T1
par(mar = c(5.1 + 2, 4.1 - 1, 4.1 - 2, 2.1))
barplot(sort(T1, decreasing = TRUE), las = 2, col = rainbow(11))
par(mar = c(5.1, 4.1, 4.1, 2.1))
\dontrun{
library(ggplot2)
T2 <- as.data.frame(T1)
T2
ggplot2::ggplot(data =T2, aes(x = reorder(type, Freq), y = Freq)) + 
           geom_bar(stat = "identity", fill = "purple") +
           theme_bw() + 
           theme(axis.text.x  = element_text(angle = 55, vjust = 0.5)) + 
           labs(x = "", y = "count")
}

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
