#' Normality Tester
#' 
#' Q-Q plots of randomly generated normal data of the same size as the tested
#' data are generated and ploted on the perimeter of the graph while a Q-Q plot
#' of the actual data is depicted in the center of the graph.
#' 
#' Q-Q plots of randomly generated normal data of the same size as the tested
#' data are generated and ploted on the perimeter of the graph sheet while a
#' Q-Q plot of the actual data is depicted in the center of the graph.  The
#' p-values are calculated form the Shapiro-Wilk W-statistic. Function will
#' only work on numeric vectors containing less than or equal to 5000
#' observations.
#' 
#' @param actual.data a numeric vector. Missing and infinite values are
#' allowed, but are ignored in the calculation. The length of
#' \code{actual.data} must be less than 5000 after dropping nonfinite values.
#' @author Alan T. Arnholt
#' @references Shapiro, S.S. and Wilk, M.B. (1965). An analysis of variance
#' test for normality (complete samples). Biometrika \bold{52} : 591-611.
#' @keywords distribution
#' @examples
#' 
#' ntester(rexp(50,1))
#'     # Q-Q plot of random exponential data in center plot
#'     # surrounded by 8 Q-Q plots of randomly generated 
#'     # standard normal data of size 50.
#' 
#' @export ntester
ntester <-
function(actual.data)
{
    Ared <- "#C00000"
    Ablue <- "#0080FF"
    par(mfrow = c(3, 3))
    par(oma = c(1, 0, 2, 0))
    par(mar = c(0, 0, 2, 0))
    par(pty = "s")
    for(i in 1:4) {
        SimData <- rnorm(length(actual.data))
        s <- shapiro.test(SimData)
        qqnorm(SimData, xlab = "", ylab = "", axes = FALSE, col = Ablue, main=paste("SimNorm p-val = ", round(s$p.value, 3)),col.main=Ablue)
        box()
        qqline(SimData, col = Ared)
    }
    sx <- shapiro.test(actual.data)
    qqnorm(actual.data, xlab = "", ylab = "", axes =FALSE, col = Ared, main = paste("YourData p-val = ", round(sx$p.value, 3)),col.main=Ared)
    box()
    qqline(actual.data, col = Ablue)
    for(i in 6:9) {
        SimData <- rnorm(length(actual.data))
        s <- shapiro.test(SimData)
        qqnorm(SimData, xlab = "", ylab = "", axes = FALSE, col = Ablue, main= paste("SimNorm p-val = ", round(s$p.value, 3)),col.main=Ablue)
        box()
        qqline(SimData, col = Ared)
    }
    mtext("Simulated Normal Data on Perimeter - Actual Data in Center", side = 3, outer = TRUE, cex = 1.5, col = Ared)
    par(oma = c(0, 0, 0, 0))
    par(mfrow = c(1, 1))
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    par(pty = "m")
}
