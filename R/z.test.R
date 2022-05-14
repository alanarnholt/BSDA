#' Z-test
#' 
#' This function is based on the standard normal distribution and creates
#' confidence intervals and tests hypotheses for both one and two sample
#' problems.
#' 
#' If \code{y} is \code{NULL}, a one-sample z-test is carried out with
#' \code{x}.  If y is not \code{NULL}, a standard two-sample z-test is
#' performed.
#' 
#' @param x numeric vector; \code{NA}s and \code{Inf}s are allowed but will be
#' removed.
#' @param y numeric vector; \code{NA}s and \code{Inf}s are allowed but will be
#' removed.
#' @param alternative character string, one of \code{"greater"}, \code{"less"}
#' or \code{"two.sided"}, or the initial letter of each, indicating the
#' specification of the alternative hypothesis. For one-sample tests,
#' \code{alternative} refers to the true mean of the parent population in
#' relation to the hypothesized value \code{mu}. For the standard two-sample
#' tests, \code{alternative} refers to the difference between the true
#' population mean for \code{x} and that for \code{y}, in relation to
#' \code{mu}.
#' @param mu a single number representing the value of the mean or difference
#' in means specified by the null hypothesis
#' @param sigma.x a single number representing the population standard
#' deviation for \code{x}
#' @param sigma.y a single number representing the population standard
#' deviation for \code{y}
#' @param conf.level confidence level for the returned confidence interval,
#' restricted to lie between zero and one
#' @return A list of class \code{htest}, containing the following components:
#' \item{statistic}{the z-statistic, with names attribute \code{"z"}}
#' \item{p.value}{the p-value for the test} \item{conf.int}{is a confidence
#' interval (vector of length 2) for the true mean or difference in means. The
#' confidence level is recorded in the attribute \code{conf.level}.  When
#' alternative is not \code{"two.sided"}, the confidence interval will be
#' half-infinite, to reflect the interpretation of a confidence interval as the
#' set of all values \code{k} for which one would not reject the null
#' hypothesis that the true mean or difference in means is \code{k} . Here
#' infinity will be represented by \code{Inf}.} \item{estimate}{vector of
#' length 1 or 2, giving the sample mean(s) or mean of differences; these
#' estimate the corresponding population parameters. Component \code{estimate}
#' has a names attribute describing its elements.} \item{null.value}{is the
#' value of the mean or difference in means specified by the null hypothesis.
#' This equals the input argument \code{mu}. Component \code{null.value} has a
#' names attribute describing its elements.} \item{alternative}{records the
#' value of the input argument alternative: \code{"greater"}, \code{"less"} or
#' \code{"two.sided"}.} \item{data.name}{a character string (vector of length
#' 1) containing the actual names of the input vectors \code{x} and \code{y}}
#' @section Null Hypothesis: For the one-sample z-test, the null hypothesis is
#' that the mean of the population from which \code{x} is drawn is \code{mu}.
#' For the standard two-sample z-tests, the null hypothesis is that the
#' population mean for \code{x} less that for \code{y} is \code{mu}.
#' 
#' The alternative hypothesis in each case indicates the direction of
#' divergence of the population mean for \code{x} (or difference of means for
#' \code{x} and \code{y}) from \code{mu} (i.e., \code{"greater"},
#' \code{"less"}, \code{"two.sided"}).
#' @author Alan T. Arnholt
#' @seealso \code{\link{zsum.test}}, \code{\link{tsum.test}}
#' @references Kitchens, L.J. (2003). \emph{Basic Statistics and Data
#' Analysis}. Duxbury.
#' 
#' Hogg, R. V. and Craig, A. T. (1970). \emph{Introduction to Mathematical
#' Statistics, 3rd ed}. Toronto, Canada: Macmillan.
#' 
#' Mood, A. M., Graybill, F. A. and Boes, D. C. (1974). \emph{Introduction to
#' the Theory of Statistics, 3rd ed}. New York: McGraw-Hill.
#' 
#' Snedecor, G. W. and Cochran, W. G. (1980). \emph{Statistical Methods, 7th
#' ed}. Ames, Iowa: Iowa State University Press.
#' @keywords htest
#' @examples
#' 
#' x <- rnorm(12)
#' z.test(x,sigma.x=1)
#'         # Two-sided one-sample z-test where the assumed value for
#'         # sigma.x is one. The null hypothesis is that the population
#'         # mean for 'x' is zero. The alternative hypothesis states
#'         # that it is either greater or less than zero. A confidence
#'         # interval for the population mean will be computed.
#' 
#' x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7., 6.4, 7.1, 6.7, 7.6, 6.8)
#' y <- c(4.5, 5.4, 6.1, 6.1, 5.4, 5., 4.1, 5.5)
#' z.test(x, sigma.x=0.5, y, sigma.y=0.5, mu=2)
#'         # Two-sided standard two-sample z-test where both sigma.x
#'         # and sigma.y are both assumed to equal 0.5. The null hypothesis
#'         # is that the population mean for 'x' less that for 'y' is 2.
#'         # The alternative hypothesis is that this difference is not 2.
#'         # A confidence interval for the true difference will be computed.
#' 
#' z.test(x, sigma.x=0.5, y, sigma.y=0.5, conf.level=0.90)
#'         # Two-sided standard two-sample z-test where both sigma.x and
#'         # sigma.y are both assumed to equal 0.5. The null hypothesis
#'         # is that the population mean for 'x' less that for 'y' is zero.
#'         # The alternative hypothesis is that this difference is not
#'         # zero.  A 90% confidence interval for the true difference will
#'         # be computed.
#' rm(x, y)
#' 
#' @export z.test
z.test <-
function(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = NULL, 
    sigma.y = NULL, conf.level = 0.95)
{
    choices <- c("two.sided", "greater", "less")
    alt <- pmatch(alternative, choices)
    alternative <- choices[alt]
    if(length(alternative) > 1 || is.na(alternative))
        stop("alternative must be one \"greater\", \"less\", \"two.sided\""
            )
    if(!missing(mu))
        if(length(mu) != 1 || is.na(mu))
            stop("mu must be a single number")
    if(is.null(sigma.x) && !is.null(x) && is.null(y))
        stop("You must enter the value for sigma.x")
    if(!is.null(y) && is.null(sigma.y) || is.null(sigma.x))
        stop("You must enter values for both sigma.x and sigma.y")
    if(!missing(conf.level))
        if(length(conf.level) != 1 || is.na(conf.level) || conf.level <
            0 || conf.level > 1)
            stop("conf.level must be a number between 0 and 1")
    if(!is.null(y)) {
        dname <- paste(deparse(substitute(x)), "and", paste(deparse(
            substitute(y))))
    }
    else {
        dname <- deparse(substitute(x))
    }
    # remove NAs 8/07/17
    xok <- !is.na(x)
    x <- x[xok]
    nx <- length(x)
    if(nx <= 2)
        stop("not enough x observations")
    mx <- mean(x)
    estimate <- mx
    if(is.null(y)) {
        stderr <- sigma.x/sqrt(nx)
        zobs <- (mx - mu)/stderr
        method <- c("One-sample z-Test")
        names(estimate) <- c("mean of x")
    }
    else {
        yok <- !is.na(y)
        y <- y[yok]
        ny <- length(y)
        if(ny <= 2)
            stop("not enough y observations")
        my <- mean(y)
        method <- c("Two-sample z-Test")
        estimate <- c(mx, my)
        names(estimate) <- c("mean of x", "mean of y")
        stderr <- sqrt(((sigma.x^2)/nx) + ((sigma.y^2)/ny))
        zobs <- (mx - my - mu)/stderr
    }
    if(alternative == "less") {
        pval <- pnorm(zobs)
        cint <- c(NA, zobs * stderr + qnorm(conf.level) * stderr)
    }
    else if(alternative == "greater") {
        pval <- 1 - pnorm(zobs)
        cint <- c(zobs * stderr - qnorm(conf.level) * stderr, NA)
    }
    else {
        pval <- 2 * pnorm( - abs(zobs))
        alpha <- 1 - conf.level
        cint <- c(zobs * stderr - qnorm((1 - alpha/2)) * stderr, zobs *
            stderr + qnorm((1 - alpha/2)) * stderr)
    }
    cint <- cint + mu
    names(zobs) <- "z"
    if(!is.null(y))
        names(mu) <- "difference in means"
    else names(mu) <- "mean"
    attr(cint, "conf.level") <- conf.level
    rval <- list(statistic = zobs, p.value = pval, conf.int = cint, 
        estimate = estimate, null.value = mu, alternative = 
        alternative, method = method, data.name = dname)
    attr(rval, "class") <- "htest"
    return(rval)
}
