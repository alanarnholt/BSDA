#' Summarized t-test
#' 
#' Performs a one-sample, two-sample, or a Welch modified two-sample t-test
#' based on user supplied summary information. Output is identical to that
#' produced with \code{t.test}.
#' 
#' If \code{y} is \code{NULL}, a one-sample t-test is carried out with
#' \code{x}. If y is not \code{NULL}, either a standard or Welch modified
#' two-sample t-test is performed, depending on whether \code{var.equal} is
#' \code{TRUE} or \code{FALSE}.
#' 
#' @param mean.x a single number representing the sample mean of \code{x}
#' @param s.x a single number representing the sample standard deviation for
#' \code{x}
#' @param n.x a single number representing the sample size for \code{x}
#' @param mean.y a single number representing the sample mean of \code{y}
#' @param s.y a single number representing the sample standard deviation for
#' \code{y}
#' @param n.y a single number representing the sample size for \code{y}
#' @param alternative is a character string, one of \code{"greater"},
#' \code{"less"} or \code{"two.sided"}, or just the initial letter of each,
#' indicating the specification of the alternative hypothesis. For one-sample
#' tests, \code{alternative} refers to the true mean of the parent population
#' in relation to the hypothesized value \code{mu}.  For the standard
#' two-sample tests, \code{alternative} refers to the difference between the
#' true population mean for \code{x} and that for \code{y}, in relation to
#' \code{mu}.  For the one-sample and paired t-tests, \code{alternative} refers
#' to the true mean of the parent population in relation to the hypothesized
#' value \code{mu}. For the standard and Welch modified two-sample t-tests,
#' \code{alternative} refers to the difference between the true population mean
#' for \code{x} and that for \code{y}, in relation to \code{mu}.  For the
#' one-sample t-tests, alternative refers to the true mean of the parent
#' population in relation to the hypothesized value \code{mu}. For the standard
#' and Welch modified two-sample t-tests, alternative refers to the difference
#' between the true population mean for \code{x} and that for \code{y}, in
#' relation to \code{mu}.
#' @param mu is a single number representing the value of the mean or
#' difference in means specified by the null hypothesis.
#' @param var.equal logical flag: if \code{TRUE}, the variances of the parent
#' populations of \code{x} and \code{y} are assumed equal. Argument
#' \code{var.equal} should be supplied only for the two-sample tests.
#' @param conf.level is the confidence level for the returned confidence
#' interval; it must lie between zero and one.
#' @return A list of class \code{htest}, containing the following components:
#' \item{statistic}{the t-statistic, with names attribute \code{"t"}}
#' \item{parameters}{is the degrees of freedom of the t-distribution associated
#' with statistic.  Component \code{parameters} has names attribute
#' \code{"df"}.} \item{p.value}{the p-value for the test.} \item{conf.int }{is
#' a confidence interval (vector of length 2) for the true mean or difference
#' in means. The confidence level is recorded in the attribute
#' \code{conf.level}. When alternative is not \code{"two.sided"}, the
#' confidence interval will be half-infinite, to reflect the interpretation of
#' a confidence interval as the set of all values \code{k} for which one would
#' not reject the null hypothesis that the true mean or difference in means is
#' \code{k} . Here infinity will be represented by \code{Inf}.}
#' \item{estimate}{vector of length 1 or 2, giving the sample mean(s) or mean
#' of differences; these estimate the corresponding population parameters.
#' Component \code{estimate} has a names attribute describing its elements.}
#' \item{null.value}{the value of the mean or difference in means specified by
#' the null hypothesis. This equals the input argument \code{mu}. Component
#' \code{null.value} has a names attribute describing its elements.}
#' \item{alternative}{records the value of the input argument alternative:
#' \code{"greater"} , \code{"less"} or \code{"two.sided"}.} \item{data.name}{a
#' character string (vector of length 1) containing the names x and y for the
#' two summarized samples.}
#' @section Null Hypothesis: For the one-sample t-test, the null hypothesis is
#' that the mean of the population from which \code{x} is drawn is \code{mu}.
#' For the standard and Welch modified two-sample t-tests, the null hypothesis
#' is that the population mean for \code{x} less that for \code{y} is
#' \code{mu}.
#' 
#' The alternative hypothesis in each case indicates the direction of
#' divergence of the population mean for \code{x} (or difference of means for
#' \code{x} and \code{y}) from \code{mu} (i.e., \code{"greater"},
#' \code{"less"}, or \code{"two.sided"}).
#' @author Alan T. Arnholt
#' @seealso \code{\link{z.test}}, \code{\link{zsum.test}}
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
#' tsum.test(mean.x=5.6, s.x=2.1, n.x=16, mu=4.9, alternative="greater")
#'         # Problem 6.31 on page 324 of BSDA states:  The chamber of commerce
#'         # of a particular city claims that the mean carbon dioxide
#'         # level of air polution is no greater than 4.9 ppm.  A random
#'         # sample of 16 readings resulted in a sample mean of 5.6 ppm,
#'         # and s=2.1 ppm.  One-sided one-sample t-test.  The null 
#'         # hypothesis is that the population mean for 'x' is 4.9.   
#'         # The alternative hypothesis states that it is greater than 4.9.  
#' 
#' x <- rnorm(12) 
#' tsum.test(mean(x), sd(x), n.x=12)
#'         # Two-sided one-sample t-test. The null hypothesis is that  
#'         # the population mean for 'x' is zero. The alternative 
#'         # hypothesis states  that it is either greater or less 
#'         # than zero. A confidence interval for the population mean 
#'         # will be computed.  Note: above returns same answer as: 
#' t.test(x)
#'    
#' x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7.0, 6.4, 7.1, 6.7, 7.6, 6.8) 
#' y <- c(4.5, 5.4, 6.1, 6.1, 5.4, 5.0, 4.1, 5.5) 
#' tsum.test(mean(x), s.x=sd(x), n.x=11 ,mean(y), s.y=sd(y), n.y=8, mu=2)
#'         # Two-sided standard two-sample t-test.  The null hypothesis  
#'         # is that the population mean for 'x' less that for 'y' is 2. 
#'         # The alternative hypothesis is that this difference is not 2. 
#'         # A confidence interval for the true difference will be computed.
#'         # Note: above returns same answer as: 
#' t.test(x, y)
#'         
#' tsum.test(mean(x), s.x=sd(x), n.x=11, mean(y), s.y=sd(y), n.y=8, conf.level=0.90)
#'         # Two-sided standard two-sample t-test.  The null hypothesis 
#'         # is that the population mean for 'x' less that for 'y' is zero.  
#'         # The alternative hypothesis is that this difference is not
#'         # zero.  A 90% confidence interval for the true difference will 
#'         # be computed.  Note: above returns same answer as:
#' t.test(x, y, conf.level=0.90)
#' 
#' 
#' @export tsum.test
tsum.test <-
function(mean.x, s.x = NULL, n.x = NULL, mean.y = NULL, s.y = NULL, n.y = NULL,
    alternative = "two.sided", mu = 0, var.equal = FALSE, conf.level = 0.95)
{
    alt.expanded <- if(!missing(alternative)) char.expand(alternative,
            c("two.sided", "greater", "less"), stop(
            "argument 'alternative' must match one of \"greater\", \"less\", \"two.sided\"."
            )) else alternative
    if(!missing(mu))
        if((length(mu) != 1) || !is.finite(mu))
            stop("argument 'mu' must be a single finite numeric value."
                )
    if(!missing(conf.level))
        if((length(conf.level) != 1) || !is.finite(conf.level) || (
            conf.level <= 0) || (conf.level >= 1))
            stop("argument 'conf.level' must be a single number greater than zero and less than one \n.")
    if(!is.null(mean.x) && is.null(mean.y) && is.null(n.x) && is.null(
        s.x))
        stop("You must enter the value for both s.x and n.x")
    if(is.null(n.x) && !is.null(mean.x) && !is.null(s.x) && is.null(mean.y
        ))
        stop("You must enter the value for n.x")
    if(is.null(s.x) && !is.null(mean.x) && !is.null(n.x) && is.null(mean.y
        ))
        stop("You must enter the value for s.x")
    if(is.null(n.y) && !is.null(mean.x) && !is.null(mean.y) && !is.null(
        s.y) && !is.null(s.x) && !is.null(n.x))
        stop("You must enter the value for n.y")
    if(is.null(n.y) && is.null(n.x) && !is.null(mean.x) && !is.null(mean.y
        ) && !is.null(s.y) && !is.null(s.x))
        stop("You must enter the value for both n.x and n.y")
    if(is.null(s.x) && is.null(s.y) && !is.null(mean.x) && !is.null(mean.y
        ) && !is.null(n.x) && !is.null(n.y))
        stop("You must enter the value for both s.x and s.y")
    if(!is.null(s.x) && is.null(s.y) && !is.null(mean.x) && !is.null(
        mean.y) && !is.null(n.x) && !is.null(n.y))
        stop("You must enter the value for s.y")
    if(is.null(n.y) && is.null(s.y) && !is.null(mean.x) && !is.null(mean.y
        ) && !is.null(s.x) && !is.null(n.x))
        stop("You must enter the value for both s.y and n.y")
    alpha <- 1 - conf.level
    if(is.null(mean.y)) {
        # one-sample t-test.
        if(!var.equal) warning(
                "argument 'var.equal' ignored for one-sample test."
                )
        conf.int.xbar <- mean.x
        conf.int.s <- sqrt(s.x^2/n.x)
        ret.val <- list(statistic = (conf.int.xbar - mu)/conf.int.s,
            parameters = n.x - 1, estimate = conf.int.xbar, 
            null.value = mu, alternative = alt.expanded, method = 
            "One-sample t-Test", data.name = c("Summarized x"))
        names(ret.val$estimate) <- "mean of x"
        names(ret.val$null.value) <- "mean"
    }
    else {
        # a two-sample test
        mean.x <- mean.x
        mean.y <- mean.y
        conf.int.xbar <- mean.x - mean.y
        var.x <- s.x^2
        var.y <- s.y^2
        conf.int.s <- if(var.equal) sqrt((((n.x - 1) * var.x + (n.y -
                1) * var.y) * (1/n.x + 1/n.y))/(n.x + n.y -
                2)) else sqrt((var.x/n.x) + (var.y/n.y))
        ret.val <- c(if(var.equal) list(method = 
                "Standard Two-Sample t-Test", parameters = n.x +
                n.y - 2) else list(method = 
                "Welch Modified Two-Sample t-Test", parameters
                 = {
                const <- 1/(1 + (n.x * var.y)/(n.y * var.x))
                1/((const^2)/(n.x - 1) + ((1 - const)^2)/
                    (n.y - 1))
            }
            ), list(statistic = (conf.int.xbar - mu)/conf.int.s,
            estimate = c(mean.x, mean.y), null.value = mu, 
            alternative = alt.expanded, 
            data.name = paste("Summarized ", deparse(substitute (x)), " and ", deparse(substitute(y)), sep = "")))
        names(ret.val$estimate) <- c("mean of x", "mean of y")
        names(ret.val$null.value) <- "difference in means"
    }
    ret.val <- c(ret.val, switch(alt.expanded,
        two.sided = {
            conf.int.hw <- qt((1 - alpha/2), ret.val$parameters) *
                conf.int.s
            list(p.value = 2 * pt( - abs(ret.val$statistic), 
                ret.val$parameters), conf.int = c(
                conf.int.xbar - conf.int.hw, conf.int.xbar +
                conf.int.hw))
        }
        ,
        greater = {
            list(p.value = 1 - pt(ret.val$statistic, ret.val$
                parameters), conf.int = c(conf.int.xbar - qt(
                (1 - alpha), ret.val$parameters) * conf.int.s,
                NA))
        }
        ,
        less = {
            list(p.value = pt(ret.val$statistic, ret.val$
                parameters), conf.int = c(NA, conf.int.xbar +
                qt((1 - alpha), ret.val$parameters) * 
                conf.int.s))
        }
        ))
    names(ret.val$statistic) <- "t"
    names(ret.val$parameters) <- "df"
    attr(ret.val$conf.int, "conf.level") <- conf.level
    ret.val <- ret.val[c("statistic", "parameters", "p.value", "conf.int",
        "estimate", "null.value", "alternative", "method", "data.name"
        )]
    oldClass(ret.val) <- "htest"
    return(ret.val)
}
