#' Required Sample Size
#' 
#' Function to determine required sample size to be within a given margin of
#' error.
#' 
#' Answer is based on a normal approximation when using type \code{"pi"}.
#' 
#' @param b the desired bound.
#' @param sigma population standard deviation. Not required if using type
#' \code{"pi"}.
#' @param p estimate for the population proportion of successes. Not required
#' if using type \code{"mu"}.
#' @param conf.level confidence level for the problem, restricted to lie
#' between zero and one.
#' @param type character string, one of \code{"mu"} or \code{"pi"}, or just the
#' initial letter of each, indicating the appropriate parameter. Default value
#' is \code{"mu"}.
#' @return Returns required sample size.
#' @author Alan T. Arnholt
#' @keywords univar
#' @examples
#' 
#' nsize(b=.03, p=708/1200, conf.level=.90, type="pi")
#'     # Returns the required sample size (n) to estimate the population 
#'     # proportion of successes with a 0.9 confidence interval 
#'     # so that the margin of error is no more than 0.03 when the
#'     # estimate of the population propotion of successes is 708/1200.
#'     # This is problem 5.38 on page 257 of Kitchen's BSDA.
#'     
#' nsize(b=.15, sigma=.31, conf.level=.90, type="mu")
#'     # Returns the required sample size (n) to estimate the population 
#'     # mean with a 0.9 confidence interval so that the margin 
#'     # of error is no more than 0.15.  This is Example 5.17 on page
#'     # 261 of Kitchen's BSDA.
#' 
#' @export nsize
nsize <-
function(b, sigma = NULL, p = 0.5, conf.level = 0.95, type = "mu")
{
    choices <- c("mu", "pi")
    alt <- pmatch(type, choices)
    type <- choices[alt]
    if(length(type) > 1 || is.na(type))
        stop("type must be one \"mu\", \"pi\"")
    if(type == "pi" && b > 1)
        stop("b must be less than 1")
    if(!missing(b))
        if(length(b) != 1 || is.na(b))
            stop("b must be a single number")
    if(type == "mu") {
        z <- qnorm(1 - (1 - conf.level)/2)
        n <- ((z * sigma)/b)^2
        n <- ceiling(n)
        cat("\n")
        cat("The required sample size (n) to estimate the population",
            "\n")
        cat("mean with a", conf.level, 
            "confidence interval so that the margin", "\n")
        cat("of error is no more than", b, "is", n, ".", "\n")
        cat("\n\n")
    }
    else if(type == "pi") {
        z <- qnorm(1 - (1 - conf.level)/2)
        n <- p * (1 - p) * (z/b)^2
        n <- ceiling(n)
        cat("\n")
        cat("The required sample size (n) to estimate the population",
            "\n")
        cat("proportion of successes with a", conf.level, 
            "confidence interval", "\n")
        cat("so that the margin of error is no more than", b, "is",
            n, ".", "\n")
        cat("\n\n")
    }
}
