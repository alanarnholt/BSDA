#############################################################################
#' @import lattice 
#' @importFrom graphics abline axis box boxplot dotchart hist legend lines mtext par plot plot.design points polygon segments text title
#' @importFrom stats dbinom density dnorm fitted fivenum median pnorm pt qchisq qnorm qqline qqnorm qt quantile rbinom rnorm rstandard sd shapiro.test var
#' @importFrom utils combn
#' @importFrom e1071 skewness kurtosis
#' 
NULL
###############################################################################
#
#' Daily price returns (in pence) of Abbey National shares between 7/31/91 and
#' 10/8/91
#' 
#' Data used in problem 6.39
#' 
#' 
#' @name Abbey
#' @docType data
#' @format A data frame/tibble with 50 observations on the following variable.
#' \describe{ 
#' \item{price}{daily price returns (in pence) of Abbey National shares} 
#' }
#' 
#' @source Buckle, D. (1995), Bayesian Inference for Stable Distributions, 
#' \emph{Journal of the American Statistical Association}, 90, 605-613.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Abbey$price)
#' qqline(Abbey$price)
#' t.test(Abbey$price, mu = 300)
#' hist(Abbey$price)
#' 
"Abbey"


#' Three samples to illustrate analysis of variance
#' 
#' Data used in Exercise 10.1
#' 
#' 
#' @name Abc
#' @docType data
#' @format A data frame/tibble with 54 observations on two variables.
#' \describe{ 
#' \item{response}{a numeric vector}
#' \item{group}{a character vector \code{A}, \code{B}, and \code{C}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(response ~ group, col=c("red", "blue", "green"), data = Abc )
#' anova(lm(response ~ group, data = Abc))
#' 
"Abc"





#' Crimes reported in Abilene, Texas
#' 
#' Data used in Exercise 1.23 and 2.79
#' 
#' 
#' @name Abilene
#' @docType data
#' @format A data frame/tibble with 16 observations on three variables.
#' \describe{ 
#' \item{crimetype}{a character variable with values \code{Aggravated
#' assault}, \code{Arson}, \code{Burglary}, \code{Forcible rape}, \code{Larceny
#' theft}, \code{Murder}, \code{Robbery}, and \code{Vehicle theft}.}
#' \item{year}{a factor with levels \code{1992} and \code{1999}} 
#' \item{number}{number of reported crimes} 
#' }
#' 
#' @source \emph{Uniform Crime Reports}, US Dept. of Justice.
#' 
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(mfrow = c(2, 1))
#' barplot(Abilene$number[Abilene$year=="1992"],
#' names.arg = Abilene$crimetype[Abilene$year == "1992"],
#' main = "1992 Crime Stats", col = "red")
#' barplot(Abilene$number[Abilene$year=="1999"],
#' names.arg = Abilene$crimetype[Abilene$year == "1999"],
#' main = "1999 Crime Stats", col = "blue")
#' par(mfrow = c(1, 1))
#' 
#' \dontrun{
#' library(ggplot2)
#' ggplot(data = Abilene, aes(x = crimetype, y = number, fill = year)) +
#' geom_bar(stat = "identity", position = "dodge") +
#' theme_bw() +
#' theme(axis.text.x = element_text(angle = 30, hjust = 1))
#' }
#' 
"Abilene"





#' Perceived math ability for 13-year olds by gender
#' 
#' Data used in Exercise 8.57
#' 
#' 
#' @name Ability
#' @docType data
#' @format A data frame/tibble with 400 observations on two variables.
#' \describe{ 
#' \item{gender}{a factor with levels \code{girls} and \code{boys}} 
#' \item{ability}{a factor with levels  \code{hopeless},  \code{belowavg}, \code{average}, \code{aboveavg}, and \code{superior}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' CT <- xtabs(~gender + ability, data = Ability)
#' CT
#' chisq.test(CT)
#' 
"Ability"





#' Abortion rate by region of country
#' 
#' Data used in Exercise 8.51
#' 
#' 
#' @name Abortion
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 10 variables.
#' \describe{ 
#' \item{state}{a character variable with values \code{alabama}
#' \code{alaska} \code{arizona} \code{arkansas} \code{california}
#' \code{colorado} \code{connecticut} \code{delaware} \code{dist of columbia}
#' \code{florida} \code{georgia} \code{hawaii} \code{idaho} \code{illinois}
#' \code{indiana} \code{iowa} \code{kansas} \code{kentucky} \code{louisiana}
#' \code{maine} \code{maryland} \code{massachusetts} \code{michigan}
#' \code{minnesota} \code{mississippi} \code{missouri} \code{montana}
#' \code{nebraska} \code{nevada} \code{new hampshire} \code{new jersey}
#' \code{new mexico} \code{new york} \code{north carolina} \code{north dakota}
#' \code{ohio} \code{oklahoma} \code{oregon} \code{pennsylvania} \code{rhode
#' island} \code{south carolina} \code{south dakota} \code{tennessee}
#' \code{texas} \code{utah} \code{vermont} \code{virginia} \code{washington}
#' \code{west virginia} \code{wisconsin} \code{wyoming}}
#' \item{region}{a character variable with values \code{midwest} \code{northeast}
#' \code{south} \code{west}} 
#' \item{regcode}{a numeric vector}
#' \item{rate1988}{a numeric vector} 
#' \item{rate1992}{a numeric vector} 
#' \item{rate1996}{a numeric vector} 
#' \item{provide1988}{a numeric vector} 
#' \item{provide1992}{a numeric vector}
#' \item{lowhigh}{a numeric vector} 
#' \item{rate}{a factor with levels \code{Low} and \code{High}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~region + rate, data = Abortion)
#' T1
#' chisq.test(T1)
#' 
"Abortion"




#' Number of absent days for 20 employees
#' 
#' Data used in Exercise 1.28
#' 
#' 
#' @name Absent
#' @docType data
#' @format A data frame/tibble with 20 observations on one variable.
#' \describe{ 
#' \item{days}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' CT <- xtabs(~ days, data = Absent)
#' CT
#' barplot(CT, col = "pink")
#' plot(ecdf(Absent$days), main = "ECDF")
#' 
"Absent"





#' Math achievement test scores by gender for 25 high school students
#' 
#' Data used in Example 7.14 and Exercise 10.7
#' 
#' 
#' @name Achieve
#' @docType data
#' @format A data frame/tibble with 25 observations on the following two variables.
#' \describe{ 
#' \item{score}{mathematics achiement score} 
#' \item{gender}{a factor with 2 levels \code{boys} and \code{girls}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' anova(lm(score ~ gender, data = Achieve))
#' t.test(score ~ gender, var.equal = TRUE, data = Achieve)
#' 
"Achieve"





#' Number of ads versus number of sales for a retailer of satellite dishes
#' 
#' Data used in Exercise 9.15
#' 
#' 
#' @name Adsales
#' @docType data
#' @format A data frame/tibble with six observations on the following three variables.
#' \describe{ 
#' \item{month}{a character vector listing month}
#' \item{ads}{a numeric vector containing number of ads} 
#' \item{sales}{a numeric vector containing number of sales} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(sales ~ ads, data = Adsales)
#' mod <- lm(sales ~ ads, data = Adsales)
#' abline(mod)
#' summary(mod)
#' predict(mod, newdata = data.frame(ads = 6), interval = "conf", level = 0.99)
#' 
"Adsales"





#' Agressive tendency scores for a group of teenage members of a street gang
#' 
#' Data used in Exercises 1.66 and 1.81
#' 
#' 
#' @name Aggress
#' @docType data
#' @format A data frame/tibble with 28 observations on the following variable.
#' \describe{ 
#' \item{aggres}{measure of aggresive tendency, ranging from 10-50} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' with(data = Aggress,
#'      EDA(aggres))
#' # OR
#' IQR(Aggress$aggres)
#' diff(range(Aggress$aggres))
#'
"Aggress"





#' Monthly payments per person for families in the AFDC federal program
#' 
#' Data used in Exercises 1.91 and 3.68
#' 
#' 
#' @name Aid
#' @docType data
#' @format A data frame/tibble with 51 observations on the following two variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of
#' Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho}
#' \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky}
#' \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts}
#' \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour}
#' \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New
#' Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North
#' Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania}
#' \code{Rhode Island} \code{South Carolina} \code{South Dakota}
#' \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia}
#' \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
#' \item{payment}{average monthly payment per person in a family} 
#' }
#' 
#' @source US Department of Health and Human Services, 1993
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Aid$payment, xlab = "payment", main = "Your Title Here", 
#' col = "lightblue")
#' boxplot(Aid$payment)
#' dotplot(state ~ payment, data = Aid)
#' 
"Aid"





#' Incubation times for 295 patients thought to be infected with HIV by a blood
#' transfusion
#' 
#' Data used in Exercise 6.60
#' 
#' 
#' @name Aids
#' @docType data
#' @format A data frame/tibble with 295 observations on the following three variables.
#' \describe{ 
#' \item{duration}{time (in months) from HIV infection to the clinical manifestation of full-blown AIDS} 
#' \item{age}{age (in years) of patient} 
#' \item{group}{a numeric vector}
#' }
#' 
#' @source Kalbsleich, J. and Lawless, J., (1989), An analysis of the data on transfusion
#' related AIDS, \emph{Journal of the American Statistical Association, 84}, 360-372
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' with(data = Aids,
#' EDA(duration)
#' )
#' with(data = Aids, 
#' t.test(duration, mu = 30, alternative = "greater")
#' )
#' with(data = Aids, 
#' SIGN.test(duration, md = 24, alternative = "greater")
#' )
#' 
NULL





#' Aircraft disasters in five different decades
#' 
#' Data used in Exercise 1.12
#' 
#' 
#' @name Airdisasters
#' @docType data
#' @format A data frame /tibble with 141 observations on the following seven variables.
#' \describe{ 
#' \item{year}{a numeric vector indicating the year of an aircraft accident} 
#' \item{deaths}{a numeric vector indicating the number of deaths of an aircraft accident}
#' \item{decade}{a character vector indicating the decade of an aircraft accident} 
#' }
#' 
#' @source 2000 \emph{World Almanac and Book of Facts}
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(las = 1)
#' stripchart(deaths ~ decade, data = Airdisasters, 
#'            subset = decade != "1930s" & decade != "1940s", 
#'            method = "stack", pch = 19, cex = 0.5, col = "red", 
#'            main = "Aircraft Disasters 1950 - 1990", 
#'            xlab = "Number of fatalities")
#' par(las = 0)
#' 
"Airdisasters"





#' Percentage of on-time arrivals and number of complaints for 11 airlines
#' 
#' Data for Example 2.9
#' 
#' 
#' @name Airline
#' @docType data
#' @format A data frame/tibble with 11 observations on the following three variables.
#' \describe{ 
#' \item{airline}{a charater variable with values \code{Alaska}
#' \code{Amer West} \code{American} \code{Continental} \code{Delta}
#' \code{Northwest} \code{Pan Am} \code{Southwest} \code{TWA} \code{United}
#' \code{USAir}} 
#' \item{ontime}{a numeric vector}
#' \item{complaints}{complaints per 1000 passengers} 
#' }
#' 
#' @source Transportation Department
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' with(data = Airline, 
#' barplot(complaints, names.arg = airline, col = "lightblue")
#' )
#' plot(complaints ~ ontime, data = Airline, pch = 19, col = "red",
#' xlab = "On time", ylab = "Complaints")
#' 
"Airline"





#' Ages at which 14 female alcoholics began drinking
#' 
#' Data used in Exercise 5.79
#' 
#' 
#' @name Alcohol
#' @docType data
#' @format A data frame/tibble with 14 observations on the following variable.
#' \describe{ 
#' \item{age}{age when individual started drinking} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Alcohol$age)
#' qqline(Alcohol$age)
#' SIGN.test(Alcohol$age, md = 20, conf.level = 0.99)
#' 
"Alcohol"


#' Allergy medicines by adverse events
#' 
#' Data used in Exercise 8.22
#' 
#' 
#' @name Allergy
#' @docType data
#' @format A data frame/tibble with 406 observations on two variables.
#' \describe{ 
#' \item{event}{a factor with levels \code{insomnia},
#' \code{headache}, and \code{drowsiness}} 
#' \item{medication}{a factor with levels \code{seldane-d},
#' \code{pseudoephedrine}, and \code{placebo}}
#'  }
#'  
#' @source Marion Merrel Dow, Inc. Kansas City, Mo. 64114
#'  
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~event + medication, data = Allergy)
#' T1
#' chisq.test(T1)
#' 
"Allergy"





#' Recovery times for anesthetized patients
#' 
#' Data used in Exercise 5.58
#' 
#' 
#' @name Anesthet
#' @docType data
#' @format A  with 10 observations on the following variable.
#' \describe{ 
#' \item{recover}{recovery time (in hours)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Anesthet$recover)
#' qqline(Anesthet$recover)
#' with(data = Anesthet,
#' t.test(recover, conf.level = 0.90)$conf
#' )
#' 
"Anesthet"





#' Math test scores versus anxiety scores before the test
#' 
#' Data used in Exercise 2.96
#' 
#' 
#' @name Anxiety
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following two variables.
#' \describe{ 
#' \item{anxiety}{anxiety score before a major math test} 
#' \item{math}{math test score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(math ~ anxiety, data = Anxiety)
#' with(data = Anxiety,
#' cor(math, anxiety)
#' )
#' linmod <- lm(math ~ anxiety, data = Anxiety)
#' abline(linmod)
#' summary(linmod)
#' 
"Anxiety"





#' Level of apolipoprotein B and number of cups of coffee consumed per day for
#' 15 adult males
#' 
#' Data used in Examples 9.2 and 9.9
#' 
#' 
#' @name Apolipop
#' @docType data
#' @format A data frame/tibble  with 15 observations on the following two variables.
#' \describe{ 
#' \item{coffee}{number of cups of coffee per day} 
#' \item{apolipB}{level of apoliprotein B} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(apolipB ~ coffee, data = Apolipop)
#' linmod <- lm(apolipB ~ coffee, data = Apolipop)
#' summary(linmod)
#' summary(linmod)$sigma
#' anova(linmod)
#' anova(linmod)[2, 3]^.5
#' par(mfrow = c(2, 2))
#' plot(linmod)
#' par(mfrow = c(1, 1))
#' 
"Apolipop"


#' Median costs of an appendectomy at 20 hospitals in North Carolina
#' 
#' Data for Exercise 1.119
#' 
#' 
#' @name Append
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following variable.
#' \describe{ 
#' \item{fee}{fees for an appendectomy for a random sample of 20 hospitals in North Carolina} 
#' }
#' 
#' @source North Carolina Medical Database Commission, August 1994
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' fee <- Append$fee
#' ll <- mean(fee) - 2*sd(fee)
#' ul <- mean(fee) + 2*sd(fee)
#' limits <-c(ll, ul)
#' limits
#' fee[fee < ll | fee > ul]
#' 
"Append"


#' Median costs of appendectomies at three different types of North Carolina
#' hospitals
#' 
#' Data for Exercise 10.60
#' 
#' 
#' @name Appendec
#' @docType data
#' @format A data frame/tibble  with 59 observations on the following two variables.
#' \describe{ 
#' \item{cost}{median costs of appendectomies at hospitals across the state of North Carolina in 1992} 
#' \item{region}{a vector classifying each hospital as rural, regional, or metropolitan} 
#' }
#' 
#' @source \emph{Consumer's Guide to Hospitalization Charges in North Carolina Hospitals}
#' (August 1994), North Carolina Medical Database Commission, Department of Insurance
#'    
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(cost ~ region, data = Appendec, col = c("red", "blue", "cyan"))
#' anova(lm(cost ~ region, data = Appendec))
#' 
"Appendec"



#' Aptitude test scores versus productivity in a factory
#' 
#' Data for Exercises 2.1, 2.26, 2.35 and 2.51
#' 
#' 
#' @name Aptitude
#' @docType data
#' @format A data frame/tibble  with 8 observations on the following two variables.
#' \describe{ 
#' \item{aptitude}{aptitude test scores}
#' \item{product}{productivity scores} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(product ~ aptitude, data = Aptitude, main = "Exercise 2.1")
#' model1 <- lm(product ~ aptitude, data = Aptitude)
#' model1
#' abline(model1, col = "red", lwd=3)
#' resid(model1)
#' fitted(model1)
#' cor(Aptitude$product, Aptitude$aptitude)
#' 
"Aptitude"





#' Radiocarbon ages of observations taken from an archaeological site
#' 
#' Data for Exercises 5.120, 10.20 and Example 1.16
#' 
#' 
#' @name Archaeo
#' @docType data
#' @format A data frame/tibble  with 60 observations on the following two variables.
#' \describe{ 
#' \item{age}{number of years before 1983 - the year the data were obtained}
#' \item{phase}{Ceramic Phase numbers} 
#' }
#' 
#' @source Cunliffe, B. (1984) and Naylor and Smith (1988)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(age ~ phase, data = Archaeo, col = "yellow", 
#'         main = "Example 1.16", xlab = "Ceramic Phase", ylab = "Age")
#' anova(lm(age ~ as.factor(phase), data= Archaeo))
#' 
"Archaeo"





#' Time of relief for three treatments of arthritis
#' 
#' Data for Exercise 10.58
#' 
#' 
#' @name Arthriti
#' @docType data
#' @format A data frame/tibble  with 51 observations on the following two variables.
#' \describe{ 
#' \item{time}{time (measured in days) until an arthritis sufferer experienced relief} 
#' \item{treatment}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(time ~ treatment, data = Arthriti)
#' anova(lm(time ~ treatment, data = Arthriti))
#' 
"Arthriti"





#' Durations of operation for 15 artificial heart transplants
#' 
#' Data for Exercise 1.107
#' 
#' 
#' @name Artifici
#' @docType data
#' @format A data frame/tibble  with 15 observations on the following variable.
#' \describe{ 
#' \item{duration}{duration (in hours) for transplant} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Artifici$duration, 2)
#' summary(Artifici$duration)
#' values <- Artifici$duration[Artifici$duration < 6.5]
#' values
#' summary(values)
#' 
"Artifici"





#' Dissolving time versus level of impurities in aspirin tablets
#' 
#' Data for Exercise 10.51
#' 
#' 
#' @name Asprin
#' @docType data
#' @format A data frame/tibble  with 15 observations on two variables.
#' \describe{ 
#' \item{time}{time (in seconds) for aspirin to dissolve} 
#' \item{impurity}{impurity of an ingredient with levels \code{1\%}, \code{5\%}, and \code{10\%}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(time ~ impurity, data = Asprin, 
#'         col = c("red", "blue", "green"))
#' 
"Asprin"





#' Asthmatic relief index on nine subjects given a drug and a placebo
#' 
#' Data for Exercise 7.52
#' 
#' 
#' @name Asthmati
#' @docType data
#' @format A data frame/tibble  with nine observations on the following three variables.
#' \describe{
#' \item{drug}{asthmatic relief index for patients given a drug} 
#' \item{placebo}{asthmatic relief index for patients given a placebo} 
#' \item{difference}{difference between the \code{placebo} and \code{drug}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Asthmati$difference)
#' qqline(Asthmati$difference)
#' shapiro.test(Asthmati$difference)
#' with(data = Asthmati,
#' t.test(placebo, drug, paired = TRUE, mu = 0, 
#' alternative = "greater")
#' )
#' 
"Asthmati"





#' Number of convictions reported by U.S. attorney's offices
#' 
#' Data for Example 2.2 and Exercises 2.43 and 2.57
#' 
#' 
#' @name Attorney
#' @docType data
#' @format A data frame/tibble  with 88 observations on the following three variables.
#' \describe{ 
#' \item{staff}{U.S. attorneys' office staff per 1 million population} 
#' \item{convict}{U.S. attorneys' office convictions per 1 million population} 
#' \item{district}{a factor with levels
#' \code{Albuquerque}, \code{Alexandria, Va}, \code{Anchorage}, \code{Asheville,
#' NC}, \code{Atlanta}, \code{Baltimore}, \code{Baton Rouge}, \code{Billings, Mt},
#' \code{Birmingham, Al}, \code{Boise, Id}, \code{Boston}, \code{Buffalo},
#' \code{Burlington, Vt}, \code{Cedar Rapids}, \code{Charleston, WVA},
#' \code{Cheyenne, Wy}, \code{Chicago}, \code{Cincinnati}, \code{Cleveland},
#' \code{Columbia, SC}, \code{Concord, NH}, \code{Denver}, \code{Des Moines},
#' \code{Detroit}, \code{East St. Louis}, \code{Fargo, ND}, \code{Fort Smith, Ark},
#' \code{Fort Worth}, \code{Grand Rapids, Mi}, \code{Greensboro, NC},
#' \code{Honolulu}, \code{Houston}, \code{Indianapolis}, \code{Jackson, Miss},
#' \code{Kansas City}, \code{Knoxville, Tn}, \code{Las Vegas}, \code{Lexington,
#' Ky}, \code{Little Rock}, \code{Los Angeles}, \code{Louisville}, \code{Memphis},
#' \code{Miami}, \code{Milwaukee}, \code{Minneapolis}, \code{Mobile, Ala},
#' \code{Montgomery, Ala}, \code{Muskogee, Ok}, \code{Nashville}, \code{New Haven,
#' Conn}, \code{New Orleans}, \code{New York (Brooklyn)}, \code{New York
#' (Manhattan)}, \code{Newark, NJ}, \code{Oklahoma City}, \code{Omaha},
#' \code{Oxford, Miss}, \code{Pensacola, Fl}, \code{Philadelphia}, \code{Phoenix},
#' \code{Pittsburgh}, \code{Portland, Maine}, \code{Portland, Ore},
#' \code{Providence, RI}, \code{Raleigh, NC}, \code{Roanoke, Va},
#' \code{Sacramento}, \code{Salt Lake City}, \code{San Antonio}, \code{San Diego},
#' \code{San Francisco}, \code{Savannah, Ga}, \code{Scranton, Pa}, \code{Seattle},
#' \code{Shreveport, La}, \code{Sioux Falls, SD}, \code{South Bend, Ind},
#' \code{Spokane, Wash} ,\code{Springfield, Ill}, \code{St. Louis},
#' \code{Syracuse, NY}, \code{Tampa}, \code{Topeka, Kan}, \code{Tulsa},
#' \code{Tyler, Tex}, \code{Washington}, \code{Wheeling, WVa}, and \code{Wilmington,
#' Del}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(mfrow=c(1, 2))
#' plot(convict ~ staff, data = Attorney, main = "With Washington, D.C.")
#' plot(convict[-86] ~staff[-86], data = Attorney, 
#' main = "Without Washington, D.C.")
#' par(mfrow=c(1, 1))
#' 
"Attorney"





#' Number of defective auto gears produced by two manufacturers
#' 
#' Data for Exercise 7.46
#' 
#' 
#' @name Autogear
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{defectives}{number of defective gears in the production of 100 gears per day} 
#' \item{manufacturer}{a factor with levels \code{A} and \code{B}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(defectives ~ manufacturer, data = Autogear)
#' wilcox.test(defectives ~ manufacturer, data = Autogear)
#' t.test(defectives ~ manufacturer, var.equal = TRUE, data = Autogear)
#' 
"Autogear"





#' Illustrates inferences based on pooled t-test versus Wilcoxon rank sum test
#' 
#' Data for Exercise 7.40
#' 
#' 
#' @name Backtoback
#' @docType data
#' @format A data frame/tibble  with 24 observations on the following two variables.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' \item{group}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' wilcox.test(score ~ group, data = Backtoback)
#' t.test(score ~ group, data = Backtoback)
#' 
"Backtoback"





#' Baseball salaries for members of five major league teams
#' 
#' Data for Exercise 1.11
#' 
#' 
#' @name Bbsalaries
#' @docType data
#' @format A data frame/tibble  with 142 observations on two variables.
#' \describe{ 
#' \item{salary}{1999 salary for baseball player} 
#' \item{team}{a factor with levels \code{Angels}, \code{Indians}, \code{Orioles}, \code{Redsoxs}, and \code{Whitesoxs}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripchart(salary ~ team, data = Bbsalaries, method = "stack", 
#'            pch = 19, col = "blue", cex = 0.75)
#' title(main = "Major League Salaries")
#' 
"Bbsalaries"





#' Graduation rates for student athletes and nonathletes in the Big Ten Conf.
#' 
#' Data for Exercises 1.124 and 2.94
#' 
#' 
#' @name Bigten
#' @docType data
#' @format A data frame/tibble  with 44 observations on the following four variables.
#' \describe{ 
#' \item{school}{a factor with levels \code{Illinois}
#' \code{Indiana} \code{Iowa} \code{Michigan} \code{Michigan State}
#' \code{Minnesota} \code{Northwestern} \code{Ohio State} \code{Penn State}
#' \code{Purdue} \code{Wisconsin}} 
#' \item{rate}{graduation rate} 
#' \item{year}{factor with two levels \code{1984-1985} and \code{1993-1994}}
#' \item{status}{factor with two levels \code{athlete} and \code{student}}
#' }
#' 
#' @source NCAA Graduation Rates Report, 2000
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(rate ~ status, data = subset(Bigten, year = "1993-1994"), 
#' horizontal = TRUE, main = "Graduation Rates 1993-1994")
#' with(data = Bigten,
#'      tapply(rate, list(year, status), mean)
#'     )
#' 
"Bigten"





#' Test scores on first exam in biology class
#' 
#' Data for Exercise 1.49
#' 
#' 
#' @name Biology
#' @docType data
#' @format A data frame/tibble  with 30 observations on the following variable.
#' \describe{ 
#' \item{score}{test scores on the first test in a beginning biology class} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Biology$score, breaks = "scott", col = "brown", freq = FALSE, 
#' main = "Problem 1.49", xlab = "Test Score")
#' lines(density(Biology$score), lwd=3)
#' 
"Biology"





#' Live birth rates in 1990 and 1998 for all states
#' 
#' Data for Example 1.10
#' 
#' 
#' @name Birth
#' @docType data
#' @format A data frame/tibble  with 51 observations on the following three variables.
#' \describe{ 
#' \item{state}{a character with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of
#' Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho}
#' \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky}
#' \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts}
#' \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour}
#' \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New
#' Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North
#' Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania}
#' \code{Rhode Island} \code{South Carolina} \code{South Dakota}
#' \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia}
#' \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
#' \item{rate}{live birth rates per 1000 population} 
#' \item{year}{a factor with levels \code{1990} and \code{1998}} 
#' }
#' 
#' @source \emph{National Vital Statistics Report, 48}, March 28, 2000, National
#' Center for Health Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' rate1998 <- subset(Birth, year == "1998", select = rate)
#' stem(x = rate1998$rate, scale = 2)
#' hist(rate1998$rate, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
#'      main = "Figure 1.14 in BSDA", col = "pink")
#' hist(rate1998$rate, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
#'      main = "Figure 1.16 in BSDA", col = "pink", freq = FALSE)      
#' lines(density(rate1998$rate), lwd = 3)
#' rm(rate1998)
#' 
"Birth"





#' Education level of blacks by gender
#' 
#' Data for Exercise 8.55
#' 
#' 
#' @name Blackedu
#' @docType data
#' @format A data frame/tibble  with 3800 observations on two variables.
#' \describe{
#' \item{gender}{a factor with levels \code{Female} and \code{Male}}
#' \item{education}{a factor with levels \code{High school dropout},
#' \code{High school graudate}, \code{Some college}, \code{Bachelor}'\code{s degree}, and
#' \code{Graduate degree}} 
#' }
#' 
#' @source Bureau of Census data
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~gender + education, data = Blackedu)
#' T1
#' chisq.test(T1)
#' 
"Blackedu"





#' Blood pressure of 15 adult males taken by machine and by an expert
#' 
#' Data for Exercise 7.84
#' 
#' 
#' @name Blood
#' @docType data
#' @format A data frame/tibble  with 15 observations on the following two variables.
#' \describe{ 
#' \item{machine}{blood pressure recorded from an automated blood pressure machine} 
#' \item{expert}{blood pressure recorded by an expert using an at-home device} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' DIFF <- Blood$machine - Blood$expert
#' shapiro.test(DIFF)
#' qqnorm(DIFF)
#' qqline(DIFF)
#' rm(DIFF)
#' t.test(Blood$machine, Blood$expert, paired = TRUE)
#' 
"Blood"





#' Incomes of board members from three different universities
#' 
#' Data for Exercise 10.14
#' 
#' 
#' @name Board
#' @docType data
#' @format A data frame/tibble  with 7 observations on the following three variables.
#' \describe{ 
#' \item{salary}{1999 salary (in $1000) for board directors} 
#' \item{university}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(salary ~ university, data = Board, col = c("red", "blue", "green"), 
#'         ylab = "Income")
#' tapply(Board$salary, Board$university, summary)
#' anova(lm(salary ~ university, data = Board))
#' 
"Board"



#' Bone density measurements of 35 physically active and 35 non-active women
#' 
#' Data for Example 7.22
#' 
#' 
#' @name Bones
#' @docType data
#' @format A data frame/tibble  with 70 observations on two variables.
#' \describe{ 
#' \item{density}{bone density measurements}
#' \item{group}{a factor with levels \code{active} and \code{nonactive}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(density ~ group, data = Bones, alternative = "greater")
#' t.test(rank(density) ~ group, data = Bones, alternative = "greater")
#' wilcox.test(density ~ group, data = Bones, alternative = "greater")
#' 
#' 
"Bones"





#' Number of books read and final spelling scores for 17 third graders
#' 
#' Data for Exercise 9.53
#' 
#' 
#' @name Books
#' @docType data
#' @format A data frame/tibble  with 17 observations on the following two variables.
#' \describe{ 
#' \item{book}{number of books read} 
#' \item{spelling}{spelling score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(spelling ~ book, data = Books)
#' mod <- lm(spelling ~ book, data = Books)
#' summary(mod)
#' abline(mod, col = "blue", lwd = 2)
#' 
"Books"





#' Prices paid for used books at three different bookstores
#' 
#' Data for Exercise 10.30 and 10.31
#' 
#' 
#' @name Bookstor
#' @docType data
#' @format A data frame/tibble  with 72 observations on 2 variables.
#' \describe{ 
#' \item{dollars}{money obtained for selling textbooks} 
#' \item{store}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(dollars ~ store, data = Bookstor, 
#'         col = c("purple", "lightblue", "cyan"))
#' kruskal.test(dollars ~ store, data = Bookstor)
#' 
"Bookstor"





#' Brain weight versus body weight of 28 animals
#' 
#' Data for Exercises 2.15, 2.44, 2.58 and Examples 2.3 and 2.20
#' 
#' 
#' @name Brain
#' @docType data
#' @format A data frame/tibble  with 28 observations on the following three variables.
#' \describe{ 
#' \item{species}{a factor with levels \code{African
#' elephant}, \code{Asian Elephant}, \code{Brachiosaurus}, \code{Cat},
#' \code{Chimpanzee}, \code{Cow}, \code{Diplodocus}, \code{Donkey}, \code{Giraffe},
#' \code{Goat}, \code{Gorilla}, \code{Gray wolf}, \code{Guinea Pig}, \code{Hamster},
#' \code{Horse}, \code{Human}, \code{Jaguar}, \code{Kangaroo}, \code{Mole},
#' \code{Mouse}, \code{Mt Beaver}, \code{Pig}, \code{Potar monkey}, \code{Rabbit},
#' \code{Rat}, \code{Rhesus monkey}, \code{Sheep}, and \code{Triceratops}}
#' \item{bodyweight}{body weight (in kg)} 
#' \item{brainweight}{brain weight (in g)} 
#' }
#' 
#' @source P. Rousseeuw and A. Leroy, \emph{Robust Regression and Outlier Detection} 
#' (New York: Wiley, 1987)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(log(brainweight) ~ log(bodyweight), data = Brain, 
#'      pch = 19, col = "blue", main = "Example 2.3")
#' mod <- lm(log(brainweight) ~ log(bodyweight), data = Brain)      
#' abline(mod, lty = "dashed", col = "blue")
#' 
#' 
"Brain"





#' Basic Statistics and Data Analysis
#' 
#' Data and functions for the book \emph{Basic Statistics and Data Analysis}
#' 
#' \tabular{ll}{ Package: \tab PASWR\cr Type: \tab Package\cr Version: \tab
#' 1.02\cr Date: \tab 2016-02-25\cr License: \tab GPL (>=2) \cr } The package
#' BSDA provides data and functions for the book \emph{Basic Statistics and
#' Data Analysis}
#' 
#' @name BSDA-package
#' @aliases BSDA-package BSDA
#' @docType package
#' @author Alan T. Arnholt
#' 
#' Maintainer: <arnholtat@@appstate.edu>
#' @references Kitchens, L. J (2003) \emph{Basic Statistics and Data Analysis}.
#' Brooks/Cole.
#' @keywords package
NULL


#' Repair costs of vehicles crashed into a barrier at 5 miles per hour
#' 
#' Data for Exercise 1.73
#' 
#' 
#' @name Bumpers
#' @docType data
#' @format A data frame/tibble  with 23 observations on the following two variables.
#' \describe{ 
#' \item{car}{a factor with levels \code{Buick Century},
#' \code{Buick Skylark}, \code{Chevrolet Cavalier}, \code{Chevrolet Corsica},
#' \code{Chevrolet Lumina}, \code{Dodge Dynasty}, \code{Dodge Monaco}, \code{Ford
#' Taurus}, \code{Ford Tempo}, \code{Honda Accord}, \code{Hyundai Sonata},
#' \code{Mazda 626}, \code{Mitsubishi Galant}, \code{Nissan Stanza},
#' \code{Oldsmobile Calais}, \code{Oldsmobile Ciere}, \code{Plymouth Acclaim},
#' \code{Pontiac 6000}, \code{Pontiac Grand Am}, \code{Pontiac Sunbird},
#' \code{Saturn SL2}, \code{Subaru Legacy}, and \code{Toyota Camry}}
#' \item{repair}{total repair cost (in dollars)  after crashing a car into a barrier four times while the car was traveling at 5 miles per hour} 
#' }
#' 
#' @source Insurance Institute of Highway Safety
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Bumpers$repair)
#' stripchart(Bumpers$repair, method = "stack", pch = 19, col = "blue")
#' library(lattice)
#' dotplot(car ~ repair, data = Bumpers)
#' 
"Bumpers"





#' Attendance of bus drivers versus shift
#' 
#' Data for Exercise 8.25
#' 
#' 
#' @name Bus
#' @docType data
#' @format A data frame/tibble  with 29363 observations on two variables.
#' \describe{ 
#' \item{attendance}{a factor with levels \code{absent} and
#' \code{present}} 
#' \item{shift}{a factor with levels \code{am}, \code{noon}, \code{pm}, \code{swing}, and \code{split}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~attendance + shift, data = Bus)
#' T1
#' chisq.test(T1)
#' 
"Bus"





#' Median charges for coronary bypass at 17 hospitals in North Carolina
#' 
#' Data for Exercises 5.104 and 6.43
#' 
#' 
#' @name Bypass
#' @docType data
#' @format A data frame/tibble  with 17 observations on the following two variables.
#' \describe{ 
#' \item{hospital}{a factor with levels \code{Carolinas Med
#' Ct}, \code{Duke Med Ct}, \code{Durham Regional}, \code{Forsyth Memorial},
#' \code{Frye Regional}, \code{High Point Regional}, \code{Memorial Mission},
#' \code{Mercy}, \code{Moore Regional}, \code{Moses Cone Memorial}, \code{NC
#' Baptist}, \code{New Hanover Regional}, \code{Pitt Co. Memorial},
#' \code{Presbyterian}, \code{Rex}, \code{Univ of North Carolina}, and \code{Wake
#' County}}
#' \item{charge}{median charge for coronary bypass} 
#' }
#' 
#' @source \emph{Consumer's Guide to Hospitalization Charges in North Carolina Hospitals}
#' (August 1994), North Carolina Medical Database Commission, Department of Insurance
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Bypass$charge)
#' t.test(Bypass$charge, conf.level=.90)$conf
#' t.test(Bypass$charge, mu = 35000)
#' 
"Bypass"





#' Estimates of costs of kitchen cabinets by two suppliers on 20 prospective
#' homes
#' 
#' Data for Exercise 7.83
#' 
#' 
#' @name Cabinets
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following three variables.
#' \describe{ 
#' \item{home}{a numeric vector} 
#' \item{supplA}{estimate for kitchen cabinets from supplier A (in dollars)} 
#' \item{supplB}{estimate for kitchen cabinets from supplier A (in dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' DIF <- Cabinets$supplA - Cabinets$supplB
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' with(data = Cabinets, 
#'      t.test(supplA, supplB, paired = TRUE)
#' )
#' with(data = Cabinets,
#'      wilcox.test(supplA, supplB, paired = TRUE)
#')
#' rm(DIF)
#' 
"Cabinets"





#' Survival times of terminal cancer patients treated with vitamin C
#' 
#' Data for Exercises 6.55 and 6.64
#' 
#' 
#' @name Cancer
#' @docType data
#' @format A data frame/tibble  with 64 observations on two variables.
#' \describe{ 
#' \item{survival}{survival time (in days) of terminal patients 
#' treated with vitamin C}
#' \item{type}{a factor indicating type of cancer with levels 
#' \code{breast}, \code{bronchus}, \code{colon}, \code{ovary}, and 
#' \code{stomach}} 
#' }
#' @source Cameron, E and Pauling, L. 1978. \dQuote{Supplemental Ascorbate in the 
#' Supportive Treatment of Cancer.} \emph{Proceedings of the National Academy of 
#' Science}, 75, 4538-4542.
#' 
#' 
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(survival ~ type, Cancer)
#' stomach <- Cancer$survival[Cancer$type == "stomach"]
#' bronchus <- Cancer$survival[Cancer$type == "bronchus"]
#' boxplot(stomach, ylab = "Days")
#' SIGN.test(stomach, md = 100, alternative = "greater")
#' SIGN.test(bronchus, md = 100, alternative = "greater")
#' rm(bronchus, stomach)
#' 
#' 
"Cancer"





#' Carbon monoxide level measured at three industrial sites
#' 
#' Data for Exercise 10.28 and 10.29
#' 
#' 
#' @name Carbon
#' @docType data
#' @format A data frame/tibble  with 24 observations on two variables.
#' \describe{ 
#' \item{CO}{carbon monoxide measured (in parts per million)} 
#' \item{site}{a factor with levels \code{SiteA}, \code{SiteB}, and \code{SiteC}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(CO ~ site, data = Carbon)
#' kruskal.test(CO ~ site, data = Carbon)
#' 
"Carbon"





#' Reading scores on the California achievement test for a group of 3rd graders
#' 
#' Data for Exercise 1.116
#' 
#' 
#' @name Cat
#' @docType data
#' @format A data frame/tibble  with 17 observations on the following variable.
#' \describe{ 
#' \item{score}{reading score on the California Achievement Test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Cat$score)
#' fivenum(Cat$score)
#' boxplot(Cat$score, main = "Problem 1.116", col = "green")
#' 
"Cat"





#' Entry age and survival time of patients with small cell lung cancer under
#' two different treatments
#' 
#' Data for Exercises 7.34 and 7.48
#' 
#' 
#' @name Censored
#' @docType data
#' @format A data frame/tibble  with 121 observations on the following 3 variables.
#' \describe{ 
#' \item{survival}{survival time (in days) of patients with small cell lung cancer} 
#' \item{treatment}{a factor with levels \code{armA} and \code{armB} indicating the 
#' treatment a patient received} 
#' \item{age}{the age of the patient} 
#' }
#' 
#' @source Ying, Z., Jung, S., Wei, L. 1995. \dQuote{Survival Analysis with Median Regression Models.} 
#' \emph{Journal of the American Statistical Association}, 90, 178-184.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(survival ~ treatment, data = Censored)
#' wilcox.test(survival ~ treatment, data = Censored, alternative = "greater")
#' 
"Censored"





#' Temperatures and O-ring failures for the launches of the space shuttle
#' Challenger
#' 
#' Data for Examples 1.11, 1.12, 1.13, 2.11 and 5.1
#' 
#' 
#' @name Challeng
#' @docType data
#' @format A data frame/tibble  with 25 observations on the following four variables.
#' \describe{ 
#' \item{flight}{a character variable indicating the flight}
#' \item{date}{date of the flight} 
#' \item{temp}{temperature (in fahrenheit)}
#' \item{failures}{number of failures} 
#' }
#' 
#' @source Dalal, S. R., Fowlkes, E. B., Hoadley, B. 1989. \dQuote{Risk Analysis of the Space Shuttle: Pre-Challenger 
#' Prediction of Failure.} 
#' \emph{Journal of the American Statistical Association}, 84, No. 408, 945-957.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Challeng$temp)
#' summary(Challeng$temp)
#' IQR(Challeng$temp)
#' quantile(Challeng$temp)
#' fivenum(Challeng$temp)
#' stem(sort(Challeng$temp)[-1])
#' summary(sort(Challeng$temp)[-1])
#' IQR(sort(Challeng$temp)[-1])
#' quantile(sort(Challeng$temp)[-1])
#' fivenum(sort(Challeng$temp)[-1])
#' par(mfrow=c(1, 2))
#' qqnorm(Challeng$temp)
#' qqline(Challeng$temp)
#' qqnorm(sort(Challeng$temp)[-1])
#' qqline(sort(Challeng$temp)[-1])
#' par(mfrow=c(1, 1))
#' 
"Challeng"





#' Starting salaries of 50 chemistry majors
#' 
#' Data for Example 5.3
#' 
#' 
#' @name Chemist
#' @docType data
#' @format A data frame/tibble  with 50 observations on the following variable.
#' \describe{ 
#' \item{salary}{starting salary (in dollars) for chemistry major} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Chemist$salary)
#' 
"Chemist"





#' Surface salinity measurements taken offshore from Annapolis, Maryland in
#' 1927
#' 
#' Data for Exercise 6.41
#' 
#' 
#' @name Chesapea
#' @docType data
#' @format A data frame/tibble  with 16 observations on the following variable.
#' \describe{ 
#' \item{salinity}{surface salinity measurements (in parts per 1000) for station 11, 
#' offshore from Annanapolis, Maryland, on July 3-4, 1927.} 
#' }
#' 
#' @source Davis, J. (1986) \emph{Statistics and Data Analysis in Geology, Second Edition}. 
#' John Wiley and Sons, New York.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Chesapea$salinity)
#' qqline(Chesapea$salinity)
#' shapiro.test(Chesapea$salinity)
#' t.test(Chesapea$salinity, mu = 7)
#' 
"Chesapea"





#' Insurance injury ratings of Chevrolet vehicles for 1990 and 1993 models
#' 
#' Data for Exercise 8.35
#' 
#' 
#' @name Chevy
#' @docType data
#' @format A data frame/tibble  with 67 observations on two variables.
#' \describe{ 
#' \item{year}{a factor with levels \code{1988-90} and
#' \code{1991-93}} 
#' \item{frequency}{a factor with levels \code{much better than average}, \code{above average},
#' \code{average}, \code{below average}, and \code{much worse than average}} 
#' }
#' 
#' @source Insurance Institute for Highway Safety and the Highway Loss Data Institute, 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~year + frequency, data = Chevy)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Chevy"





#' Weight gain of chickens fed three different rations
#' 
#' Data for Exercise 10.15
#' 
#' 
#' @name Chicken
#' @docType data
#' @format A data frame/tibble  with 13 observations on the following three variables.
#' \describe{ 
#' \item{gain}{weight gain over a specified period} 
#' \item{feed}{a factor with levels \code{ration1}, \code{ration2}, 
#' and \code{ration3}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(gain ~ feed, col = c("red","blue","green"), data = Chicken)
#' anova(lm(gain ~ feed, data = Chicken))
#' 
"Chicken"





#' Measurements of the thickness of the oxide layer of manufactured integrated
#' circuits
#' 
#' Data for Exercises 6.49 and 7.47
#' 
#' 
#' @name Chipavg
#' @docType data
#' @format A data frame/tibble  with 30 observations on the following three variables.
#' \describe{ 
#' \item{wafer1}{thickness of the oxide layer for \code{wafer1}} 
#' \item{wafer2}{thickness of the oxide layer for \code{wafer2}}
#' \item{thickness}{average thickness of the oxide layer of the eight measurements
#' obtained from each set of two wafers} 
#' }
#' 
#' @source Yashchin, E. 1995. \dQuote{Likelihood Ratio Methods 
#' for Monitoring Parameters of a Nested Random Effect Model.} 
#' \emph{Journal of the American Statistical Association}, 90, 729-738.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Chipavg$thickness)
#' t.test(Chipavg$thickness, mu = 1000)
#' boxplot(Chipavg$wafer1, Chipavg$wafer2, name = c("Wafer 1", "Wafer 2"))
#' shapiro.test(Chipavg$wafer1)
#' shapiro.test(Chipavg$wafer2)
#' t.test(Chipavg$wafer1, Chipavg$wafer2, var.equal = TRUE)
#' 
"Chipavg"





#' Four measurements on a first wafer and four measurements on a second wafer
#' selected from 30 lots
#' 
#' Data for Exercise 10.9
#' 
#' 
#' @name Chips
#' @docType data
#' @format A data frame/tibble  with 30 observations on the following eight variables.
#' \describe{ 
#' \item{wafer11}{first measurement of thickness of the oxide layer for \code{wafer1}} 
#' \item{wafer12}{second measurement of thickness of the oxide layer for \code{wafer1}}
#' \item{wafer13}{third measurement of thickness of the oxide layer for \code{wafer1}}
#' \item{wafer14}{fourth measurement of thickness of the oxide layer for \code{wafer1}}
#' \item{wafer21}{first measurement of thickness of the oxide layer for \code{wafer2}} 
#' \item{wafer22}{second measurement of thickness of the oxide layer for \code{wafer2}} 
#' \item{wafer23}{third measurement of thickness of the oxide layer for \code{wafer2}} 
#' \item{wafer24}{fourth measurement of thickness of the oxide layer for \code{wafer2}} 
#' }
#' 
#' @source Yashchin, E. 1995. \dQuote{Likelihood Ratio Methods 
#' for Monitoring Parameters of a Nested Random Effect Model.} 
#' \emph{Journal of the American Statistical Association}, 90, 729-738.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' with(data = Chips, 
#' boxplot(wafer11, wafer12, wafer13, wafer14, wafer21, wafer22, wafer23, wafer24)
#' )
#' 
"Chips"





#' Effect of mother's smoking on birth weight of newborn
#' 
#' Data for Exercise 2.27
#' 
#' 
#' @name Cigarett
#' @docType data
#' @format A data frame/tibble  with 16 observations on the following two variables.
#' \describe{ 
#' \item{cigarettes}{mothers' estimated average number of cigarettes smoked per day} 
#' \item{weight}{children's birth weights (in pounds)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(weight ~ cigarettes, data = Cigarett)
#' model <- lm(weight ~ cigarettes, data = Cigarett)
#' abline(model)
#' with(data = Cigarett,
#' cor(weight, cigarettes)
#' )
#' rm(model)
#' 
"Cigarett"





#' Milligrams of tar in 25 cigarettes selected randomly from 4 different brands
#' 
#' Data for Example 10.4
#' 
#' 
#' @name Cigar
#' @docType data
#' @format A data frame/tibble  with 100 observations on the following two variables.
#' \describe{ 
#' \item{tar}{amount of tar (measured in milligrams)}
#' \item{brand}{a factor indicating cigarette brand with levels \code{brandA}, \code{brandB},
#' \code{brandC}, and \code{brandD}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(tar ~ brand, data = Cigar)
#' anova(lm(tar ~ brand, data = Cigar))
#' 
"Cigar"





#' Percent of peak bone density of different aged children
#' 
#' Data for Exercise 9.7
#' 
#' 
#' @name Citrus
#' @docType data
#' @format A data frame/tibble  with nine observations on the following two variables.
#' \describe{ 
#' \item{age}{age of children} 
#' \item{percent}{percent peak bone density} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(percent ~ age, data = Citrus)
#' summary(model)
#' anova(model)
#' rm(model)
#' 
"Citrus"





#' Residual contaminant following the use of three different cleansing agents
#' 
#' Data for Exercise 10.16
#' 
#' 
#' @name Clean
#' @docType data
#' @format A data frame/tibble  with 45 observations on the following two variables.
#' \describe{ 
#' \item{clean}{residual contaminants} 
#' \item{agent}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(clean ~ agent, col = c("red", "blue", "green"), data = Clean)
#' anova(lm(clean ~ agent, data = Clean))
#' 
"Clean"





#' Signal loss from three types of coxial cable
#' 
#' Data for Exercise 10.24 and 10.25
#' 
#' 
#' @name Coaxial
#' @docType data
#' @format A data frame/tibble  with 45 observations on the following two variables.
#' \describe{ 
#' \item{signal}{signal loss per 1000 feet} 
#' \item{cable}{factor with three levels of coaxial cable \code{typeA}, 
#' \code{typeB}, and \code{typeC}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(signal ~ cable, data = Coaxial, col = c("red", "green", "yellow"))
#' kruskal.test(signal ~ cable, data = Coaxial)
#' 
"Coaxial"





#' Productivity of workers with and without a coffee break
#' 
#' Data for Exercise 7.55
#' 
#' 
#' @name Coffee
#' @docType data
#' @format A data frame/tibble  with 9 observations on the following three variables.
#' \describe{ 
#' \item{without}{workers' productivity scores without a coffee break} 
#' \item{with}{workers' productivity scores with a coffee break}
#' \item{differences}{\code{with} minus \code{without}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Coffee$differences)
#' qqline(Coffee$differences)
#' shapiro.test(Coffee$differences)
#' t.test(Coffee$with, Coffee$without, paired = TRUE, alternative = "greater")
#' wilcox.test(Coffee$with, Coffee$without, paired = TRUE, 
#' alterantive = "greater")
#' 
"Coffee"





#' Yearly returns on 12 investments
#' 
#' Data for Exercise 5.68
#' 
#' 
#' @name Coins
#' @docType data
#' @format A data frame/tibble  with 12 observations on the following variable.
#' \describe{ 
#' \item{return}{yearly returns on each of 12 possible investments} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Coins$return)
#' qqline(Coins$return)
#' 
"Coins"





#' Commuting times for selected cities in 1980 and 1990
#' 
#' Data for Exercises 1.13, and 7.85
#' 
#' 
#' @name Commute
#' @docType data
#' @format A data frame/tibble  with 39 observations on the following three variables.
#' \describe{ 
#' \item{city}{a factor with levels \code{Atlanta},
#' \code{Baltimore}, \code{Boston}, \code{Buffalo}, \code{Charlotte},
#' \code{Chicago}, \code{Cincinnati}, \code{Cleveland}, \code{Columbus},
#' \code{Dallas}, \code{Denver}, \code{Detroit}, \code{Hartford}, \code{Houston},
#' \code{Indianapolis}, \code{Kansas City}, \code{Los Angeles}, \code{Miami},
#' \code{Milwaukee}, \code{Minneapolis}, \code{New Orleans}, \code{New York},
#' \code{Norfolk}, \code{Orlando}, \code{Philadelphia}, \code{Phoenix},
#' \code{Pittsburgh}, \code{Portland}, \code{Providence}, \code{Rochester},
#' \code{Sacramento}, \code{Salt Lake City}, \code{San Antonio}, \code{San Diego},
#' \code{San Francisco}, \code{Seattle}, \code{St. Louis}, \code{Tampa}, and
#' \code{Washington}}
#' \item{year}{year}
#' \item{time}{commute times}
#' }
#' 
#' @source Federal Highway Administration.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripplot(year ~ time, data = Commute, jitter = TRUE) 
#' dotplot(year ~ time, data = Commute)
#' bwplot(year ~ time, data = Commute)
#' stripchart(time ~ year, data = Commute, method = "stack", pch = 1, 
#'            cex = 2, col = c("red", "blue"), 
#'            group.names = c("1980", "1990"), 
#'            main = "", xlab = "minutes")
#' title(main = "Commute Time") 
#' boxplot(time ~ year, data = Commute, names=c("1980", "1990"),
#'         horizontal = TRUE, las = 1)
#' 
#' 
"Commute"





#' Tennessee self concept scale scores for a group of teenage boys
#' 
#' Data for Exercise 1.68 and 1.82
#' 
#' 
#' @name Concept
#' @docType data
#' @format A data frame/tibble  with 28 observations on the following variable.
#' \describe{ 
#' \item{self}{Tennessee self concept scores} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(Concept$self)
#' sd(Concept$self)
#' diff(range(Concept$self))
#' IQR(Concept$self)
#' summary(Concept$self/10)
#' IQR(Concept$self/10)
#' sd(Concept$self/10)
#' diff(range(Concept$self/10))
#' 
"Concept"





#' Compressive strength of concrete blocks made by two different methods
#' 
#' Data for Example 7.17
#' 
#' 
#' @name Concrete
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{strength}{comprehensive strength (in pounds per square inch)} 
#' \item{method}{factor with levels \code{new} and \code{old} indicating the 
#' method used to construct a concrete block} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' wilcox.test(strength ~ method, data = Concrete, alternative = "greater")
#' 
"Concrete"





#' Comparison of the yields of a new variety and a standard variety of corn
#' planted on 12 plots of land
#' 
#' Data for Exercise 7.77
#' 
#' 
#' @name Corn
#' @docType data
#' @format A data frame/tibble  with 12 observations on the following three variables.
#' \describe{ 
#' \item{new}{corn yield with new meathod} 
#' \item{standard}{corn yield with standard method}
#' \item{differences}{\code{new} minus \code{standard}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Corn$differences)
#' qqnorm(Corn$differences)
#' qqline(Corn$differences)
#' shapiro.test(Corn$differences)
#' t.test(Corn$new, Corn$standard, paired = TRUE, alternative = "greater")
#' 
"Corn"





#' Exercise to illustrate correlation
#' 
#' Data for Exercise 2.23
#' 
#' 
#' @name Correlat
#' @docType data
#' @format A data frame/tibble  with 13 observations on the following two variables.
#' \describe{ 
#' \item{x}{a numeric vector} 
#' \item{y}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(y ~ x, data = Correlat)
#' model <- lm(y ~ x, data = Correlat)
#' abline(model)
#' rm(model)
#' 
"Correlat"





#' Scores of 18 volunteers who participated in a counseling process
#' 
#' Data for Exercise 6.96
#' 
#' 
#' @name Counsel
#' @docType data
#' @format A data frame/tibble  with 18 observations on the following variable.
#' \describe{ 
#' \item{score}{standardized psychology scores after a counseling process} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Counsel$score)
#' t.test(Counsel$score, mu = 70)
#' 
"Counsel"





#' Consumer price index from 1979 to 1998
#' 
#' Data for Exercise 1.34
#' 
#' 
#' @name Cpi
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following two variables.
#' \describe{ 
#' \item{year}{year} 
#' \item{cpi}{consumer price index} 
#' }
#' 
#' @source Bureau of Labor Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(cpi ~ year, data = Cpi, type = "l", lty = 2, lwd = 2, col = "red")   
#' barplot(Cpi$cpi, col = "pink", las = 2, main = "Problem 1.34")   
#' 
"Cpi"





#' Violent crime rates for the states in 1983 and 1993
#' 
#' Data for Exercises 1.90, 2.32, 3.64, and 5.113
#' 
#' 
#' @name Crime
#' @docType data
#' @format A data frame/tibble  with 102 observations on the following three variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{Alabama},
#' \code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
#' \code{Colorado}, \code{Connecticut}, \code{DC}, \code{Delaware}, \code{Florida},
#' \code{Georgia}, \code{Hawaii}, \code{Idaho}, \code{Illinois}, \code{Indiana},
#' \code{Iowa}, \code{Kansas}, \code{Kentucky}, \code{Louisiana}, \code{Maine},
#' \code{Maryland}, \code{Massachusetts}, \code{Michigan}, \code{Minnesota},
#' \code{Mississippi}, \code{Missour}, \code{Montana}, \code{Nebraska},
#' \code{Nevada}, \code{New Hampshire}, \code{New Jersey}, \code{New Mexico},
#' \code{New York}, \code{North Carolina}, \code{North Dakota}, \code{Ohio},
#' \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania}, \code{Rhode Island},
#' \code{South Carolina}, \code{South Dakota}, \code{Tennessee}, \code{Texas},
#' \code{Utah}, \code{Vermont}, \code{Virginia}, \code{Washington}, \code{West
#' Virginia}, \code{Wisconsin}, and \code{Wyoming}}
#' \item{year}{a factor with levels \code{1983} and \code{1993}} 
#' \item{rate}{crime rate per 100,000 inhabitants} 
#' }
#' 
#' @source U.S. Department of Justice, Bureau of Justice Statistics, \emph{Sourcebook of
#' Criminal Justice Statistics}, 1993
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(rate ~ year, data = Crime)
#' 
"Crime"





#' Charles Darwin's study of cross-fertilized and self-fertilized plants
#' 
#' Data for Exercise 7.62
#' 
#' 
#' @name Darwin
#' @docType data
#' @format A data frame/tibble  with 15 observations on the following three variables.
#' \describe{ 
#' \item{pot}{number of pot} 
#' \item{cross}{height of plant (in inches) after a fixed period of time when cross-fertilized} 
#' \item{self}{height of plant (in inches) after a fixed period of time when self-fertilized}
#' }
#' 
#' @source Darwin, C. (1876) \emph{The Effect of Cross- and Self-Fertilization in the 
#' Vegetable Kingdom}, 2nd edition, London.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' differ <- Darwin$cross - Darwin$self
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' wilcox.test(Darwin$cross, Darwin$self, paired = TRUE)
#' rm(differ)
#' 
"Darwin"





#' Automobile dealers classified according to type dealership and service
#' rendered to customers
#' 
#' Data for Example 2.22
#' 
#' 
#' @name Dealers
#' @docType data
#' @format A data frame/tibble  with 122 observations on the following two variables.
#' \describe{ 
#' \item{type}{a factor with levels \code{Honda}, \code{Toyota}, \code{Mazda}, 
#' \code{Ford}, \code{Dodge}, and \code{Saturn}} 
#' \item{service}{a factor with levels \code{Replaces unnecessarily} and \code{Follows manufacturer guidelines}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' xtabs(~type + service, data = Dealers)
#' T1 <- xtabs(~type + service, data = Dealers)
#' T1
#' addmargins(T1)
#' pt <- prop.table(T1, margin = 1)
#' pt
#' barplot(t(pt),  col = c("red", "skyblue"), legend = colnames(T1))
#' rm(T1, pt)
#' 
"Dealers"





#' Number of defective items produced by 20 employees
#' 
#' Data for Exercise 1.27
#' 
#' 
#' @name Defectiv
#' @docType data
#' @format A data frame/tibble  with 20 observations on one variable.
#' \describe{ 
#' \item{number}{number of defective items produced by the employees in a small business firm} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~ number, data = Defectiv)
#' T1
#' barplot(T1, col = "pink", ylab = "Frequency",
#' xlab = "Defective Items Produced by Employees", main = "Problem 1.27")
#' rm(T1)
#' 
"Defectiv"

#' Percent of bachelor's degrees awarded women in 1970 versus 1990
#' 
#' Data for Exercise 2.75
#' 
#' 
#' @name Degree
#' @docType data
#' @format A data frame/tibble  with 1064 observations on the following two variables.
#' \describe{ 
#' \item{field}{a factor with levels \code{Health},
#' \code{Education}, \code{Foreign Language}, \code{Psychology}, \code{Fine Arts},
#' \code{Life Sciences}, \code{Business}, \code{Social Science}, \code{Physical Sciences},
#' \code{Engineering}, and \code{All Fields}} 
#' \item{awarded}{a factor with levels \code{1970} and \code{1990}} 
#' }
#' 
#' @source U.S. Department of Health and Human Services, National Center for Education Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~field + awarded, data = Degree)
#' T1
#' barplot(t(T1), beside = TRUE, col = c("red", "skyblue"), legend = colnames(T1))
#' rm(T1)
#' 
"Degree"





#' Delay times on 20 flights from four major air carriers
#' 
#' Data for Exercise 10.55
#' 
#' 
#' @name Delay
#' @docType data
#' @format A data frame/tibble  with 80 observations on the following two variables.
#' \describe{ 
#' \item{delay}{the delay time (in minutes) for 80 randomly selected flights}
#' \item{carrier}{a factor with levels \code{A}, \code{B}, \code{C}, and \code{D}} 
#' }
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(delay ~ carrier, data = Delay)
#' kruskal.test(delay ~carrier, data = Delay)
#' 
"Delay"





#' Number of dependent children for 50 families
#' 
#' Data for Exercise 1.26
#' 
#' 
#' @name Depend
#' @docType data
#' @format A data frame/tibble  with 50 observations on one variable.
#' \describe{ 
#' \item{number}{number of dependent children in a family} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~ number, data = Depend)
#' T1
#' barplot(T1, col = "lightblue", main = "Problem 1.26",
#' xlab = "Number of Dependent Children", ylab = "Frequency")
#' rm(T1)
#' 
"Depend"





#' Educational levels of a sample of 40 auto workers in Detroit
#' 
#' Data for Exercise 5.21
#' 
#' 
#' @name Detroit
#' @docType data
#' @format A data frame/tibble  with 40 observations on the following variable.
#' \describe{ 
#' \item{educ}{the educational level (in years) of a sample of 40 auto workers in a plant in Detroit} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Detroit$educ)
#' 
"Detroit"





#' Demographic characteristics of developmental students at 2-year colleges and
#' 4-year colleges
#' 
#' Data used for Exercise 8.50
#' 
#' 
#' @name Develop
#' @docType data
#' @format A data frame/tibble  with 5656 observations on the following two variables.
#' \describe{ 
#' \item{race}{a factor with levels \code{African American}, \code{American Indian},
#' \code{Asian}, \code{Latino}, and \code{White}} 
#' \item{college}{a factor with levels \code{Two-year} and \code{Four-year}}
#' }
#' 
#' @source \emph{Research in Development Education} (1994), V. 11, 2.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~race + college, data = Develop)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Develop"





#' Test scores for students who failed developmental mathematics in the fall
#' semester 1995
#' 
#' Data for Exercise 6.47
#' 
#' 
#' @name Devmath
#' @docType data
#' @format A data frame/tibble  with 40 observations on the following variable.
#' \describe{ 
#' \item{score}{first exam score} 
#' }
#' 
#' @source Data provided by Dr. Anita Kitchens
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Devmath$score)
#' t.test(Devmath$score, mu = 80, alternative = "less")
#' 
"Devmath"





#' Outcomes and probabilities of the roll of a pair of fair dice
#' 
#' Data for Exercise 3.109
#' 
#' 
#' @name Dice
#' @docType data
#' @format A data frame/tibble  with 11 observations on the following two variables.
#' \describe{ 
#' \item{x}{possible outcomes for the sum of two dice} 
#' \item{px}{probability for outcome \code{x}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' roll1 <- sample(1:6, 20000, replace = TRUE)
#' roll2 <- sample(1:6, 20000, replace = TRUE)
#' outcome <- roll1 + roll2
#' T1 <- table(outcome)/length(outcome)
#' remove(roll1, roll2, outcome)
#' T1
#' round(t(Dice), 5)
#' rm(roll1, roll2, T1)
#' 
"Dice"





#' Diesel fuel prices in 1999-2000 in nine regions of the country
#' 
#' Data for Exercise 2.8
#' 
#' 
#' @name Diesel
#' @docType data
#' @format A data frame/tibble  with 650 observations on the following three variables.
#' \describe{ 
#' \item{date}{date when price was recorded}
#' \item{pricepergallon}{price per gallon (in dollars)}
#' \item{location}{a factor with levels \code{California}, \code{CentralAtlantic},
#' \code{Coast}, \code{EastCoast}, \code{Gulf}, \code{LowerAtlantic}, \code{NatAvg},
#' \code{NorthEast}, \code{Rocky}, and \code{WesternMountain}}
#' }
#' 
#' @source Energy Information Administration, National Enerfy Information Center:
#' 1000 Independence Ave., SW, Washington, D.C., 20585
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(las = 2)
#' boxplot(pricepergallon ~ location, data = Diesel)
#' boxplot(pricepergallon ~ location, 
#'          data = droplevels(Diesel[Diesel$location == "EastCoast" | 
#'          Diesel$location == "Gulf" | Diesel$location == "NatAvg" | 
#'          Diesel$location == "Rocky" | Diesel$location == "California", ]), 
#'          col = "pink", main = "Exercise 2.8")
#' par(las = 1) 
#' \dontrun{
#' ggplot2::ggplot(data = Diesel, aes(x = date, y = pricepergallon, 
#'                color = location)) + 
#'                geom_point() + 
#'                geom_smooth(se = FALSE) + 
#'                theme_bw() + 
#'                labs(y = "Price per Gallon (in dollars)")
#' }         
"Diesel"





#' Parking tickets issued to diplomats
#' 
#' Data for Exercises 1.14 and 1.37
#' 
#' 
#' @name Diplomat
#' @docType data
#' @format A data frame/tibble  with 10 observations on the following three variables.
#' \describe{ 
#' \item{country}{a factor with levels \code{Brazil}
#' \code{Bulgaria} \code{Egypt} \code{Indonesia} \code{Israel} \code{Nigeria}
#' \code{Russia} \code{S. Korea} \code{Ukraine} \code{Venezuela}}
#' \item{number}{total number of tickets} 
#' \item{rate}{number of tickets per vehicle per month} 
#' }
#' 
#' @source \emph{Time}, November 8, 1993. Figures are from January to June 1993
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(las = 2, mfrow = c(2, 2))
#' stripchart(number ~ country, data = Diplomat, pch = 19, 
#'            col= "red", vertical = TRUE)
#' stripchart(rate ~ country, data = Diplomat, pch = 19, 
#'            col= "blue", vertical = TRUE) 
#' with(data = Diplomat, 
#'      barplot(number, names.arg = country, col = "red"))
#' with(data = Diplomat, 
#'      barplot(rate, names.arg = country, col = "blue"))           
#' par(las = 1, mfrow = c(1, 1))
#' \dontrun{
#' ggplot2::ggplot(data = Diplomat, aes(x = reorder(country, number), 
#'                  y = number)) + 
#'            geom_bar(stat = "identity", fill = "pink", color = "black") + 
#'            theme_bw() + labs(x = "", y = "Total Number of Tickets")
#' ggplot2::ggplot(data = Diplomat, aes(x = reorder(country, rate), 
#'                  y = rate)) +
#'            geom_bar(stat = "identity", fill = "pink", color = "black") + 
#'            theme_bw() + labs(x = "", y = "Tickets per vehicle per month")
#' }
"Diplomat"





#' Toxic intensity for manufacturing plants producing herbicidal preparations
#' 
#' Data for Exercise 1.127
#' 
#' 
#' @name Disposal
#' @docType data
#' @format A data frame/tibble  with 29 observations on the following variable.
#' \describe{ 
#' \item{pounds}{pounds of toxic waste per $1000 of shipments of its products} 
#' }
#' 
#' @source Bureau of the Census, \emph{Reducing Toxins}, Statistical Brief SB/95-3,
#' February 1995.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Disposal$pounds)
#' fivenum(Disposal$pounds)
#' EDA(Disposal$pounds)
#' 
"Disposal"





#' Rankings of the favorite breeds of dogs
#' 
#' Data for Exercise 2.88
#' 
#' 
#' @name Dogs
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following three variables.
#' \describe{ 
#' \item{breed}{a factor with levels \code{Beagle},
#' \code{Boxer}, \code{Chihuahua}, \code{Chow}, \code{Dachshund}, 
#' \code{Dalmatian}, \code{Doberman}, \code{Huskie}, \code{Labrador}, 
#' \code{Pomeranian}, \code{Poodle}, \code{Retriever}, \code{Rotweiler}, 
#' \code{Schnauzer}, \code{Shepherd}, \code{Shetland}, \code{ShihTzu}, 
#' \code{Spaniel}, \code{Springer}, and  \code{Yorkshire}}
#' \item{ranking}{numeric ranking}
#' \item{year}{a factor with levels \code{1992}, \code{1993}, \code{1997}, 
#' and \code{1998}} 
#' }
#' 
#' @source \emph{The World Almanac and Book of Facts}, 2000
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' cor(Dogs$ranking[Dogs$year == "1992"], Dogs$ranking[Dogs$year == "1993"])
#' cor(Dogs$ranking[Dogs$year == "1997"], Dogs$ranking[Dogs$year == "1998"])
#' \dontrun{
#' ggplot2::ggplot(data = Dogs, aes(x = reorder(breed, ranking), y = ranking)) + 
#'      geom_bar(stat = "identity") + 
#'      facet_grid(year ~. ) + 
#'      theme(axis.text.x  = element_text(angle = 85, vjust = 0.5)) 
#' }
"Dogs"





#' Rates of domestic violence per 1,000 women by age groups
#' 
#' Data for Exercise 1.20
#' 
#' 
#' @name Domestic
#' @docType data
#' @format A data frame/tibble  with 5 observations on the following two variables.
#' \describe{ 
#' \item{age}{a factor with levels \code{12-19}, \code{20-24},
#' \code{25-34}, \code{35-49}, and \code{50-64}} 
#' \item{rate}{rate of domestic violence per 1000 women} 
#' }
#' 
#' @source U.S. Department of Justice.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' barplot(Domestic$rate, names.arg = Domestic$age)
#' \dontrun{
#' ggplot2::ggplot(data = Domestic, aes(x = age, y = rate)) + 
#'      geom_bar(stat = "identity", fill = "purple", color = "black") + 
#'      labs(x = "", y = "Domestic violence per 1000 women") + 
#'      theme_bw()
#' }
"Domestic"





#' Dopamine b-hydroxylase activity of schizophrenic patients treated with an
#' antipsychotic drug
#' 
#' Data for Exercises 5.14 and 7.49
#' 
#' 
#' @name Dopamine
#' @docType data
#' @format A data frame/tibble  with 25 observations on the following two variables.
#' \describe{ 
#' \item{dbh}{dopamine b-hydroxylase activity (units are nmol/(ml)(h)/(mg) of protein)} 
#' \item{group}{a factor with levels \code{nonpsychotic} and \code{psychotic}} 
#' }
#' 
#' @source D.E. Sternberg, D.P. Van Kammen, and W.E. Bunney, "Schizophrenia: Dopamine
#' b-Hydroxylase Activity and Treatment Respsonse," \emph{Science, 216} (1982), 1423 - 1425
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(dbh ~ group, data = Dopamine)
#' t.test(dbh ~ group, data = Dopamine, var.equal = TRUE)
#' 
"Dopamine"





#' Closing yearend Dow Jones Industrial averages from 1896 through 2000
#' 
#' Data for Exercise 1.35
#' 
#' 
#' @name Dowjones
#' @docType data
#' @format A data frame/tibble  with 105 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{date} 
#' \item{close}{Dow Jones closing price} 
#' \item{change}{percent change from previous year} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(close ~ year, data = Dowjones, type = "l", main = "Exercise 1.35")
#' \dontrun{
#' ggplot2::ggplot(data = Dowjones, aes(x = year, y = close)) +
#' geom_point(size = 0.5) + 
#' geom_line(color = "red") + 
#' theme_bw() + 
#' labs(y = "Dow Jones Closing Price")
#' }
"Dowjones"


#' Opinion on referendum by view on moral issue of selling alcoholic beverages
#' 
#' Data for Exercise 8.53
#' 
#' 
#' @name Drink
#' @docType data
#' @format A data frame/tibble  with 472 observations on two variables.
#' \describe{ 
#' \item{drinking}{a factor with levels \code{ok},
#' \code{tolerated}, and \code{immoral}} 
#' \item{referendum}{a factor with levels \code{for}, \code{against}, and \code{undecided}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~drinking + referendum, data = Drink)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Drink"





#' Number of trials to master a task for a group of 28 subjects assigned to a
#' control and an experimental group
#' 
#' Data for Example 7.15
#' 
#' 
#' @name Drug
#' @docType data
#' @format A data frame/tibble  with 28 observations on the following two variables.
#' \describe{ 
#' \item{trials}{number of trials to master a task} 
#' \item{group}{a factor with levels \code{control} and \code{experimental}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(trials ~ group, data = Drug)
#' wilcox.test(trials ~ group, data = Drug)
#' t.test(rank(trials) ~ group, data = Drug, var.equal = TRUE)
#' 
"Drug"





#' Data on a group of college students diagnosed with dyslexia
#' 
#' Data for Exercise 2.90
#' 
#' 
#' @name Dyslexia
#' @docType data
#' @format A data frame/tibble  with 8 observations on the following seven variables.
#' \describe{ 
#' \item{words}{number of words read per minute} 
#' \item{age}{age of participant} 
#' \item{gender}{a factor with levels \code{female} and 
#' \code{male}} 
#' \item{handed}{a factor with levels \code{left} and \code{right}}
#' \item{weight}{weight of participant (in pounds)} 
#' \item{height}{height of participant (in inches)} 
#' \item{children}{number of children in family} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(height ~ weight, data = Dyslexia)
#' plot(words ~ factor(handed), data = Dyslexia)
#' 
"Dyslexia"





#' One hundred year record of worldwide seismic activity(1770-1869)
#' 
#' Data for Exercise 6.97
#' 
#' 
#' @name Earthqk
#' @docType data
#' @format A data frame/tibble  with 100 observations on the following two variables.
#' \describe{ 
#' \item{year}{year seimic activity recorded} 
#' \item{severity}{annual incidence of sever earthquakes} 
#' }
#' 
#' @source Quenoille, M.H. (1952), \emph{Associated Measurements}, Butterworth, London.
#' p 279.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Earthqk$severity)
#' t.test(Earthqk$severity, mu = 100, alternative = "greater")
#' 
"Earthqk"





#' Crime rates versus the percent of the population without a high school
#' degree
#' 
#' Data for Exercise 2.41
#' 
#' 
#' @name Educat
#' @docType data
#' @format A data frame/tibble  with 51 observations on the following three variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{Alabama},
#' \code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
#' \code{Colorado}, \code{Connecticut}, \code{DC}, \code{Delaware}, \code{Florida},
#' \code{Georgia}, \code{Hawaii}, \code{Idaho}, \code{Illinois}, \code{Indiana},
#' \code{Iowa}, \code{Kansas}, \code{Kentucky}, \code{Louisiana}, \code{Maine},
#' \code{Maryland}, \code{Massachusetts}, \code{Michigan}, \code{Minnesota},
#' \code{Mississippi}, \code{Missour}, \code{Montana}, \code{Nebraska},
#' \code{Nevada}, \code{New Hampshire}, \code{New Jersey}, \code{New Mexico},
#' \code{New York}, \code{North Carolina}, \code{North Dakota}, \code{Ohio},
#' \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania}, \code{Rhode Island},
#' \code{South Carolina}, \code{South Dakota}, \code{Tennessee}, \code{Texas},
#' \code{Utah}, \code{Vermont}, \code{Virginia}, \code{Washington}, \code{West
#' Virginia}, \code{Wisconsin}, and \code{Wyoming}} 
#' \item{nodegree}{percent of the population without a high school degree} 
#' \item{crime}{violent crimes per 100,000 population} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(crime ~ nodegree, data = Educat, 
#'      xlab = "Percent of population without high school degree",
#'      ylab = "Violent Crime Rate per 100,000")
#' 
"Educat"





#' Number of eggs versus amounts of feed supplement
#' 
#' Data for Exercise 9.22
#' 
#' 
#' @name Eggs
#' @docType data
#' @format A data frame/tibble  with 12 observations on the following two variables.
#' \describe{ 
#' \item{feed}{amount of feed supplement} 
#' \item{eggs}{number of eggs per day for 100 chickens} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(eggs ~ feed, data = Eggs)
#' model <- lm(eggs ~ feed, data = Eggs)
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Eggs"





#' Percent of the population over the age of 65
#' 
#' Data for Exercise 1.92 and 2.61
#' 
#' 
#' @name Elderly
#' @docType data
#' @format A data frame/tibble  with 51 observations on the following three variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{Alabama},
#' \code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
#' \code{Colorado}, \code{Connecticut}, \code{Delaware}, \code{District of
#' Colunbia}, \code{Florida}, \code{Georgia}, \code{Hawaii}, \code{Idaho},
#' \code{Illinois}, \code{Indiana}, \code{Iowa}, \code{Kansas}, \code{Kentucky},
#' \code{Louisiana}, \code{Maine}, \code{Maryland}, \code{Massachusetts},
#' \code{Michigan}, \code{Minnesota}, \code{Mississippi}, \code{Missour},
#' \code{Montana}, \code{Nebraska}, \code{Nevada}, \code{New Hampshire}, \code{New
#' Jersey}, \code{New Mexico}, \code{New York}, \code{North Carolina}, \code{North
#' Dakota}, \code{Ohio}, \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania},
#' \code{Rhode Island}, \code{South Carolina}, \code{South Dakota},
#' \code{Tennessee}, \code{Texas}, \code{Utah}, \code{Vermont}, \code{Virginia},
#' \code{Washington}, \code{West Virginia}, \code{Wisconsin}, and \code{Wyoming}}
#' \item{percent1985}{percent of the population over the age of 65 in 1985} 
#' \item{percent1998}{percent of the population over the age of 65 in 1998} 
#' }
#' 
#' @source U.S. Census Bureau Internet site, February 2000
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' with(data = Elderly, 
#' stripchart(x = list(percent1998, percent1985), method = "stack", pch = 19,
#'            col = c("red","blue"), group.names = c("1998", "1985"))
#'            )
#' with(data = Elderly, cor(percent1998, percent1985))
#' \dontrun{
#' ggplot2::ggplot(data = Elderly, aes(x = percent1985, y = percent1998)) +
#'                geom_point() + theme_bw()
#' }
"Elderly"





#' Amount of energy consumed by homes versus their sizes
#' 
#' Data for Exercises 2.5, 2.24, and 2.55
#' 
#' 
#' @name Energy
#' @docType data
#' @format A data frame/tibble  with 12 observations on the following two variables.
#' \describe{ 
#' \item{size}{size of home (in square feet)} 
#' \item{kilowatt}{killowatt-hours per month} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(kilowatt ~ size, data = Energy)
#' with(data = Energy, cor(size, kilowatt))
#' model <- lm(kilowatt ~ size, data = Energy)
#' plot(Energy$size, resid(model))
#' 
"Energy"


#' Salaries after 10 years for graduates of three different universities
#' 
#' Data for Example 10.7
#' 
#' 
#' @name Engineer
#' @docType data
#' @format A data frame/tibble  with 51 observations on the following two variables.
#' \describe{ 
#' \item{salary}{salary (in $1000) 10 years after graduation} 
#' \item{university}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(salary ~ university, data = Engineer)
#' kruskal.test(salary ~ university, data = Engineer)
#' anova(lm(salary ~ university, data = Engineer))
#' anova(lm(rank(salary) ~ university, data = Engineer))
#' 
"Engineer"





#' College entrance exam scores for 24 high school seniors
#' 
#' Data for Example 1.8
#' 
#' 
#' @name Entrance
#' @docType data
#' @format A data frame/tibble  with 24 observations on the following variable.
#' \describe{ 
#' \item{score}{college entrance exam score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Entrance$score)
#' stem(Entrance$score, scale = 2)
#' 
"Entrance"





#' Fuel efficiency ratings for compact vehicles in 2001
#' 
#' Data for Exercise 1.65
#' 
#' 
#' @name Epaminicompact
#' @docType data
#' @format A data frame/tibble with 22 observations on the following 10 variables.
#' \describe{ 
#' \item{class}{a character variable with values \code{MINICOMPACT CARS}} 
#' \item{manufacturer}{a character variable with values \code{AUDI}
#' \code{BMW} \code{JAGUAR} \code{MERCEDES-BENZ} \code{MITSUBISHI}
#' \code{PORSCHE}} 
#' \item{carline}{a character variable with values \code{325CI
#' CONVERTIBLE} \code{330CI CONVERTIBLE} \code{911 CARRERA 2/4} \code{911
#' TURBO} \code{CLK320 (CABRIOLET)} \code{CLK430 (CABRIOLET)} \code{ECLIPSE
#' SPYDER} \code{JAGUAR XK8 CONVERTIBLE} \code{JAGUAR XKR CONVERTIBLE} \code{M3
#' CONVERTIBLE} \code{TT COUPE} \code{TT COUPE QUATTRO}} 
#' \item{displ}{a numeric vector} 
#' \item{cyl}{a numeric vector} 
#' \item{trans}{a factor with levels \code{Auto(L5)} \code{Auto(S4)} \code{Auto(S5)}
#' \code{Manual(M5)} \code{Manual(M6)}} 
#' \item{drv}{a factor with levels \code{4} \code{F} \code{R}} 
#' \item{cty}{a numeric vector}
#' \item{hwy}{a numeric vector} 
#' \item{cmb}{a numeric vector} 
#' }
#' 
#' @source EPA data
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(Epaminicompact$cty)
#' plot(hwy ~ cty, data = Epaminicompact)
#' 
"Epaminicompact"





#' Fuel efficiency ratings for two-seater vehicles in 2001
#' 
#' Data for Exercise 5.8
#' 
#' 
#' @name Epatwoseater
#' @docType data
#' @format A data frame/tibble with 36 observations on the following 10 variables.
#' \describe{ 
#' \item{class}{a character variable with values \code{TWO SEATERS}}
#' \item{manufacturer}{a character variable with values \code{ACURA} \code{AUDI}
#' \code{BMW} \code{CHEVROLET} \code{DODGE} \code{FERRARI} \code{HONDA}
#' \code{LAMBORGHINI} \code{MAZDA} \code{MERCEDES-BENZ} \code{PLYMOUTH}
#' \code{PORSCHE} \code{TOYOTA}} 
#' \item{carline}{a character variable with values
#' \code{BOXSTER} \code{BOXSTER S} \code{CORVETTE} \code{DB132/144
#' DIABLO} \code{FERRARI 360 MODENA/SPIDER} \code{FERRARI 550
#' MARANELLO/BARCHETTA} \code{INSIGHT} \code{MR2} \code{MX-5 MIATA} \code{NSX}
#' \code{PROWLER} \code{S2000} \code{SL500} \code{SL600} \code{SLK230
#' KOMPRESSOR} \code{SLK320} \code{TT ROADSTER} \code{TT ROADSTER QUATTRO}
#' \code{VIPER CONVERTIBLE} \code{VIPER COUPE} \code{Z3 COUPE} \code{Z3
#' ROADSTER} \code{Z8}} 
#' \item{displ}{a numeric vector}
#' \item{cyl}{a numeric vector} 
#' \item{trans}{a factor with levels \code{Auto(L4)} \code{Auto(L5)} \code{Auto(S4)} \code{Auto(S5)}
#' \code{Auto(S6)} \code{Manual(M5)} \code{Manual(M6)}} 
#' \item{drv}{a factor with levels \code{4} \code{F} \code{R}} 
#' \item{cty}{a numeric vector} 
#' \item{hwy}{a numeric vector} 
#' \item{cmb}{a numeric vector} 
#'  }
#'  
#'  @source Environmental Protection Agency
#'  
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(Epatwoseater$cty)
#' plot(hwy ~ cty, data = Epatwoseater)
#' boxplot(cty ~ drv, data = Epatwoseater)
#' 
"Epatwoseater"





#' Ages of 25 executives
#' 
#' Data for Exercise 1.104
#' 
#' 
#' @name Executiv
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{age}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Executiv$age, xlab = "Age of banking executives", 
#' breaks = 5, main = "", col = "gray")
#' 
"Executiv"





#' Weight loss for 30 members of an exercise program
#' 
#' Data for Exercise 1.44
#' 
#' 
#' @name Exercise
#' @docType data
#' @format A data frame/tibble with 30 observations on the following variable.
#' \describe{ 
#' \item{loss}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Exercise$loss)
#' 
"Exercise"





#' Measures of softness of 10 different clothing garments washed with and
#' without a softener
#' 
#' Data for Example 7.21
#' 
#' 
#' @name Fabric
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{garment}{a numeric vector} 
#' \item{softner}{a character variable with values \code{with} and \code{without}} 
#' \item{softness}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' wilcox.test(softness ~ softner, data = Fabric, 
#'             paired = TRUE, alternative = "greater")
#' 
#' \dontrun{
#' T7 <- tidyr::spread(Fabric, softner, softness) %>% 
#' mutate(di = with - without, adi = abs(di), rk = rank(adi), 
#'        srk = sign(di)*rk)
#' T7
#' t.test(T7$srk, alternative = "greater")
#' }
"Fabric"




#' Waiting times between successive eruptions of the Old Faithful geyser
#' 
#' Data for Exercise 5.12 and 5.111
#' 
#' 
#' @name Faithful
#' @docType data
#' @format A data frame/tibble with 299 observations on the following 2 variables.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' \item{eruption}{a factor with levels \code{1} and \code{2}} 
#' }
#' 
#' @source A. Azzalini and A. Bowman, "A Look at Some Data on the Old Faithful Geyser,"
#' \emph{Journal of the Royal Statistical Society}, Series C, \emph{39} (1990), 357-366.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(time ~ eruption, data = Faithful)
#' hist(Faithful$time, xlab = "wait time", main = "", freq = FALSE)
#' lines(density(Faithful$time))
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Faithful, aes(x = time, y = ..density..)) + 
#' geom_histogram(binwidth = 5, fill = "pink", col = "black") + 
#' geom_density() + 
#' theme_bw() + 
#' labs(x = "wait time")
#' }
"Faithful"





#' Size of family versus cost per person per week for groceries
#' 
#' Data for Exercise 2.89
#' 
#' 
#' @name Family
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{number}{a numeric vector} 
#' \item{cost}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(cost ~ number, data = Family)
#' abline(lm(cost ~ number, data = Family))
#' cor(Family$cost, Family$number)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Family, aes(x = number, y = cost)) + 
#'    geom_point() + 
#'    geom_smooth(method = "lm") + 
#'    theme_bw()
#' }
#' 
"Family"





#' Choice of presidental ticket in 1984 by gender
#' 
#' Data for Exercise 8.23
#' 
#' 
#' @name Ferraro1
#' @docType data
#' @format A data frame/tibble with 1000 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{a factor with levels \code{Men}
#' \code{Women}} 
#' \item{candidate}{a character vector of 1984 president and vice-president candidates} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~gender + candidate, data = Ferraro1)
#' T1
#' chisq.test(T1)  
#' rm(T1)
#' 
"Ferraro1"





#' Choice of vice presidental candidate in 1984 by gender
#' 
#' Data for Exercise 8.23
#' 
#' 
#' @name Ferraro2
#' @docType data
#' @format A data frame/tibble with 1000 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{a factor with levels \code{Men}
#' \code{Women}} 
#' \item{candidate}{a character vector of 1984 president and vice-president candidates} 
#' }
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~gender + candidate, data = Ferraro2)
#' T1
#' chisq.test(T1)  
#' rm(T1)
#' 
"Ferraro2"





#' Fertility rates of all 50 states and DC
#' 
#' Data for Exercise 1.125
#' 
#' 
#' @name Fertility
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 2 variables.
#' \describe{ 
#' \item{state}{a character variable with values \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of
#' Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho}
#' \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky}
#' \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts}
#' \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour}
#' \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New
#' Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North
#' Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania}
#' \code{Rhode Island} \code{South Carolina} \code{South Dakota}
#' \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia}
#' \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
#' \item{rate}{a numeric vector}
#' }
#' 
#' @source Population Reference Bureau.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Fertility$rate)
#' fivenum(Fertility$rate)
#' EDA(Fertility$rate)
#' 
"Fertility"





#' Ages of women at the birth of their first child
#' 
#' Data for Exercise 5.11
#' 
#' 
#' @name Firstchi
#' @docType data
#' @format A data frame/tibble with 87 observations on the following variable.
#' \describe{ 
#' \item{age}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Firstchi$age)
#' 
"Firstchi"





#' Length and number of fish caught with small and large mesh codend
#' 
#' Data for Exercises 5.83, 5.119, and 7.29
#' 
#' 
#' @name Fish
#' @docType data
#' @format A data frame/tibble with 1534 observations on two variables.
#' \describe{ 
#' \item{codend}{a character variable with values \code{smallmesh} and \code{largemesh} } 
#' \item{length}{length of the fish measured in centimeters} 
#' }
#' 
#' @source R. Millar, \dQuote{Estimating the Size - Selectivity of Fishing Gear by Conditioning
#' on the Total Catch,} \emph{Journal of the American Statistical Association, 87} (1992), 962 - 968
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' tapply(Fish$length, Fish$codend, median, na.rm = TRUE)
#' SIGN.test(Fish$length[Fish$codend == "smallmesh"], conf.level = 0.99)
#' \dontrun{
#' dplyr::group_by(Fish, codend) %>%
#'     summarize(MEDIAN = median(length, na.rm = TRUE))
#' }
#' 
"Fish"





#' Number of sit-ups before and after a physical fitness course
#' 
#' Data for Exercise 7.71
#' 
#' 
#' @name Fitness
#' @docType data
#' @format A data frame/tibble with 18 observations on the following 3 variables.
#' \describe{ 
#' \item{subject}{a character variable indicating subject number}
#' \item{test}{a character variable with values \code{After} and \code{Before}} 
#' \item{number}{a numeric vector recording the number of sit-ups performed in one minute} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(number ~ test, data = Fitness, alternative = "greater", paired = TRUE)
#' \dontrun{
#' Wide <- tidyr::spread(Fitness, test, number) %>%
#' mutate(diff = After - Before)
#' Wide
#' qqnorm(Wide$diff)
#' qqline(Wide$diff)
#' t.test(Wide$diff, alternative = "greater")
#' }
#' 
"Fitness"





#' Florida voter results in the 2000 presidential election
#' 
#' Data for Statistical Insight Chapter 2
#' 
#' 
#' @name Florida2000
#' @docType data
#' @format A data frame/tibble with 67 observations on the following 12 variables.
#' \describe{ 
#' \item{county}{a character variable with values \code{ALACHUA}
#' \code{BAKER} \code{BAY} \code{BRADFORD} \code{BREVARD} \code{BROWARD}
#' \code{CALHOUN} \code{CHARLOTTE} \code{CITRUS} \code{CLAY} \code{COLLIER}
#' \code{COLUMBIA} \code{DADE} \code{DE SOTO} \code{DIXIE} \code{DUVAL}
#' \code{ESCAMBIA} \code{FLAGLER} \code{FRANKLIN} \code{GADSDEN}
#' \code{GILCHRIST} \code{GLADES} \code{GULF} \code{HAMILTON} \code{HARDEE}
#' \code{HENDRY} \code{HERNANDO} \code{HIGHLANDS} \code{HILLSBOROUGH}
#' \code{HOLMES} \code{INDIAN RIVER} \code{JACKSON} \code{JEFFERSON}
#' \code{LAFAYETTE} \code{LAKE} \code{LEE} \code{LEON} \code{LEVY}
#' \code{LIBERTY} \code{MADISON} \code{MANATEE} \code{MARION} \code{MARTIN}
#' \code{MONROE} \code{NASSAU} \code{OKALOOSA} \code{OKEECHOBEE} \code{ORANGE}
#' \code{OSCEOLA} \code{PALM BEACH} \code{PASCO} \code{PINELLAS} \code{POLK}
#' \code{PUTNAM} \code{SANTA ROSA} \code{SARASOTA} \code{SEMINOLE} \code{ST.
#' JOHNS} \code{ST. LUCIE} \code{SUMTER} \code{SUWANNEE} \code{TAYLOR}
#' \code{UNION} \code{VOLUSIA} \code{WAKULLA} \code{WALTON} \code{WASHINGTON}}
#' \item{gore}{a numeric vector} 
#' \item{bush}{a numeric vector}
#' \item{buchanan}{a numeric vector} 
#' \item{nader}{a numeric vector} 
#' \item{browne}{a numeric vector} 
#' \item{hagelin}{a numeric vector} 
#' \item{harris}{a numeric vector}
#' \item{mcreynolds}{a numeric vector} 
#' \item{moorehead}{a numeric vector} 
#' \item{phillips}{a numeric vector}
#' \item{total}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(buchanan ~ total, data = Florida2000, 
#' xlab = "Total votes cast (in thousands)", 
#' ylab = "Votes for Buchanan")
#' 
"Florida2000"





#' Breakdown times of an insulating fluid under various levels of voltage
#' stress
#' 
#' Data for Exercise 5.76
#' 
#' 
#' @name Fluid
#' @docType data
#' @format A data frame/tibble with 76 observations on the following 2 variables.
#' \describe{ 
#' \item{kilovolts}{a character variable showing kilowats} 
#' \item{time}{breakdown time (in minutes)} 
#' }
#' 
#' @source E. Soofi, N. Ebrahimi, and M. Habibullah, 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' DF1 <- Fluid[Fluid$kilovolts == "34kV", ]
#' DF1
#' # OR
#' DF2 <- subset(Fluid, subset = kilovolts == "34kV")
#' DF2
#' stem(DF2$time)
#' SIGN.test(DF2$time)
#' \dontrun{
#' DF3 <- dplyr::filter(Fluid, kilovolts == "34kV") 
#' DF3
#' }
#' 
"Fluid"





#' Annual food expenditures for 40 single households in Ohio
#' 
#' Data for Exercise 5.106
#' 
#' 
#' @name Food
#' @docType data
#' @format A data frame/tibble with 40 observations on the following variable.
#' \describe{ 
#' \item{expenditure}{a numeric vector recording annual food expenditure (in dollars) in the state of Ohio.} 
#' }
#' 
#' @source Bureau of Labor Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Food$expenditure)
#' 
"Food"





#' Cholesterol values of 62 subjects in the Framingham Heart Study
#' 
#' Data for Exercises 1.56, 1.75, 3.69, and 5.60
#' 
#' 
#' @name Framingh
#' @docType data
#' @format A data frame/tibble with 62 observations on the following variable.
#' \describe{ 
#' \item{cholest}{a numeric vector with cholesterol values} 
#' }
#' 
#' @source R. D'Agostino, et al., (1990) "A Suggestion for Using Powerful and Informative
#' Tests for Normality," \emph{The American Statistician, 44} 316-321
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Framingh$cholest)
#' boxplot(Framingh$cholest, horizontal = TRUE)
#' hist(Framingh$cholest, freq = FALSE)
#' lines(density(Framingh$cholest))
#' mean(Framingh$cholest > 200 & Framingh$cholest < 240)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Framingh, aes(x = factor(1), y = cholest)) + 
#' geom_boxplot() +                 # boxplot
#' labs(x = "") +                   # no x label  
#' theme_bw() +                     # black and white theme  
#' geom_jitter(width = 0.2) +       # jitter points
#' coord_flip()                     # Create horizontal plot
#' ggplot2::ggplot(data = Framingh, aes(x = cholest, y = ..density..)) +
#' geom_histogram(fill = "pink", binwidth = 15, color = "black") + 
#' geom_density() + 
#' theme_bw()
#' }
#' 
"Framingh"



#' Ages of a random sample of 30 college freshmen
#' 
#' Data for Exercise 6.53
#' 
#' 
#' @name Freshman
#' @docType data
#' @format A data frame/tibble with 30 observations on the following variable.
#' \describe{ 
#' \item{age}{a numeric vector of ages} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' SIGN.test(Freshman$age, md = 19)
#' 
"Freshman"





#' Cost of funeral by region of country
#' 
#' Data for Exercise 8.54
#' 
#' 
#' @name Funeral
#' @docType data
#' @format A data frame/tibble with 400 observations on the following 2 variables.
#' \describe{ 
#' \item{region}{a factor with levels \code{Central}
#' \code{East} \code{South} \code{West}} 
#' \item{cost}{a factor with levels \code{less than expected}, \code{about what expected}, 
#' and \code{more than expected}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~region + cost, data = Funeral)
#' T1
#' chisq.test(T1)  
#' rm(T1)
#' 
"Funeral"





#' Velocities of 82 galaxies in the Corona Borealis region
#' 
#' Data for Example 5.2
#' 
#' 
#' @name Galaxie
#' @docType data
#' @format A data frame/tibble with 82 observations on the following variable.
#' \describe{ 
#' \item{velocity}{velocity measured in kilometers per second} 
#' }
#' 
#' @source K. Roeder, "Density Estimation with Confidence Sets Explained by Superclusters
#' and Voids in the Galaxies," \emph{Journal of the American Statistical Association}, 85
#' (1990), 617-624
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' EDA(Galaxie$velocity)
#' 
"Galaxie"





#' Results of a Gallup poll on possession of marijuana as a criminal offense
#' conducted in 1980
#' 
#' Data for Exercise 2.76
#' 
#' 
#' @name Gallup
#' @docType data
#' @format A data frame/tibble with 1,200 observations on the following 2 variables.
#' \describe{ 
#' \item{demographics}{a factor with levels \code{National}, \code{Gender: Male}
#' \code{Gender: Female}, \code{Education: College}, \code{Eduction: High School},
#' \code{Education: Grade School}, \code{Age: 18-24}, \code{Age: 25-29}, \code{Age: 30-49},
#' \code{Age: 50-older}, \code{Religion: Protestant}, and \code{Religion: Catholic}} 
#' \item{opinion}{a factor with levels \code{Criminal}, \code{Not Criminal}, and \code{No Opinion}}
#' }
#' 
#' @source George H. Gallup \emph{The Gallup Opinion Index Report No. 179} (Princeton, NJ:
#' The Gallup Poll, July 1980), p. 15
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~demographics + opinion, data = Gallup)
#' T1
#' t(T1[c(2, 3), ])
#' barplot(t(T1[c(2, 3), ]))
#' barplot(t(T1[c(2, 3), ]), beside = TRUE)
#' 
#' \dontrun{
#' dplyr::filter(Gallup, demographics == "Gender: Male" | demographics == "Gender: Female") %>%
#' ggplot2::ggplot(aes(x = demographics, fill = opinion)) + 
#' geom_bar() + 
#' theme_bw() + 
#' labs(y = "Fraction")
#' }
#' 
"Gallup"





#' Price of regular unleaded gasoline obtained from 25 service stations
#' 
#' Data for Exercise 1.45
#' 
#' 
#' @name Gasoline
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{price}{price for one gallon of gasoline} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Gasoline$price)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Gasoline, aes(x = factor(1), y = price)) + 
#'     geom_violin() + 
#'     geom_jitter() + 
#'     theme_bw()
#' }
#' 
"Gasoline"





#' Number of errors in copying a German passage before and after an
#' experimental course in German
#' 
#' Data for Exercise 7.60
#' 
#' 
#' @name German
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 3 variables.
#' \describe{ 
#' \item{student}{a character variable indicating student number} 
#' \item{when}{a character variable with either code\{Before} or \code{After} 
#' to indicate when the student received experimental instruction in German
#' \item{errors}{A vector containing the number of errors}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(errors ~ when, data = German, paired = TRUE)
#' wilcox.test(errors ~ when, data = German)
#' 
#' \dontrun{
#' T8 <- tidyr::spread(German, when, errors) %>%
#' mutate(di = After - Before, adi = abs(di), rk = rank(adi), srk = sign(di)*rk)
#' T8
#' qqnorm(T8$di)
#' qqline(T8$di)
#' t.test(T8$srk)
#' }
#' 
"German"





#' Distances a golf ball can be driven by 20 professional golfers
#' 
#' Data for Exercise 5.24
#' 
#' 
#' @name Golf
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{yards}{distance a golf ball is driven in yards} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Golf$yards)
#' qqnorm(Golf$yards)
#' qqline(Golf$yards)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Golf, aes(sample = yards)) + 
#' geom_qq() + 
#' theme_bw()
#' }
#' 
"Golf"




#' Annual salaries for state governors in 1994 and 1999
#' 
#' Data for Exercise 5.112
#' 
#' 
#' @name Governor
#' @docType data
#' @format A data frame/tibble with 50 observations on the following 3 variables.
#' \describe{ 
#' \item{state}{a character variable with values \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{Florida}
#' \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana}
#' \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine}
#' \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota}
#' \code{Mississippi} \code{Missouri} \code{Montana} \code{Nebraska}
#' \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico}
#' \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio}
#' \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island}
#' \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas}
#' \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West
#' Virginia} \code{Wisconsin} \code{Wyoming}} 
#' \item{year}{a factor indicating year} 
#' \item{salary}{a numeric vector with the governor's salary in dollars} 
#' }
#' 
#' @source \emph{The 2000 World Almanac and Book of Facts}
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(salary ~ year, data = Governor)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Governor, aes(x = salary)) + 
#' geom_density(fill = "pink") + 
#' facet_grid(year ~ .) + 
#' theme_bw()
#' }
#' 
"Governor"





#' High school GPA versus college GPA
#' 
#' Data for Example 2.13
#' 
#' 
#' @name Gpa
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{hsgpa}{high school gpa} 
#' \item{collgpa}{college gpa} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(collgpa ~ hsgpa, data = Gpa)
#' mod <- lm(collgpa ~ hsgpa, data = Gpa)
#' abline(mod)               # add line
#' yhat <- predict(mod)      # fitted values
#' e <- resid(mod)           # residuals
#' cbind(Gpa, yhat, e)       # Table 2.1
#' cor(Gpa$hsgpa, Gpa$collgpa)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Gpa, aes(x = hsgpa, y = collgpa)) + 
#' geom_point() + 
#' geom_smooth(method = "lm") + 
#' theme_bw()
#' }
#' 
#' 
"Gpa"





#' Test grades in a beginning statistics class
#' 
#' Data for Exercise 1.120
#' 
#' 
#' @name Grades
#' @docType data
#' @format A data frame with 29 observations on the following variable.
#' \describe{ 
#' \item{grades}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Grades$grades, main = "", xlab = "Test grades", right = FALSE)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Grades, aes(x = grades, y = ..density..)) + 
#'     geom_histogram(fill = "pink", binwidth = 5, color = "black") + 
#'     geom_density(lwd = 2, color = "red") + 
#'     theme_bw() 
#' }
#' 
"Grades"





#' Graduation rates for student athletes in the Southeastern Conf.
#' 
#' Data for Exercise 1.118
#' 
#' 
#' @name Graduate
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{school}{a character variable with values \code{Alabama}
#' \code{Arkansas} \code{Auburn} \code{Florida} \code{Georgia} \code{Kentucky}
#' \code{Louisiana St} \code{Mississippi} \code{Mississippi St} \code{South
#' Carolina} \code{Tennessee} \code{Vanderbilt}} 
#' \item{code}{a character variable with values \code{Al} \code{Ar} \code{Au} \code{Fl} \code{Ge} \code{Ke}
#' \code{LSt} \code{Mi} \code{MSt} \code{SC} \code{Te} \code{Va}}
#' \item{percent}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' barplot(Graduate$percent, names.arg = Graduate$school, 
#'         las = 2, cex.names = 0.7, col = "tomato")
#' 
"Graduate"





#' Varve thickness from a sequence through an Eocene lake deposit in the Rocky
#' Mountains
#' 
#' Data for Exercise 6.57
#' 
#' 
#' @name Greenriv
#' @docType data
#' @format A data frame/tibble with 37 observations on the following variable.
#' \describe{ 
#' \item{thick}{varve thickness in millimeters} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Greenriv$thick)
#' SIGN.test(Greenriv$thick, md = 7.3, alternative = "greater")
#' 
"Greenriv"





#' Thickness of a varved section of the Green river oil shale deposit near a
#' major lake in the Rocky Mountains
#' 
#' Data for Exercises 6.45 and 6.98
#' 
#' 
#' @name Grnriv2
#' @docType data
#' @format A data frame/tibble with 101 observations on the following variable.
#' \describe{ 
#' \item{thick}{varve thickness in millimeters} 
#' }
#' 
#' @source J. Davis, \emph{Statistics and Data Analysis in Geology}, 2nd Ed., Jon Wiley and Sons, New York.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Grnriv2$thick)
#' t.test(Grnriv2$thick, mu = 8, alternative = "less")
#' 
"Grnriv2"





#' Group data to illustrate analysis of variance
#' 
#' Data for Exercise 10.42
#' 
#' 
#' @name Groupabc
#' @docType data
#' @format A data frame/tibble with 45 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' \item{response}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(response ~ group, data = Groupabc, 
#'         col = c("red", "blue", "green"))
#'         anova(lm(response ~ group, data = Groupabc))
#' 
"Groupabc"





#' An illustration of analysis of variance
#' 
#' Data for Exercise 10.4
#' 
#' 
#' @name Groups
#' @docType data
#' @format A data frame/tibble with 78 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' \item{response}{a numeric vector} 
#' }
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(response ~ group, data = Groups, col = c("red", "blue", "green"))
#' anova(lm(response ~ group, data = Groups))
#' 
#' 
"Groups"





#' Children's age versus number of completed gymnastic activities
#' 
#' Data for Exercises 2.21 and 9.14
#' 
#' 
#' @name Gym
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 3 variables.
#' \describe{
#' \item{age}{a numeric vector} 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(number ~ age, data = Gym)
#' model <- lm(number ~ age, data = Gym)
#' abline(model)
#' summary(model)
#' 
"Gym"





#' Study habits of students in two matched school districts
#' 
#' Data for Exercise 7.57
#' 
#' 
#' @name Habits
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 4 variables.
#' \describe{ 
#' \item{A}{a numeric vector} 
#' \item{B}{a numeric vector} 
#' \item{differ}{\code{B} minus \code{A}} 
#' \item{signrks}{the signed-ranked-differences} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' shapiro.test(Habits$differ)
#' qqnorm(Habits$differ)
#' qqline(Habits$differ)
#' wilcox.test(Habits$B, Habits$A, paired = TRUE, alternative = "less")
#' t.test(Habits$signrks, alternative = "less")
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Habits, aes(x = differ)) + 
#'     geom_dotplot(fill = "blue") + 
#'     theme_bw()
#' }
#' 
"Habits"





#' Haptoglobin concentration in blood serum of 8 healthy adults
#' 
#' Data for Example 6.9
#' 
#' 
#' @name Haptoglo
#' @docType data
#' @format A data frame/tibble with 8 observations on the following variable.
#' \describe{ 
#' \item{concent}{haptoglobin concentration (in grams per liter)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' shapiro.test(Haptoglo$concent)
#' t.test(Haptoglo$concent, mu = 2, alternative = "less")
#' 
#' 
"Haptoglo"





#' Daily receipts for a small hardware store for 31 working days
#' 
#' Data for ???
#' 
#' 
#' @name Hardware
#' @docType data
#' @format A data frame with 31 observations on the following variable.
#' \describe{ 
#' \item{receipt}{a numeric vector} 
#' }
#' 
#' @source J.C. Miller and J.N. Miller, (1988), \emph{Statistics for Analytical Chemistry}, 2nd Ed. 
#' (New York: Halsted Press)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Hardware$receipt)
#' 
"Hardware"





#' Tensile strength of Kraft paper for different percentages of hardwood in the
#' batches of pulp
#' 
#' Data for Example 2.18 and Exercise 9.34 
#' 
#' 
#' @name Hardwood
#' @docType data
#' @format A data frame/tibble with 19 observations on 2 variables.
#' \describe{ 
#' \item{tensile}{tensile strength of kraft paper (in pounds per square inch)}
#' \item{hardwood}{percent of hardwood in the batch of pulp that was used to produce the paper} 
#' }
#' 
#' @source G. Joglekar, et al., "Lack-of-Fit Testing When Replicates Are Not Available,"
#' \emph{The American Statistician}, 43(3), (1989), 135-143
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(tensile ~ hardwood, data = Hardwood)
#' model <- lm(tensile ~ hardwood, data = Hardwood)
#' abline(model)
#' plot(model, which = 1)
#' 
#' 
"Hardwood"

#' Primary heating sources of homes on indian reservations versus all
#' households
#' 
#' Data for Exercise 1.29
#' 
#' 
#' @name Heat
#' @docType data
#' @format A data frame/tibble with 301 observations on the following 2 variables.
#' \describe{ 
#' \item{fuel}{a factor with levels \code{Utility gas},
#' \code{LP bottled gas}, \code{Electricity}, \code{Fuel oil}, \code{Wood}, and
#' \code{Other}} 
#' \item{location}{a factor with levels \code{American Indians on reservation} \code{All U.S. households},
#'  and \code{American Indians not on reservations}} 
#' }
#' 
#' @source Bureau of the Census, \emph{Housing of the American Indians on Reservations},
#' Statistical Brief 95-11, April 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~ fuel + location, data = Heat)
#' T1
#' barplot(t(T1), beside = TRUE, legend = TRUE)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Heat, aes(x = fuel, fill = location)) + 
#'    geom_bar(position = "dodge") + 
#'    labs(y = "percent") + 
#'    theme_bw() + 
#'    theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
#' }
#' 
"Heat"




#' Fuel efficiency ratings for three types of oil heaters
#' 
#' Data for Exercise 10.32
#' 
#' 
#' @name Heating
#' @docType data
#' @format A data frame/tibble with 90 observations on the following 2 variables.
#' \describe{ 
#' \item{type}{a factor with levels \code{A}, \code{B}, and \code{C} denoting the type of oil heater} 
#' \item{efficiency}{heater efficiency rating} 
#' }
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(efficiency ~ type, data = Heating, 
#'         col = c("red", "blue", "green"))
#' kruskal.test(efficiency ~ type, data = Heating)
#' 
"Heating"




#' Results of treatments for Hodgkin's disease
#' 
#' Data for Exercise 2.77
#' 
#' 
#' @name Hodgkin
#' @docType data
#' @format A data frame/tibble with 538 observations on the following 2 variables.
#' \describe{ 
#' \item{type}{a factor with levels \code{LD},
#' \code{LP}, \code{MC}, and \code{NS}} 
#' \item{response}{a factor with levels \code{Positive}, \code{Partial}. and \code{None}}
#' }
#' 
#' @source I. Dunsmore, F. Daly, \emph{Statistical Methods, Unit 9, Categorical Data}, 
#' Milton Keynes, The Open University, 18
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~type + response, data = Hodgkin)
#' T1
#' barplot(t(T1), legend = TRUE, beside = TRUE)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Hodgkin, aes(x = type, fill = response)) + 
#'     geom_bar(position = "dodge") + 
#'     theme_bw()
#' }
#' 
"Hodgkin"

#' Median prices of single-family homes in 65 metropolitan statistical areas
#' 
#' Data for Statistical Insight Chapter 5
#' 
#' 
#' @name Homes
#' @docType data
#' @format A data frame/tibble with 65 observations on the following 4 variables.
#' \describe{ 
#' \item{city}{a character variable with values \code{Akron OH},
#' \code{Albuquerque NM}, \code{Anaheim CA}, \code{Atlanta GA}, \code{Baltimore
#' MD}, \code{Baton Rouge LA}, \code{Birmingham AL}, \code{Boston MA},
#' \code{Bradenton FL}, \code{Buffalo NY}, \code{Charleston SC}, \code{Chicago
#' IL}, \code{Cincinnati OH}, \code{Cleveland OH}, \code{Columbia SC},
#' \code{Columbus OH}, \code{Corpus Christi TX}, \code{Dallas TX},
#' \code{Daytona Beach FL}, \code{Denver CO}, \code{Des Moines IA},
#' \code{Detroit MI}, \code{El Paso TX}, \code{Grand Rapids MI},
#' \code{Hartford CT}, \code{Honolulu HI}, \code{Houston TX},
#' \code{Indianapolis IN}, \code{Jacksonville FL}, \code{Kansas City MO},
#' \code{Knoxville TN}, \code{Las Vegas NV}, \code{Los Angeles CA},
#' \code{Louisville KY}, \code{Madison WI}, \code{Memphis TN}, \code{Miami FL},
#' \code{Milwaukee WI}, \code{Minneapolis MN}, \code{Mobile AL},
#' \code{Nashville TN}, \code{New Haven CT}, \code{New Orleans LA}, \code{New
#' York NY}, \code{Oklahoma City OK}, \code{Omaha NE}, \code{Orlando FL},
#' \code{Philadelphia PA}, \code{Phoenix AZ}, \code{Pittsburgh PA},
#' \code{Portland OR}, \code{Providence RI}, \code{Sacramento CA}, \code{Salt
#' Lake City UT}, \code{San Antonio TX}, \code{San Diego CA}, \code{San
#' Francisco CA}, \code{Seattle WA}, \code{Spokane WA}, \code{St Louis MO},
#' \code{Syracuse NY}, \code{Tampa FL}, \code{Toledo OH}, \code{Tulsa OK}, and
#' \code{Washington DC}} 
#' \item{region}{a character variable with values \code{Midwest}, \code{Northeast},
#' \code{South}, and \code{West}} 
#' \item{year}{a factor with levels \code{1994} and \code{2000}}
#' \item{price}{median house price in dollars} 
#' }
#' 
#' @source National Association of Realtors
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' tapply(Homes$price, Homes$year, mean)
#' tapply(Homes$price, Homes$region, mean)
#' p2000 <- subset(Homes, year == "2000")
#' p1994 <- subset(Homes, year == "1994")
#' \dontrun{
#' ggplot2::ggplot(data = Homes, aes(x = region, y = price)) + 
#'    geom_boxplot() + 
#'    theme_bw() + 
#'    facet_grid(year ~ .)
#' }
#' 
#' 
"Homes"





#' Number of hours per week spent on homework for private and public high
#' school students
#' 
#' Data for Exercise 7.78
#' 
#' 
#' @name Homework
#' @docType data
#' @format A data frame with 30 observations on the following 2 variables.
#' \describe{ 
#' \item{school}{type of school either \code{private} or \code{public}} 
#' \item{time}{number of hours per week spent on homework} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(time ~ school, data = Homework, 
#'         ylab = "Hours per week spent on homework")
#' #
#' t.test(time ~ school, data = Homework)
#' 
"Homework"





#' Miles per gallon for a Honda Civic on 35 different occasions
#' 
#' Data for Statistical Insight Chapter 6
#' 
#' 
#' @name Honda
#' @docType data
#' @format A data frame/tibble with 35 observations on the following variable.
#' \describe{ 
#' \item{mileage}{miles per gallon for a Honda Civic} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' 
#' @examples
#' 
#' t.test(Honda$mileage, mu = 40, alternative = "less")
#' 
"Honda"





#' Hostility levels of high school students from rural, suburban, and urban
#' areas
#' 
#' Data for Example 10.6
#' 
#' 
#' @name Hostile
#' @docType data
#' @format A data frame/tibble with 135 observations on the following 2 variables.
#' \describe{ 
#' \item{location}{a factor with the location of the high school student (\code{Rural}, \code{Suburban}, or \code{Urban})} 
#' \item{hostility}{the score from the Hostility Level Test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(hostility ~ location, data = Hostile, 
#'         col = c("red", "blue", "green"))
#' kruskal.test(hostility ~ location, data = Hostile)
#' 
"Hostile"





#' Median home prices for 1984 and 1993 in 37 markets across the U.S.
#' 
#' Data for Exercise 5.82
#' 
#' 
#' @name Housing
#' @docType data
#' @format A data frame/tibble with 74 observations on the following 3 variables.
#' \describe{ 
#' \item{city}{a character variable with values \code{Albany}
#' \code{Anaheim} \code{Atlanta} \code{Baltimore} \code{Birmingham}
#' \code{Boston} \code{Chicago} \code{Cincinnati} \code{Cleveland}
#' \code{Columbus} \code{Dallas} \code{Denver} \code{Detroit} \code{Ft
#' Lauderdale} \code{Houston} \code{Indianapolis} \code{Kansas City} \code{Los
#' Angeles} \code{Louisville} \code{Memphis} \code{Miami} \code{Milwaukee}
#' \code{Minneapolis} \code{Nashville} \code{New York} \code{Oklahoma City}
#' \code{Philadelphia} \code{Providence} \code{Rochester} \code{Salt Lake City}
#' \code{San Antonio} \code{San Diego} \code{San Francisco} \code{San Jose}
#' \code{St Louis} \code{Tampa} \code{Washington}} 
#' \item{year}{a factor with levels \code{1984} and \code{1993}} 
#' \item{price}{median house price} 
#' }
#' 
#' @source National Association of Realtors
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripchart(price ~ year, data = Housing, method = "stack", 
#'            pch = 1, col = c("red", "blue"))
#' \dontrun{
#' ggplot2::ggplot(data = Housing, aes(x = price, fill = year)) + 
#'               geom_dotplot() + 
#'               facet_grid(year ~ .) + 
#'               theme_bw()
#' }               
#' 
"Housing"





#' Number of storms, hurricanes and El Nino effects from 1950 through 1995
#' 
#' Data for Exercises 1.38, 10.19, and Example 1.6
#' 
#' 
#' @name Hurrican
#' @docType data
#' @format A data frame/tibble with 46 observations on the following 4 variables.
#' \describe{ 
#' \item{year}{a numeric vector indicating year} 
#' \item{storms}{a numeric vector recording number of storms} 
#' \item{hurrican}{a numeric vector recording number of hurricanes}
#' \item{elnino}{a factor with levels \code{cold}, \code{neutral}, and
#' \code{warm}} 
#' }
#' 
#' @source National Hurricane Center
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~hurrican, data = Hurrican)
#' T1
#' barplot(T1, col = "blue", main = "Problem 1.38",
#'      xlab = "Number of hurricanes", 
#'      ylab = "Number of seasons")
#' boxplot(storms ~ elnino, data = Hurrican, 
#'      col = c("blue", "yellow", "red"))
#'      anova(lm(storms ~ elnino, data = Hurrican))
#' rm(T1)
#' 
"Hurrican"





#' Number of icebergs sighted each month south of Newfoundland and south of the
#' Grand Banks in 1920
#' 
#' Data for Exercise 2.46 and 2.60
#' 
#' 
#' @name Iceberg
#' @docType data
#' @format A data frame with 12 observations on the following 3 variables.
#' \describe{
#'  \item{month}{a character variable with abbreviated months of the year} 
#' \item{Newfoundland}{number of icebergs sighted south of Newfoundland}
#' \item{Grand Banks}{number of icebergs sighted south of Grand Banks} 
#' }
#' 
#' @source N. Shaw, \emph{Manual of Meteorology}, Vol. 2 (London: Cambridge University Press 1942),
#' 7; and F. Mosteller and J. Tukey, \emph{Data Analysis and Regression} (Reading, MA: Addison - Wesley, 1977)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(Newfoundland ~ `Grand Banks`, data = Iceberg)
#' abline(lm(Newfoundland ~ `Grand Banks`, data = Iceberg))
#' 
"Iceberg"





#' Percent change in personal income from 1st to 2nd quarter in 2000
#' 
#' Data for Exercise 1.33
#' 
#' 
#' @name Income
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 2 variables.
#' \describe{ 
#' \item{state}{a character variable with values \code{Alabama},
#' \code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
#' \code{Colorado}, \code{Connecticut}, \code{Delaware}, \code{District of
#' Colunbia}, \code{Florida}, \code{Georgia}, \code{Hawaii}, \code{Idaho},
#' \code{Illinois}, \code{Indiana}, \code{Iowa}, \code{Kansas}, \code{Kentucky},
#' \code{Louisiana}, \code{Maine}, \code{Maryland}, \code{Massachusetts},
#' \code{Michigan}, \code{Minnesota}, \code{Mississippi}, \code{Missour},
#' \code{Montana}, \code{Nebraska}, \code{Nevada}, \code{New Hampshire}, \code{New
#' Jersey}, \code{New Mexico}, \code{New York}, \code{North Carolina}, \code{North
#' Dakota}, \code{Ohio}, \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania},
#' \code{Rhode Island}, \code{South Carolina}, \code{South Dakota},
#' \code{Tennessee}, \code{Texas}, \code{Utah}, \code{Vermont}, \code{Virginia},
#' \code{Washington}, \code{West Virginia}, \code{Wisconsin}, and \code{Wyoming}}
#' \item{percent_change}{percent change in income from first quarter to the second quarter of 2000} 
#' }
#' 
#' @source US Department of Commerce
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' Income$class <- cut(Income$percent_change, 
#'                     breaks = c(-Inf, 0.5, 1.0, 1.5, 2.0, Inf))
#' T1 <- xtabs(~class, data = Income)
#' T1
#' barplot(T1, col = "pink")   
#' \dontrun{
#' DF <- as.data.frame(T1)
#' DF
#' ggplot2::ggplot(data = DF,  aes(x = class, y = Freq)) + 
#'    geom_bar(stat = "identity", fill = "purple") + 
#'    theme_bw()
#' }  
#' 
"Income"





#' Illustrates a comparison problem for long-tailed distributions
#' 
#' Data for Exercise 7.41
#' 
#' 
#' @name Independent
#' @docType data
#' @format A data frame/tibble with 46 observations on the following 2 variables.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' \item{group}{a factor with levels \code{A} and \code{B}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Independent$score[Independent$group=="A"])
#' qqline(Independent$score[Independent$group=="A"])
#' qqnorm(Independent$score[Independent$group=="B"])
#' qqline(Independent$score[Independent$group=="B"])
#' boxplot(score ~ group, data = Independent)
#' wilcox.test(score ~ group, data = Independent)
#' 
"Independent"





#' Educational attainment versus per capita income and poverty rate for
#' American indians living on reservations
#' 
#' Data for Exercise 2.95
#' 
#' 
#' @name Indian
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 4 variables.
#' \describe{ 
#' \item{reservation}{a character variable with values \code{Blackfeet},
#' \code{Fort Apache}, \code{Gila River}, \code{Hopi}, \code{Navajo}, \code{Papago},
#' \code{Pine Ridge}, \code{Rosebud}, \code{San Carlos}, and \code{Zuni Pueblo}}
#' \item{percent high school}{percent who have graduated from high school} 
#' \item{per capita income}{per capita income} 
#' \item{poverty rate}{percent poverty} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(mfrow = c(1, 2))
#' plot(`per capita income` ~ `percent high school`, data = Indian, 
#'   xlab = "Percent high school graudates", ylab = "Per capita income")
#' plot(`poverty rate` ~ `percent high school`, data = Indian, 
#'   xlab = "Percent high school graudates", ylab = "Percent poverty")
#' par(mfrow = c(1, 1))
#' 
"Indian"





#' Average miles per hour for the winners of the Indianapolis 500 race
#' 
#' Data for Exercise 1.128
#' 
#' 
#' @name Indiapol
#' @docType data
#' @format A data frame/tibble with 39 observations on the following 2 variables.
#' \describe{ 
#' \item{year}{the year of the race} 
#' \item{speed}{the winners average speed} 
#' }
#' 
#' @source The World Almanac and Book of Facts, 2000, p. 1004
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(speed ~ year, data = Indiapol, type = "b")
#' 
"Indiapol"





#' Qualifying miles per hour and number of previous starts for drivers in 79th
#' Indianapolis 500 race
#' 
#' Data for Exercises 7.11 and 7.36
#' 
#' 
#' @name Indy500
#' @docType data
#' @format A data frame/tibble with 33 observations on the following 4 variables.
#' \describe{ 
#' \item{driver}{a character variable with values \code{andretti},
#' \code{bachelart}, \code{boesel}, \code{brayton}, \code{c.guerrero},
#' \code{cheever}, \code{fabi}, \code{fernandez}, \code{ferran}, \code{fittipaldi},
#' \code{fox}, \code{goodyear}, \code{gordon}, \code{gugelmin}, \code{herta},
#' \code{james}, \code{johansson}, \code{jones}, \code{lazier}, \code{luyendyk},
#' \code{matsuda}, \code{matsushita}, \code{pruett}, \code{r.guerrero},
#' \code{rahal}, \code{ribeiro}, \code{salazar}, \code{sharp}, \code{sullivan},
#' \code{tracy}, \code{vasser}, \code{villeneuve}, and \code{zampedri}}
#' \item{qualif}{qualifying speed (mph)} 
#' \item{starts}{number of Indianapolis 500 starts} 
#' \item{group}{a numeric vector where 1 indicates the driver has 4 or fewer 
#' Indianapolis 500 starts and a 2 for drivers with 5 or more Indianapolis 500 starts} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripchart(qualif ~ group, data = Indy500, method = "stack",
#'            pch = 19, col = c("red", "blue"))
#' boxplot(qualif ~ group, data = Indy500)
#' t.test(qualif ~ group, data = Indy500)
#' \dontrun{
#' ggplot2::ggplot(data = Indy500, aes(sample = qualif)) + 
#'           geom_qq() + 
#'           facet_grid(group ~ .) + 
#'           theme_bw()
#' }
#' 
"Indy500"



#' Private pay increase of salaried employees versus inflation rate
#' 
#' Data for Exercises 2.12 and 2.29
#' 
#' 
#' @name Inflatio
#' @docType data
#' @format A data frame/tibble with 24 observations on the following 4 variables.
#' \describe{
#' \item{year}{a numeric vector of years} 
#' \item{pay}{average hourly wage for salaried employees} 
#' \item{increase}{percent increase in hourly wage over previous year}
#' \item{inflation}{percent inflation rate} 
#' }
#' 
#' @source Bureau of Labor Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(increase ~ inflation, data = Inflatio)
#' cor(Inflatio$increase, Inflatio$inflation, use = "complete.obs")
#' 
"Inflatio"





#' Inlet oil temperature through a valve
#' 
#' Data for Exercises 5.91 and 6.48
#' 
#' 
#' @name Inletoil
#' @docType data
#' @format A data frame/tibble with 12 observations on the following variable.
#' \describe{ 
#' \item{temp}{inlet oil temperature (Fahrenheit)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Inletoil$temp, breaks = 3)
#' qqnorm(Inletoil$temp)
#' qqline(Inletoil$temp)
#' t.test(Inletoil$temp)
#' t.test(Inletoil$temp, mu = 98, alternative = "less")
#' 
"Inletoil"





#' Type of drug offense by race
#' 
#' Data for Statistical Insight Chapter 8
#' 
#' 
#' @name Inmate
#' @docType data
#' @format A data frame/tibble with 28,037 observations on the following 2 variables.
#' \describe{ 
#' \item{race}{a factor with levels \code{white},
#' \code{black}, and \code{hispanic}} 
#' \item{drug}{a factor with levels \code{heroin}, \code{crack}, \code{cocaine}, and \code{marijuana}}
#' }
#' 
#' @source C. Wolf Harlow (1994), \emph{Comparing Federal and State Prison Inmates},
#' NCJ-145864, U.S. Department of Justice, Bureau of Justice Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~race + drug, data = Inmate)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Inmate"





#' Percent of vehicles passing inspection by type inspection station
#' 
#' Data for Exercise 8.59
#' 
#' 
#' @name Inspect
#' @docType data
#' @format A data frame/tibble with 174 observations on the following 2 variables.
#' \describe{ 
#' \item{station}{a factor with levels \code{auto inspection},
#' \code{auto repair}, \code{car care center}, \code{gas station}, \code{new car
#' dealer}, and \code{tire store}} 
#' \item{passed}{a factor with levels \code{less than 70\%}, \code{between 70\% and 84\%}, and \code{more than 85\%}}
#' }
#' 
#' @source \emph{The Charlotte Observer}, December 13, 1992
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~ station + passed, data = Inspect)
#' T1
#' barplot(T1, beside = TRUE, legend = TRUE)
#' chisq.test(T1)
#' rm(T1)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Inspect, aes(x = passed, fill = station)) + 
#'            geom_bar(position = "dodge") + 
#'            theme_bw()
#' }
#' 
"Inspect"




#' Heat loss through a new insulating medium
#' 
#' Data for Exercise 9.50
#' 
#' 
#' @name Insulate
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{temp}{outside temperature (in degrees centigrade)} 
#' \item{loss}{heat loss (in BTUs)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(loss ~ temp, data = Insulate)
#' model <- lm(loss ~ temp, data = Insulate)
#' abline(model, col = "blue") 
#' summary(model)
#' 
#' \dontrun{
#' ggplot2::ggplot(data = Insulate, aes(x = temp, y = loss)) + 
#'            geom_point() + 
#'            geom_smooth(method = "lm", se = FALSE) + 
#'            theme_bw()
#' }
#' 
"Insulate"





#' GPA versus IQ for 12 individuals
#' 
#' Data for Exercises 9.51 and 9.52
#' 
#' 
#' @name Iqgpa
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{iq}{IQ scores} 
#' \item{gpa}{Grade point average} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(gpa ~ iq, data = Iqgpa, col = "blue", pch = 19)
#' model <- lm(gpa ~ iq, data = Iqgpa)
#' summary(model)
#' rm(model)
#' 
"Iqgpa"





#' R.A. Fishers famous data on Irises
#' 
#' Data for Examples 1.15 and 5.19
#' 
#' 
#' @name Irises
#' @docType data
#' @format A data frame/tibble with 150 observations on the following 5 variables.
#' \describe{ 
#' \item{sepal_length}{sepal length (in cm)} 
#' \item{sepal_width}{sepal width (in cm)} 
#' \item{petal_length}{petal length (in cm)}
#' \item{petal_width}{petal width (in cm)} 
#' \item{species}{a factor with levels \code{setosa}, \code{versicolor}, and \code{virginica}} 
#' }
#' @source Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. 
#' \emph{Annals of Eugenics}, \strong{7}, Part II, 179-188.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' tapply(Irises$sepal_length, Irises$species, mean)
#' t.test(Irises$sepal_length[Irises$species == "setosa"], conf.level = 0.99)
#' hist(Irises$sepal_length[Irises$species == "setosa"], 
#'      main = "Sepal length for\n Iris Setosa",
#'      xlab = "Length (in cm)")
#' boxplot(sepal_length ~ species, data = Irises)
#' 
"Irises"





#' Number of problems reported per 100 cars in 1994 versus 1995s
#' 
#' Data for Exercise 2.14, 2.17, 2.31, 2.33, and 2.40
#' 
#' 
#' @name Jdpower
#' @docType data
#' @format A data frame/tibble with 29 observations on the following 3 variables.
#' \describe{ 
#' \item{car}{a factor with levels \code{Acura}, \code{BMW},
#' \code{Buick}, \code{Cadillac}, \code{Chevrolet}, \code{Dodge} \code{Eagle},
#' \code{Ford}, \code{Geo}, \code{Honda}, \code{Hyundai}, \code{Infiniti},
#' \code{Jaguar}, \code{Lexus}, \code{Lincoln}, \code{Mazda}, \code{Mercedes-Benz},
#' \code{Mercury}, \code{Mitsubishi}, \code{Nissan}, \code{Oldsmobile},
#' \code{Plymouth}, \code{Pontiac}, \code{Saab}, \code{Saturn}, and \code{Subaru},
#' \code{Toyota} \code{Volkswagen}, \code{Volvo}} 
#' \item{1994}{number of problems per 100 cars in 1994} 
#' \item{1995}{number of problems per 100 cars in 1995} 
#' }
#' 
#' @source \emph{USA Today}, May 25, 1995.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(`1995` ~ `1994`, data = Jdpower)
#' summary(model)
#' plot(`1995` ~ `1994`, data = Jdpower)
#' abline(model, col = "red")
#' rm(model)
#' 
"Jdpower"





#' Job satisfaction and stress level for 9 school teachers
#' 
#' Data for Exercise 9.60
#' 
#' 
#' @name Jobsat
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 2 variables.
#' \describe{ 
#' \item{wspt}{Wilson Stress Profile score for teachers} 
#' \item{satisfaction}{job satisfaction score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(satisfaction ~ wspt, data = Jobsat)
#' model <- lm(satisfaction ~ wspt, data = Jobsat)
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Jobsat"





#' Smoking habits of boys and girls ages 12 to 18
#' 
#' Data for Exercise 4.85
#' 
#' 
#' @name Kidsmoke
#' @docType data
#' @format A data frame/tibble with 1000 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{character vector with values \code{female} and \code{male}} 
#' \item{smoke}{a character vector with values \code{no} and \code{yes}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~smoke + gender, data = Kidsmoke)
#' T1
#' prop.table(T1)
#' prop.table(T1, 1)
#' prop.table(T1, 2)
#' 
"Kidsmoke"





#' Rates per kilowatt-hour for each of the 50 states and DC
#' 
#' Data for Example 5.9
#' 
#' 
#' @name Kilowatt
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 2 variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{Alabama}
#' \code{Alaska}, \code{Arizona}, \code{Arkansas} \code{California},
#' \code{Colorado}, \code{Connecticut}, \code{Delaware}, \code{District of
#' Columbia}, \code{Florida},\code{Georgia}, \code{Hawaii}, \code{Idaho},
#' \code{Illinois}, \code{Indiana}, \code{Iowa} \code{Kansas} \code{Kentucky},
#' \code{Louisiana}, \code{Maine}, \code{Maryland}, \code{Massachusetts},
#' \code{Michigan}, \code{Minnesota}, \code{Mississippi}, \code{Missour},
#' \code{Montana} \code{Nebraska}, \code{Nevada}, \code{New Hampshire}, \code{New
#' Jersey}, \code{New Mexico}, \code{New York}, \code{North Carolina}, \code{North
#' Dakota}, \code{Ohio}, \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania},
#' \code{Rhode Island}, \code{South Carolina}, \code{South Dakota},
#' \code{Tennessee}, \code{Texas}, \code{Utah}, \code{Vermont}, \code{Virginia}
#' \code{Washington}, \code{West Virginia}, \code{Wisconsin}, and \code{Wyoming}}
#' \item{rate}{a numeric vector indicating rates for kilowatt per hour} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Kilowatt$rate)
#' 
"Kilowatt"





#' Reading scores for first grade children who attended kindergarten versus
#' those who did not
#' 
#' Data for Exercise 7.68
#' 
#' 
#' @name Kinder
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 3 variables.
#' \describe{ 
#' \item{pair}{a numeric indicator of pair} 
#' \item{kinder}{reading score of kids who went to kindergarten} 
#' \item{nokinder}{reading score of kids who did not go to kindergarten} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Kinder$kinder, Kinder$nokinder)
#' diff <- Kinder$kinder - Kinder$nokinder
#' qqnorm(diff)
#' qqline(diff)
#' shapiro.test(diff)
#' t.test(Kinder$kinder, Kinder$nokinder, paired = TRUE)
#' # Or
#' t.test(diff)
#' rm(diff)
#' 
"Kinder"





#' Median costs of laminectomies at hospitals across North Carolina in 1992
#' 
#' Data for Exercise 10.18
#' 
#' 
#' @name Laminect
#' @docType data
#' @format A data frame/tibble with 138 observations on the following 2 variables.
#' \describe{
#'  \item{area}{a character vector indicating the area of the hospital with \code{Rural}, \code{Regional},
#'  and \code{Metropol}} 
#'  \item{cost}{a numeric vector indicating cost of a laminectomy} 
#'  }
#'  
#'@source \emph{Consumer's Guide to Hospitalization Charges in North Carolina Hospitals} (August 1994),
#'North Carolina Medical Database Commission, Department of Insurance
#'  
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#'boxplot(cost ~ area, data = Laminect, col = topo.colors(3))
#'anova(lm(cost ~ area, data = Laminect))
#' 
"Laminect"


#' Lead levels in children's blood whose parents worked in a battery factory
#' 
#' Data for Example 1.17
#' 
#' 
#' @name Lead
#' @docType data
#' @format A data frame/tibble with 66 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{a character vector with values \code{exposed} and \code{control}} 
#' \item{lead}{a numeric vector indicating the level of lead in children's blood in micrograms/dl} 
#' }
#' 
#' @source Morton, D. et al. (1982), "Lead Absorption in Children of Employees in a Lead-Related
#' Industry," \emph{American Journal of Epidemiology, 155,} 549-555. 
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(lead ~ group, data = Lead)
#' 
"Lead"



#' Leadership exam scores by age for employees on an industrial plant
#' 
#' Data for Exercise 7.31
#' 
#' 
#' @name Leader
#' @docType data
#' @format A data frame/tibble with 34 observations on the following 2 variables.
#' \describe{ 
#' \item{age}{a character vector indicating age with values \code{under35} and \code{over35}} 
#' \item{score}{score on a leadership exam} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#'boxplot(score ~ age, data = Leader, col = c("gray", "green"))
#'t.test(score ~ age, data = Leader)
#' 
"Leader"


#' Survival time of mice injected with an experimental lethal drug
#' 
#' Data for Example 6.12
#' 
#' 
#' @name Lethal
#' @docType data
#' @format A data frame/tibble with 30 observations on the following variable.
#' \describe{ 
#' \item{survival}{a numeric vector indicating time surivived after injection} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#'BSDA::SIGN.test(Lethal$survival, md = 45, alternative = "less")
#'
#'
"Lethal"





#' Life expectancy of men and women in U.S.
#' 
#' Data for Exercise 1.31
#' 
#' 
#' @name Life
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{a numeric vector indicating year} 
#' \item{men}{life expectancy for men} 
#' \item{women}{life expectancy for women} 
#' }
#' 
#' @source National Center for Health Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#'plot(men ~ year, type = "l", ylim = c(min(men, women), max(men, women)), col = "blue",
#'main = "Life Expectancy vs Year", ylab = "Age", xlab = "Year", data = Life)
#'lines(women ~ year, col = "red", data = Life)
#'text(1955, 65, "Men", col = "blue")
#'text(1955, 70, "Women", col = "red")
#' 
"Life"





#' Life span of electronic components used in a spacecraft versus heat
#' 
#' Data for Exercise 2.4, 2.37, and 2.49
#' 
#' 
#' @name Lifespan
#' @docType data
#' @format A data frame/tibble with 6 observations on the following 2 variables.
#' \describe{ 
#' \item{heat}{temperature in degrees C} 
#' \item{life}{lifespan in hours} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(life ~ heat, data = Lifespan)
#' model <- lm(life ~ heat, data = Lifespan)
#' abline(model)
#' resid(model)
#' sum((resid(model))^2)
#' anova(model)
#' rm(model)
#' 
"Lifespan"





#' Relationship between damage reports and deaths caused by lightning
#' 
#' Data for Exercise 2.6
#' 
#' 
#' @name Ligntmonth
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 4 variables.
#' \describe{ 
#' \item{month}{a factor with levels \code{1/01/2000}
#' \code{10/01/2000} \code{11/01/2000} \code{12/01/2000} \code{2/01/2000}
#' \code{3/01/2000} \code{4/01/2000} \code{5/01/2000} \code{6/01/2000}
#' \code{7/01/2000} \code{8/01/2000} \code{9/01/2000}} 
#' \item{deaths}{a numeric vector} 
#' \item{injuries}{a numeric vector}
#' \item{damage}{a numeric vector} 
#' }
#' 
#' @source \emph{Lighting Fatalities, Injuries and Damage Reports in the United States},
#' 1959-1994, NOAA Technical Memorandum NWS SR-193, Dept. of Commerce.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(deaths ~ damage, data = Ligntmonth)
#' model = lm(deaths ~ damage, data = Ligntmonth)
#' abline(model)
#' rm(model)
#' 
"Ligntmonth"





#' Measured traffic at three prospective locations for a motor lodge
#' 
#' Data for Exercise 10.33
#' 
#' 
#' @name Lodge
#' @docType data
#' @format A data frame/tibble with 45 observations on the following 6 variables.
#' \describe{ 
#' \item{traffic}{a numeric vector indicating the amount of vehicles that passed a site in 1 hour} 
#' \item{site}{a numeric vector with values \code{1}, \code{2}, and \code{3}} 
#' \item{ranks}{ranks for variable \code{traffic}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(traffic ~ site, data = Lodge, col = cm.colors(3))
#' anova(lm(traffic ~ factor(site), data = Lodge))
#' 
"Lodge"





#' Long-tailed distributions to illustrate Kruskal Wallis test
#' 
#' Data for Exercise 10.45
#' 
#' 
#' @name Longtail
#' @docType data
#' @format A data frame/tibble with 60 observations on the following 3 variables.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' \item{group}{a numeric vector with values \code{1}, \code{2}, and \code{3}} 
#' \item{ranks}{ranks for variable \code{score}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ group, data = Longtail, col = heat.colors(3))
#' kruskal.test(score ~ factor(group), data = Longtail)
#' anova(lm(score ~ factor(group), data = Longtail))
#' 
"Longtail"





#' Reading skills of 24 matched low ability students
#' 
#' Data for Example 7.18
#' 
#' 
#' @name Lowabil
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{pair}{a numeric indicator of pair} 
#' \item{experiment}{score of the  child with the experimental method} 
#' \item{control}{score of the child with the standard method} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' diff = Lowabil$experiment - Lowabil$control
#' qqnorm(diff)
#' qqline(diff)
#' shapiro.test(diff)
#' t.test(Lowabil$experiment, Lowabil$control, paired = TRUE)
#' # OR
#' t.test(diff)
#' rm(diff)
#' 
"Lowabil"





#' Magnesium concentration and distances between samples
#' 
#' Data for Exercise 9.9
#' 
#' 
#' @name Magnesiu
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{distance}{distance between samples}
#' \item{magnesium}{concentration of magnesium} 
#' }
#' 
#' @source Davis, J. (1986), \emph{Statistics and Data Analysis in Geology}, 2d. Ed.,
#' John Wiley and Sons, New York, p. 146
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(magnesium ~ distance, data = Magnesiu)
#' model = lm(magnesium ~ distance, data = Magnesiu)
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Magnesiu"





#' Amounts awarded in 17 malpractice cases
#' 
#' Data for Exercise 5.73
#' 
#' 
#' @name Malpract
#' @docType data
#' @format A data frame/tibble with 17 observations on the following variable.
#' \describe{ 
#' \item{award}{malpractice reward in $1000} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::SIGN.test(Malpract$award, conf.level = 0.90)
#' 
"Malpract"





#' Advertised salaries offered general managers of major corporations in 1995
#' 
#' Data for Exercise 5.81
#' 
#' 
#' @name Manager
#' @docType data
#' @format A data frame/tibble with 26 observations on the following variable.
#' \describe{ 
#' \item{salary}{random sample of advertised salaries of top executives} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Manager$salary)
#' BSDA::SIGN.test(Manager$salary)
#' 
"Manager"





#' Percent of marked cars in 65 police departments in Florida
#' 
#' Data for Exercise 6.100
#' 
#' 
#' @name Marked
#' @docType data
#' @format A data frame/tibble with 65 observations on the following variable.
#' \describe{ 
#' \item{percent}{percentage of marked cars in 65 Florida police departments}
#' }
#' 
#' @source \emph{Law Enforcement Management and Administrative Statistics, 1993}, Bureau of 
#' Justice Statistics, NCJ-148825, September 1995, p. 147-148
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Marked$percent)
#' BSDA::SIGN.test(Marked$percent, md = 60, alternative = "greater")
#' t.test(Marked$percent, mu = 60, alternative = "greater")
#' 
"Marked"






#' Standardized math test scores for 30 students
#' 
#' Data for Exercise 1.69
#' 
#' 
#' @name Math
#' @docType data
#' @format A data frame/tibble with 30 observations on the following variable.
#' \describe{ 
#' \item{score}{scores on a standardized test for 30 tenth graders} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Math$score)
#' hist(Math$score, main = "Math Scores", xlab = "score", freq = FALSE)
#' lines(density(Math$score), col = "red")
#' CharlieZ <- (62 - mean(Math$score))/sd(Math$score)
#' CharlieZ
#' scale(Math$score)[which(Math$score == 62)]
#' 
"Math"








#' Standardized math competency for a group of entering freshmen at a small
#' community college
#' 
#' Data for Exercise 5.26
#' 
#' 
#' @name Mathcomp
#' @docType data
#' @format A data frame/tibble with 31 observations on the following variable.
#' \describe{ 
#' \item{score}{scores of 31 entering freshmen at a community college on a national standardized test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Mathcomp$score)
#' BSDA::EDA(Mathcomp$score)
#' 
"Mathcomp"





#' Math proficiency and SAT scores by states
#' 
#' Data for Exercise 9.24, Example 9.1, and Example 9.6
#' 
#' 
#' @name Mathpro
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 4 variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{} \code{Conn},
#' \code{D.C.}, \code{Del}, \code{Ga}, \code{Hawaii}, \code{Ind}, \code{Maine},
#' \code{Mass}, \code{Md}, \code{N.C.}, \code{N.H.}, \code{N.J.}, \code{N.Y.},
#' \code{Ore}, \code{Pa}, \code{R.I.}, \code{S.C.}, \code{Va}, and \code{Vt}}
#' \item{sat_math}{SAT math scores for high school seniors} 
#' \item{profic}{math proficiency scores for eigth graders} 
#' \item{group}{a numeric vector} 
#' }
#' 
#' @source National Assessment of Educational Progress and The College Board
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(sat_math ~ profic, data = Mathpro)
#' plot(sat_math ~ profic, data = Mathpro, ylab = "SAT", xlab = "proficiency")
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Mathpro"






#' Error scores for four groups of experimental animals running a maze
#' 
#' Data for Exercise 10.13
#' 
#' 
#' @name Maze
#' @docType data
#' @format A data frame/tibble with 32 observations on the following 2 variables.
#' \describe{ 
#' \item{score}{error scores for animals running through a maze under different conditions} 
#' \item{condition}{a factor with levels \code{CondA},
#' \code{CondB,} \code{CondC}, and \code{CondD}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ condition, data = Maze, col = rainbow(4))
#' anova(lm(score ~ condition, data = Maze))
#' 
"Maze"





#' Illustrates test of equality of medians with the Kruskal Wallis test
#' 
#' Data for Exercise 10.52
#' 
#' 
#' @name Median
#' @docType data
#' @format A data frame/tibble with 45 observations on the following 2 variables.
#' \describe{ 
#' \item{sample}{a vector with values \code{Sample1}, \code{Sample 2}, and \code{Sample 3}} 
#' \item{value}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(value ~ sample, data = Median, col = rainbow(3))
#' anova(lm(value ~ sample, data = Median))
#' kruskal.test(value ~ factor(sample), data = Median)
#' 
"Median"





#' Median mental ages of 16 girls
#' 
#' Data for Exercise 6.52
#' 
#' 
#' @name Mental
#' @docType data
#' @format A data frame/tibble with 16 observations on the following variable.
#' \describe{ 
#' \item{age}{mental age of 16 girls} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::SIGN.test(Mental$age, md = 100)
#' 
"Mental"





#' Concentration of mercury in 25 lake trout
#' 
#' Data for Example 1.9
#' 
#' 
#' @name Mercury
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{mercury}{a numeric vector measuring mercury in parts per million} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Mercury$mercury)
#' 
"Mercury"





#' Monthly rental costs in metro areas with 1 million or more persons
#' 
#' Data for Exercise 5.117
#' 
#' 
#' @name Metrent
#' @docType data
#' @format A data frame/tibble with 46 observations on the following variable.
#' \describe{ 
#' \item{rent}{monthly rent in dollars} 
#' }
#' 
#' @source U.S. Bureau of the Census, \emph{Housing in the Metropolitan Areas, Statistical Brief} SB/94/19,
#' September 1994
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Metrent$rent)
#' t.test(Metrent$rent, conf.level = 0.99)$conf
#' 
"Metrent"




#' Miller personality test scores for a group of college students applying for
#' graduate school
#' 
#' Data for Example 5.7
#' 
#' 
#' @name Miller
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{miller}{scores on the Miller Personality test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Miller$miller)
#' fivenum(Miller$miller)
#' boxplot(Miller$miller)
#' qqnorm(Miller$miller,col = "blue")
#' qqline(Miller$miller, col = "red")
#' 
"Miller"





#' Twenty scores on the Miller personality test
#' 
#' Data for Exercise 1.41
#' 
#' 
#' @name Miller1
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{miller}{scores on the Miller personality test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Miller1$miller)
#' stem(Miller1$miller, scale = 2)
#' 
"Miller1"





#' Moisture content and depth of core sample for marine muds in eastern
#' Louisiana
#' 
#' Data for Exercise 9.37
#' 
#' 
#' @name Moisture
#' @docType data
#' @format A data frame/tibble with 16 observations on the following 4 variables.
#' \describe{ 
#' \item{depth}{a numeric vector} 
#' \item{moisture}{g of water per 100 g of dried sediment} 
#' \item{lnmoist}{a numeric vector}
#' \item{depthsq}{a numeric vector} 
#' }
#' 
#' @source Davis, J. C. (1986), \emph{Statistics and Data Analysis in Geology}, 2d. ed.,
#' John Wiley and Sons, New York, pp. 177, 185
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(moisture ~ depth, data = Moisture)
#' model <- lm(moisture ~ depth, data = Moisture)
#' abline(model)
#' plot(resid(model) ~ depth, data = Moisture)
#' rm(model)
#' 
"Moisture"





#' Carbon monoxide emitted by smoke stacks of a manufacturer and a competitor
#' 
#' Data for Exercise 7.45
#' 
#' 
#' @name Monoxide
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{company}{a vector with values \code{manufacturer} and \code{competitor}} 
#' \item{emission}{carbon monoxide emitted} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(emission ~ company, data = Monoxide, col = topo.colors(2))
#' t.test(emission ~ company, data = Monoxide)
#' wilcox.test(emission ~ company, data = Monoxide)
#' \dontrun{
#' ggplot2::ggplot(data = Monoxide, aes(x = company, y = emission)) + 
#'   geom_boxplot() + 
#'     theme_bw()
#' }
#' 
"Monoxide"





#' Moral attitude scale on 15 subjects before and after viewing a movie
#' 
#' Data for Exercise 7.53
#' 
#' 
#' @name Movie
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{before}{moral aptitude before viewing the movie} 
#' \item{after}{moral aptitude after viewing the movie} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Movie$differ)
#' qqline(Movie$differ)
#' shapiro.test(Movie$differ)
#' t.test(Movie$after, Movie$before, paired = TRUE, conf.level = 0.99)
#' wilcox.test(Movie$after, Movie$before, paired = TRUE)
#' 
"Movie"





#' Improvement scores for identical twins taught music recognition by two
#' techniques
#' 
#' Data for Exercise 7.59
#' 
#' 
#' @name Music
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{method1}{a numeric vector measuring the improvement scores on a music recognition test} 
#' \item{method2}{a numeric vector measuring the improvement scores on a music recognition test} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Music$differ)
#' qqline(Music$differ)
#' shapiro.test(Music$differ)
#' t.test(Music$method1, Music$method2, paired = TRUE)
#' # Or
#' t.test(Music$differ)
#' \dontrun{
#' ggplot2::ggplot(data = Music, aes(x = differ)) + 
#' geom_dotplot() + 
#' theme_bw()
#' }
#' 
"Music"





#' Estimated value of a brand name product and the conpany's revenue
#' 
#' Data for Exercises 2.28, 9.19, and Example 2.8
#' 
#' 
#' @name Name
#' @docType data
#' @format A data frame/tibble with 42 observations on the following 3 variables.
#' \describe{ 
#' \item{brand}{a factor with levels \code{Band-Aid},
#' \code{Barbie}, \code{Birds Eye}, \code{Budweiser}, \code{Camel}, \code{Campbell},
#' \code{Carlsberg}, \code{Coca-Cola}, \code{Colgate}, \code{Del Monte},
#' \code{Fisher-Price}, \verb{Gordon's}, \code{Green Giant}, \code{Guinness},
#' \code{Haagen-Dazs}, \code{Heineken}, \code{Heinz}, \code{Hennessy},
#' \code{Hermes}, \code{Hershey}, \code{Ivory}, \code{Jell-o}, \code{Johnnie
#' Walker}, \code{Kellogg}, \code{Kleenex}, \code{Kraft}, \code{Louis Vuitton},
#' \code{Marlboro}, \code{Nescafe}, \code{Nestle}, \code{Nivea}, \code{Oil of Olay},
#' \code{Pampers}, \code{Pepsi-Cola}, \code{Planters}, \code{Quaker}, \code{Sara
#' Lee}, \code{Schweppes}, \code{Smirnoff}, \code{Tampax}, \code{Winston}, and
#' \verb{Wrigley's}}
#' \item{value}{value in billions of dollars}
#' \item{revenue}{revenue in billions of dollars} 
#' }
#' 
#' @source Financial World
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(value ~ revenue, data = Name)
#' model <- lm(value ~ revenue, data = Name)
#' abline(model)
#' cor(Name$value, Name$revenue)
#' summary(model)
#' rm(model)
#' 
"Name"





#' Efficiency of pit crews for three major NASCAR teams
#' 
#' Data for Exercise 10.53
#' 
#' 
#' @name Nascar
#' @docType data
#' @format A data frame/tibble with 36 observations on the following 6 variables.
#' \describe{ 
#' \item{time}{duration of pit stop} 
#' \item{team}{a numeric vector representing team 1, 2, or 3} 
#' \item{ranks}{a numeric vector ranking each pit stop in order of speed} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(time ~ team, data = Nascar)
#' model <- lm(time ~ factor(team), data = Nascar)
#' summary(model)
#' anova(model)
#' rm(model)
#' 
"Nascar"





#' Reaction effects of 4 drugs on 25 subjects with a nervous disorder
#' 
#' Data for Example 10.3
#' 
#' 
#' @name Nervous
#' @docType data
#' @format A data frame/tibble with 25 observations on the following 2 variables.
#' \describe{ 
#' \item{react}{a numeric vector representing reaction time} 
#' \item{drug}{a numeric vector indicating each of the 4 drugs} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(react ~ drug, data = Nervous, col = rainbow(4))
#' model <- aov(react ~ factor(drug), data = Nervous)
#' summary(model)
#' TukeyHSD(model)
#' plot(TukeyHSD(model), las = 1)
#' 
"Nervous"





#' Daily profits for 20 newsstands
#' 
#' Data for Exercise 1.43
#' 
#' 
#' @name Newsstand
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{profit}{profit of each newsstand} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Newsstand$profit)
#' stem(Newsstand$profit, scale = 3)
#' 
"Newsstand"





#' Rating, time in 40-yard dash, and weight of top defensive linemen in the
#' 1994 NFL draft
#' 
#' Data for Exercise 9.63
#' 
#' 
#' @name Nfldraf2
#' @docType data
#' @format A data frame/tibble with 47 observations on the 3 following variables.
#' \describe{ 
#' \item{rating}{rating of each player on a scale out of 10}
#' \item{forty}{forty yard dash time in seconds}
#' \item{weight}{weight of each player in pounds}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(rating ~ forty, data = Nfldraf2)
#' summary(lm(rating ~ forty, data = Nfldraf2))
#' 
"Nfldraf2"





#' Rating, time in 40-yard dash, and weight of top offensive linemen in the
#' 1994 NFL draft
#' 
#' Data for Exercises 9.10 and 9.16
#' 
#' 
#' @name Nfldraft
#' @docType data
#' @format A data frame/tibble with 29 observations on the 3 following variables.
#' \describe{ 
#' \item{rating}{rating of each player on a scale out of 10}
#' \item{forty}{forty yard dash time in seconds}
#' \item{weight}{weight of each player in pounds}
#' }
#' 
#' @source \emph{USA Today}, April 20, 1994
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(rating ~ forty, data = Nfldraft)
#' cor(Nfldraft$rating, Nfldraft$forty)
#' summary(lm(rating ~ forty, data = Nfldraft))
#' 
"Nfldraft"





#' Nicotine content versus sales for 8 major brands of cigarettes
#' 
#' Data for Exercise 9.21
#' 
#' 
#' @name Nicotine
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{nicotine}{nicotine content in milligrams} 
#' \item{sales}{sales figures in $100,000} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(sales ~ nicotine, data = Nicotine)
#' plot(sales ~ nicotine, data = Nicotine)
#' abline(model)
#' summary(model)
#' predict(model, newdata = data.frame(nicotine = 1), 
#'      interval = "confidence", level = 0.99)
#' 
"Nicotine"





#' Price of oranges versus size of the harvest
#' 
#' Data for Exercise 9.61
#' 
#' 
#' @name Orange
#' @docType data
#' @format A data frame/tibble with 6 observations on the following 2 variables.
#' \describe{ 
#' \item{harvest}{harvest in millions of boxes} 
#' \item{price}{price is average price charged by California growers for for 75-pound box of navel oranges} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(price ~ harvest, data = Orange)
#' model <- lm(price ~ harvest, data = Orange)
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Orange"





#' Salaries of members of the Baltimore Orioles baseball team
#' 
#' Data for Example 1.3
#' 
#' 
#' @name Orioles
#' @docType data
#' @format A data frame/tibble with 27 observations on the following 3 variables.
#' \describe{ 
#' \item{first name}{a factor with levels \code{Albert},
#' \code{Arthur}, \code{B.J.}, \code{Brady}, \code{Cal}, \code{Charles},
#' \code{dl-Delino}, \code{dl-Scott}, \code{Doug}, \code{Harold}, \code{Heathcliff},
#' \code{Jeff}, \code{Jesse}, \code{Juan}, \code{Lenny}, \code{Mike}, \code{Rich},
#' \code{Ricky}, \code{Scott}, \code{Sidney}, \code{Will}, and \code{Willis}}
#' \item{last name}{a factor with levels \code{Amaral}, \code{Anderson},
#' \code{Baines}, \code{Belle}, \code{Bones}, \code{Bordick}, \code{Clark},
#' \code{Conine}, \code{Deshields}, \code{Erickson}, \code{Fetters}, \code{Garcia},
#' \code{Guzman}, \code{Johns}, \code{Johnson}, \code{Kamieniecki}, \code{Mussina},
#' \code{Orosco}, \code{Otanez}, \code{Ponson}, \code{Reboulet}, \code{Rhodes},
#' \code{Ripken Jr.}, \code{Slocumb}, \code{Surhoff},\code{Timlin}, and
#' \code{Webster}} 
#' \item{1999salary}{a numeric vector containing each player's salary in dollars} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripchart(Orioles$`1999salary`, method = "stack", pch = 19)
#' \dontrun{
#' ggplot2::ggplot(data = Orioles, aes(x = `1999salary`)) + 
#'   geom_dotplot(dotsize = 0.5) + 
#'     labs(x = "1999 Salary") +
#'       theme_bw()
#'       }
#' 
"Orioles"





#' Arterial blood pressure of 11 subjects before and after receiving oxytocin
#' 
#' Data for Exercise 7.86
#' 
#' 
#' @name Oxytocin
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 3 variables.
#' \describe{ 
#' \item{subject}{a numeric vector indicating each subject} 
#' \item{before}{mean arterial blood pressure of 11 subjects before receiving oxytocin} 
#' \item{after}{mean arterial blood pressure of 11 subjects after receiving oxytocin} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' diff = Oxytocin$after - Oxytocin$before
#' qqnorm(diff)
#' qqline(diff)
#' shapiro.test(diff)
#' t.test(Oxytocin$after, Oxytocin$before, paired = TRUE)
#' rm(diff)
#' 
"Oxytocin"





#' Education backgrounds of parents of entering freshmen at a state university
#' 
#' Data for Exercise 1.32
#' 
#' 
#' @name Parented
#' @docType data
#' @format A data frame/tibble with 200 observations on the following 2 variables.
#' \describe{ 
#' \item{education}{a factor with levels \code{4yr college
#' degree}, \code{Doctoral degree}, \code{Grad degree}, \code{H.S grad or less},
#' \code{Some college}, and \code{Some grad school}} 
#' \item{parent}{a factor with levels \code{mother} and \code{father}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~education + parent, data = Parented)
#' T1
#' barplot(t(T1), beside = TRUE, legend = TRUE, col = c("blue", "red"))
#' rm(T1)
#' \dontrun{
#' ggplot2::ggplot(data = Parented, aes(x = education, fill = parent)) + 
#'   geom_bar(position = "dodge") + 
#'     theme_bw() +
#'       theme(axis.text.x  = element_text(angle = 85, vjust = 0.5)) + 
#'         scale_fill_manual(values = c("pink", "blue")) + 
#'           labs(x = "", y = "") 
#'           }
#'           
"Parented"





#' Years of experience and number of tickets given by patrolpersons in New York
#' City
#' 
#' Data for Example 9.3
#' 
#' 
#' @name Patrol
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 3 variables.
#' \describe{ 
#' \item{tickets}{number of tickets written per week} 
#' \item{years}{patrolperson's years of experience} 
#' \item{log_tickets}{natural log of \code{tickets}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(tickets ~ years, data = Patrol)
#' summary(model)
#' confint(model, level = 0.98)
#' 
"Patrol"





#' Karl Pearson's data on heights of brothers and sisters
#' 
#' Data for Exercise 2.20
#' 
#' 
#' @name Pearson
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 3 variables.
#' \describe{ 
#' \item{family}{number indicating family of brother and sister pair}
#' \item{brother}{height of brother (in inches)} 
#' \item{sister}{height of sister (in inches)} 
#' }
#' 
#' @source Pearson, K. and Lee, A. (1902-3), On the Laws of Inheritance in Man, 
#' \emph{Biometrika, 2}, 357
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(brother ~ sister, data = Pearson)
#' cor(Pearson$brother, Pearson$sister)
#' 
"Pearson"





#' Length of long-distance phone calls for a small business firm
#' 
#' Data for Exercise 6.95
#' 
#' 
#' @name Phone
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{time}{duration of long distance phone call (in minutes)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Phone$time)
#' qqline(Phone$time)
#' shapiro.test(Phone$time)
#' BSDA::SIGN.test(Phone$time, md = 5, alternative = "greater")
#' 
"Phone"





#' Number of poisonings reported to 16 poison control centers
#' 
#' Data for Exercise 1.113
#' 
#' 
#' @name Poison
#' @docType data
#' @format A data frame/tibble with 226,261 observations on the following variable.
#' \describe{ 
#' \item{type}{a factor with levels \code{Alcohol},
#' \code{Cleaning agent}, \code{Cosmetics}, \code{Drugs}, \code{Insecticides}, and
#' \code{Plants}} 
#' }
#' 
#' @source Centers for Disease Control, Atlanta, Georgia.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~type, data = Poison)
#' T1
#' par(mar = c(5.1 + 2, 4.1, 4.1, 2.1))
#' barplot(sort(T1, decreasing = TRUE), las = 2, col = rainbow(6))
#' par(mar = c(5.1, 4.1, 4.1, 2.1))
#' rm(T1)
#' \dontrun{
#' ggplot2::ggplot(data = Poison, aes(x = type, fill = type)) + 
#'   geom_bar() + 
#'     theme_bw() + 
#'       theme(axis.text.x  = element_text(angle = 85, vjust = 0.5)) +
#'         guides(fill = FALSE)
#'         }
#' 
"Poison"





#' Political party and gender in a voting district
#' 
#' Data for Example 8.3
#' 
#' 
#' @name Politic
#' @docType data
#' @format A data frame/tibble with 250 observations on the following 2 variables.
#' \describe{ 
#' \item{party}{a factor with levels \code{republican}, \code{democrat}, and \code{other}} 
#' \item{gender}{a factor with levels \code{female} and \code{male}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~party + gender, data = Politic)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Politic"





#' Air pollution index for 15 randomly selected days for a major western city
#' 
#' Data for Exercise 5.59
#' 
#' 
#' @name Pollutio
#' @docType data
#' @format A data frame/tibble with 15 observations on the following variable.
#' \describe{ 
#' \item{inde}{air pollution index} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Pollutio$inde)
#' t.test(Pollutio$inde, conf.level = 0.98)$conf
#' 
"Pollutio"





#' Porosity measurements on 20 samples of Tensleep Sandstone, Pennsylvanian
#' from Bighorn Basin in Wyoming
#' 
#' Data for Exercise 5.86
#' 
#' 
#' @name Porosity
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{porosity}{porosity measurement (percent)} 
#' }
#' 
#' @source Davis, J. C. (1986), \emph{Statistics and Data Analysis in Geology}, 2nd edition,
#' pages 63-65.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Porosity$porosity)
#' fivenum(Porosity$porosity)
#' boxplot(Porosity$porosity)
#' 
"Porosity"





#' Percent poverty and crime rate for selected cities
#' 
#' Data for Exercise 9.11 and 9.17
#' 
#' 
#' @name Poverty
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{city}{a factor with levels \code{Atlanta},
#' \code{Buffalo}, \code{Cincinnati}, \code{Cleveland}, \code{Dayton, O},
#' \code{Detroit}, \code{Flint, Mich}, \code{Fresno, C}, \code{Gary, Ind},
#' \code{Hartford, C}, \code{Laredo}, \code{Macon, Ga}, \code{Miami},
#' \code{Milwaukee}, \code{New Orleans}, \code{Newark, NJ}, \code{Rochester,NY},
#' \code{Shreveport}, \code{St. Louis}, and \code{Waco, Tx}} 
#' \item{poverty}{percent of children living in poverty} 
#' \item{crime}{crime rate (per 1000 people)}
#' \item{population}{population of city} 
#' }
#' 
#' @source Children's Defense Fund and the Bureau of Justice Statistics
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(poverty ~ crime, data = Poverty)
#' model <- lm(poverty ~ crime, data = Poverty)
#' abline(model)
#' summary(model)
#' rm(model)
#' 
"Poverty"





#' Robbery rates versus percent low income in 8 precincts
#' 
#' Data for Exercise 2.2 and 2.38
#' 
#' 
#' @name Precinct
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{rate}{robbery rate (per 1000 people)} 
#' \item{income}{percent with low income} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(rate ~ income, data = Precinct)
#' model <- (lm(rate ~ income, data = Precinct))
#' abline(model)
#' rm(model)
#' 
"Precinct"





#' Racial prejudice measured on a sample of 25 high school students
#' 
#' Data for Exercise 5.10 and 5.22
#' 
#' 
#' @name Prejudic
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{prejud}{racial prejudice score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Prejudic$prejud)
#' BSDA::EDA(Prejudic$prejud)
#' 
"Prejudic"





#' Ages at inauguration and death of U.S. presidents
#' 
#' Data for Exercise 1.126
#' 
#' 
#' @name Presiden
#' @docType data
#' @format A data frame/tibble with 43 observations on the following 5 variables.
#' \describe{ 
#' \item{first_initial}{a factor with levels \code{A.}, \code{B.},
#' \code{C.}, \code{D.}, \code{F.}, \code{G.}, \code{G. W.}, \code{H.}, \code{J.},
#' \code{L.}, \code{M.}, \code{R.}, \code{T.}, \code{U.}, \code{W.}, and \code{Z.}}
#' \item{last_name}{a factor with levels \code{Adams}, \code{Arthur},
#' \code{Buchanan}, \code{Bush}, \code{Carter}, \code{Cleveland}, \code{Clinton},
#' \code{Coolidge}, \code{Eisenhower}, \code{Fillmore}, \code{Ford},
#' \code{Garfield}, \code{Grant}, \code{Harding}, \code{Harrison}, \code{Hayes},
#' \code{Hoover}, \code{Jackson}, \code{Jefferson}, \code{Johnson}, \code{Kennedy},
#' \code{Lincoln}, \code{Madison}, \code{McKinley}, \code{Monroe}, \code{Nixon},
#' \code{Pierce}, \code{Polk}, \code{Reagan}, \code{Roosevelt}, \code{Taft},
#' \code{Taylor}, \code{Truman}, \code{Tyler}, \code{VanBuren}, \code{Washington}, and
#' \code{Wilson}} 
#' \item{birth_state}{a factor with levels \code{ARK},
#' \code{CAL}, \code{CONN}, \code{GA}, \code{IA}, \code{ILL}, \code{KY}, \code{MASS},
#' \code{MO}, \code{NC}, \code{NEB}, \code{NH}, \code{NJ}, \code{NY}, \code{OH},
#' \code{PA}, \code{SC}, \code{TEX}, \code{VA}, and \code{VT}}
#' \item{inaugural_age}{President's age at inauguration} 
#' \item{death_age}{President's age at death} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' pie(xtabs(~birth_state, data = Presiden))
#' stem(Presiden$inaugural_age)
#' stem(Presiden$death_age)
#' par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#' stripchart(x=list(Presiden$inaugural_age, Presiden$death_age), 
#'            method = "stack",
#'                       col = c("green","brown"), pch = 19, las = 1)
#'                       par(mar = c(5.1, 4.1, 4.1, 2.1)) 
#' 
"Presiden"





#' Degree of confidence in the press versus education level for 20 randomly
#' selected persons
#' 
#' Data for Exercise 9.55
#' 
#' 
#' @name Press
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{education_yrs}{years of education} 
#' \item{confidence}{degree of confidence in the press (the higher the score, the more confidence)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(confidence ~ education_yrs, data = Press)
#' model <- lm(confidence ~ education_yrs, data = Press)
#' abline(model, col = "purple")
#' summary(model)
#' rm(model)
#' 
"Press"





#' Klopfer's prognostic rating scale for subjects receiving behavior
#' modification therapy
#' 
#' Data for Exercise 6.61
#' 
#' 
#' @name Prognost
#' @docType data
#' @format A data frame/tibble with 15 observations on the following variable.
#' \describe{ 
#' \item{kprs_score}{Kloper's Prognostic Rating Scale score} 
#' }
#' 
#' @source Newmark, C., et al. (1973), Predictive Validity of the Rorschach Prognostic Rating Scale
#' with Behavior Modification Techniques, \emph{Journal of Clinical Psychology, 29}, 246-248
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Prognost$kprs_score)
#' t.test(Prognost$kprs_score, mu = 9)
#' 
"Prognost"





#' Effects of four different methods of programmed learning for statistics
#' students
#' 
#' Data for Exercise 10.17
#' 
#' 
#' @name Program
#' @docType data
#' @format A data frame/tibble with 44 observations on the following 2 variables.
#' \describe{ 
#' \item{method}{a character variable with values \code{method1}, \code{method2}, 
#' \code{method3}, and \code{method4}} 
#' \item{score}{standardized test score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ method, col = c("red", "blue", "green", "yellow"), data = Program)
#' anova(lm(score ~ method, data = Program))
#' TukeyHSD(aov(score ~ method, data = Program))
#' par(mar = c(5.1, 4.1 + 4, 4.1, 2.1))
#' plot(TukeyHSD(aov(score ~ method, data = Program)), las = 1)
#' par(mar = c(5.1, 4.1, 4.1, 2.1))
#' 
"Program"





#' PSAT scores versus SAT scores
#' 
#' Data for Exercise 2.50
#' 
#' 
#' @name Psat
#' @docType data
#' @format A data frame/tibble with 7 observations on the following 2 variables.
#' \describe{
#' \item{psat}{PSAT score} 
#' \item{sat}{SAT score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(sat ~ psat, data = Psat)
#' par(mfrow = c(1, 2))
#' plot(Psat$psat, resid(model))
#' plot(model, which = 1)
#' rm(model)
#' par(mfrow = c(1, 1))
#' 
"Psat"





#' Correct responses for 24 students in a psychology experiment
#' 
#' Data for Exercise 1.42
#' 
#' 
#' @name Psych
#' @docType data
#' @format A data frame/tibble with 23 observations on the following variable.
#' \describe{ 
#' \item{score}{number of correct repsonses in a psychology experiment} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Psych$score)
#' BSDA::EDA(Psych$score)
#' 
"Psych"





#' Weekly incomes of a random sample of 50 Puerto Rican families in Miami
#' 
#' Data for Exercise 5.22 and 5.65
#' 
#' 
#' @name Puerto
#' @docType data
#' @format A data frame/tibble with 50 observations on the following variable.
#' \describe{ 
#' \item{income}{weekly family income (in dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Puerto$income)
#' boxplot(Puerto$income)
#' t.test(Puerto$income,conf.level = .90)$conf
#' 
"Puerto"





#' Plasma LDL levels in two groups of quail
#' 
#' Data for Exercise 1.53, 1.77, 1.88, 5.66, and 7.50
#' 
#' 
#' @name Quail
#' @docType data
#' @format A data frame/tibble with 40 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{a character variable with values \code{placebo} and \code{treatment}}
#' \item{level}{low-density lipoprotein (LDL) cholestrol level} 
#' }
#' 
#' @source J. McKean, and T. Vidmar (1994), "A Comparison of Two Rank-Based Methods for the
#' Analysis of Linear Models," \emph{The American Statistician, 48}, 220-229.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(level ~ group, data = Quail, horizontal = TRUE, xlab = "LDL Level",
#' col = c("yellow", "lightblue"))
#' 
"Quail"





#' Quality control test scores on two manufacturing processes
#' 
#' Data for Exercise 7.81
#' 
#' 
#' @name Quality
#' @docType data
#' @format A data frame/tibble with 15 observations on the following 2 variables.
#' \describe{ 
#' \item{process}{a character variable with values \code{Process1} and \code{Process2}}
#' \item{score}{results of a quality control test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ process, data = Quality)
#' t.test(score ~ process, data = Quality)
#' 
"Quality"





#' Rainfall in an area of west central Kansas and four surrounding counties
#' 
#' Data for Exercise 9.8
#' 
#' 
#' @name Rainks
#' @docType data
#' @format A data frame/tibble with 35 observations on the following 5 variables.
#' \describe{ 
#' \item{rain}{rainfall (in inches)} 
#' \item{x1}{rainfall (in inches)} 
#' \item{x2}{rainfall (in inches)} 
#' \item{x3}{rainfall (in inches)} 
#' \item{x4}{rainfall (in inches)} 
#' }
#' 
#' @source R. Picard, K. Berk (1990), Data Splitting, \emph{The American Statistician, 44}, (2),
#' 140-147.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' cor(Rainks)
#' model <- lm(rain ~ x2, data = Rainks)
#' summary(model)
#' 
"Rainks"





#' Research and development expenditures and sales of a large company
#' 
#' Data for Exercise 9.36 and Example 9.8
#' 
#' 
#' @name Randd
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{rd}{research and development expenditures (in million dollars)} 
#' \item{sales}{sales (in million dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(sales ~ rd, data = Randd)
#' model <- lm(sales ~ rd, data = Randd)
#' abline(model)
#' summary(model)
#' plot(model, which = 1)
#' rm(model)
#' 
"Randd"



#' Survival times of 20 rats exposed to high levels of radiation
#' 
#' Data for Exercise 1.52, 1.76, 5.62, and 6.44
#' 
#' 
#' @name Rat
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{survival_time}{survival time in weeks for rats exposed to a high level of radiation} 
#' }
#' 
#' @source J. Lawless, \emph{Statistical Models and Methods for Lifetime Data} (New York: Wiley, 1982)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Rat$survival_time)
#' qqnorm(Rat$survival_time)
#' qqline(Rat$survival_time)
#' summary(Rat$survival_time)
#' t.test(Rat$survival_time)
#' t.test(Rat$survival_time, mu = 100, alternative = "greater")
#' 
"Rat"





#' Grade point averages versus teacher's ratings
#' 
#' Data for Example 2.6
#' 
#' 
#' @name Ratings
#' @docType data
#' @format A data frame/tibble with 250 observations on the following 2 variables.
#' \describe{
#' \item{rating}{character variable with students' ratings of instructor (A-F)}
#' \item{gpa}{students' grade point average}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(gpa ~ rating, data = Ratings, xlab = "Student rating of instructor", 
#' ylab = "Student GPA")
#' \dontrun{
#' ggplot2::ggplot(data = Ratings, aes(x = rating, y = gpa, fill = rating)) +
#'   geom_boxplot() + 
#'     theme_bw() + 
#'       theme(legend.position = "none") + 
#'         labs(x = "Student rating of instructor", y = "Student GPA")
#'         }
#' 
"Ratings"






#' Threshold reaction time for persons subjected to emotional stress
#' 
#' Data for Example 6.11
#' 
#' 
#' @name Reaction
#' @docType data
#' @format A data frame/tibble with 12 observations on the following variable.
#' \describe{ 
#' \item{time}{threshold reaction time (in seconds) for persons subjected to emotional stress} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Reaction$time)
#' BSDA::SIGN.test(Reaction$time, md = 15, alternative = "less")
#' 
"Reaction"





#' Standardized reading scores for 30 fifth graders
#' 
#' Data for Exercise 1.72 and 2.10
#' 
#' 
#' @name Reading
#' @docType data
#' @format A data frame/tibble with 30 observations on the following 4 variables.
#' \describe{ 
#' \item{score}{standardized reading test score} 
#' \item{sorted}{sorted values of \code{score}} 
#' \item{trimmed}{trimmed values of \code{sorted}}
#' \item{winsoriz}{winsorized values of \code{score}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Reading$score, main = "Exercise 1.72", 
#' col = "lightgreen", xlab = "Standardized reading score")
#' summary(Reading$score)
#' sd(Reading$score)
#' 
"Reading"





#' Reading scores versus IQ scores
#' 
#' Data for Exercises 2.10 and 2.53
#' 
#' 
#' @name Readiq
#' @docType data
#' @format A data frame/tibble with 14 observations on the following 2 variables.
#' \describe{ 
#' \item{reading}{reading achievement score} 
#' \item{iq}{IQ score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(reading ~ iq, data = Readiq)
#' model <- lm(reading ~ iq, data = Readiq)
#' abline(model)
#' predict(model, newdata = data.frame(iq = c(100, 120)))
#' residuals(model)[c(6, 7)]
#' rm(model)
#' 
"Readiq"





#' Opinion on referendum by view on freedom of the press
#' 
#' Data for Exercise 8.20
#' 
#' 
#' @name Referend
#' @docType data
#' @format A data frame with 237 observations on the following 2 variables.
#' \describe{ 
#' \item{choice}{a factor with levels \code{A}, \code{B}, and \code{C}} 
#' \item{response}{a factor with levels \code{for}, \code{against}, and \code{undecided}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~choice + response, data = Referend)
#' T1
#' chisq.test(T1)
#' chisq.test(T1)$expected
#' 
"Referend"





#' Pollution index taken in three regions of the country
#' 
#' Data for Exercise 10.26
#' 
#' 
#' @name Region
#' @docType data
#' @format A data frame/tibble with 48 observations on the following 3 variables.
#' \describe{
#'  \item{pollution}{pollution index} 
#'  \item{region}{region of a county (\code{west}, \code{central}, and \code{east})}
#'  \item{ranks}{ranked values of \code{pollution}} 
#'  }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(pollution ~ region, data = Region)
#' anova(lm(pollution ~ region, data = Region))
#' 
"Region"





#' Maintenance cost versus age of cash registers in a department store
#' 
#' Data for Exercise 2.3, 2.39, and 2.54
#' 
#' 
#' @name Register
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 2 variables.
#' \describe{ 
#' \item{age}{age of cash register (in years)} 
#' \item{cost}{maintenance cost of cash register (in dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(cost ~ age, data = Register)
#' model <- lm(cost ~ age, data = Register)
#' abline(model)
#' predict(model, newdata = data.frame(age = c(5, 10)))
#' plot(model, which = 1)
#' rm(model)
#' 
"Register"





#' Rehabilitative potential of 20 prison inmates as judged by two psychiatrists
#' 
#' Data for Exercise 7.61
#' 
#' 
#' @name Rehab
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{inmate}{inmate identification number} 
#' \item{psych1}{rating from first psychiatrist on the inmates rehabilative potential} 
#' \item{psych2}{rating from second psychiatrist on the inmates rehabilative potential} 
#' \item{differ}{\code{psych1} - \code{psych2}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Rehab$differ)
#' qqnorm(Rehab$differ)
#' qqline(Rehab$differ)
#' t.test(Rehab$differ)
#' # Or
#' t.test(Rehab$psych1, Rehab$psych2, paired = TRUE)
#' 
"Rehab"





#' Math placement test score for 35 freshmen females and 42 freshmen males
#' 
#' Data for Exercise 7.43
#' 
#' 
#' @name Remedial
#' @docType data
#' @format A data frame/tibble with 84 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{a character variable with values \code{female} and \code{male}} 
#' \item{score}{math placement score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ gender, data = Remedial, 
#' col = c("purple", "blue"))
#' t.test(score ~ gender, data = Remedial, conf.level = 0.98)
#' t.test(score ~ gender, data = Remedial, conf.level = 0.98)$conf
#' wilcox.test(score ~ gender, data = Remedial, 
#'             conf.int = TRUE, conf.level = 0.98)
#' 
"Remedial"





#' Weekly rentals for 45 apartments
#' 
#' Data for Exercise 1.122
#' 
#' 
#' @name Rentals
#' @docType data
#' @format A data frame/tibble with 45 observations on the following variable.
#' \describe{ 
#' \item{rent}{weekly apartment rental price (in dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Rentals$rent)
#' sum(Rentals$rent < mean(Rentals$rent) - 3*sd(Rentals$rent) | 
#'    Rentals$rent > mean(Rentals$rent) + 3*sd(Rentals$rent))
#' 
"Rentals"





#' Recorded times for repairing 22 automobiles involved in wrecks
#' 
#' Data for Exercise 5.77
#' 
#' 
#' @name Repair
#' @docType data
#' @format A data frame/tibble with 22 observations on the following variable.
#' \describe{ 
#' \item{time}{time to repair a wrecked in car (in hours)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Repair$time)
#' BSDA::SIGN.test(Repair$time, conf.level = 0.98)
#' 
"Repair"





#' Length of employment versus gross sales for 10 employees of a large retail
#' store
#' 
#' Data for Exercise 9.59
#' 
#' 
#' @name Retail
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{months}{length of employment (in months)}
#' \item{sales}{employee gross sales (in dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(sales ~ months, data = Retail)
#' model <- lm(sales ~ months, data = Retail)
#' abline(model)
#' summary(model)
#' 
"Retail"





#' Oceanography data obtained at site 1 by scientist aboard the ship Ron Brown
#' 
#' Data for Exercise 2.9
#' 
#' 
#' @name Ronbrown1
#' @docType data
#' @format A data frame/tibble with 75 observations on the following 2 variables.
#' \describe{ 
#' \item{depth}{ocen depth (in meters)} 
#' \item{temperature}{ocean temperature (in Celsius)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(temperature ~ depth, data = Ronbrown1, ylab = "Temperature")
#' 
"Ronbrown1"





#' Oceanography data obtained at site 2 by scientist aboard the ship Ron Brown
#' 
#' Data for Exercise 2.56 and Example 2.4
#' 
#' 
#' @name Ronbrown2
#' @docType data
#' @format A data frame/tibble with 150 observations on the following 3 variables.
#' \describe{ 
#' \item{depth}{ocean depth (in meters)}
#' \item{temperature}{ocean temperature (in Celcius)} 
#' \item{salinity}{ocean salinity level} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(salinity ~ depth, data = Ronbrown2)
#' model <- lm(salinity ~ depth, data = Ronbrown2)
#' summary(model)
#' plot(model, which = 1)
#' rm(model)
#' 
"Ronbrown2"





#' Social adjustment scores for a rural group and a city group of children
#' 
#' Data for Example 7.16
#' 
#' 
#' @name Rural
#' @docType data
#' @format A data frame/tibble with 33 observations on the following 2 variables.
#' \describe{ 
#' \item{score}{child's social adjustment score} 
#' \item{area}{character variable with values \code{city} and \code{rural}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ area, data = Rural)
#' wilcox.test(score ~ area, data = Rural)
#' \dontrun{
#' Rural <- dplyr::mutate(Rural, r = rank(score))
#' Rural
#' t.test(r ~ area, data = Rural)
#' }
#' 
"Rural"





#' Starting salaries for 25 new PhD psychologist
#' 
#' Data for Exercise 3.66
#' 
#' 
#' @name Salary
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{salary}{starting salary for Ph.D. psycholgists} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Salary$salary, pch = 19, col = "purple")
#' qqline(Salary$salary, col = "blue")
#' 
"Salary"





#' Surface-water salinity measurements from Whitewater Bay, Florida
#' 
#' Data for Exercise 5.27 and 5.64
#' 
#' 
#' @name Salinity
#' @docType data
#' @format A data frame/tibble with 48 observations on the following variable.
#' \describe{ 
#' \item{salinity}{surface-water salinity value} 
#' }
#' 
#' @source J. Davis, \emph{Statistics and Data Analysis in Geology}, 2nd ed. (New York: John Wiley, 1986)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Salinity$salinity)
#' qqnorm(Salinity$salinity, pch = 19, col = "purple")
#' qqline(Salinity$salinity, col = "blue")
#' t.test(Salinity$salinity, conf.level = 0.99)
#' t.test(Salinity$salinity, conf.level = 0.99)$conf
#' 
"Salinity"





#' SAT scores, percent taking exam and state funding per student by state for
#' 1994, 1995 and 1999
#' 
#' Data for Statistical Insight Chapter 9
#' 
#' 
#' @name Sat
#' @docType data
#' @format A data frame/tibble with 102 observations on the following 7 variables.
#' \describe{ 
#' \item{state}{U.S. state}
#' \item{verbal}{verbal SAT score} 
#' \item{math}{math SAT score} 
#' \item{total}{combined verbal and math SAT score} 
#' \item{percent}{percent of high school seniors taking the SAT} 
#' \item{expend}{state expenditure per student (in dollars)} 
#' \item{year}{year} 
#' }
#' 
#' @source \emph{The 2000 World Almanac and Book of Facts}, Funk and Wagnalls Corporation, New Jersey.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' Sat94 <- Sat[Sat$year == 1994, ]
#' Sat94
#' Sat99 <- subset(Sat, year == 1999)
#' Sat99
#' stem(Sat99$total)
#' plot(total ~ percent, data = Sat99)
#' model <- lm(total ~ percent, data = Sat99)
#' abline(model, col = "blue")
#' summary(model)
#' rm(model)
#' 
"Sat"





#' Problem asset ration for savings and loan companies in California, New York,
#' and Texas
#' 
#' Data for Exercise 10.34 and 10.49
#' 
#' 
#' @name Saving
#' @docType data
#' @format A data frame/tibble with 65 observations on the following 2 variables.
#' \describe{ 
#' \item{par}{problem-asset-ratio for Savings & Loans that were listed as being financially troubled in 1992} 
#' \item{state}{U.S. state}
#'  }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(par ~ state, data = Saving)
#' boxplot(par ~ state, data = Saving, log = "y")
#' model <- aov(par ~ state, data = Saving)
#' summary(model)
#' plot(TukeyHSD(model))
#' kruskal.test(par ~ factor(state), data = Saving)
#' 
"Saving"





#' Readings obtained from a 100 pound weight placed on four brands of bathroom
#' scales
#' 
#' Data for Exercise 1.89
#' 
#' 
#' @name Scales
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{brand}{variable indicating brand of bathroom scale (\code{A}, \code{B}, \code{C}, or \code{D})} 
#' \item{reading}{recorded value (in pounds) of a 100 pound weight} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(reading ~ brand, data = Scales, col = rainbow(4), 
#' ylab = "Weight (lbs)")
#' \dontrun{
#' ggplot2::ggplot(data = Scales, aes(x = brand, y = reading, fill = brand)) + 
#'   geom_boxplot() + 
#'     labs(y = "weight (lbs)") +
#'      theme_bw() + 
#'        theme(legend.position = "none") 
#'        }
#' 
"Scales"





#' Exam scores for 17 patients to assess the learning ability of schizophrenics
#' after taking a specified does of a tranquilizer
#' 
#' Data for Exercise 6.99
#' 
#' 
#' @name Schizop2
#' @docType data
#' @format A data frame/tibble with 17 observations on the following variable.
#' \describe{ 
#' \item{score}{schizophrenics score on a second standardized exam} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Schizop2$score, xlab = "score on standardized test after a tranquilizer", 
#' main = "Exercise 6.99", breaks = 10, col = "orange")
#' BSDA::EDA(Schizop2$score)
#' BSDA::SIGN.test(Schizop2$score, md = 22, alternative = "greater")
#' 
"Schizop2"





#' Standardized exam scores for 13 patients to investigate the learning ability
#' of schizophrenics after a specified dose of a tranquilizer
#' 
#' Data for Example 6.10
#' 
#' 
#' @name Schizoph
#' @docType data
#' @format A data frame/tibble with 13 observations on the following variable.
#' \describe{ 
#' \item{score}{schizophrenics score on a standardized exam one hour after recieving a specified dose of a tranqilizer.} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Schizoph$score, xlab = "score on standardized test", 
#' main = "Example 6.10", breaks = 10, col = "orange")
#' BSDA::EDA(Schizoph$score)
#' t.test(Schizoph$score, mu = 20)
#' 
"Schizoph"





#' Injury level versus seatbelt usage
#' 
#' Data for Exercise 8.24
#' 
#' 
#' @name Seatbelt
#' @docType data
#' @format A data frame/tibble with 86,759 observations on the following 2 variables.
#' \describe{ 
#' \item{seatbelt}{a factor with levels \code{No}
#' \code{Yes}} 
#' \item{injuries}{a factor with levels \code{None}, \code{Minimal}, \code{Minor}, or \code{Major} indicating the extent of the drivers injuries.a numeric vector}
#' }
#' 
#' @source Jobson, J. (1982), \emph{Applied Multivariate Data Analysis}, Springer-Verlag, New York, p. 18
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~seatbelt + injuries, data = Seatbelt)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Seatbelt"





#' Self-confidence scores for 9 women before and after instructions on
#' self-defense
#' 
#' Data for Example 7.19
#' 
#' 
#' @name Selfdefe
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{woman}{number identifying the woman} 
#' \item{before}{before the course self-confidence score} 
#' \item{after}{after the course self-confidence score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' Selfdefe$differ <- Selfdefe$after - Selfdefe$before
#' Selfdefe
#' t.test(Selfdefe$differ, alternative = "greater")
#' t.test(Selfdefe$after, Selfdefe$before, 
#'       paired = TRUE, alternative = "greater")
#' 
"Selfdefe"





#' Reaction times of 30 senior citizens applying for drivers license renewals
#' 
#' Data for Exercise 1.83 and 3.67
#' 
#' 
#' @name Senior
#' @docType data
#' @format A data frame/tibble with 31 observations on the following variable.
#' \describe{ 
#' \item{reaction}{reaction time for senior citizens applying for a driver's license renewal} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Senior$reaction)
#' fivenum(Senior$reaction)
#' boxplot(Senior$reaction, main = "Problem 1.83, part d",
#'         horizontal = TRUE, col = "purple")
#' 
"Senior"





#' Sentences of 41 prisoners convicted of a homicide offense
#' 
#' Data for Exercise 1.123
#' 
#' 
#' @name Sentence
#' @docType data
#' @format A data frame/tibble with 41 observations on the following variable.
#' \describe{ 
#' \item{months}{sentence length (in months) for prisoners convicted of homocide} 
#' }
#' 
#' @source U.S. Department of Justice, Bureau of Justice Statistics, \emph{Prison Sentences
#' and Time Served for Violence}, NCJ-153858, April 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Sentence$months)
#' ll <- mean(Sentence$months)-2*sd(Sentence$months)
#' ul <- mean(Sentence$months)+2*sd(Sentence$months)
#' limits <- c(ll, ul)
#' limits
#' rm(ul, ll, limits)
#' 
"Sentence"





#' Effects of a drug and electroshock therapy on the ability to solve simple
#' tasks
#' 
#' Data for Exercises 10.11 and 10.12
#' 
#' 
#' @name Shkdrug
#' @docType data
#' @format A data frame/tibble with 64 observations on the following 2 variables.
#' \describe{ 
#' \item{treatment}{type of treament \code{Drug/NoS}, \code{Drug/Shk}, \code{NoDg/NoS}, or \code{NoDrug/S}}
#' \item{response}{number of tasks completed in a 10-minute period}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(response ~ treatment, data = Shkdrug)
#' model <- lm(response ~ treatment, data = Shkdrug)
#' anova(model)
#' rm(model)
#' 
"Shkdrug"





#' Effect of experimental shock on time to complete difficult task
#' 
#' Data for Exercise 10.50
#' 
#' 
#' @name Shock
#' @docType data
#' @format A data frame/tibble with 27 observations on the following 2 variables.
#' \describe{ 
#' \item{group}{grouping variable with values of \code{Group1} (no shock), \code{Group2} (medium shock), 
#' and \code{Group3} (severe shock)} 
#' \item{attempts}{number of attempts to complete a task}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(attempts ~ group, data = Shock)
#' model <- lm(attempts ~ group, data = Shock)
#' anova(model)
#' rm(model)
#' 
#' 
"Shock"





#' Sales receipts versus shoplifting losses for a department store
#' 
#' Data for Exercise 9.58
#' 
#' 
#' @name Shoplift
#' @docType data
#' @format A data frame/tibble with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{sales}{sales (in 1000 dollars)} 
#' \item{loss}{loss (in 100 dollars)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(loss ~ sales, data = Shoplift)
#' model <- lm(loss ~ sales, data = Shoplift)
#' summary(model)
#' rm(model)
#' 
"Shoplift"





#' James Short's measurements of the parallax of the sun
#' 
#' Data for Exercise 6.65
#' 
#' 
#' @name Short
#' @docType data
#' @format A data frame/tibble with 158 observations on the following 10 variables.
#' \describe{
#' \item{sample}{sample number} 
#' \item{parallax}{parallax measurements (seconds of a degree)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Short$parallax, main = "Problem 6.65", 
#' xlab = "", col = "orange")
#' BSDA::SIGN.test(Short$parallax, md = 8.798)
#' t.test(Short$parallax, mu = 8.798)
#' 
"Short"





#' Number of people riding shuttle versus number of automobiles in the downtown
#' area
#' 
#' Data for Exercise 9.20
#' 
#' 
#' @name Shuttle
#' @docType data
#' @format A data frame/tibble with 15 observations on the following 2 variables.
#' \describe{ 
#' \item{users}{number of shuttle riders} 
#' \item{autos}{number of automobiles in the downtown area} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(autos ~ users, data = Shuttle)
#' model <- lm(autos ~ users, data = Shuttle)
#' summary(model)
#' rm(model)
#' 
"Shuttle"





#' Grade point averages of men and women participating in various sports-an
#' illustration of Simpson's paradox
#' 
#' Data for Example 1.18
#' 
#' 
#' @name Simpson
#' @docType data
#' @format A data frame/tibble with 100 observations on the following 3 variables.
#' \describe{ 
#' \item{gpa}{grade point average} 
#' \item{sport}{sport played (basketball, soccer, or track)} 
#' \item{gender}{athlete sex (male, female)}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(gpa ~ gender, data = Simpson)
#' boxplot(gpa ~ sport, data = Simpson)
#' \dontrun{
#' ggplot2::ggplot(data = Simpson, aes(x = gender, y = gpa, 
#'                                     fill = gender)) +
#'                                       geom_boxplot() + 
#'                                         facet_grid(.~sport) + theme_bw()
#'                                         }
#' 
"Simpson"





#' Maximum number of situps by participants in an exercise class
#' 
#' Data for Exercise 1.47
#' 
#' 
#' @name Situp
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{number}{maximum number of situps completed in an exercise class after 1 month in the program} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Situp$number)
#' hist(Situp$number, breaks = seq(0, 70, 10), right = FALSE)
#' hist(Situp$number, breaks = seq(0, 70, 10), right = FALSE, 
#'      freq = FALSE, col = "pink", main = "Problem 1.47", 
#'           xlab = "Maximum number of situps")
#'           lines(density(Situp$number), col = "red")
#' 
"Situp"





#' Illustrates the Wilcoxon Rank Sum test
#' 
#' Data for Exercise 7.65
#' 
#' 
#' @name Skewed
#' @docType data
#' @format A data frame/tibble with 21 observations on the following 2 variables.
#' \describe{ 
#' \item{C1}{values from a sample of size 16 from a particular population} 
#' \item{C2}{values from a sample of size 14 from a particular population} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Skewed$C1, Skewed$C2, col = c("pink", "lightblue"))
#' wilcox.test(Skewed$C1, Skewed$C2)
#' 
"Skewed"





#' Survival times of closely and poorly matched skin grafts on burn patients
#' 
#' Data for Exercise 5.20
#' 
#' 
#' @name Skin
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 4 variables.
#' \describe{ 
#' \item{patient}{patient identification number}
#' \item{close}{graft survival time in days for a closely matched skin graft on the same burn patient} 
#' \item{poor}{graft survival time in days for a poorly matched skin graft on the same burn patient} 
#' \item{differ}{difference between close and poor (in days)}
#' }
#' 
#' @source R. F. Woolon and P. A. Lachenbruch, "Rank Tests for Censored Matched Pairs,"
#' \emph{Biometrika}, 67(1980), 597-606.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Skin$differ)
#' boxplot(Skin$differ)
#' summary(Skin$differ)
#' 
"Skin"





#' Sodium-lithium countertransport activity on 190 individuals from six large
#' English kindred
#' 
#' Data for Exercise 5.116
#' 
#' 
#' @name Slc
#' @docType data
#' @format A data frame/tibble with 190 observations on the following variable.
#' \describe{ 
#' \item{slc}{Red blood cell sodium-lithium countertransport} 
#' }
#' 
#' @source Roeder, K., (1994), "A Graphical Technique for Determining the Number of Components
#' in a Mixture of Normals," \emph{Journal of the American Statistical Association, 89}, 497-495.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Slc$slc)
#' hist(Slc$slc, freq = FALSE, xlab = "sodium lithium countertransport",
#'      main = "", col = "lightblue")
#'      lines(density(Slc$slc), col = "purple")
#' 
"Slc"





#' Water pH levels of 75 water samples taken in the Great Smoky Mountains
#' 
#' Data for Exercises 6.40, 6.59, 7.10, and 7.35
#' 
#' 
#' @name Smokyph
#' @docType data
#' @format A data frame/tibble with 75 observations on the following 3 variables.
#' \describe{ 
#' \item{waterph}{water sample pH level} 
#' \item{code}{charater variable with values \code{low} (elevation below 0.6 miles), and \code{high} (elevation above 0.6 miles)} 
#' \item{elev}{elevation in miles} 
#' }
#' 
#' @source Schmoyer, R. L. (1994), Permutation Tests for Correlation in Regression Errors,
#' \emph{Journal of the American Statistical Association, 89}, 1507-1516.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(Smokyph$waterph)
#' tapply(Smokyph$waterph, Smokyph$code, mean)
#' stripchart(waterph ~ code, data = Smokyph, method = "stack",
#'            pch = 19, col = c("red", "blue"))
#'            t.test(Smokyph$waterph, mu = 7)
#'            BSDA::SIGN.test(Smokyph$waterph, md = 7)
#'            t.test(waterph ~ code, data = Smokyph, alternative = "less")
#'            t.test(waterph ~ code, data = Smokyph, conf.level = 0.90)
#'           \dontrun{
#'            ggplot2::ggplot(data = Smokyph, aes(x = waterph, fill = code)) + 
#'              geom_dotplot() + 
#'                facet_grid(code ~ .) + 
#'                  guides(fill = FALSE)
#'                  }
#' 
"Smokyph"





#' Snoring versus heart disease
#' 
#' Data for Exercise 8.21
#' 
#' 
#' @name Snore
#' @docType data
#' @format A data frame/tibble with 2,484 observations on the following 2 variables.
#' \describe{ 
#' \item{snore}{factor with levels \code{nonsnorer}, \code{ocassional snorer}, \code{nearly every night}, and \code{snores every night}}
#' \item{heartdisease}{factor indicating whether the indiviudal has heart disease (\code{no} or \code{yes})} 
#' }
#' 
#' @source Norton, P. and Dunn, E. (1985), Snoring as a Risk Factor for Disease, \emph{British Medical Journal, 291},
#' 630-632.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~ heartdisease + snore, data = Snore)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Snore"





#' Concentration of microparticles in snowfields of Greenland and Antarctica
#' 
#' Data for Exercise 7.87
#' 
#' 
#' @name Snow
#' @docType data
#' @format A data frame/tibble with 34 observations on the following 2 variables.
#' \describe{ 
#' \item{concent}{concentration of microparticles from melted snow (in parts per billion)}
#' \item{site}{location of snow sample (\code{Antarctica} or \code{Greenland})} 
#' }
#' 
#' @source Davis, J., \emph{Statistics and Data Analysis in Geology}, John Wiley, New York.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(concent ~ site, data = Snow, col = c("lightblue", "lightgreen"))
#' 
"Snow"





#' Weights of 25 soccer players
#' 
#' Data for Exercise 1.46
#' 
#' 
#' @name Soccer
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{weight}{soccer players weight (in pounds)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Soccer$weight, scale = 2)
#' hist(Soccer$weight, breaks = seq(110, 210, 10), col = "orange",
#'      main = "Problem 1.46 \n Weights of Soccer Players", 
#'           xlab = "weight (lbs)", right = FALSE)
#' 
"Soccer"





#' Median income level for 25 social workers from North Carolina
#' 
#' Data for Exercise 6.63
#' 
#' 
#' @name Social
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{income}{annual income (in dollars) of North Carolina social workers with less than five years experience.} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::SIGN.test(Social$income, md = 27500, alternative = "less")
#' 
"Social"





#' Grade point averages, SAT scores and final grade in college algebra for 20
#' sophomores
#' 
#' Data for Exercise 2.42
#' 
#' 
#' @name Sophomor
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{student}{student's identification number} 
#' \item{gpa}{student's grade point average} 
#' \item{sat}{student's SAT math score} 
#' \item{exam}{student's final exam grade in college algebra } 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' cor(Sophomor)
#' plot(exam ~ gpa, data = Sophomor)
#' \dontrun{
#' ggplot2::ggplot(data = Sophomor, aes(x = gpa, y = exam)) + 
#'   geom_point()
#'   ggplot2::ggplot(data = Sophomor, aes(x = sat, y = exam)) + 
#'     geom_point()
#'     }
#' 
"Sophomor"





#' Murder rates for 30 cities in the South
#' 
#' Data for Exercise 1.84
#' 
#' 
#' @name South
#' @docType data
#' @format A data frame/tibble with 31 observations on the following variable.
#' \describe{ 
#' \item{rate}{murder rate per 100,000 people} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(South$rate, col = "gray", ylab = "Murder rate per 100,000 people")
#' 
"South"





#' Speed reading scores before and after a course on speed reading
#' 
#' Data for Exercise 7.58
#' 
#' 
#' @name Speed
#' @docType data
#' @format A data frame/tibble with 15 observations on the following 4 variables.
#' \describe{ 
#' \item{before}{reading comprehension score before taking a speed-reading course} 
#' \item{after}{reading comprehension score after taking a speed-reading course} 
#' \item{differ}{after - before (comprehension reading scores)}
#' \item{signranks}{signed ranked differences} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(Speed$differ, alternative = "greater")
#' t.test(Speed$signranks, alternative = "greater")
#' wilcox.test(Speed$after, Speed$before, paired = TRUE, alternative = "greater")
#' 
"Speed"





#' Standardized spelling test scores for two fourth grade classes
#' 
#' Data for Exercise 7.82
#' 
#' 
#' @name Spellers
#' @docType data
#' @format A data frame/tibble with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{teacher}{character variable with values \code{Fourth} and \code{Colleague}} 
#' \item{score}{score on a standardized spelling test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ teacher, data = Spellers)
#' t.test(score ~ teacher, data = Spellers)
#' 
"Spellers"





#' Spelling scores for 9 eighth graders before and after a 2-week course of
#' instruction
#' 
#' Data for Exercise 7.56
#' 
#' 
#' @name Spelling
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{before}{spelling score before a 2-week course of instruction} 
#' \item{after}{spelling score after a 2-week course of instruction} 
#' \item{differ}{after - before (spelling score)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Spelling$differ)
#' qqline(Spelling$differ)
#' shapiro.test(Spelling$differ)
#' t.test(Spelling$before, Spelling$after, paired = TRUE)
#' t.test(Spelling$differ)
#' 
"Spelling"





#' Favorite sport by gender
#' 
#' Data for Exercise 8.32
#' 
#' 
#' @name Sports
#' @docType data
#' @format A data frame/tibble with 200 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{a factor with levels \code{male} and \code{female}}
#' \item{sport}{a factor with levels \code{football}, \code{basketball}, \code{baseball}, and \code{tennis}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~gender + sport, data = Sports)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Sports"





#' Convictions in spouse murder cases by gender
#' 
#' Data for Exercise 8.33
#' 
#' 
#' @name Spouse
#' @docType data
#' @format A data frame/tibble with 540 observations on the following 2 variables.
#' \describe{ 
#' \item{result}{a factor with levels \code{not prosecuted}, \code{pleaded guilty}, \code{convicted}, and \code{acquited}}
#' \item{spouse}{a factor with levels \code{husband} and \code{wife}} 
#' }
#' 
#' @source Bureau of Justice Statistics (September 1995), \emph{Spouse Murder Defendants in Large
#' Urban Counties}, Executive Summary, NCJ-156831.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~result + spouse, data = Spouse)
#' T1
#' chisq.test(T1)
#' rm(T1)
#' 
"Spouse"





#' Times of a 2-year old stallion on a one mile run
#' 
#' Data for Exercise 6.93
#' 
#' 
#' @name Stable
#' @docType data
#' @format A data frame/tibble with 9 observations on the following variable.
#' \describe{ 
#' \item{time}{time (in seconds) for horse to run 1 mile} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::SIGN.test(Stable$time, md = 98.5, alternative = "greater")
#' 
"Stable"





#' Thicknesses of 1872 Hidalgo stamps issued in Mexico
#' 
#' Data for Statistical Insight Chapter 1 and Exercise 5.110
#' 
#' 
#' @name Stamp
#' @docType data
#' @format A data frame/tibble with 485 observations on the following variable.
#' \describe{ 
#' \item{thickness}{stamp thickness (in mm)}
#' }
#' 
#' @source Izenman, A., Sommer, C. (1988), Philatelic Mixtures and Multimodal Densities,
#' \emph{Journal of the American Statistical Association}, 83, 941-953.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Stamp$thickness, freq = FALSE, col = "lightblue", 
#' main = "", xlab = "stamp thickness (mm)")
#' lines(density(Stamp$thickness), col = "blue")
#' t.test(Stamp$thickness, conf.level = 0.99)
#' 
"Stamp"





#' Grades for two introductory statistics classes
#' 
#' Data for Exercise 7.30
#' 
#' 
#' @name Statclas
#' @docType data
#' @format A data frame/tibble with 72 observations on the following 2 variables.
#' \describe{ 
#' \item{class}{class meeting time (9am or 2pm)} 
#' \item{score}{grade for an introductory statistics class} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Statclas)
#' boxplot(score ~ class, data = Statclas)
#' t.test(score ~ class, data = Statclas)
#' 
"Statclas"





#' Operating expenditures per resident for each of the state law enforcement
#' agencies
#' 
#' Data for Exercise 6.62
#' 
#' 
#' @name Statelaw
#' @docType data
#' @format A data frame/tibble with 50 observations on the following 2 variables.
#' \describe{ 
#' \item{state}{U.S. state} 
#' \item{cost}{dollars spent per resident on law enforcement} 
#' }
#' 
#' @source Bureau of Justice Statistics, \emph{Law Enforcement Management and Administrative Statistics, 1993},
#' NCJ-148825, September 1995, page 84.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Statelaw$cost)
#' BSDA::SIGN.test(Statelaw$cost, md = 8, alternative = "less")
#' 
"Statelaw"





#' Test scores for two beginning statistics classes
#' 
#' Data for Exercises 1.70 and 1.87
#' 
#' 
#' @name Statisti
#' @docType data
#' @format A data frame/tibble with 62 observations on the following 2 variables.
#' \describe{ 
#' \item{class}{character variable with values \code{Class1} and \code{Class2}} 
#' \item{score}{test score for an introductory statistics test} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ class, data = Statisti)
#' tapply(Statisti$score, Statisti$class, summary, na.rm = TRUE)
#' \dontrun{
#' dplyr::group_by(Statisti, class) %>%
#'  summarize(Mean = mean(score, na.rm = TRUE), 
#'             Median = median(score, na.rm = TRUE), 
#'                         SD = sd(score, na.rm = TRUE),
#'                                     RS = IQR(score, na.rm = TRUE))
#'                                     }
#' 
"Statisti"





#' STEP science test scores for a class of ability-grouped students
#' 
#' Data for Exercise 6.79
#' 
#' 
#' @name Step
#' @docType data
#' @format A data frame/tibble with 12 observations on the following variable.
#' \describe{ 
#' \item{score}{State test of educational progress (STEP) science test score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Step$score)
#' t.test(Step$score, mu = 80, alternative = "less")
#' wilcox.test(Step$score, mu = 80, alternative = "less")
#' 
"Step"





#' Short-term memory test scores on 12 subjects before and after a stressful
#' situation
#' 
#' Data for Example 7.20
#' 
#' 
#' @name Stress
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{prestress}{short term memory score before being exposed to a stressful situation}
#' \item{poststress}{short term memory score after being exposed to a stressful situation} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' diff <- Stress$prestress - Stress$poststress
#' qqnorm(diff)
#' qqline(diff)
#' t.test(diff)
#' t.test(Stress$prestress, Stress$poststress, paired = TRUE)
#' \dontrun{
#' wilcox.test(Stress$prestress, Stress$poststress, paired = TRUE)
#' }
#' 
"Stress"





#' Number of hours studied per week by a sample of 50 freshmen
#' 
#' Data for Exercise 5.25
#' 
#' 
#' @name Study
#' @docType data
#' @format A data frame/tibble with 50 observations on the following variable.
#' \describe{ 
#' \item{hours}{number of hours a week freshmen reported studying for their courses} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Study$hours)
#' hist(Study$hours)
#' summary(Study$hours)
#' 
"Study"





#' Number of German submarines sunk by U.S. Navy in World War II
#' 
#' Data for Exercises 2.16, 2.45, and 2.59
#' 
#' 
#' @name Submarin
#' @docType data
#' @format A data frame/tibble with 16 observations on the following 3 variables.
#' \describe{ 
#' \item{month}{month} 
#' \item{reported}{number of submarines reported sunk by U.S. Navy} 
#' \item{actual}{number of submarines actually sunk by U.S. Navy} 
#' }
#' 
#' @source F. Mosteller, S. Fienberg, and R. Rourke, \emph{Beginning Statistics with Data Analysis}
#' (Reading, MA: Addison-Wesley, 1983)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(actual ~ reported, data = Submarin)
#' summary(model)
#' plot(actual ~ reported, data = Submarin)
#' abline(model, col = "red")
#' rm(model)
#' 
"Submarin"





#' Time it takes a subway to travel from the airport to downtown
#' 
#' Data for Exercise 5.19
#' 
#' 
#' @name Subway
#' @docType data
#' @format A data frame/tibble with 30 observations on the following variable.
#' \describe{ 
#' \item{time}{time (in minutes) it takes a subway to travel from the airport to downtown} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Subway$time, main = "Exercise 5.19", 
#' xlab = "Time (in minutes)", col = "purple")
#' summary(Subway$time)
#' 
"Subway"





#' Wolfer sunspot numbers from 1700 through 2000
#' 
#' Data for Example 1.7
#' 
#' 
#' @name Sunspot
#' @docType data
#' @format A data frame/tibble with 301 observations on the following 2 variables.
#' \describe{ 
#' \item{year}{year} 
#' \item{sunspots}{average number of sunspots for the year} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(sunspots ~ year, data = Sunspot, type = "l")
#' \dontrun{
#' lattice::xyplot(sunspots ~ year, data = Sunspot, 
#'                 main = "Yearly sunspots", type = "l")
#'                 lattice::xyplot(sunspots ~ year, data = Sunspot, type = "l", 
#'                                 main = "Yearly sunspots", aspect = "xy")
#'                                 ggplot2::ggplot(data = Sunspot, aes(x = year, y = sunspots)) + 
#'                                   geom_line() + 
#'                                     theme_bw()
#'                                    }
#' 
"Sunspot"





#' Margin of victory in Superbowls I to XXXV
#' 
#' Data for Exercise 1.54
#' 
#' 
#' @name Superbowl
#' @docType data
#' @format A data frame/tibble with 35 observations on the following 5 variables.
#' \describe{ 
#' \item{winning_team}{name of Suberbowl winning team}
#' \item{winner_score}{winning score for the Superbowl} 
#' \item{losing_team}{name of Suberbowl losing team}
#' \item{loser_score}{score of losing teama numeric vector} 
#' \item{victory_margin}{winner_score - loser_score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Superbowl$victory_margin)
#' 
"Superbowl"





#' Top speeds attained by five makes of supercars
#' 
#' Data for Statistical Insight Chapter 10
#' 
#' 
#' @name Supercar
#' @docType data
#' @format A data frame/tibble with 30 observations on the following 2 variables.
#' \describe{
#' \item{speed}{top speed (in miles per hour) of car without redlining} 
#' \item{car}{name of sports car} 
#' }
#' 
#' @source \emph{Car and Drvier} (July 1995)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(speed ~ car, data = Supercar, col = rainbow(6),
#' ylab = "Speed (mph)")
#' summary(aov(speed ~ car, data = Supercar))
#' anova(lm(speed ~ car, data = Supercar))
#' 
"Supercar"





#' Ozone concentrations at Mt. Mitchell, North Carolina
#' 
#' Data for Exercise 5.63
#' 
#' 
#' @name Tablrock
#' @docType data
#' @format A data frame/tibble with 719 observations on the following 17 variables.
#' \describe{ 
#' \item{day}{date}
#' \item{hour}{time of day} 
#' \item{ozone}{ozone concentration}
#' \item{tmp}{temperature (in Celcius)} 
#' \item{vdc}{a numeric vector}
#' \item{wd}{a numeric vector} 
#' \item{ws}{a numeric vector}
#' \item{amb}{a numeric vector} 
#' \item{dew}{a numeric vector}
#' \item{so2}{a numeric vector} 
#' \item{no}{a numeric vector}
#' \item{no2}{a numeric vector} 
#' \item{nox}{a numeric vector}
#' \item{co}{a numeric vector} 
#' \item{co2}{a numeric vector}
#' \item{gas}{a numeric vector} 
#' \item{air}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(Tablrock$ozone)
#' boxplot(Tablrock$ozone)
#' qqnorm(Tablrock$ozone)
#' qqline(Tablrock$ozone)
#' par(mar = c(5.1 - 1, 4.1 + 2, 4.1 - 2, 2.1))
#' boxplot(ozone ~ day, data = Tablrock, 
#'         horizontal = TRUE, las = 1, cex.axis = 0.7)
#'         par(mar = c(5.1, 4.1, 4.1, 2.1))
#'        \dontrun{
#'        ggplot2::ggplot(data = Tablrock, aes(sample = ozone)) + 
#'          geom_qq() + 
#'            theme_bw()
#'            ggplot2::ggplot(data = Tablrock, aes(x = as.factor(day), y = ozone)) + 
#'              geom_boxplot(fill = "pink") + 
#'                coord_flip() + 
#'                  labs(x = "") + 
#'                    theme_bw()
#'                    }
#' 
"Tablrock"





#' Average teacher's salaries across the states in the 70s 80s and 90s
#' 
#' Data for Exercise 5.114
#' 
#' 
#' @name Teacher
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 3 variables.
#' \describe{
#'  \item{state}{U.S. state}
#' \item{year}{academic year} 
#' \item{salary}{avaerage salary (in dollars)}
#' }
#' 
#' @source National Education Association 
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(mfrow = c(3, 1))
#' hist(Teacher$salary[Teacher$year == "1973-74"],
#'      main = "Teacher salary 1973-74", xlab = "salary",
#'           xlim = range(Teacher$salary, na.rm = TRUE))
#'           hist(Teacher$salary[Teacher$year == "1983-84"],
#'                main = "Teacher salary 1983-84", xlab = "salary",
#'                     xlim = range(Teacher$salary, na.rm = TRUE))
#'                     hist(Teacher$salary[Teacher$year == "1993-94"],
#'                          main = "Teacher salary 1993-94", xlab = "salary",
#'                               xlim = range(Teacher$salary, na.rm = TRUE))
#'                               par(mfrow = c(1, 1))
#'     \dontrun{                       
#'     ggplot2::ggplot(data = Teacher, aes(x = salary)) + 
#'      geom_histogram(fill = "purple", color = "black") +  facet_grid(year ~ .) + 
#'        theme_bw()
#'        }
#' 
"Teacher"





#' Tennessee self concept scores for 20 gifted high school students
#' 
#' Data for Exercise 6.56
#' 
#' 
#' @name Tenness
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{score}{Tennessee Self-Concept Scale score} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Tenness$score, freq= FALSE, main = "", col = "green",
#' xlab = "Tennessee Self-Concept Scale score")
#' lines(density(Tenness$score))
#' \dontrun{
#' ggplot2::ggplot(data = Tenness, aes(x = score, y = ..density..)) + 
#'   geom_histogram(binwidth = 2, fill = "purple", color = "black") +
#'     geom_density(color = "red", fill = "pink", alpha = 0.3) + 
#'       theme_bw()
#'       }
#' 
"Tenness"





#' Tensile strength of plastic bags from two production runs
#' 
#' Data for Example 7.11
#' 
#' 
#' @name Tensile
#' @docType data
#' @format A data frame/tibble with 72 observations on the following 2 variables.
#' \describe{
#' \item{tensile}{plastic bag tensile strength (pounds per square inch)}
#' \item{run}{factor with run number (1 or 2)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(tensile ~ run, data = Tensile, 
#' col = c("purple", "cyan"))
#' t.test(tensile ~ run, data = Tensile)
#' 
"Tensile"





#' Grades on the first test in a statistics class
#' 
#' Data for Exercise 5.80
#' 
#' 
#' @name Test1
#' @docType data
#' @format A data frame/tibble with 25 observations on the following variable.
#' \describe{ 
#' \item{score}{score on first statistics exam} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Test1$score)
#' boxplot(Test1$score)
#' 
"Test1"





#' Heat loss of thermal pane windows versus outside temperature
#' 
#' Data for Example 9.5
#' 
#' 
#' @name Thermal
#' @docType data
#' @format A data frame/tibble with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{temp}{temperature (degrees Celcius) } 
#' \item{loss}{heat loss (BTUs)} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' model <- lm(loss ~ temp, data = Thermal)
#' summary(model)
#' plot(loss ~ temp, data = Thermal)
#' abline(model)
#' rm(model)
#' 
"Thermal"





#' 1999-2000 closing prices for TIAA-CREF stocks
#' 
#' Data for your enjoyment
#' 
#' 
#' @name Tiaa
#' @docType data
#' @format A data frame/tibble with 365 observations on the following 4 variables.
#' \describe{ 
#' \item{crefstk}{a numeric vector} 
#' \item{crefgwt}{a numeric vector} 
#' \item{tiaa}{a numeric vector} 
#' \item{date}{day of the year} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Tiaa)
#' 
"Tiaa"





#' Time to complete an airline ticket reservation
#' 
#' Data for Exercise 5.18
#' 
#' 
#' @name Ticket
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{time}{time (in seconds) to check out a reservation}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Ticket$time)
#' 
"Ticket"





#' Consumer Reports (Oct 94) rating of toaster ovens versus the cost
#' 
#' Data for Exercise 9.36
#' 
#' 
#' @name Toaster
#' @docType data
#' @format A data frame/tibble with 17 observations on the following 3 variables.
#' \describe{ 
#' \item{toaster}{name of toaster} 
#' \item{score}{Consumer Reports score}
#' \item{cost}{price of toaster (in dollars)} 
#' }
#' 
#' @source \emph{Consumer Reports} (October 1994)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(cost ~ score, data = Toaster)
#' model <- lm(cost ~ score, data = Toaster)
#' summary(model)
#' names(summary(model))
#' summary(model)$r.squared
#' plot(model, which = 1)
#' 
"Toaster"





#' Size of tonsils collected from 1,398 children
#' 
#' Data for Exercise 2.78
#' 
#' 
#' @name Tonsils
#' @docType data
#' @format A data frame/tibble with 1398 observations on the following 2 variables.
#' \describe{ 
#' \item{size}{a factor with levels \code{Normal}, \code{Large}, and \code{Very Large}} 
#' \item{status}{a factor with levels \code{Carrier} and \code{Non-carrier}}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~size + status, data = Tonsils)
#' T1
#' prop.table(T1, 1)
#' prop.table(T1, 1)[2, 1]
#' barplot(t(T1), legend = TRUE, beside = TRUE, col = c("red", "green"))
#' \dontrun{
#' NDF <- dplyr::count(Tonsils, size, status) 
#' ggplot2::ggplot(data = NDF, aes(x = size, y = n, fill = status)) + 
#'  geom_bar(stat = "identity", position = "dodge") + 
#'    scale_fill_manual(values = c("red", "green")) + 
#'      theme_bw()
#'      }
#' 
"Tonsils"





#' The number of torts, average number of months to process a tort, and county
#' population from the court files of the nation's largest counties
#' 
#' Data for Exercise 5.13
#' 
#' 
#' @name Tort
#' @docType data
#' @format A data frame/tibble with 45 observations on the following 5 variables.
#' \describe{ 
#' \item{county}{U.S. county}
#' \item{months}{average number of months to process a tort} 
#' \item{population}{population of the county} 
#' \item{torts}{number of torts} 
#' \item{rate}{rate per 10,000 residents} 
#' }
#' 
#' @source U.S. Department of Justice, \emph{Tort Cases in Large Counties}, Bureau of Justice
#' Statistics Special Report, April 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::EDA(Tort$months)
#' 
"Tort"





#' Hazardous waste sites near minority communities
#' 
#' Data for Exercises 1.55, 5.08, 5.109, 8.58, and 10.35
#' 
#' 
#' @name Toxic
#' @docType data
#' @format A data frame/tibble with 51 observations on the following 5 variables.
#' \describe{ 
#' \item{state}{U.S. state}
#' \item{region}{U.S. region} 
#' \item{sites}{number of commercial hazardous waste sites}
#' \item{minority}{percent of minorities living in communities with commercial hazardous waste sites} 
#' \item{percent}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' hist(Toxic$sites)
#' hist(Toxic$minority)
#' qqnorm(Toxic$minority)
#' qqline(Toxic$minority)
#' boxplot(sites ~ region, data = Toxic)
#' tapply(Toxic$sites, Toxic$region, median)
#' kruskal.test(sites ~ factor(region), data = Toxic)
#' 
"Toxic"




#' National Olympic records for women in several races
#' 
#' Data for Exercises 2.97, 5.115, and 9.62
#' 
#' 
#' @name Track
#' @docType data
#' @format A data frame with 55 observations on the following 8 variables.
#' \describe{ 
#' \item{country}{athlete's country} 
#' \item{100m}{time in seconds for 100 m} 
#' \item{200m}{time in seconds for 200 m}
#' \item{400m}{time in seconds for 400 m} 
#' \item{800m}{time in minutes for 800 m} 
#' \item{1500m}{time in minutes for 1500 m} 
#' \item{3000m}{time in minutes for 3000 m} 
#' \item{marathon}{time in minutes for marathon} 
#' }
#' 
#' @source Dawkins, B. (1989), "Multivariate Analysis of National Track Records," \emph{The American Statistician, 43}(2), 110-115
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(`200m` ~ `100m`, data = Track)
#' plot(`400m` ~ `100m`, data = Track)
#' plot(`400m` ~ `200m`, data = Track)
#' cor(Track[, 2:8])
#' 
"Track"





#' Olympic winning times for the men's 1500-meter run
#' 
#' Data for Exercise 1.36
#' 
#' 
#' @name Track15
#' @docType data
#' @format A data frame/tibble with 26 observations on the following 2 variables.
#' \describe{ 
#' \item{year}{Olympic year} 
#' \item{time}{Olympic winning time (in seconds) for the 1500-meter run} 
#' }
#' 
#' @source \emph{The World Almanac and Book of Facts}, 2000
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(time~ year, data = Track15, type = "b", pch = 19,
#' ylab = "1500m time in seconds", col = "green") 
#' 
"Track15"






#' Illustrates analysis of variance for three treatment groups
#' 
#' Data for Exercise 10.44
#' 
#' 
#' @name Treatments
#' @docType data
#' @format A data frame/tibble with 24 observations on the following 2 variables.
#' \describe{
#' \item{score}{score from an experiment} 
#' \item{group}{factor with levels 1, 2, and 3} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(score ~ group, data = Treatments)
#' summary(aov(score ~ group, data = Treatments))
#' summary(lm(score ~ group, data = Treatments))
#' anova(lm(score ~ group, data = Treatments))
#' 
"Treatments"





#' Number of trees in 20 grids
#' 
#' Data for Exercise 1.50
#' 
#' 
#' @name Trees
#' @docType data
#' @format A data frame/tibble with 20 observations on the following variable.
#' \describe{ 
#' \item{number}{number of trees in a grid} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Trees$number)
#' hist(Trees$number, main = "Exercise 1.50", xlab = "number",
#'      col = "brown")
#' 
"Trees"





#' Miles per gallon for standard 4-wheel drive trucks manufactured by
#' Chevrolet, Dodge and Ford
#' 
#' Data for Example 10.2
#' 
#' 
#' @name Trucks
#' @docType data
#' @format A data frame/tibble with 15 observations on the following 2 variables.
#' \describe{ 
#' \item{mpg}{miles per gallon} 
#' \item{truck}{a factor with levels \code{chevy}, \code{dodge}, and \code{ford}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(mpg ~ truck, data = Trucks, horizontal = TRUE, las = 1)
#' summary(aov(mpg ~ truck, data = Trucks))
#' 
"Trucks"





#' Percent of students that watch more than 6 hours of TV per day versus
#' national math test scores
#' 
#' Data for Examples 2.1 and 2.7
#' 
#' 
#' @name Tv
#' @docType data
#' @format A data frame/tibble with 53 observations on the following 3 variables.
#' \describe{ 
#' \item{state}{U.S. state}
#' \item{percent}{percent of students who watch more than six hours of TV a day} 
#' \item{test}{state average on national math test} 
#' }
#' 
#' @source Educational Testing Services
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(test ~ percent, data = Tv, col = "blue")
#' cor(Tv$test, Tv$percent)
#' 
"Tv"





#' Intelligence test scores for identical twins in which one twin is given a
#' drug
#' 
#' Data for Exercise 7.54
#' 
#' 
#' @name Twin
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{twinA}{score on intelligence test without drug} 
#' \item{twinB}{score on intelligence test after taking drug} 
#' \item{differ}{twinA - twinB} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Twin$differ)
#' qqline(Twin$differ)
#' shapiro.test(Twin$differ)
#' t.test(Twin$twinA, Twin$twinB, paired = TRUE)
#' 
"Twin"





#' Data set describing a sample of undergraduate students
#' 
#' Data for Exercise 1.15
#' 
#' 
#' @name Undergrad
#' @docType data
#' @format A data frame/tibble with 100 observations on the following 6 variables.
#' \describe{ 
#' \item{gender}{character variable with values \code{Female} and \code{Male}} 
#' \item{major}{college major}
#' \item{class}{college year group classification} 
#' \item{gpa}{grade point average}
#' \item{sat}{Scholastic Assessment Test score} 
#' \item{drops}{number of courses dropped}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stripchart(gpa ~ class, data = Undergrad, method = "stack", 
#' col = c("blue","red","green","lightblue"),
#' pch = 19, main = "GPA versus Class")
#' stripchart(gpa ~ gender, data = Undergrad, method = "stack", 
#' col = c("red", "blue"), pch = 19,
#'            main = "GPA versus Gender")
#'            stripchart(sat ~ drops, data = Undergrad, method = "stack", 
#'            col = c("blue", "red", "green", "lightblue"),
#'                       pch = 19, main = "SAT versus Drops")
#'                       stripchart(drops ~ gender, data = Undergrad, 
#'                       method = "stack", col = c("red", "blue"), pch = 19,
#'                                  main = "Drops versus Gender")
#'  \dontrun{
#'  ggplot2::ggplot(data = Undergrad, aes(x = sat, y = drops, fill = factor(drops))) + 
#'    facet_grid(drops ~.) +
#'     geom_dotplot() +
#'       guides(fill = FALSE)
#'       }
#' 
"Undergrad"





#' Number of days of paid holidays and vacation leave for sample of 35 textile
#' workers
#' 
#' Data for Exercise 6.46 and 6.98
#' 
#' 
#' @name Vacation
#' @docType data
#' @format A data frame/tibble with 35 observations on the following variable.
#' \describe{ 
#' \item{number}{number of days of paid holidays and vacation leave taken} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Vacation$number)
#' hist(Vacation$number, main = "Exercise 6.46", col = "blue",
#'      xlab = "number of days of paid holidays and vacation leave taken")
#'      t.test(Vacation$number, mu = 24)
#' 
"Vacation"





#' Reported serious reactions due to vaccines in 11 southern states
#' 
#' Data for Exercise 1.111
#' 
#' 
#' @name Vaccine
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{state}{U.S. state} 
#' \item{number}{number of reported serious reactions per million doses of a vaccine} 
#' }
#' 
#' @source Center for Disease Control, Atlanta, Georgia.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Vaccine$number, scale = 2) 
#' fn <- fivenum(Vaccine$number)
#' fn
#' iqr <- IQR(Vaccine$number)
#' iqr
#' 
"Vaccine"





#' Fatality ratings for foreign and domestic vehicles
#' 
#' Data for Exercise 8.34
#' 
#' 
#' @name Vehicle
#' @docType data
#' @format A data frame/tibble with 151 observations on the following 2 variables.
#' \describe{ 
#' \item{make}{a factor with levels \code{domestic} and \code{foreign}} 
#' \item{rating}{a factor with levels \code{Much better than average}, \code{Above average}, \code{Average}, \code{Below average}, and \code{Much worse than average}} 
#' }
#' 
#' @source Insurance Institute for Highway Safety and the Highway Loss Data Institute, 1995
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~make + rating, data = Vehicle)
#' T1
#' chisq.test(T1)
#' 
"Vehicle"





#' Verbal test scores and number of library books checked out for 15 eighth
#' graders
#' 
#' Data for Exercise 9.30
#' 
#' 
#' @name Verbal
#' @docType data
#' @format A data frame/tibble with 15 observations on the following 2 variables.
#' \describe{ 
#' \item{number}{number of library books checked out} 
#' \item{verbal}{verbal test score}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(verbal ~ number, data = Verbal)
#' abline(lm(verbal ~ number, data = Verbal))
#' summary(lm(verbal ~ number, data = Verbal))
#' 
"Verbal"





#' Number of sunspots versus mean annual level of Lake Victoria Nyanza from
#' 1902 to 1921
#' 
#' Data for Exercise 2.98
#' 
#' 
#' @name Victoria
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{year} 
#' \item{level}{mean annual level of Lake Victoria Nyanza} 
#' \item{sunspot}{number of sunspots} 
#' }
#' 
#' @source N. Shaw, \emph{Manual of Meteorology}, Vol. 1 (London: Cambridge University Press, 1942),
#' p. 284; and F. Mosteller and J. W. Tukey, \emph{Data Analysis and Regression} (Reading, MA: Addison-Wesley, 1977)
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(level ~ sunspot, data = Victoria)
#' model <- lm(level ~ sunspot, data = Victoria)
#' summary(model)
#' rm(model)
#' 
"Victoria"





#' Viscosity measurements of a substance on two different days
#' 
#' Data for Exercise 7.44
#' 
#' 
#' @name Viscosit
#' @docType data
#' @format A data frame/tibble with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{first}{viscosity measurement for a certain substance on day one} 
#' \item{second}{viscosity measurement for a certain substance on day two} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(Viscosit$first, Viscosit$second)
#' t.test(Viscosit$first, Viscosit$second, var.equal = TRUE)
#' 
"Viscosit"





#' Visual acuity of a group of subjects tested under a specified dose of a drug
#' 
#' Data for Exercise 5.6
#' 
#' 
#' @name Visual
#' @docType data
#' @format A data frame/tibble with 18 observations on the following variable.
#' \describe{ 
#' \item{visual}{visual acuity measurement} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' stem(Visual$visual)
#' boxplot(Visual$visual, col = "purple")
#' 
"Visual"





#' Reading scores before and after vocabulary training for 14 employees who did
#' not complete high school
#' 
#' Data for Exercise 7.80
#' 
#' 
#' @name Vocab
#' @docType data
#' @format A data frame/tibble with 14 observations on the following 2 variables.
#' \describe{ 
#' \item{first}{reading test score before formal vocabulary training} 
#' \item{second}{reading test score after formal vocabulary training} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' t.test(Vocab$first, Vocab$second, paired = TRUE)
#' 
"Vocab"





#' Volume of injected waste water from Rocky Mountain Arsenal and number of
#' earthquakes near Denver
#' 
#' Data for Exercise 9.18
#' 
#' 
#' @name Wastewat
#' @docType data
#' @format A data frame/tibble with 44 observations on the following 2 variables.
#' \describe{ 
#' \item{gallons}{injected water (in million gallons)} 
#' \item{number}{number of earthqueakes detected in Denver}
#' }
#' 
#' @source Davis, J. C. (1986), \emph{Statistics and Data Analysis in Geology}, 2 ed., John Wiley and Sons,
#' New York, p. 228, and Bardwell, G. E. (1970), Some Statistical Features of the Relationship between
#' Rocky Mountain Arsenal Waste Disposal and Frequency of Earthquakes, \emph{Geological Society of America, Engineering
#' Geology Case Histories, 8}, 33-337
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(number ~ gallons, data = Wastewat)
#' model <- lm(number ~ gallons, data = Wastewat)
#' summary(model)
#' anova(model)
#' plot(model, which = 2)
#' 
"Wastewat"





#' Weather casualties in 1994
#' 
#' Data for Exercise 1.30
#' 
#' 
#' @name Weather94
#' @docType data
#' @format A data frame/tibble with 388 observations on the following 1 variables.
#' \describe{ 
#' \item{type}{factor with levels Extreme Temp, Flash Flood, Fog, High Wind, Hurricane, 
#'  Lighting, Other, River Flood, Thunderstorm, Tornado, and Winter Weather.} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' T1 <- xtabs(~type, data = Weather94)
#' T1
#' par(mar = c(5.1 + 2, 4.1 - 1, 4.1 - 2, 2.1))
#' barplot(sort(T1, decreasing = TRUE), las = 2, col = rainbow(11))
#' par(mar = c(5.1, 4.1, 4.1, 2.1))
#' \dontrun{
#' T2 <- as.data.frame(T1)
#' T2
#' ggplot2::ggplot(data =T2, aes(x = reorder(type, Freq), y = Freq)) + 
#'   geom_bar(stat = "identity", fill = "purple") +
#'     theme_bw() + 
#'       theme(axis.text.x  = element_text(angle = 55, vjust = 0.5)) + 
#'         labs(x = "", y = "count")
#'         }
#' 
"Weather94"





#' Price of a bushel of wheat versus the national weekly earnings of production
#' workers
#' 
#' Data for Exercise 2.11
#' 
#' 
#' @name Wheat
#' @docType data
#' @format A data frame/tibble with 19 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{year} 
#' \item{earnings}{national weekly earnings (in dollars) for production workers} 
#' \item{price}{price for a bushel of wheat (in dollars)} 
#' }
#' 
#' @source \emph{The World Almanac and Book of Facts}, 2000
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' par(mfrow = c(1, 2))
#' plot(earnings ~ year, data = Wheat)
#' plot(price ~ year, data = Wheat)
#' par(mfrow = c(1, 1))
#' 
"Wheat"





#' Direct current produced by different wind velocities
#' 
#' Data for Exercise 9.34
#' 
#' 
#' @name Windmill
#' @docType data
#' @format A data frame/tibble with 25 observations on the following 2 variables.
#' \describe{ 
#' \item{velocity}{wind velocity (miles per hour)} 
#' \item{output}{power generated (DC volts)} 
#' }
#' 
#' @source Joglekar, et al. (1989), Lack of Fit Testing when Replicates Are Not Available,
#' \emph{The American Statistician, 43},(3), 135-143
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' summary(lm(output ~ velocity, data = Windmill))
#' anova(lm(output ~ velocity, data = Windmill))
#' 
"Windmill"





#' Wind leakage for storm windows exposed to a 50 mph wind
#' 
#' Data for Exercise 6.54
#' 
#' 
#' @name Window
#' @docType data
#' @format A data frame/tibble with 9 observations on the following 2 variables.
#' \describe{ 
#' \item{window}{window number} 
#' \item{leakage}{percent leakage from a 50 mph wind} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' BSDA::SIGN.test(Window$leakage, md = 0.125, alternative = "greater")
#' 
"Window"





#' Baseball team wins versus 7 independent variables for National league teams
#' in 1990
#' 
#' Data for Exercise 9.23
#' 
#' 
#' @name Wins
#' @docType data
#' @format A data frame with 12 observations on the following 9 variables.
#' \describe{ 
#' \item{team}{name of team}
#' \item{wins}{number of wins} 
#' \item{batavg}{batting average} 
#' \item{rbi}{runs batted in} 
#' \item{stole}{bases stole} 
#' \item{strkout}{number of strikeots} 
#' \item{caught}{number of times caught stealing} 
#' \item{errors}{number of errors} 
#' \item{era}{earned run average} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(wins ~ era, data = Wins)
#' \dontrun{
#' ggplot2::ggplot(data = Wins, aes(x = era, y = wins)) + 
#'   geom_point() + 
#'     geom_smooth(method = "lm", se = FALSE) + 
#'       theme_bw()
#'       }
#' 
"Wins"





#' Strength tests of two types of wool fabric
#' 
#' Data for Exercise 7.42
#' 
#' 
#' @name Wool
#' @docType data
#' @format A data frame/tibble with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{type}{type of wool (\code{Type I}, \code{Type 2})} 
#' \item{strength}{strength of wool} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' boxplot(strength ~ type, data = Wool, col = c("blue", "purple"))
#' t.test(strength ~ type, data = Wool, var.equal = TRUE)
#' 
"Wool"





#' Monthly sunspot activity from 1974 to 2000
#' 
#' Data for Exercise 2.7
#' 
#' 
#' @name Yearsunspot
#' @docType data
#' @format A data frame/tibble with 252 observations on the following 22 variables.
#' \describe{ 
#' \item{number}{average number of sunspots} 
#' \item{year}{date} 
#' }
#' 
#' @source NASA/Marshall Space Flight Center, Huntsville, AL 35812.
#' 
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' plot(number ~ year, data = Yearsunspot)
#' 
"Yearsunspot"
#' 








