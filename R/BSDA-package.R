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
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' qqnorm(Abbey$price)
#' qqline(Abbey$price)
#' t.test(Abbey$price, mu = 300)
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
#' \item{aggres}{measure of aggresive tendency} 
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
#' Data for Exercise 2.9
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
#' \item{complaints}{a numeric vector} 
#' }
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
#' \item{region}{a numeric vector classifying each hospital (1 = rural), (2 = regional), (3 = metropolitan)} 
#' }
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
#' Data for Exercise 1.10
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





#' Attendance of bus drivers versus attendance
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
#' Data for Exercise 16.8 and 1.82
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
#' Data for Exercise 7.17
#' 
#' 
#' @name Concrete
#' @docType data
#' @format A data frame/tibble  with 20 observations on the following 2twovariables.
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
#' Data for Exercise 7.15
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
#' Data for Exercise 7.21
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
#' Data for Exercises 1.55, 1.75, 3.69, and 5.60
#' 
#' 
#' @name Framingh
#' @docType data
#' @format A data frame/tibble with 62 observations on the following variable.
#' \describe{ 
#' \item{cholest}{a numeric vector with cholesterol values} 
#' }
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
#' Data for Exercise 2.13
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
#' @format A data frame with 33 observations on the following 4 variables.
#' \describe{ 
#' \item{driver}{a factor with levels \code{andretti}
#' \code{bachelart} \code{boesel} \code{brayton} \code{c.guerrero}
#' \code{cheever} \code{fabi} \code{fernandez} \code{ferran} \code{fittipaldi}
#' \code{fox} \code{goodyear} \code{gordon} \code{gugelmin} \code{herta}
#' \code{james} \code{johansson} \code{jones} \code{lazier} \code{luyendyk}
#' \code{matsuda} \code{matsushita} \code{pruett} \code{r.guerrero}
#' \code{rahal} \code{ribeiro} \code{salazar} \code{sharp} \code{sullivan}
#' \code{tracy} \code{vasser} \code{villeneuve} \code{zampedri}}
#' \item{qualif}{a numeric vector} 
#' \item{starts}{a numeric vector} 
#' \item{group}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Indy500)
#' attach(Indy500)
#' stripchart(qualif~group, method="stack",pch=19,col=c("red","blue"))
#' boxplot(qualif~group)
#' t.test(qualif~group)
#' detach(Indy500)
#' 
NULL





#' Private pay increase of salaried employees versus inflation rate
#' 
#' Data for Exercises 2.12 and 2.29
#' 
#' 
#' @name Inflatio
#' @docType data
#' @format A data frame with 24 observations on the following 5 variables.
#' \describe{
#' \item{year}{a numeric vector} 
#' \item{pay}{a numeric vector} 
#' \item{increase}{a numeric vector}
#' \item{inflation}{a numeric vector} 
#' \item{C6.T}{a factor with levels \code{alow} \code{bmiddle} \code{high}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Inflatio)
#' attach(Inflatio)
#' plot(inflation,increase)
#' cor(inflation,increase,use="complete.obs")
#' detach(Inflatio)
#' 
NULL





#' Inlet oil temperature through a valve
#' 
#' Data for Exercises 5.91 and 6.48
#' 
#' 
#' @name Inletoil
#' @docType data
#' @format A data frame with 12 observations on the following variable.
#' \describe{ 
#' \item{temp}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Inletoil)
#' attach(Inletoil)
#' t.test(temp)$conf
#' t.test(temp,mu=98,alternative="less")
#' detach(Inletoil)
#' 
NULL





#' Type of drug offense by race
#' 
#' Data for Statistical Insight Chapter 8
#' 
#' 
#' @name Inmate
#' @docType data
#' @format A data frame with 3 observations on the following 5 variables.
#' \describe{ 
#' \item{Race}{a factor with levels \code{black}
#' \code{hispanic} \code{white}} 
#' \item{heroin}{a numeric vector}
#' \item{crack}{a numeric vector} 
#' \item{cocaine}{a numeric vector} 
#' \item{marijuan}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Inmate)
#' attach(Inmate)
#' Inmate
#' chisq.test(Inmate[,2:5])
#' detach(Inmate)
#' 
NULL





#' Percent of vehicles passing inspection by type inspection station
#' 
#' Data for Exercise 5.89
#' 
#' 
#' @name Inspect
#' @docType data
#' @format A data frame with 6 observations on the following 4 variables.
#' \describe{ 
#' \item{Type}{a factor with levels \code{auto inspection}
#' \code{auto repair} \code{car care center} \code{gas station} \code{new car
#' dealer} \code{tire store}} 
#' \item{less70}{a numeric vector}
#' \item{X70.85}{a numeric vector} 
#' \item{great85}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Inspect)
#' attach(Inspect)
#' Inspect
#' chisq.test(Inspect[,2:4])
#' detach(Inspect)
#' 
NULL





#' Heat loss through a new insulating medium
#' 
#' Data for Exercise 9.50
#' 
#' 
#' @name Insulate
#' @docType data
#' @format A data frame with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{temp}{a numeric vector} 
#' \item{loss}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Insulate)
#' attach(Insulate)
#' summary(lm(loss~temp))
#' detach(Insulate)
#' 
NULL





#' GPA versus IQ for 12 individuals
#' 
#' Data for Exercises 9.51 and 9.52
#' 
#' 
#' @name Iqgpa
#' @docType data
#' @format A data frame with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{IQ}{a numeric vector} 
#' \item{GPA}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Iqgpa)
#' attach(Iqgpa)
#' plot(IQ,GPA)
#' model <- lm(GPA~IQ)
#' abline(model)
#' summary(model)
#' detach(Iqgpa)
#' remove(model)
#' 
NULL





#' R.A. Fishers famous data on sepal length of a species of Iris Setosa
#' 
#' Data for Exercises 1.15 and 5.19
#' 
#' 
#' @name Irises
#' @docType data
#' @format A data frame with 150 observations on the following 14 variables.
#' \describe{ 
#' \item{sepalL1}{a numeric vector} 
#' \item{sepalW1}{a numeric vector} 
#' \item{petalL1}{a numeric vector}
#' \item{petalW1}{a numeric vector} 
#' \item{sepalL2}{a numeric vector} 
#' \item{sepalW2}{a numeric vector} 
#' \item{petalL2}{a numeric vector} 
#' \item{peatalW2}{a numeric vector}
#' \item{sepalL3}{a numeric vector} 
#' \item{sepalW3}{a numeric vector} 
#' \item{petalL3}{a numeric vector} 
#' \item{petalW3}{a numeric vector} 
#' \item{sepalL}{a numeric vector}
#' \item{sample}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Irises)
#' attach(Irises)
#' EDA(sepalL1)
#' t.test(sepalL1,conf.level=.99)$conf
#' detach(Irises)
#' 
NULL





#' Number of problems reported per 100 cars in 1994 versus 1995s
#' 
#' Data for Exercise 2.14, 2.17, 2.31, 2.33, and 2.40
#' 
#' 
#' @name Jdpower
#' @docType data
#' @format A data frame with 29 observations on the following 3 variables.
#' \describe{ 
#' \item{Car}{a factor with levels \code{Acura} \code{BMW}
#' \code{Buick} \code{Cadillac} \code{Chevrolet} \code{Dodge} \code{Eagle}
#' \code{Ford} \code{Geo} \code{Honda} \code{Hyundai} \code{Infiniti}
#' \code{Jaguar} \code{Lexus} \code{Lincoln} \code{Mazda} \code{Mercedes-Benz}
#' \code{Mercury} \code{Mitsubishi} \code{Nissan} \code{Oldsmobile}
#' \code{Plymouth} \code{Pontiac} \code{Saab} \code{Saturn} \code{Subaru}
#' \code{Toyota} \code{Volkswagen} \code{Volvo}} 
#' \item{X1994}{a numeric vector} 
#' \item{X1995}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Jdpower)
#' attach(Jdpower)
#' plot(X1994,X1995)
#' model <- lm(X1995~X1994)
#' abline(model)
#' model
#' cor(X1995,X1994)
#' detach(Jdpower)
#' 
NULL





#' Job satisfaction and stress level for 9 school teachers
#' 
#' Data for Exercise 9.60
#' 
#' 
#' @name Jobsat
#' @docType data
#' @format A data frame with 9 observations on the following 2 variables.
#' \describe{ 
#' \item{WSPT}{a numeric vector} 
#' \item{satisfac}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Jobsat)
#' attach(Jobsat)
#' plot(WSPT,satisfac)
#' model <- lm(satisfac~WSPT)
#' abline(model)
#' summary(model)
#' detach(Jobsat)
#' remove(model)
#' 
NULL





#' Smoking habits of boys and girls ages 12 to 18
#' 
#' Data for Exercise 4.85
#' 
#' 
#' @name Kidsmoke
#' @docType data
#' @format A data frame with 1000 observations on the following 2 variables.
#' \describe{ 
#' \item{gender}{a numeric vector} 
#' \item{smoke}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Kidsmoke)
#' attach(Kidsmoke)
#' table(gender,smoke)
#' addmargins(table(gender,smoke))
#' addmargins(table(gender,smoke)/1000)
#' detach(Kidsmoke)
#' 
NULL





#' Rates per kilowatt-hour for each of the 50 states and DC
#' 
#' Data for Example 5.9
#' 
#' 
#' @name Kilowatt
#' @docType data
#' @format A data frame with 51 observations on the following 2 variables.
#' \describe{ 
#' \item{State}{a factor with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of
#' Columbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho}
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
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Kilowatt)
#' attach(Kilowatt)
#' EDA(rate)
#' detach(Kilowatt)
#' 
NULL





#' Reading scores for first grade children who attended kindergarten versus
#' those who did not
#' 
#' Data for Exercise 7.68
#' 
#' 
#' @name Kinder
#' @docType data
#' @format A data frame with 8 observations on the following 3 variables.
#' \describe{ 
#' \item{Pair}{a numeric vector} 
#' \item{Kinder}{a numeric vector} 
#' \item{NoKinder}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Kinder)
#' attach(Kinder)
#' DIF <- Kinder - NoKinder
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(Kinder, NoKinder,paired=TRUE,alternative="greater")
#' detach(Kinder)
#' remove(DIF)
#' 
NULL





#' Median costs of laminectomies at hospitals across North Carolina in 1992
#' 
#' Data for Exercise 10.18
#' 
#' 
#' @name Laminect
#' @docType data
#' @format A data frame with 46 observations on the following 5 variables.
#' \describe{
#'  \item{cost}{a numeric vector} 
#'  \item{class}{a numeric vector} 
#'  \item{Rural}{a numeric vector}
#'  \item{Regional}{a numeric vector} 
#'  \item{Metropol}{a numeric vector} 
#'  }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Laminect)
#' attach(Laminect)
#' boxplot(cost~class)
#' anova(lm(cost~as.factor(class)))
#' detach(Laminect)
#' 
NULL





#' Leadership exam scores by age for employees on an industrial plant
#' 
#' Data for Exercise 7.31
#' 
#' 
#' @name Leader
#' @docType data
#' @format A data frame with 34 observations on the following 2 variables.
#' \describe{ 
#' \item{under35}{a numeric vector} 
#' \item{over35}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Leader)
#' attach(Leader)
#' boxplot(under35,over35,names=c("Under 35","Over 35"),col=c("green","brown"))
#' t.test(under35,over35)
#' detach(Leader)
#' 
NULL





#' Lead levels in children's blood whose parents worked in a battery factory
#' 
#' Data for Example 1.17
#' 
#' 
#' @name Lead
#' @docType data
#' @format A data frame with 33 observations on the following 3 variables.
#' \describe{ 
#' \item{Pair}{a numeric vector} 
#' \item{exposed}{a numeric vector} 
#' \item{control}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Lead)
#' attach(Lead)
#' boxplot(exposed,control, names=c("Exposed","Control"),col=c("red","blue"))
#' detach(Lead)
#' 
NULL





#' Survival time of mice injected with an experimental lethal drug
#' 
#' Data for Example 6.12
#' 
#' 
#' @name Lethal
#' @docType data
#' @format A data frame with 30 observations on the following variable.
#' \describe{ 
#' \item{survival}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Lethal)
#' attach(Lethal)
#' SIGN.test(survival,md=45,alternative="less")
#' detach(Lethal)
#' 
NULL





#' Life expectancy of men and women in U.S.
#' 
#' Data for Exercise 1.31
#' 
#' 
#' @name Life
#' @docType data
#' @format A data frame with 8 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{a numeric vector} 
#' \item{Men}{a numeric vector} 
#' \item{Women}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Life)
#' attach(Life)
#' plot(year,Men,type="l",ylim=c(min(Men,Women),max(Men,Women)),col="blue",
#' main="Life Expectancy versus Year",ylab="Age",xlab="Year")
#' lines(year,Women,col="red")
#' text(1955,65,"Men",col="blue")
#' text(1955,70,"Women",col="red")
#' detach(Life)
#' 
NULL





#' Life span of electronic components used in a spacecraft versus heat
#' 
#' Data for Exercise 2.4, 2.37, and 2.49
#' 
#' 
#' @name Lifespan
#' @docType data
#' @format A data frame with 6 observations on the following 4 variables.
#' \describe{ 
#' \item{heat}{a numeric vector} 
#' \item{life}{a numeric vector} 
#' \item{RESI1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Lifespan)
#' attach(Lifespan)
#' plot(heat,life)
#' model <- lm(life~heat)
#' model
#' resid(model)
#' sum((resid(model))^2)
#' anova(model)
#' # plot(model)  # Used for diagnostic purposes
#' detach(Lifespan)
#' 
NULL





#' Relationship between damage reports and deaths caused by lightning
#' 
#' Data for Exercise 2.6
#' 
#' 
#' @name Ligntmonth
#' @docType data
#' @format A data frame with 12 observations on the following 4 variables.
#' \describe{ 
#' \item{Month}{a factor with levels \code{1/01/2000}
#' \code{10/01/2000} \code{11/01/2000} \code{12/01/2000} \code{2/01/2000}
#' \code{3/01/2000} \code{4/01/2000} \code{5/01/2000} \code{6/01/2000}
#' \code{7/01/2000} \code{8/01/2000} \code{9/01/2000}} 
#' \item{deaths}{a numeric vector} 
#' \item{injuries}{a numeric vector}
#' \item{damage}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Ligntmonth)
#' attach(Ligntmonth)
#' plot(damage,deaths)
#' detach(Ligntmonth)
#' 
NULL





#' Measured traffic at three prospective locations for a motor lodge
#' 
#' Data for Exercise 10.33
#' 
#' 
#' @name Lodge
#' @docType data
#' @format A data frame with 45 observations on the following 6 variables.
#' \describe{ 
#' \item{SiteA}{a numeric vector} 
#' \item{SiteB}{a numeric vector} 
#' \item{SiteC}{a numeric vector}
#' \item{Traffic}{a numeric vector} 
#' \item{Site}{a numeric vector} 
#' \item{Ranks}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Lodge)
#' attach(Lodge)
#' boxplot(Traffic~Site)
#' anova(lm(Traffic~as.factor(Site)))
#' detach(Lodge)
#' 
NULL





#' Long-tailed distributions to illustrate Kruskal Wallis test
#' 
#' Data for Exercise 10.45
#' 
#' 
#' @name Longtail
#' @docType data
#' @format A data frame with 60 observations on the following 6 variables.
#' \describe{ 
#' \item{GroupA}{a numeric vector} 
#' \item{GroupB}{a numeric vector} 
#' \item{GroupC}{a numeric vector}
#' \item{score}{a numeric vector} 
#' \item{Group}{a numeric vector} 
#' \item{Ranks}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Longtail)
#' attach(Longtail)
#' boxplot(score~Group)
#' kruskal.test(score~as.factor(Group))
#' anova(lm(score~as.factor(Group)))
#' detach(Longtail)
#' 
NULL





#' Reading skills of 24 matched low ability students
#' 
#' Data for Example 7.18
#' 
#' 
#' @name Lowabil
#' @docType data
#' @format A data frame with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{Pair}{a numeric vector} 
#' \item{Experimt}{a numeric vector} 
#' \item{Control}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Lowabil)
#' attach(Lowabil)
#' DIF <- Experimt - Control
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(Experimt,Control,paired=TRUE)
#' detach(Lowabil)
#' remove(DIF)
#' 
NULL





#' Magnesium concentration and distances between samples
#' 
#' Data for Exercise 9.9
#' 
#' 
#' @name Magnesiu
#' @docType data
#' @format A data frame with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{distance}{a numeric vector}
#' \item{magnesiu}{a numeric vector} 
#' \item{SRES1}{a numeric vector} 
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Magnesiu)
#' attach(Magnesiu)
#' model <- lm(magnesiu~distance)
#' plot(distance,magnesiu)
#' abline(model)
#' summary(model)
#' detach(Magnesiu)
#' remove(model)
#' 
NULL





#' Amounts awarded in 17 malpractice cases
#' 
#' Data for Exercise 5.73
#' 
#' 
#' @name Malpract
#' @docType data
#' @format A data frame with 17 observations on the following variable.
#' \describe{ 
#' \item{award}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Malpract)
#' attach(Malpract)
#' SIGN.test(award,conf.level=.90)
#' detach(Malpract)
#' 
NULL





#' Advertised salaries offered general managers of major corporations in 1995
#' 
#' Data for Exercise 5.81
#' 
#' 
#' @name Manager
#' @docType data
#' @format A data frame with 26 observations on the following variable.
#' \describe{ 
#' \item{salary}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Manager)
#' attach(Manager)
#' stem(salary)
#' SIGN.test(salary)
#' detach(Manager)
#' 
NULL





#' Percent of marked cars in 65 police departments in Florida
#' 
#' Data for Exercise 6.100
#' 
#' 
#' @name Marked
#' @docType data
#' @format A data frame with 65 observations on the following variable.
#' \describe{ 
#' \item{percent}{a numeric vector}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Marked)
#' attach(Marked)
#' EDA(percent)
#' t.test(percent,mu=60,alternative="greater")
#' SIGN.test(percent,md=60,alternative="greater")
#' detach(Marked)
#' 
NULL





#' Standardized math competency for a group of entering freshmen at a small
#' community college
#' 
#' Data for Exercise 5.26
#' 
#' 
#' @name Mathcomp
#' @docType data
#' @format A data frame with 31 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Mathcomp)
#' attach(Mathcomp)
#' stem(score)
#' EDA(score)
#' detach(Mathcomp)
#' 
NULL





#' Math proficiency and SAT scores by states
#' 
#' Data for Exercise 9.24, Example 9.1, and Example 9.6
#' 
#' 
#' @name Mathpro
#' @docType data
#' @format A data frame with 51 observations on the following 10 variables.
#' \describe{ 
#' \item{state1}{a factor with levels \code{} \code{Conn}
#' \code{D.C.} \code{Del} \code{Ga} \code{Hawaii} \code{Ind} \code{Maine}
#' \code{Mass} \code{Md} \code{N.C.} \code{N.H.} \code{N.J.} \code{N.Y.}
#' \code{Ore} \code{Pa} \code{R.I.} \code{S.C.} \code{Va} \code{Vt}}
#' \item{Sat.M1}{a numeric vector} 
#' \item{Profic1}{a numeric vector} 
#' \item{state2}{a factor with levels \code{} \code{Ala}
#' \code{Alaska} \code{Ariz} \code{Ark} \code{Calif} \code{Colo} \code{Fla}
#' \code{Idaho} \code{Ill} \code{Iowa} \code{Kan} \code{Ky} \code{La}
#' \code{Mich} \code{Minn} \code{Miss} \code{Mo} \code{Mont} \code{N.D.}
#' \code{N.M.} \code{Neb} \code{Nev} \code{Ohio} \code{Okla} \code{S.D.}
#' \code{Tenn} \code{Texas} \code{Utah} \code{W.V.} \code{Wash} \code{Wis}
#' \code{Wyo}} 
#' \item{Sat.M2}{a numeric vector} 
#' \item{Profic2}{a numeric vector} 
#' \item{state}{a factor with levels \code{Ala}
#' \code{Alaska} \code{Ariz} \code{Ark} \code{Calif} \code{Colo} \code{Conn}
#' \code{D.C.} \code{Del} \code{Fla} \code{Ga} \code{Hawaii} \code{Idaho}
#' \code{Ill} \code{Ind} \code{Iowa} \code{Kan} \code{Ky} \code{La}
#' \code{Maine} \code{Mass} \code{Md} \code{Mich} \code{Minn} \code{Miss}
#' \code{Mo} \code{Mont} \code{N.C.} \code{N.D.} \code{N.H.} \code{N.J.}
#' \code{N.M.} \code{N.Y.} \code{Neb} \code{Nev} \code{Ohio} \code{Okla}
#' \code{Ore} \code{Pa} \code{R.I.} \code{S.C.} \code{S.D.} \code{Tenn}
#' \code{Texas} \code{Utah} \code{Va} \code{Vt} \code{W.V.} \code{Wash}
#' \code{Wis} \code{Wyo}} 
#' \item{Sat.M}{a numeric vector}
#' \item{Profic}{a numeric vector} 
#' \item{Group}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Mathpro)
#' attach(Mathpro)
#' model <- lm(Sat.M1~Profic1)
#' plot(Profic1,Sat.M1)
#' abline(model)
#' model
#' detach(Mathpro)
#' remove(model)
#' 
NULL





#' Standardized math test scores for 30 students
#' 
#' Data for Exercise 1.69
#' 
#' 
#' @name MATH
#' @docType data
#' @format A data frame with 30 observations on the following variable.
#' \describe{ 
#' \item{math}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(MATH)
#' attach(MATH)
#' hist(math,col="pink")
#' CharlieZ <- (62-mean(math))/sd(math)
#' CharlieZ
#' detach(MATH)
#' remove(CharlieZ)
#' 
NULL





#' Error scores for four groups of experimental animals running a maze
#' 
#' Data for Exercise 10.13
#' 
#' 
#' @name Maze
#' @docType data
#' @format A data frame with 32 observations on the following 6 variables.
#' \describe{ 
#' \item{CondA}{a numeric vector} 
#' \item{CondB}{a numeric vector} 
#' \item{CondC}{a numeric vector}
#' \item{CondD}{a numeric vector} 
#' \item{score}{a numeric vector} 
#' \item{condition}{a factor with levels \code{CondA}
#' \code{CondB} \code{CondC} \code{CondD}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Maze)
#' attach(Maze)
#' boxplot(score~condition)
#' anova(lm(score~condition))
#' detach(Maze)
#' 
NULL





#' Illustrates test of equality of medians with the Kruskal Wallis test
#' 
#' Data for Exercise 10.52
#' 
#' 
#' @name Median
#' @docType data
#' @format A data frame with 15 observations on the following 3 variables.
#' \describe{ 
#' \item{Sample1}{a numeric vector} 
#' \item{Sample2}{a numeric vector} 
#' \item{Sample3}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Median)
#' attach(Median)
#' STACKED <-stack(Median)
#' STACKED[1:5,]
#' boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
#' anova(lm(values~ind,data=STACKED))
#' kruskal.test(values~ind,data=STACKED)
#' remove(STACKED)
#' detach(Median)
#' 
NULL





#' Median mental ages of 16 girls
#' 
#' Data for Exercise 6.52
#' 
#' 
#' @name Mental
#' @docType data
#' @format A data frame with 16 observations on the following variable.
#' \describe{ 
#' \item{age}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Mental)
#' attach(Mental)
#' SIGN.test(age,md=100)
#' detach(Mental)
#' 
NULL





#' Concentration of mercury in 25 lake trout
#' 
#' Data for Example 1.9
#' 
#' 
#' @name Mercury
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{mercury}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Mercury)
#' attach(Mercury)
#' stem(mercury)
#' detach(Mercury)
#' 
NULL





#' Monthly rental costs in metro areas with 1 million or more persons
#' 
#' Data for Exercise 5.117
#' 
#' 
#' @name Metrent
#' @docType data
#' @format A data frame with 46 observations on the following variable.
#' \describe{ 
#' \item{rent}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Metrent)
#' attach(Metrent)
#' EDA(rent)
#' t.test(rent,conf.level=.99)$conf
#' detach(Metrent)
#' 
NULL





#' Twenty scores on the Miller personality test
#' 
#' Data for Exercise 1.41
#' 
#' 
#' @name Miller1
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{miller}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Miller1)
#' attach(Miller1)
#' stem(miller)
#' stem(miller,scale=2)
#' detach(Miller1)
#' 
NULL





#' Miller personality test scores for a group of college students applying for
#' graduate school
#' 
#' Data for Example 5.7
#' 
#' 
#' @name Miller
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{miller}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Miller)
#' attach(Miller)
#' stem(miller)
#' fivenum(miller)
#' boxplot(miller)
#' qqnorm(miller,col="blue")
#' qqline(miller,col="red")
#' detach(Miller)
#' 
NULL





#' Moisture content and depth of core sample for marine muds in eastern
#' Louisiana
#' 
#' Data for Exercise 9.37
#' 
#' 
#' @name Moisture
#' @docType data
#' @format A data frame with 16 observations on the following 4 variables.
#' \describe{ 
#' \item{depth}{a numeric vector} 
#' \item{moisture}{a numeric vector} 
#' \item{lnmoist}{a numeric vector}
#' \item{depthsq}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Moisture)
#' attach(Moisture)
#' model <- lm(moisture~depth)
#' plot(depth,resid(model))
#' detach(Moisture)
#' remove(model)
#' 
NULL





#' Carbon monoxide emitted by smoke stacks of a manufacturer and a competitor
#' 
#' Data for Exercise 7.45
#' 
#' 
#' @name Monoxide
#' @docType data
#' @format A data frame with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{manufac}{a numeric vector} 
#' \item{compet}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Monoxide)
#' attach(Monoxide)
#' t.test(manufac,compet)
#' wilcox.test(manufac,compet)
#' detach(Monoxide)
#' 
NULL





#' Moral attitude scale on 15 subjects before and after viewing a movie
#' 
#' Data for Exercise 7.53
#' 
#' 
#' @name Movie
#' @docType data
#' @format A data frame with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{Before}{a numeric vector} 
#' \item{After}{a numeric vector} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Movie)
#' attach(Movie)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' t.test(After,Before,paired=TRUE,conf.level=.99)
#' wilcox.test(After,Before,paired=TRUE)
#' detach(Movie)
#' 
NULL





#' Improvement scores for identical twins taught music recognition by two
#' techniques
#' 
#' Data for Exercise 7.59
#' 
#' 
#' @name Music
#' @docType data
#' @format A data frame with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{Method1}{a numeric vector} 
#' \item{Method2}{a numeric vector} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Music)
#' attach(Music)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' t.test(Method1,Method2,paired=TRUE)
#' detach(Music)
#' 
NULL





#' Estimated value of a brand name product and the conpany's revenue
#' 
#' Data for Exercises 2.28, 9.19, and Example 2.8
#' 
#' 
#' @name Name
#' @docType data
#' @format A data frame with 42 observations on the following 3 variables.
#' \describe{ 
#' \item{Brand}{a factor with levels \code{Band-Aid}
#' \code{Barbie} \code{Birds Eye} \code{Budweiser} \code{Camel} \code{Campbell}
#' \code{Carlsberg} \code{Coca-Cola} \code{Colgate} \code{Del Monte}
#' \code{Fisher-Price} \code{Gordon's} \code{Green Giant} \code{Guinness}
#' \code{Haagen-Dazs} \code{Heineken} \code{Heinz} \code{Hennessy}
#' \code{Hermes} \code{Hershey} \code{Ivory} \code{Jell-o} \code{Johnnie
#' Walker} \code{Kellogg} \code{Kleenex} \code{Kraft} \code{Louis Vuitton}
#' \code{Marlboro} \code{Nescafe} \code{Nestle} \code{Nivea} \code{Oil of Olay}
#' \code{Pampers} \code{Pepsi-Cola} \code{Planters} \code{Quaker} \code{Sara
#' Lee} \code{Schweppes} \code{Smirnoff} \code{Tampax} \code{Winston}
#' \code{Wrigley's}}
#' \item{value}{a numeric vector}
#' \item{revenue}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Name)
#' attach(Name)
#' plot(revenue,value)
#' model <- lm(value~revenue)
#' abline(model)
#' cor(value,revenue)
#' summary(model)
#' detach(Name)
#' remove(model)
#' 
NULL





#' Efficiency of pit crews for three major NASCAR teams
#' 
#' Data for Example 10.53
#' 
#' 
#' @name Nascar
#' @docType data
#' @format A data frame with 36 observations on the following 6 variables.
#' \describe{ 
#' \item{TeamA}{a numeric vector} 
#' \item{TeamB}{a numeric vector} 
#' \item{TeamC}{a numeric vector} 
#' \item{Time}{a numeric vector} 
#' \item{Team}{a numeric vector} 
#' \item{Ranks}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Nascar)
#' attach(Nascar)
#' boxplot(Time~Team)
#' anova(lm(Time~as.factor(Team)))
#' detach(Nascar)
#' 
NULL





#' Reaction effects of 4 drugs on 25 subjects with a nervous disorder
#' 
#' Data for Exercise 10.3
#' 
#' 
#' @name Nervous
#' @docType data
#' @format A data frame with 25 observations on the following 2 variables.
#' \describe{ 
#' \item{react}{a numeric vector} 
#' \item{drug}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Nervous)
#' attach(Nervous)
#' boxplot(react~drug)
#' anova(lm(react~as.factor(drug)))
#' detach(Nervous)
#' 
NULL





#' Daily profits for 20 newsstands
#' 
#' Data for Exercise 1.43
#' 
#' 
#' @name Newsstand
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{profit}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Newsstand)
#' attach(Newsstand)
#' stem(profit)
#' stem(profit,scale=3)
#' detach(Newsstand)
#' 
NULL





#' Rating, time in 40-yard dash, and weight of top defensive linemen in the
#' 1994 NFL draft
#' 
#' Data for Exercise 9.63
#' 
#' 
#' @name Nfldraf2
#' @docType data
#' @format A data frame with 47 observations on the following variable.
#' \describe{ 
#' \item{Rating.forty.weight}{a factor with levels
#' \code{5.000000000e+000 4.870000000e+000 2.810000000e+002}
#' \code{5.000000000e+000 4.900000000e+000 2.810000000e+002}
#' \code{5.100000000e+000 5.100000000e+000 2.980000000e+002}
#' \code{5.200000000e+000 5.000000000e+000 2.920000000e+002}
#' \code{5.200000000e+000 5.030000000e+000 2.920000000e+002}
#' \code{5.300000000e+000 4.780000000e+000 2.720000000e+002}
#' \code{5.300000000e+000 4.800000000e+000 2.720000000e+002}
#' \code{5.400000000e+000 4.890000000e+000 2.450000000e+002}
#' \code{5.400000000e+000 4.900000000e+000 2.450000000e+002}
#' \code{5.400000000e+000 5.000000000e+000 2.650000000e+002}
#' \code{5.400000000e+000 5.000000000e+000 2.770000000e+002}
#' \code{5.400000000e+000 5.030000000e+000 2.770000000e+002}
#' \code{5.500000000e+000 4.800000000e+000 2.640000000e+002}
#' \code{5.500000000e+000 4.820000000e+000 2.640000000e+002}
#' \code{5.600000000e+000 4.900000000e+000 2.470000000e+002}
#' \code{5.600000000e+000 5.100000000e+000 2.870000000e+002}
#' \code{5.600000000e+000 5.130000000e+000 2.870000000e+002}
#' \code{5.700000000e+000 4.880000000e+000 2.830000000e+002}
#' \code{5.700000000e+000 4.900000000e+000 2.830000000e+002}
#' \code{5.800000000e+000 4.800000000e+000 2.390000000e+002}
#' \code{5.800000000e+000 4.850000000e+000 2.390000000e+002}
#' \code{5.800000000e+000 4.900000000e+000 2.710000000e+002}
#' \code{5.800000000e+000 4.930000000e+000 2.710000000e+002}
#' \code{5.900000000e+000 4.700000000e+000 2.600000000e+002}
#' \code{5.900000000e+000 4.720000000e+000 2.600000000e+002}
#' \code{6.000000000e+000 5.100000000e+000 2.830000000e+002}
#' \code{6.200000000e+000 5.100000000e+000 3.120000000e+002}
#' \code{6.400000000e+000 4.800000000e+000 2.840000000e+002}
#' \code{6.400000000e+000 4.840000000e+000 2.840000000e+002}
#' \code{6.400000000e+000 5.070000000e+000 2.850000000e+002}
#' \code{6.400000000e+000 5.100000000e+000 2.850000000e+002}
#' \code{6.600000000e+000 5.090000000e+000 3.000000000e+002}
#' \code{6.600000000e+000 5.100000000e+000 3.000000000e+002}
#' \code{6.900000000e+000 4.680000000e+000 2.550000000e+002}
#' \code{6.900000000e+000 4.700000000e+000 2.550000000e+002}
#' \code{7.000000000e+000 5.060000000e+000 2.890000000e+002}
#' \code{7.000000000e+000 5.100000000e+000 2.890000000e+002}
#' \code{7.200000000e+000 4.900000000e+000 2.920000000e+002}
#' \code{7.500000000e+000 4.900000000e+000 2.760000000e+002}
#' \code{7.500000000e+000 4.940000000e+000 2.760000000e+002}
#' \code{8.200000000e+000 4.720000000e+000 3.130000000e+002}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Nfldraf2)
#' 
NULL





#' Rating, time in 40-yard dash, and weight of top offensive linemen in the
#' 1994 NFL draft
#' 
#' Data for Exercises 9.10 and 9.16
#' 
#' 
#' @name Nfldraft
#' @docType data
#' @format A data frame with 29 observations on the following variable.
#' \describe{ 
#' \item{Rating.forty.weight}{a factor with levels
#' \code{5.000000000e+000 5.300000000e+000 3.100000000e+002}
#' \code{5.200000000e+000 5.230000000e+000 2.740000000e+002}
#' \code{5.200000000e+000 5.260000000e+000 2.950000000e+002}
#' \code{5.300000000e+000 5.180000000e+000 3.010000000e+002}
#' \code{5.400000000e+000 5.290000000e+000 3.010000000e+002}
#' \code{5.500000000e+000 5.090000000e+000 2.800000000e+002}
#' \code{5.500000000e+000 5.260000000e+000 3.050000000e+002}
#' \code{5.500000000e+000 5.560000000e+000 3.590000000e+002}
#' \code{5.700000000e+000 5.140000000e+000 2.770000000e+002}
#' \code{5.700000000e+000 5.290000000e+000 2.860000000e+002}
#' \code{5.900000000e+000 5.200000000e+000 3.130000000e+002}
#' \code{5.900000000e+000 5.250000000e+000 2.920000000e+002}
#' \code{6.000000000e+000 5.030000000e+000 2.740000000e+002}
#' \code{6.000000000e+000 5.270000000e+000 2.850000000e+002}
#' \code{6.000000000e+000 5.290000000e+000 3.050000000e+002}
#' \code{6.100000000e+000 5.270000000e+000 2.850000000e+002}
#' \code{6.100000000e+000 5.350000000e+000 3.210000000e+002}
#' \code{6.200000000e+000 5.230000000e+000 2.900000000e+002}
#' \code{6.200000000e+000 5.290000000e+000 2.890000000e+002}
#' \code{6.300000000e+000 5.360000000e+000 3.110000000e+002}
#' \code{6.400000000e+000 5.060000000e+000 3.150000000e+002}
#' \code{6.400000000e+000 5.260000000e+000 3.020000000e+002}
#' \code{6.500000000e+000 4.940000000e+000 2.850000000e+002}
#' \code{6.500000000e+000 5.180000000e+000 3.250000000e+002}
#' \code{7.000000000e+000 5.200000000e+000 3.250000000e+002}
#' \code{7.000000000e+000 5.360000000e+000 3.170000000e+002}
#' \code{7.100000000e+000 5.060000000e+000 2.890000000e+002}
#' \code{7.200000000e+000 5.200000000e+000 3.150000000e+002}
#' \code{7.600000000e+000 5.150000000e+000 3.030000000e+002}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Nfldraft)
#' attach(Nfldraft)
#' detach(Nfldraft)
#' 
NULL





#' Nicotine content versus sales for 8 major brands of cigarettes
#' 
#' Data for Exercise 9.21
#' 
#' 
#' @name Nicotine
#' @docType data
#' @format A data frame with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{nicotine}{a numeric vector} 
#' \item{sales}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Nicotine)
#' attach(Nicotine)
#' model <- lm(sales~nicotine)
#' summary(model)
#' detach(Nicotine)
#' remove(model)
#' 
NULL





#' Price of oranges versus size of the harvest
#' 
#' Data for Exercise 9.61
#' 
#' 
#' @name Orange
#' @docType data
#' @format A data frame with 6 observations on the following 2 variables.
#' \describe{ 
#' \item{harvest}{a numeric vector} 
#' \item{price}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Orange)
#' attach(Orange)
#' summary(lm(price~harvest))
#' detach(Orange)
#' 
NULL





#' Salaries of members of the Baltimore Orioles baseball team
#' 
#' Data for Example 1.3
#' 
#' 
#' @name Orioles
#' @docType data
#' @format A data frame with 27 observations on the following 3 variables.
#' \describe{ 
#' \item{first.name}{a factor with levels \code{Albert}
#' \code{Arthur} \code{B.J.} \code{Brady} \code{Cal} \code{Charles}
#' \code{dl-Delino} \code{dl-Scott} \code{Doug} \code{Harold} \code{Heathcliff}
#' \code{Jeff} \code{Jesse} \code{Juan} \code{Lenny} \code{Mike} \code{Rich}
#' \code{Ricky} \code{Scott} \code{Sidney} \code{Will} \code{Willis}}
#' \item{last.name}{a factor with levels \code{Amaral} \code{Anderson}
#' \code{Baines} \code{Belle} \code{Bones} \code{Bordick} \code{Clark}
#' \code{Conine} \code{Deshields} \code{Erickson} \code{Fetters} \code{Garcia}
#' \code{Guzman} \code{Johns} \code{Johnson} \code{Kamieniecki} \code{Mussina}
#' \code{Orosco} \code{Otanez} \code{Ponson} \code{Reboulet} \code{Rhodes}
#' \code{Ripken Jr.} \code{Slocumb} \code{Surhoff} \code{Timlin}
#' \code{Webster}} 
#' \item{X1999salary}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Orioles)
#' 
NULL





#' Arterial blood pressure of 11 subjects before and after receiving oxytocin
#' 
#' Data for Exercise 7.86
#' 
#' 
#' @name Oxytocin
#' @docType data
#' @format A data frame with 11 observations on the following 3 variables.
#' \describe{ 
#' \item{Subject}{a numeric vector} 
#' \item{Before}{a numeric vector} 
#' \item{After}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Oxytocin)
#' attach(Oxytocin)
#' DIF <- Before - After
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(Before,After,paired=TRUE)
#' detach(Oxytocin)
#' 
NULL





#' Education backgrounds of parents of entering freshmen at a state university
#' 
#' Data for Exercise 1.32
#' 
#' 
#' @name Parented
#' @docType data
#' @format A data frame with 6 observations on the following 3 variables.
#' \describe{ 
#' \item{Educat}{a factor with levels \code{4yr college
#' degree} \code{Doctoral degree} \code{Grad degree} \code{H.S grad or less}
#' \code{Some college} \code{Some grad school}} 
#' \item{Mother}{a numeric vector} 
#' \item{Father}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Parented)
#' attach(Parented)
#' MAT <- cbind(Mother, Father)
#' row.names(MAT) <- Educat 
#' MAT 
#' barplot(t(MAT),beside=TRUE,legend=TRUE,col=c("blue","red")) 
#' detach(Parented) 
#' remove(MAT)
#' 
NULL





#' Years of experience and number of tickets given by patrolpersons in New York
#' City
#' 
#' Data for Example 9.3
#' 
#' 
#' @name Patrol
#' @docType data
#' @format A data frame with 10 observations on the following 7 variables.
#' \describe{ 
#' \item{tickets}{a numeric vector} 
#' \item{years}{a numeric vector} 
#' \item{ln.tickets.}{a numeric vector}
#' \item{SRES1}{a numeric vector} 
#' \item{FITS1}{a numeric vector} 
#' \item{SRES2}{a numeric vector} 
#' \item{FITS2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Patrol)
#' attach(Patrol)
#' model <- lm(tickets~years)
#' summary(model)
#' detach(Patrol)
#' remove(model)
#' 
NULL





#' Karl Pearson's data on heights of brothers and sisters
#' 
#' Data for Exercise 2.20
#' 
#' 
#' @name Pearson
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{brother}{a numeric vector} 
#' \item{sister}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Pearson)
#' attach(Pearson)
#' plot(brother,sister)
#' cor(brother,sister)
#' detach(Pearson)
#' 
NULL





#' Length of long-distance phone calls for a small business firm
#' 
#' Data for Exercise 6.95
#' 
#' 
#' @name Phone
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Phone)
#' attach(Phone)
#' qqnorm(time)
#' qqline(time)
#' shapiro.test(time)
#' SIGN.test(time,md=5,alternative="greater")
#' detach(Phone)
#' 
NULL





#' Number of poisonings reported to 16 poison control centers
#' 
#' Data for Exercise 1.113
#' 
#' 
#' @name Poison
#' @docType data
#' @format A data frame with 6 observations on the following 2 variables.
#' \describe{ 
#' \item{Type}{a factor with levels \code{Alcohol}
#' \code{Cleaning agent} \code{Cosmetics} \code{Drugs} \code{Insecticides}
#' \code{Plants}} 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Poison)
#' attach(Poison)
#' names(number) <- Type
#' barplot(number,col="red")
#' 
NULL





#' Political party and gender in a voting district
#' 
#' Data for Example 8.3
#' 
#' 
#' @name Politic
#' @docType data
#' @format A data frame with 250 observations on the following 2 variables.
#' \describe{ 
#' \item{Party}{a numeric vector} 
#' \item{Gender}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Politic)
#' attach(Politic)
#' table(Party,Gender)
#' chisq.test(table(Party,Gender))
#' detach(Politic)
#' 
NULL





#' Air pollution index for 15 randomly selected days for a major western city
#' 
#' Data for Exercise 5.59
#' 
#' 
#' @name Pollutio
#' @docType data
#' @format A data frame with 15 observations on the following variable.
#' \describe{ 
#' \item{inde}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Pollutio)
#' attach(Pollutio)
#' EDA(inde)
#' t.test(inde,conf.level=.98)$conf
#' detach(Pollutio)
#' 
NULL





#' Porosity measurements on 20 samples of Tensleep Sandstone, Pennsylvanian
#' from Bighorn Basin in Wyoming
#' 
#' Data for Exercise 5.86
#' 
#' 
#' @name Porosity
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{porosity}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Porosity)
#' attach(Porosity)
#' stem(porosity)
#' fivenum(porosity)
#' boxplot(porosity)
#' detach(Porosity)
#' 
NULL





#' Percent poverty and crime rate for selected cities
#' 
#' Data for Exercise 9.11 and 9.17
#' 
#' 
#' @name Poverty
#' @docType data
#' @format A data frame with 20 observations on the following 6 variables.
#' \describe{ 
#' \item{City}{a factor with levels \code{Atlanta}
#' \code{Buffalo} \code{Cincinnati} \code{Cleveland} \code{Dayton, O}
#' \code{Detroit} \code{Flint, Mich} \code{Fresno, C} \code{Gary, Ind}
#' \code{Hartford, C} \code{Laredo} \code{Macon, Ga} \code{Miami}
#' \code{Milwaukee} \code{New Orleans} \code{Newark, NJ} \code{Rochester,NY}
#' \code{Shreveport} \code{St. Louis} \code{Waco, Tx}} 
#' \item{Poverty}{a numeric vector} 
#' \item{Crime}{a numeric vector}
#' \item{cindex}{a numeric vector} 
#' \item{popu}{a numeric vector} 
#' \item{ratio}{a numeric vector}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Poverty)
#' attach(Poverty)
#' plot(Crime,Poverty)
#' model <- lm(Poverty~Crime)
#' abline(model)
#' summary(model)
#' detach(Poverty)
#' remove(model)
#' 
NULL





#' Robbery rates versus percent low income in 8 precincts
#' 
#' Data for Exercise 2.2 and 2.38
#' 
#' 
#' @name Precinct
#' @docType data
#' @format A data frame with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{rate}{a numeric vector} 
#' \item{income}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Precinct)
#' attach(Precinct)
#' plot(rate,income,main="Exercise 2.2")
#' model <- lm(income~rate)
#' model
#' abline(model,col="green")
#' detach(Precinct)
#' 
NULL





#' Racial prejudice measured on a sample of 25 high school students
#' 
#' Data for Exercise 5.10 and 5.22
#' 
#' 
#' @name Prejudic
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{prejud}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Prejudic)
#' attach(Prejudic)
#' EDA(prejud)
#' detach(Prejudic)
#' 
NULL





#' Ages at inauguration and death of U.S. presidents
#' 
#' Data for Exercise 1.126
#' 
#' 
#' @name Presiden
#' @docType data
#' @format A data frame with 43 observations on the following 5 variables.
#' \describe{ 
#' \item{firs}{a factor with levels \code{A.} \code{B.}
#' \code{C.} \code{D.} \code{F.} \code{G.} \code{G. W.} \code{H.} \code{J.}
#' \code{L.} \code{M.} \code{R.} \code{T.} \code{U.} \code{W.} \code{Z.}}
#' \item{Presiden}{a factor with levels \code{Adams} \code{Arthur}
#' \code{Buchanan} \code{Bush} \code{Carter} \code{Cleveland} \code{Clinton}
#' \code{Coolidge} \code{Eisenhower} \code{Fillmore} \code{Ford}
#' \code{Garfield} \code{Grant} \code{Harding} \code{Harrison} \code{Hayes}
#' \code{Hoover} \code{Jackson} \code{Jefferson} \code{Johnson} \code{Kennedy}
#' \code{Lincoln} \code{Madison} \code{McKinley} \code{Monroe} \code{Nixon}
#' \code{Pierce} \code{Polk} \code{Reagan} \code{Roosevelt} \code{Taft}
#' \code{Taylor} \code{Truman} \code{Tyler} \code{VanBuren} \code{Washington}
#' \code{Wilson}} 
#' \item{Birt}{a factor with levels \code{ARK}
#' \code{CAL} \code{CONN} \code{GA} \code{IA} \code{ILL} \code{KY} \code{MASS}
#' \code{MO} \code{NC} \code{NEB} \code{NH} \code{NJ} \code{NY} \code{OH}
#' \code{PA} \code{SC} \code{TEX} \code{VA} \code{VT}}
#' \item{Inaugage}{a numeric vector} 
#' \item{Deathage}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Presiden)
#' attach(Presiden)
#' table(Birt)
#' pie(table(Birt))
#' stripchart(x=list(Inaugage,Deathage),method="stack",
#' group.names=c("Inaugural Age","Death Age"),col=c("green","brown"),pch=19)
#' detach(Presiden) 
#' 
NULL





#' Degree of confidence in the press versus education level for 20 randomly
#' selected persons
#' 
#' Data for Exercise 9.55
#' 
#' 
#' @name Press
#' @docType data
#' @format A data frame with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{educat}{a numeric vector} 
#' \item{confid}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Press)
#' attach(Press)
#' summary(lm(confid~educat))
#' detach(Press)
#' 
NULL





#' Klopfer's prognostic rating scale for subjects receiving behavior
#' modification therapy
#' 
#' Data for Exercise 6.61
#' 
#' 
#' @name Prognost
#' @docType data
#' @format A data frame with 15 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Prognost)
#' attach(Prognost)
#' EDA(score)
#' t.test(score,mu=9)
#' detach(Prognost)
#' 
NULL





#' Effects of four different methods of programmed learning for statistics
#' students
#' 
#' Data for Exercise 10.17
#' 
#' 
#' @name Program
#' @docType data
#' @format A data frame with 11 observations on the following 4 variables.
#' \describe{ 
#' \item{Method1}{a numeric vector} 
#' \item{Method2}{a numeric vector} 
#' \item{Method3}{a numeric vector}
#' \item{Method4}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Program)
#' attach(Program)
#' STACKED <-stack(Program)
#' STACKED[1:5,]
#' boxplot(values~ind,col=c("red","blue","green","yellow"),data=STACKED)
#' anova(lm(values~ind,data=STACKED))
#' remove(STACKED)
#' detach(Program)
#' 
NULL





#' PSAT scores versus SAT scores
#' 
#' Data for Exercise 2.50
#' 
#' 
#' @name Psat
#' @docType data
#' @format A data frame with 7 observations on the following 4 variables.
#' \describe{
#' \item{psat}{a numeric vector} 
#' \item{sat}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Psat)
#' attach(Psat)
#' model <- lm(sat~psat)
#' plot(psat,resid(model))
#' detach(Psat)
#' 
NULL





#' Correct responses for 24 students in a psychology experiment
#' 
#' Data for Exercise 1.42
#' 
#' 
#' @name Psych
#' @docType data
#' @format A data frame with 23 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Psych)
#' attach(Psych)
#' stem(score)
#' EDA(score)
#' detach(Psych)
#' 
NULL





#' Weekly incomes of a random sample of 50 Puerto Rican families in Miami
#' 
#' Data for Exercise 5.22 and 5.65
#' 
#' 
#' @name Puerto
#' @docType data
#' @format A data frame with 50 observations on the following variable.
#' \describe{ 
#' \item{income}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Puerto)
#' attach(Puerto)
#' EDA(income)
#' t.test(income,conf.level=.90)$conf
#' detach(Puerto)
#' 
NULL





#' Plasma LDL levels in two groups of quail
#' 
#' Data for Exercise 1.53, 1.77, 1.88, 5.66, and 7.50
#' 
#' 
#' @name Quail
#' @docType data
#' @format A data frame with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{placebo}{a numeric vector}
#' \item{treatmen}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Quail)
#' attach(Quail)
#' boxplot(placebo,treatmen,names=c("Placebo","Treatment"),
#' horizontal=TRUE,xlab="LDL level",col=c("lightblue","yellow"))
#' boxplot(placebo,treatmen,names=c("Placebo","Treatment"),
#' ylab="LDL level",col=c("lightblue","yellow"))
#' detach(Quail)
#' 
NULL





#' Quality control test scores on two manufacturing processes
#' 
#' Data for Exercise 7.81
#' 
#' 
#' @name Quality
#' @docType data
#' @format A data frame with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{Process1}{a numeric vector}
#' \item{Process2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Quality)
#' attach(Quality)
#' qqnorm(Process1)
#' qqline(Process1)
#' shapiro.test(Process1)
#' qqnorm(Process2)
#' qqline(Process2)
#' shapiro.test(Process2)
#' t.test(Process1,Process2)
#' detach(Quality)
#' 
NULL





#' Rainfall in an area of west central Kansas and four surrounding counties
#' 
#' Data for Exercise 9.8
#' 
#' 
#' @name Rainks
#' @docType data
#' @format A data frame with 35 observations on the following 5 variables.
#' \describe{ 
#' \item{rain}{a numeric vector} 
#' \item{x1}{a numeric vector} 
#' \item{x2}{a numeric vector} 
#' \item{x3}{a numeric vector} 
#' \item{x4}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Rainks)
#' attach(Rainks)
#' cor(Rainks)
#' lm(rain~x2)
#' detach(Rainks)
#' 
NULL





#' Research and development expenditures and sales of a large company
#' 
#' Data for Exercise 9.36 and Example 9.8
#' 
#' 
#' @name Randd
#' @docType data
#' @format A data frame with 12 observations on the following 5 variables.
#' \describe{ 
#' \item{rd}{a numeric vector} 
#' \item{sales}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' \item{RESI1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Randd)
#' attach(Randd)
#' plot(rd,sales)
#' model <- lm(sales~rd)
#' abline(model)
#' summary(model)
#' # plot(model)
#' detach(Randd)
#' remove(model)
#' 
NULL





#' Grade point averages versus teacher's ratings
#' 
#' Data for Example 2.6
#' 
#' 
#' @name Ratings
#' @docType data
#' @format A data frame with 250 observations on the following 7 variables.
#' \describe{ 
#' \item{F}{a numeric vector} 
#' \item{D}{a numeric vector} 
#' \item{C}{a numeric vector} 
#' \item{B}{a numeric vector} 
#' \item{A}{a numeric vector} 
#' \item{Rating}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{F}}
#' \item{GPA}{a numeric vector}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Ratings)
#' attach(Ratings)
#' boxplot(GPA~Rating,xlab="Teacher's Rating",ylab="GPA",main="Example 2.6",col="pink")
#' detach(Ratings)
#' 
NULL





#' Survival times of 20 rats exposed to high levels of radiation
#' 
#' Data for Exercise 1.52, 1.76, 5.62, and 6.44
#' 
#' 
#' @name Rat
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{survival.time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Rat)
#' attach(Rat)
#' hist(survival.time)
#' qqnorm(survival.time,col="blue")
#' qqline(survival.time,col="red")
#' t.test(survival.time)$conf
#' t.test(survival.time,mu=100,alternative="greater")
#' detach(Rat)
#' 
NULL





#' Threshold reaction time for persons subjected to emotional stress
#' 
#' Data for Example 6.11
#' 
#' 
#' @name Reaction
#' @docType data
#' @format A data frame with 12 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Reaction)
#' attach(Reaction)
#' SIGN.test(time,md=15,alternative="less")
#' detach(Reaction)
#' 
NULL





#' Standardized reading scores for 30 fifth graders
#' 
#' Data for Exercise 1.72 and 2.10
#' 
#' 
#' @name Reading
#' @docType data
#' @format A data frame with 30 observations on the following 4 variables.
#' \describe{ 
#' \item{reading}{a numeric vector} 
#' \item{sorted}{a numeric vector} 
#' \item{trimmed}{a numeric vector}
#' \item{winsoriz}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Reading)
#' attach(Reading)
#' EDA(reading)
#' detach(Reading)
#' 
NULL





#' Reading scores versus IQ scores
#' 
#' Data for Exercises 2.10 and 2.53
#' 
#' 
#' @name Readiq
#' @docType data
#' @format A data frame with 14 observations on the following 2 variables.
#' \describe{ 
#' \item{reading}{a numeric vector} 
#' \item{IQ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Readiq)
#' attach(Readiq)
#' plot(IQ,reading)
#' model <- lm(reading~IQ)
#' abline(model)
#' detach(Readiq)
#' 
NULL





#' Opinion on referendum by view on freedom of the press
#' 
#' Data for Exercise 8.20
#' 
#' 
#' @name Referend
#' @docType data
#' @format A data frame with 3 observations on the following 4 variables.
#' \describe{ 
#' \item{Response}{a factor with levels \code{A} \code{B}
#' \code{C}} 
#' \item{For}{a numeric vector} 
#' \item{Against}{a numeric vector} 
#' \item{undecide}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Referend)
#' attach(Referend)
#' chisq.test(Referend[,2:4])
#' detach(Referend)
#' 
NULL





#' Pollution index taken in three regions of the country
#' 
#' Data for Exercise 10.26
#' 
#' 
#' @name Region
#' @docType data
#' @format A data frame with 48 observations on the following 6 variables.
#' \describe{
#'  \item{West}{a numeric vector} 
#'  \item{Central}{a numeric vector} 
#'  \item{East}{a numeric vector} 
#'  \item{Index}{a numeric vector} 
#'  \item{Region}{a numeric vector}
#'  \item{Ranks}{a numeric vector} 
#'  }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Region)
#' attach(Region)
#' boxplot(Index~Region)
#' anova(lm(Index~as.factor(Region)))
#' detach(Region)
#' 
NULL





#' Maintenance cost versus age of cash registers in a department store
#' 
#' Data for Exercise 2.3, 2.39, and 2.54
#' 
#' 
#' @name Register
#' @docType data
#' @format A data frame with 9 observations on the following 4 variables.
#' \describe{ 
#' \item{age}{a numeric vector} 
#' \item{cost}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Register)
#' attach(Register)
#' plot(age,cost,main="Exercise 2.3")
#' model <- lm(cost~age)
#' abline(model)
#' plot(age,resid(model))
#' detach(Register)
#' 
NULL





#' Rehabilitative potential of 20 prison inmates as judged by two psychiatrists
#' 
#' Data for Exercise 7.61
#' 
#' 
#' @name Rehab
#' @docType data
#' @format A data frame with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{Psych1}{a numeric vector} 
#' \item{Psych2}{a numeric vector} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Rehab)
#' attach(Rehab)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' boxplot(Psych1,Psych2,names=c("Psychiatrist 1","Psychiatrist 2"),
#' col=c("pink","lightblue"))
#' t.test(Psych1,Psych2,paired=TRUE)
#' detach(Rehab)
#' 
NULL





#' Math placement test score for 35 freshmen females and 42 freshmen males
#' 
#' Data for Exercise 7.43
#' 
#' 
#' @name Remedial
#' @docType data
#' @format A data frame with 42 observations on the following 2 variables.
#' \describe{ 
#' \item{female}{a numeric vector} 
#' \item{male}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Remedial)
#' attach(Remedial)
#' boxplot(female,male,col=c("blue","red"))
#' wilcox.test(female,male,conf.int=TRUE)
#' t.test(female,male)
#' detach(Remedial)
#' 
NULL





#' Weekly rentals for 45 apartments
#' 
#' Data for Exercise 1.122
#' 
#' 
#' @name Rentals
#' @docType data
#' @format A data frame with 45 observations on the following variable.
#' \describe{ 
#' \item{rent}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Rentals)
#' attach(Rentals)
#' EDA(rent)
#' detach(Rentals)
#' 
NULL





#' Recorded times for repairing 22 automobiles involved in wrecks
#' 
#' Data for Exercise 5.77
#' 
#' 
#' @name Repair
#' @docType data
#' @format A data frame with 22 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Repair)
#' attach(Repair)
#' stem(time)
#' SIGN.test(time,conf.level=.98)
#' detach(Repair)
#' 
NULL





#' Length of employment versus gross sales for 10 employees of a large retail
#' store
#' 
#' Data for Exercise 9.59
#' 
#' 
#' @name Retail
#' @docType data
#' @format A data frame with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{months}{a numeric vector}
#' \item{sales}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Retail)
#' attach(Retail)
#' summary(lm(sales~months))
#' detach(Retail)
#' 
NULL





#' Oceanography data obtained at site 1 by scientist aboard the ship Ron Brown
#' 
#' Data for Exercise 2.9
#' 
#' 
#' @name Ronbrown1
#' @docType data
#' @format A data frame with 75 observations on the following 12 variables.
#' \describe{ 
#' \item{depth}{a numeric vector} 
#' \item{downtemp1}{a numeric vector} 
#' \item{downtemp2}{a numeric vector}
#' \item{downsalinity1}{a numeric vector}
#' \item{downsalinity2}{a numeric vector} 
#' \item{downdensity}{a numeric vector} 
#' \item{C7}{a numeric vector} 
#' \item{uptemp1}{a numeric vector} 
#' \item{uptemp2}{a numeric vector}
#' \item{upsalinity1}{a numeric vector} 
#' \item{upsalinity2}{a numeric vector} 
#' \item{updensity}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Ronbrown1)
#' attach(Ronbrown1)
#' plot(depth,downtemp1)
#' detach(Ronbrown1)
#' 
NULL





#' Oceanography data obtained at site 2 by scientist aboard the ship Ron Brown
#' 
#' Data for Exercise 2.56 and Example 2.4
#' 
#' 
#' @name Ronbrown2
#' @docType data
#' @format A data frame with 150 observations on the following 6 variables.
#' \describe{ 
#' \item{depth}{a numeric vector}
#' \item{primarytemp}{a numeric vector} 
#' \item{secondarytemp}{a numeric vector} 
#' \item{primarysalinity}{a numeric vector}
#' \item{secondarysalinity}{a numeric vector} 
#' \item{density}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Ronbrown2)
#' attach(Ronbrown2)
#' plot(depth,primarysalinity,xlab="Depth",ylab="Salinity",
#' main="Example 2.4",col="tomato")
#' detach(Ronbrown2)
#' 
NULL





#' Social adjustment scores for a rural group and a city group of children
#' 
#' Data for Exercise 7.16
#' 
#' 
#' @name Rural
#' @docType data
#' @format A data frame with 33 observations on the following 4 variables.
#' \describe{ 
#' \item{Rural}{a numeric vector} 
#' \item{City}{a numeric vector} 
#' \item{score}{a numeric vector} 
#' \item{code}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Rural)
#' attach(Rural)
#' wilcox.test(score~code)
#' wilcox.test(Rural,City)
#' detach(Rural)
#' 
NULL





#' Starting salaries for 25 new PhD psychologist
#' 
#' Data for Exercise 3.66
#' 
#' 
#' @name Salary
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{salary}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Salary)
#' 
NULL





#' Surface-water salinity measurements from Whitewater Bay, Florida
#' 
#' Data for Exercise 5.27 and 5.64
#' 
#' 
#' @name Salinity
#' @docType data
#' @format A data frame with 48 observations on the following variable.
#' \describe{ 
#' \item{salinity}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Salinity)
#' attach(Salinity)
#' EDA(salinity)
#' t.test(salinity,conf.level=.99)$conf
#' detach(Salinity)
#' 
NULL





#' SAT scores, percent taking exam and state funding per student by state for
#' 1994, 1995 and 1999
#' 
#' Data for Statistical Insight Chapter 9
#' 
#' 
#' @name Sat
#' @docType data
#' @format A data frame with 51 observations on the following 16 variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{alabama}
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
#' \item{verbal94}{a numeric vector} 
#' \item{math94}{a numeric vector} 
#' \item{total94}{a numeric vector} 
#' \item{percent94}{a numeric vector} 
#' \item{code94}{a numeric vector}
#' \item{expend94}{a numeric vector} 
#' \item{verbal95}{a numeric vector} 
#' \item{math95}{a numeric vector} 
#' \item{total95}{a numeric vector} 
#' \item{verbal99}{a numeric vector}
#' \item{math99}{a numeric vector} 
#' \item{total99}{a numeric vector} 
#' \item{percent99}{a numeric vector} 
#' \item{code99}{a numeric vector} 
#' \item{expend99}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Sat)
#' attach(Sat)
#' pairs(Sat)
#' detach(Sat)
#' 
NULL





#' Problem asset ration for savings and loan companies in California, New York,
#' and Texas
#' 
#' Data for Exercise 10.34 and 10.49
#' 
#' 
#' @name Saving
#' @docType data
#' @format A data frame with 75 observations on the following 6 variables.
#' \describe{ 
#' \item{calif}{a numeric vector} 
#' \item{newyork}{a numeric vector} 
#' \item{texas}{a numeric vector} 
#' \item{PAR}{a numeric vector} 
#' \item{state}{a numeric vector}
#' \item{ranks}{a numeric vector}
#'  }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Saving)
#' attach(Saving)
#' boxplot(PAR~state)
#' kruskal.test(PAR~as.factor(state))
#' detach(Saving)
#' 
NULL





#' Readings obtained from a 100 pound weight placed on four brands of bathroom
#' scales
#' 
#' Data for Exercise 1.89
#' 
#' 
#' @name Scales
#' @docType data
#' @format A data frame with 20 observations on the following 2 variables.
#' \describe{ 
#' \item{Brand}{a factor with levels \code{A} \code{B}
#' \code{C} \code{D}} 
#' \item{reading}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Scales)
#' attach(Scales)
#' boxplot(reading~Brand,ylab="Reading",xlab="Brand",main="Problem 1.89")
#' detach(Scales)
#' 
NULL





#' Exam scores for 17 patients to assess the learning ability of schizophrenics
#' after taking a specified does of a tranquilizer
#' 
#' Data for Exercise 6.99
#' 
#' 
#' @name Schizop2
#' @docType data
#' @format A data frame with 17 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Schizop2)
#' attach(Schizop2)
#' EDA(score)
#' SIGN.test(score,md=22,alternative="greater")
#' detach(Schizop2)
#' 
NULL





#' Standardized exam scores for 13 patients to investigate the learning ability
#' of schizophrenics after a specified dose of a tranquilizer
#' 
#' Data for Example 6.10
#' 
#' 
#' @name Schizoph
#' @docType data
#' @format A data frame with 13 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Schizoph)
#' attach(Schizoph)
#' EDA(score)
#' t.test(score,mu=20)
#' detach(Schizoph)
#' 
NULL





#' Injury level versus seatbelt usage
#' 
#' Data for Exercise 8.24
#' 
#' 
#' @name Seatbelt
#' @docType data
#' @format A data frame with 2 observations on the following 5 variables.
#' \describe{ 
#' \item{seatbelt}{a factor with levels \code{no}
#' \code{yes}} 
#' \item{None}{a numeric vector} 
#' \item{Minimal}{a numeric vector} 
#' \item{Minor}{a numeric vector}
#' \item{Major}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Seatbelt)
#' attach(Seatbelt)
#' Seatbelt
#' chisq.test(Seatbelt[,2:5])
#' detach(Seatbelt)
#' 
NULL





#' Self-confidence scores for 9 women before and after instructions on
#' self-defense
#' 
#' Data for Example 7.19
#' 
#' 
#' @name Selfdefe
#' @docType data
#' @format A data frame with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{Woman}{a numeric vector} 
#' \item{Before}{a numeric vector} 
#' \item{After}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Selfdefe)
#' attach(Selfdefe)
#' DIF <- After-Before
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(After,Before,paired=TRUE,alternative="greater")
#' detach(Selfdefe)
#' remove(DIF)
#' 
NULL





#' Reaction times of 30 senior citizens applying for drivers license renewals
#' 
#' Data for Exercise 1.83 and 3.67
#' 
#' 
#' @name Senior
#' @docType data
#' @format A data frame with 31 observations on the following variable.
#' \describe{ 
#' \item{reaction}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Senior)
#' attach(Senior)
#' fivenum(reaction)
#' boxplot(reaction,horizontal=TRUE,main="Problem 1.83 Part d.",col="orange")
#' detach(Senior)
#' 
NULL





#' Sentences of 41 prisoners convicted of a homicide offense
#' 
#' Data for Exercise 1.123
#' 
#' 
#' @name Sentence
#' @docType data
#' @format A data frame with 41 observations on the following variable.
#' \describe{ 
#' \item{months}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Sentence)
#' attach(Sentence)
#' stem(months)
#' EDA(months)
#' ll <- mean(months)-2*sd(months)
#' ul <- mean(months)+2*sd(months)
#' limits <- c(ll,ul)
#' limits
#' detach(Sentence)
#' 
NULL





#' Effects of a drug and electroshock therapy on the ability to solve simple
#' tasks
#' 
#' Data for Exercises 10.11 and 10.12
#' 
#' 
#' @name Shkdrug
#' @docType data
#' @format A data frame with 64 observations on the following 6 variables.
#' \describe{ 
#' \item{Drug.Shk}{a numeric vector}
#' \item{Drug.NoS}{a numeric vector} 
#' \item{NoDrug.S}{a numeric vector} 
#' \item{NoDg.NoS}{a numeric vector} 
#' \item{Treatment}{a factor with levels \code{Drug/NoS} \code{Drug/Shk} \code{NoDg/NoS}
#' \code{NoDrug/S}} 
#' \item{Response}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Shkdrug)
#' attach(Shkdrug)
#' boxplot(Response~Treatment)
#' anova(lm(Response~Treatment))
#' detach(Shkdrug)
#' 
NULL





#' Effect of experimental shock on time to complete difficult task
#' 
#' Data for Exercise 10.50
#' 
#' 
#' @name Shock
#' @docType data
#' @format A data frame with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{Group1}{a numeric vector} 
#' \item{Group2}{a numeric vector} 
#' \item{Group3}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Shock)
#' attach(Shock)
#' STACKED <-stack(Shock)
#' STACKED[1:5,]
#' boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
#' anova(lm(values~ind,data=STACKED))
#' remove(STACKED)
#' detach(Shock)
#' 
#' 
NULL





#' Sales receipts versus shoplifting losses for a department store
#' 
#' Data for Exercise 9.58
#' 
#' 
#' @name Shoplift
#' @docType data
#' @format A data frame with 8 observations on the following 2 variables.
#' \describe{ 
#' \item{sales}{a numeric vector} 
#' \item{loss}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Shoplift)
#' attach(Shoplift)
#' summary(lm(loss~sales))
#' detach(Shoplift)
#' 
NULL





#' James Short's measurements of the parallax of the sun
#' 
#' Data for Exercise 6.65
#' 
#' 
#' @name Short
#' @docType data
#' @format A data frame with 158 observations on the following 10 variables.
#' \describe{ 
#' \item{Sample.1}{a numeric vector}
#' \item{Sample.2}{a numeric vector} 
#' \item{Sample.3}{a numeric vector} 
#' \item{Sample.4}{a numeric vector} 
#' \item{Sample.5}{a numeric vector} 
#' \item{Sample.6}{a numeric vector}
#' \item{Sample.7}{a numeric vector} 
#' \item{Sample.8}{a numeric vector} 
#' \item{Parallax}{a numeric vector} 
#' \item{Sample}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Short)
#' attach(Short)
#' hist(Parallax)
#' EDA(Parallax)
#' SIGN.test(Parallax,md=8.798)
#' t.test(Parallax,mu=8.798)
#' detach(Short)
#' 
NULL





#' Number of people riding shuttle versus number of automobiles in the downtown
#' area
#' 
#' Data for Exercise 9.20
#' 
#' 
#' @name Shuttle
#' @docType data
#' @format A data frame with 15 observations on the following 2 variables.
#' \describe{ 
#' \item{shuttle}{a numeric vector} 
#' \item{autos}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Shuttle)
#' attach(Shuttle)
#' model <- lm(autos~shuttle)
#' summary(model)
#' detach(Shuttle)
#' remove(model)
#' 
NULL





#' Grade point averages of men and women participating in various sports-an
#' illustration of Simpson's paradox
#' 
#' Data for Example 1.18
#' 
#' 
#' @name Simpson
#' @docType data
#' @format A data frame with 100 observations on the following 15 variables.
#' \describe{ 
#' \item{gpa}{a numeric vector} 
#' \item{spor}{a numeric vector} 
#' \item{gender}{a numeric vector}
#' \item{gpamale}{a numeric vector} 
#' \item{sptmale}{a numeric vector} 
#' \item{gpafemal}{a numeric vector} 
#' \item{sptfemal}{a numeric vector} 
#' \item{bbgpa}{a numeric vector}
#' \item{genderbb}{a numeric vector} 
#' \item{sogpa}{a numeric vector} 
#' \item{genderso}{a numeric vector} 
#' \item{tkgpa}{a numeric vector} 
#' \item{gendertk}{a numeric vector}
#' \item{gradept}{a numeric vector} 
#' \item{gender2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Simpson)
#' attach(Simpson)
#' par(mfrow=c(1,2))
#' boxplot(gpa~gender,col=c("blue","pink"),names=c("Male","Female"),
#' main="GPA versus Gender",xlab="Gender",ylab="Grade Point Average")
#' boxplot(gradept~gender2,las=2,col=c("blue","pink"),
#' names=c("M-BBALL","F-BBALL","M-SOCC","F-SOCC","M-TRAC","F-TRAC"),
#' ylab="Grade Point Average",main="GPA vs Gender by Sports")
#' par(mfrow=c(1,1))
#' detach(Simpson)
#' 
NULL





#' Maximum number of situps by participants in an exercise class
#' 
#' Data for Exercise 1.47
#' 
#' 
#' @name Situp
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Situp)
#' attach(Situp)
#' stem(number)
#' hist(number,breaks=seq(0,70,10))
#' hist(number,breaks=seq(0,70,10),right=FALSE,col="blue",prob=TRUE,
#' main="Problems 1.46 & 1.47")
#' lines(density(number),col="red",lwd=3)
#' detach(Situp)
#' 
NULL





#' Illustrates the Wilcoxon Rank Sum test
#' 
#' Data for Exercise 7.65
#' 
#' 
#' @name Skewed
#' @docType data
#' @format A data frame with 21 observations on the following 2 variables.
#' \describe{ 
#' \item{C1}{a numeric vector} 
#' \item{C2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Skewed)
#' attach(Skewed)
#' boxplot(C1,C2)
#' wilcox.test(C1,C2)
#' detach(Skewed)
#' 
NULL





#' Survival times of closely and poorly matched skin grafts on burn patients
#' 
#' Data for Exercise 5.20
#' 
#' 
#' @name Skin
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{close}{a numeric vector} 
#' \item{poor}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Skin)
#' attach(Skin)
#' DIFF <- close-poor
#' stem(DIFF)
#' EDA(DIFF)
#' remove(DIFF)
#' detach(Skin)
#' 
NULL





#' Sodium-lithium countertransport activity on 190 individuals from six large
#' English kindred
#' 
#' Data for Exercise 5.116
#' 
#' 
#' @name Slc
#' @docType data
#' @format A data frame with 190 observations on the following variable.
#' \describe{ 
#' \item{SLC}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Slc)
#' attach(Slc)
#' EDA(SLC)
#' detach(Slc)
#' 
NULL





#' Water pH levels of 75 water samples taken in the Great Smoky Mountains
#' 
#' Data for Exercises 6.40, 6.59, 7.10, and 7.35
#' 
#' 
#' @name Smokyph
#' @docType data
#' @format A data frame with 75 observations on the following 5 variables.
#' \describe{ 
#' \item{waterph}{a numeric vector} 
#' \item{code}{a factor with levels \code{high} \code{low}} 
#' \item{elev}{a numeric vector} 
#' \item{SRES1}{a numeric vector} 
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Smokyph)
#' attach(Smokyph)
#' t.test(waterph,mu=7)
#' SIGN.test(waterph,md=7)
#' tapply(waterph,code,mean)
#' stripchart(waterph~code,method="stack",pch=19,col=c("red","blue"))
#' qqnorm(waterph[code=="low"])
#' qqnorm(waterph[code=="high"])
#' t.test(waterph[code=="low"],waterph[code=="high"])
#' t.test(waterph[code=="low"],waterph[code=="high"],conf.level=.90)$conf
#' detach(Smokyph)
#' 
NULL





#' Snoring versus heart disease
#' 
#' Data for Exercise 8.21
#' 
#' 
#' @name Snore
#' @docType data
#' @format A data frame with 2 observations on the following 5 variables.
#' \describe{ 
#' \item{heart}{a factor with levels \code{no} \code{yes}}
#' \item{Non}{a numeric vector} 
#' \item{occasion}{a numeric vector} 
#' \item{nearly}{a numeric vector} 
#' \item{every}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Snore)
#' attach(Snore)
#' chisq.test(Snore[,2:5])
#' detach(Snore)
#' 
NULL





#' Concentration of microparticles in snowfields of Greenland and Antarctica
#' 
#' Data for Exercise 7.87
#' 
#' 
#' @name Snow
#' @docType data
#' @format A data frame with 34 observations on the following 4 variables.
#' \describe{ 
#' \item{antarc}{a numeric vector} 
#' \item{greenld}{a numeric vector} 
#' \item{concent}{a numeric vector}
#' \item{site}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Snow)
#' attach(Snow)
#' boxplot(concent~site)
#' detach(Snow)
#' 
NULL





#' Weights of 25 soccer players
#' 
#' Data for Exercise 1.46
#' 
#' 
#' @name Soccer
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{weight}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Soccer)
#' attach(Soccer)
#' stem(weight,scale=2)
#' hist(weight,breaks=seq(110,210,10),col="orange",
#' main="Problem 1.46 \n Weights of Soccer Players",right=FALSE)
#' detach(Soccer)
#' 
NULL





#' Median income level for 25 social workers from North Carolina
#' 
#' Data for Exercise 6.63
#' 
#' 
#' @name Social
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{income}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Social)
#' attach(Social)
#' SIGN.test(income,md=27500,alternative="less")
#' detach(Social)
#' 
NULL





#' Grade point averages, SAT scores and final grade in college algebra for 20
#' sophomores
#' 
#' Data for Exercise 2.42
#' 
#' 
#' @name Sophomor
#' @docType data
#' @format A data frame with 20 observations on the following 4 variables.
#' \describe{ 
#' \item{Student}{a numeric vector} 
#' \item{GPA}{a numeric vector} 
#' \item{SAT}{a numeric vector} 
#' \item{Exam}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Sophomor)
#' attach(Sophomor)
#' cor(Sophomor)
#' detach(Sophomor)
#' 
NULL





#' Murder rates for 30 cities in the South
#' 
#' Data for Exercise 1.84
#' 
#' 
#' @name South
#' @docType data
#' @format A data frame with 31 observations on the following variable.
#' \describe{ 
#' \item{rate}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(South)
#' attach(South)
#' boxplot(rate,col="yellow",main="Problem 1.83")
#' detach(South)
#' 
NULL





#' Speed reading scores before and after a course on speed reading
#' 
#' Data for Exercise 7.58
#' 
#' 
#' @name Speed
#' @docType data
#' @format A data frame with 15 observations on the following 4 variables.
#' \describe{ 
#' \item{Before}{a numeric vector} 
#' \item{After}{a numeric vector} 
#' \item{differ}{a numeric vector}
#' \item{signrnks}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Speed)
#' attach(Speed)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' t.test(After,Before,paired=TRUE,alternative="greater")
#' wilcox.test(After,Before,paired=TRUE,alternative="greater")
#' detach(Speed)
#' 
NULL





#' Standardized spelling test scores for two fourth grade classes
#' 
#' Data for Exercise 7.82
#' 
#' 
#' @name Spellers
#' @docType data
#' @format A data frame with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{Fourth}{a numeric vector} 
#' \item{Colleag}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Spellers)
#' attach(Spellers)
#' t.test(Fourth,Colleag,alternative="greater")
#' detach(Spellers)
#' 
NULL





#' Spelling scores for 9 eighth graders before and after a 2-week course of
#' instruction
#' 
#' Data for Exercise 7.56
#' 
#' 
#' @name Spelling
#' @docType data
#' @format A data frame with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{Before}{a numeric vector} 
#' \item{After}{a numeric vector} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Spelling)
#' attach(Spelling)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' t.test(After,Before,paired=TRUE,alternative="greater")
#' detach(Spelling)
#' 
NULL





#' Favorite sport by gender
#' 
#' Data for Exercise 8.32
#' 
#' 
#' @name Sports
#' @docType data
#' @format A data frame with 2 observations on the following variable.
#' \describe{ 
#' \item{gender.football.basketbl.baseball.tennis}{a factor
#' with levels \code{female 3.800000000e+001 2.100000000e+001 1.500000000e+001
#' 2.600000000e+001} \code{male 3.300000000e+001 3.800000000e+001
#' 2.400000000e+001 5.000000000e+000}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Sports)
#' attach(Sports)
#' Sports
#' detach(Sports)
#' 
NULL





#' Convictions in spouse murder cases by gender
#' 
#' Data for Exercise 8.33
#' 
#' 
#' @name Spouse
#' @docType data
#' @format A data frame with 4 observations on the following 3 variables.
#' \describe{ 
#' \item{result}{a factor with levels \code{acquitted}
#' \code{convicted} \code{not prosecuted} \code{pleaded guilty}}
#' \item{husband}{a numeric vector} 
#' \item{wife}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Spouse)
#' attach(Spouse)
#' Spouse
#' chisq.test(Spouse[,2:3])
#' detach(Spouse)
#' 
NULL





#' Times of a 2-year old stallion on a one mile run
#' 
#' Data for Exercise 6.93
#' 
#' 
#' @name Stable
#' @docType data
#' @format A data frame with 9 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Stable)
#' attach(Stable)
#' EDA(time)
#' SIGN.test(time,md=98.5,alternative="greater")
#' detach(Stable)
#' 
NULL





#' Thicknesses of 1872 Hidalgo stamps issued in Mexico
#' 
#' Data for Statistical Insight Chapter 1 and Exercise 5.110
#' 
#' 
#' @name Stamp
#' @docType data
#' @format A data frame with 485 observations on the following 3 variables.
#' \describe{ 
#' \item{thickness}{a numeric vector}
#' \item{thick}{a numeric vector} 
#' \item{freq}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Stamp)
#' attach(Stamp)
#' hist(thickness,prob=TRUE,col="lightblue")
#' lines(density(thickness),lwd=2,col="blue")
#' t.test(thickness,conf.level=.99)$conf
#' detach(Stamp)
#' 
NULL





#' Grades for two introductory statistics classes
#' 
#' Data for Exercise 7.30
#' 
#' 
#' @name Statclas
#' @docType data
#' @format A data frame with 36 observations on the following 2 variables.
#' \describe{ 
#' \item{X9am}{a numeric vector} 
#' \item{X2pm}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Statclas)
#' attach(Statclas)
#' t.test(X9am,X2pm)
#' detach(Statclas)
#' 
NULL





#' Operating expenditures per resident for each of the state law enforcement
#' agencies
#' 
#' Data for Exercise 6.62
#' 
#' 
#' @name Statelaw
#' @docType data
#' @format A data frame with 50 observations on the following 2 variables.
#' \describe{ 
#' \item{State}{a factor with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{Florida}
#' \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana}
#' \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine}
#' \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota}
#' \code{Mississippi} \code{Missour} \code{Montana} \code{Nebraska}
#' \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico}
#' \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio}
#' \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island}
#' \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas}
#' \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West
#' Virginia} \code{Wisconsin} \code{Wyoming}} 
#' \item{cost}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Statelaw)
#' attach(Statelaw)
#' summary(cost)
#' EDA(cost)
#' SIGN.test(cost,md=8,alternative="less")
#' detach(Statelaw)
#' 
NULL





#' Test scores for two beginning statistics classes
#' 
#' Data for Exercises 1.70 and 1.87
#' 
#' 
#' @name Statisti
#' @docType data
#' @format A data frame with 31 observations on the following 2 variables.
#' \describe{ 
#' \item{Class1}{a numeric vector} 
#' \item{Class2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Statisti)
#' attach(Statisti)
#' boxplot(Class1,Class2,names=c("Class 1","Class 2"),col=c("red","blue"),
#' main="Problem 1.87")
#' detach(Statisti)
#' 
NULL





#' STEP science test scores for a class of ability-grouped students
#' 
#' Data for Exercise 6.79
#' 
#' 
#' @name Step
#' @docType data
#' @format A data frame with 12 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Step)
#' attach(Step)
#' EDA(score)
#' t.test(score,mu=80,alternative="less")
#' detach(Step)
#' 
NULL





#' Short-term memory test scores on 12 subjects before and after a stressful
#' situation
#' 
#' Data for Example 7.20
#' 
#' 
#' @name Stress
#' @docType data
#' @format A data frame with 12 observations on the following 2 variables.
#' \describe{ 
#' \item{Prestre}{a numeric vector}
#' \item{Poststre}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Stress)
#' attach(Stress)
#' DIF <- Poststre -Prestre
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(Poststre,Prestre,paired=TRUE,alternative="less")
#' detach(Stress)
#' remove(DIF)
#' 
NULL





#' Number of hours studied per week by a sample of 50 freshmen
#' 
#' Data for Exercise 5.25
#' 
#' 
#' @name Study
#' @docType data
#' @format A data frame with 50 observations on the following variable.
#' \describe{ 
#' \item{hours}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Study)
#' attach(Study)
#' stem(hours)
#' EDA(hours)
#' detach(Study)
#' 
NULL





#' Number of German submarines sunk by U.S. Navy in World War II
#' 
#' Data for Exercises 2.16, 2.45, and 2.59
#' 
#' 
#' @name Submarin
#' @docType data
#' @format A data frame with 16 observations on the following 3 variables.
#' \describe{ 
#' \item{Month}{a numeric vector} 
#' \item{reported}{a numeric vector} 
#' \item{actual}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Submarin)
#' attach(Submarin)
#' plot(reported,actual)
#' model <- lm(actual~reported)
#' abline(model)
#' anova(model)
#' summary(model)
#' detach(Submarin)
#' 
NULL





#' Time it takes a subway to travel from the airport to downtown
#' 
#' Data for Exercise 5.19
#' 
#' 
#' @name Subway
#' @docType data
#' @format A data frame with 30 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Subway)
#' attach(Subway)
#' EDA(time)
#' detach(Subway)
#' 
NULL





#' Wolfer sunspot numbers from 1700 through 2000
#' 
#' Data for Example 1.7
#' 
#' 
#' @name Sunspot
#' @docType data
#' @format A data frame with 301 observations on the following 2 variables.
#' \describe{ 
#' \item{year}{a numeric vector} 
#' \item{sunspots}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Sunspot)
#' attach(Sunspot)
#' plot(year,sunspots,type="l",main="Yearly Sunspots")    # Using standard plot
#' library(lattice)
#' xyplot(sunspots ~ 1700:2000, xlab = "", type = "l",main="Yearly Sunspots")
#' xyplot(sunspots ~ 1700:2000, xlab = "", type = "l", aspect="xy",
#' main="Yearly Sunspots")
#' detach(Sunspot)
#' 
NULL





#' Margin of victory in Superbowls I to XXXV
#' 
#' Data for Exercise 1.54
#' 
#' 
#' @name Superbowl
#' @docType data
#' @format A data frame with 35 observations on the following 5 variables.
#' \describe{ 
#' \item{Winning.team}{a factor with levels \code{Baltimore
#' Colts} \code{Baltimore Ravens} \code{Chicago Bears} \code{Dallas Cowboys}
#' \code{Denver Broncos} \code{Green Bay Packers} \code{Kansas City Chiefs}
#' \code{Los Angeles Raiders} \code{Miami Dolphins} \code{New York Giants}
#' \code{New York Jets} \code{Oakland Raiders} \code{Pittsburgh Steelers}
#' \code{San Francisco 49ers} \code{St Louis Rams} \code{Washington Redskins}}
#' \item{winner.score}{a numeric vector} 
#' \item{Losing.team}{a factor with levels \code{Atlanta Falcons} \code{Baltimore Colts}
#' \code{Buffalo Bills} \code{Cincinnati Bengals} \code{Dallas Cowboys}
#' \code{Denver Broncos} \code{Green Bay Packers} \code{Kansas City Chiefs}
#' \code{Los Angeles Rams} \code{Miami Dolphins} \code{Minnesota Vikings}
#' \code{New England Patriots} \code{New York Giants} \code{Oakland Raiders}
#' \code{Philadelphia Eagles} \code{Pittsburgh Steelers} \code{San Diego
#' Chargers} \code{Tennessee Titans} \code{Washington Redskins}}
#' \item{loser.score}{a numeric vector} 
#' \item{margin}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Superbowl)
#' attach(Superbowl)
#' stem(margin)
#' detach(Superbowl)
#' 
NULL





#' Top speeds attained by five makes of supercars
#' 
#' Data for Statistical Insight Chapter 10
#' 
#' 
#' @name Supercar
#' @docType data
#' @format A data frame with 30 observations on the following 7 variables.
#' \describe{ 
#' \item{Acura}{a numeric vector} 
#' \item{Ferrari}{a numeric vector} 
#' \item{Lotus}{a numeric vector}
#' \item{Porsche}{a numeric vector} 
#' \item{Viper}{a numeric vector} 
#' \item{speed}{a numeric vector} 
#' \item{car}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Supercar)
#' attach(Supercar)
#' boxplot(speed~car)
#' anova(lm(speed~as.factor(car)))
#' detach(Supercar)
#' 
NULL





#' Ozone concentrations at Mt. Mitchell, North Carolina
#' 
#' Data for Exercise 5.63
#' 
#' 
#' @name Tablrock
#' @docType data
#' @format A data frame with 719 observations on the following 16 variables.
#' \describe{ 
#' \item{hour}{a factor with levels \code{00:00}
#' \code{01:00} \code{02:00} \code{03:00} \code{04:00} \code{05:00}
#' \code{06:00} \code{07:00} \code{08:00} \code{09:00} \code{10:00}
#' \code{11:00} \code{12:00} \code{13:00} \code{14:00} \code{15:00}
#' \code{16:00} \code{17:00} \code{18:00} \code{19:00} \code{20:00}
#' \code{21:00} \code{22:00} \code{23:00}} 
#' \item{X03}{a numeric vector}
#' \item{tmp}{a numeric vector} 
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
#' str(Tablrock)
#' attach(Tablrock)
#' EDA(X03)
#' t.test(X03,conf.level=.99)$conf
#' detach(Tablrock)
#' 
NULL





#' Average teacher's salaries across the states in the 70s 80s and 90s
#' 
#' Data for Exercise 5.114
#' 
#' 
#' @name Teacher
#' @docType data
#' @format A data frame with 51 observations on the following 4 variables.
#' \describe{
#'  \item{State}{a factor with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of
#' Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho}
#' \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky}
#' \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts}
#' \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missouri}
#' \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New
#' Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North
#' Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania}
#' \code{Rhode Island} \code{South Carolina} \code{South Dakota}
#' \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia}
#' \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
#' \item{X1973.74}{a numeric vector} 
#' \item{X1983.84}{a numeric vector} 
#' \item{X1993.94}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Teacher)
#' attach(Teacher)
#' boxplot(X1973.74,X1983.84,X1993.94,
#' names=c("1973-1974","1983-1984","1993-1994"),ylab="Average Salary")
#' detach(Teacher)
#' 
NULL





#' Tennessee self concept scores for 20 gifted high school students
#' 
#' Data for Exercise 6.56
#' 
#' 
#' @name Tenness
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{score}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Tenness)
#' attach(Tenness)
#' EDA(score)
#' t.test(score,mu=30,alternative="less")
#' SIGN.test(score,md=30,alternative="less")
#' detach(Tenness)
#' 
NULL





#' Tensile strength of plastic bags from two production runs
#' 
#' Data for Example 7.11
#' 
#' 
#' @name Tensile
#' @docType data
#' @format A data frame with 72 observations on the following 4 variables.
#' \describe{ 
#' \item{Run.1}{a numeric vector} 
#' \item{Run.2}{a numeric vector} 
#' \item{Tensile}{a numeric vector}
#' \item{Run}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Tensile)
#' attach(Tensile)
#' boxplot(Run.1,Run.2,names=c("Run 1","Run 2"),col=c("red","Blue"))
#' boxplot(Tensile~Run,names=c("Run 1","Run 2"),col=c("red","Blue"))
#' t.test(Tensile~Run)
#' t.test(Run.1,Run.2)
#' detach(Tensile)
#' 
NULL





#' Grades on the first test in a statistics class
#' 
#' Data for Exercise 5.80
#' 
#' 
#' @name Test1
#' @docType data
#' @format A data frame with 25 observations on the following variable.
#' \describe{ 
#' \item{test1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Test1)
#' attach(Test1)
#' EDA(test1)
#' detach(Test1)
#' 
NULL





#' Heat loss of thermal pane windows versus outside temperature
#' 
#' Data for Example 9.5
#' 
#' 
#' @name Thermal
#' @docType data
#' @format A data frame with 12 observations on the following 3 variables.
#' \describe{ 
#' \item{temp}{a numeric vector} 
#' \item{loss}{a numeric vector} 
#' \item{x}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Thermal)
#' attach(Thermal)
#' model <- lm(loss~temp)
#' summary(model)
#' detach(Thermal)
#' 
NULL





#' 1999-2000 closing prices for TIAA-CREF stocks
#' 
#' Data for your enjoyment
#' 
#' 
#' @name Tiaa
#' @docType data
#' @format A data frame with 365 observations on the following 4 variables.
#' \describe{ 
#' \item{crefstk}{a numeric vector} 
#' \item{crefgwt}{a numeric vector} 
#' \item{tiaa}{a numeric vector} 
#' \item{date}{a factor with levels \code{1/01/2000} \code{1/02/2000} \code{1/03/2000}
#' \code{1/04/2000} \code{1/05/2000} \code{1/06/2000} \code{1/07/2000}
#' \code{1/08/2000} \code{1/09/2000} \code{1/10/2000} \code{1/11/2000}
#' \code{1/12/2000} \code{1/13/2000} \code{1/14/2000} \code{1/15/2000}
#' \code{1/16/2000} \code{1/17/2000} \code{1/18/2000} \code{1/19/2000}
#' \code{1/20/2000} \code{1/21/2000} \code{1/22/2000} \code{1/23/2000}
#' \code{1/24/2000} \code{1/25/2000} \code{1/26/2000} \code{1/27/2000}
#' \code{1/28/2000} \code{1/29/2000} \code{1/30/2000} \code{1/31/2000}
#' \code{10/01/1999} \code{10/02/1999} \code{10/03/1999} \code{10/04/1999}
#' \code{10/05/1999} \code{10/06/1999} \code{10/07/1999} \code{10/08/1999}
#' \code{10/09/1999} \code{10/10/1999} \code{10/11/1999} \code{10/12/1999}
#' \code{10/13/1999} \code{10/14/1999} \code{10/15/1999} \code{10/16/1999}
#' \code{10/17/1999} \code{10/18/1999} \code{10/19/1999} \code{10/20/1999}
#' \code{10/21/1999} \code{10/22/1999} \code{10/23/1999} \code{10/24/1999}
#' \code{10/25/1999} \code{10/26/1999} \code{10/27/1999} \code{10/28/1999}
#' \code{10/29/1999} \code{10/30/1999} \code{10/31/1999} \code{11/01/1999}
#' \code{11/02/1999} \code{11/03/1999} \code{11/04/1999} \code{11/05/1999}
#' \code{11/06/1999} \code{11/07/1999} \code{11/08/1999} \code{11/09/1999}
#' \code{11/10/1999} \code{11/11/1999} \code{11/12/1999} \code{11/13/1999}
#' \code{11/14/1999} \code{11/15/1999} \code{11/16/1999} \code{11/17/1999}
#' \code{11/18/1999} \code{11/19/1999} \code{11/20/1999} \code{11/21/1999}
#' \code{11/22/1999} \code{11/23/1999} \code{11/24/1999} \code{11/25/1999}
#' \code{11/26/1999} \code{11/27/1999} \code{11/28/1999} \code{11/29/1999}
#' \code{11/30/1999} \code{12/01/1999} \code{12/02/1999} \code{12/03/1999}
#' \code{12/04/1999} \code{12/05/1999} \code{12/06/1999} \code{12/07/1999}
#' \code{12/08/1999} \code{12/09/1999} \code{12/10/1999} \code{12/11/1999}
#' \code{12/12/1999} \code{12/13/1999} \code{12/14/1999} \code{12/15/1999}
#' \code{12/16/1999} \code{12/17/1999} \code{12/18/1999} \code{12/19/1999}
#' \code{12/20/1999} \code{12/21/1999} \code{12/22/1999} \code{12/23/1999}
#' \code{12/24/1999} \code{12/25/1999} \code{12/26/1999} \code{12/27/1999}
#' \code{12/28/1999} \code{12/29/1999} \code{12/30/1999} \code{12/31/1999}
#' \code{2/01/2000} \code{2/02/2000} \code{2/03/2000} \code{2/04/2000}
#' \code{2/05/2000} \code{2/06/2000} \code{2/07/2000} \code{2/08/2000}
#' \code{2/09/2000} \code{2/10/2000} \code{2/11/2000} \code{2/12/2000}
#' \code{2/13/2000} \code{2/14/2000} \code{2/15/2000} \code{2/16/2000}
#' \code{2/17/2000} \code{2/18/2000} \code{2/19/2000} \code{2/20/2000}
#' \code{2/21/2000} \code{2/22/2000} \code{2/23/2000} \code{2/24/2000}
#' \code{2/25/2000} \code{2/26/2000} \code{2/27/2000} \code{2/28/2000}
#' \code{2/29/2000} \code{3/01/2000} \code{3/02/2000} \code{3/03/2000}
#' \code{3/04/2000} \code{3/05/2000} \code{3/06/2000} \code{3/07/2000}
#' \code{3/08/2000} \code{3/09/2000} \code{3/10/2000} \code{3/11/2000}
#' \code{3/12/2000} \code{3/13/2000} \code{3/14/2000} \code{3/15/2000}
#' \code{3/16/2000} \code{3/17/2000} \code{3/18/2000} \code{3/19/2000}
#' \code{3/20/2000} \code{3/21/2000} \code{3/22/2000} \code{3/23/2000}
#' \code{3/24/2000} \code{3/25/2000} \code{3/26/2000} \code{3/27/2000}
#' \code{3/28/2000} \code{3/29/2000} \code{3/30/2000} \code{3/31/2000}
#' \code{4/01/2000} \code{4/02/2000} \code{4/03/2000} \code{4/04/2000}
#' \code{4/05/2000} \code{4/06/2000} \code{4/07/2000} \code{4/08/2000}
#' \code{4/09/2000} \code{4/10/2000} \code{4/11/2000} \code{4/12/2000}
#' \code{4/13/2000} \code{4/14/2000} \code{4/16/1999} \code{4/17/1999}
#' \code{4/18/1999} \code{4/19/1999} \code{4/20/1999} \code{4/21/1999}
#' \code{4/22/1999} \code{4/23/1999} \code{4/24/1999} \code{4/25/1999}
#' \code{4/26/1999} \code{4/27/1999} \code{4/28/1999} \code{4/29/1999}
#' \code{4/30/1999} \code{5/01/1999} \code{5/02/1999} \code{5/03/1999}
#' \code{5/04/1999} \code{5/05/1999} \code{5/06/1999} \code{5/07/1999}
#' \code{5/08/1999} \code{5/09/1999} \code{5/10/1999} \code{5/11/1999}
#' \code{5/12/1999} \code{5/13/1999} \code{5/14/1999} \code{5/15/1999}
#' \code{5/16/1999} \code{5/17/1999} \code{5/18/1999} \code{5/19/1999}
#' \code{5/20/1999} \code{5/21/1999} \code{5/22/1999} \code{5/23/1999}
#' \code{5/24/1999} \code{5/25/1999} \code{5/26/1999} \code{5/27/1999}
#' \code{5/28/1999} \code{5/29/1999} \code{5/30/1999} \code{5/31/1999}
#' \code{6/01/1999} \code{6/02/1999} \code{6/03/1999} \code{6/04/1999}
#' \code{6/05/1999} \code{6/06/1999} \code{6/07/1999} \code{6/08/1999}
#' \code{6/09/1999} \code{6/10/1999} \code{6/11/1999} \code{6/12/1999}
#' \code{6/13/1999} \code{6/14/1999} \code{6/15/1999} \code{6/16/1999}
#' \code{6/17/1999} \code{6/18/1999} \code{6/19/1999} \code{6/20/1999}
#' \code{6/21/1999} \code{6/22/1999} \code{6/23/1999} \code{6/24/1999}
#' \code{6/25/1999} \code{6/26/1999} \code{6/27/1999} \code{6/28/1999}
#' \code{6/29/1999} \code{6/30/1999} \code{7/01/1999} \code{7/02/1999}
#' \code{7/03/1999} \code{7/04/1999} \code{7/05/1999} \code{7/06/1999}
#' \code{7/07/1999} \code{7/08/1999} \code{7/09/1999} \code{7/10/1999}
#' \code{7/11/1999} \code{7/12/1999} \code{7/13/1999} \code{7/14/1999}
#' \code{7/15/1999} \code{7/16/1999} \code{7/17/1999} \code{7/18/1999}
#' \code{7/19/1999} \code{7/20/1999} \code{7/21/1999} \code{7/22/1999}
#' \code{7/23/1999} \code{7/24/1999} \code{7/25/1999} \code{7/26/1999}
#' \code{7/27/1999} \code{7/28/1999} \code{7/29/1999} \code{7/30/1999}
#' \code{7/31/1999} \code{8/01/1999} \code{8/02/1999} \code{8/03/1999}
#' \code{8/04/1999} \code{8/05/1999} \code{8/06/1999} \code{8/07/1999}
#' \code{8/08/1999} \code{8/09/1999} \code{8/10/1999} \code{8/11/1999}
#' \code{8/12/1999} \code{8/13/1999} \code{8/14/1999} \code{8/15/1999}
#' \code{8/16/1999} \code{8/17/1999} \code{8/18/1999} \code{8/19/1999}
#' \code{8/20/1999} \code{8/21/1999} \code{8/22/1999} \code{8/23/1999}
#' \code{8/24/1999} \code{8/25/1999} \code{8/26/1999} \code{8/27/1999}
#' \code{8/28/1999} \code{8/29/1999} \code{8/30/1999} \code{8/31/1999}
#' \code{9/01/1999} \code{9/02/1999} \code{9/03/1999} \code{9/04/1999}
#' \code{9/05/1999} \code{9/06/1999} \code{9/07/1999} \code{9/08/1999}
#' \code{9/09/1999} \code{9/10/1999} \code{9/11/1999} \code{9/12/1999}
#' \code{9/13/1999} \code{9/14/1999} \code{9/15/1999} \code{9/16/1999}
#' \code{9/17/1999} \code{9/18/1999} \code{9/19/1999} \code{9/20/1999}
#' \code{9/21/1999} \code{9/22/1999} \code{9/23/1999} \code{9/24/1999}
#' \code{9/25/1999} \code{9/26/1999} \code{9/27/1999} \code{9/28/1999}
#' \code{9/29/1999} \code{9/30/1999}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Tiaa)
#' 
NULL





#' Time to complete an airline ticket reservation
#' 
#' Data for Exercise 5.18
#' 
#' 
#' @name Ticket
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{time}{a numeric vector}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Ticket)
#' attach(Ticket)
#' EDA(time)
#' detach(Ticket)
#' 
NULL





#' Consumer Reports (Oct 94) rating of toaster ovens versus the cost
#' 
#' Data for Exercise 9.35
#' 
#' 
#' @name Toaster
#' @docType data
#' @format A data frame with 17 observations on the following 3 variables.
#' \describe{ 
#' \item{toaster}{a factor with levels \code{Black&D
#' SO2500G} \code{Black&D T660G} \code{Black&D TRO200} \code{Black&D TRO400}
#' \code{Black&D TRO510} \code{DeLonghi XU14} \code{DeLonghi XU20L}
#' \code{Hamilton Beach 336} \code{Munsey M88} \code{Panasonic NT855U}
#' \code{Proctor-Silex 03008} \code{Proctor-Silex 03010} \code{Proctor-Silex
#' 03030} \code{Sears Kenmore 48216} \code{Toastmaster 319V} \code{Toastmaster
#' 336V} \code{Toastmaster 342}} 
#' \item{score}{a numeric vector}
#' \item{cost}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' data(Toaster)
#' 
NULL





#' Size of tonsils collected from 1,398 children
#' 
#' Data for Exercise 2.78
#' 
#' 
#' @name Tonsils
#' @docType data
#' @format A data frame with 3 observations on the following 3 variables.
#' \describe{ 
#' \item{Size}{a factor with levels \code{Large}
#' \code{Normal} \code{Very Large}} 
#' \item{Carrier}{a numeric vector}
#' \item{Non.carrier}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Tonsils)
#' attach(Tonsils)
#' TON <- as.matrix(Tonsils[,2:3])
#' rownames(TON) <- Size
#' TON
#' barplot(t(TON),beside=TRUE,legend=TRUE)
#' remove(TON)
#' detach(Tonsils)
#' 
NULL





#' The number of torts, average number of months to process a tort, and county
#' population from the court files of the nation's largest counties
#' 
#' Data for Exercise 5.13
#' 
#' 
#' @name Tort
#' @docType data
#' @format A data frame with 45 observations on the following 5 variables.
#' \describe{ 
#' \item{county}{a factor with levels \code{alameda, ca}
#' \code{allegheny, pa} \code{bergen, nj} \code{bexar, tx} \code{contra costa,
#' ca} \code{cook, il} \code{cuyahoga, oh} \code{dade, fl} \code{dallas, tx}
#' \code{dupage, il} \code{essex, ma} \code{essex, nj} \code{fairfax, va}
#' \code{fairfield, ct} \code{franklin, oh} \code{fresno, ca} \code{fulton, ga}
#' \code{harris, tx} \code{hartford, ct} \code{hennepin, mn} \code{honolulu,
#' hi} \code{jefferson, ky} \code{king, wa} \code{los angeles, ca}
#' \code{maricopa, az} \code{marion, in} \code{middlesex, ma} \code{middlesex,
#' nj} \code{milwaukee, wi} \code{new york, ny} \code{norfolk, ma}
#' \code{oakland, mi} \code{orange, ca} \code{orange, fl} \code{palm beach, fl}
#' \code{philadelphia, pa} \code{pima, az} \code{san bernadino, ca} \code{san
#' francisco, ca} \code{santa clara, ca} \code{st. louis, mo} \code{suffolk,
#' ma} \code{ventura, ca} \code{wayne, mi} \code{worchester, ma}}
#' \item{months}{a numeric vector} 
#' \item{populat}{a numeric vector} 
#' \item{torts}{a numeric vector} 
#' \item{rate}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Tort)
#' attach(Tort)
#' EDA(months)
#' detach(Tort)
#' 
NULL





#' Hazardous waste sites near minority communities
#' 
#' Data for Exercises 1.55, 5.08, 5.109, 8.58, and 10.35
#' 
#' 
#' @name Toxic
#' @docType data
#' @format A data frame with 51 observations on the following 5 variables.
#' \describe{ 
#' \item{state}{a factor with levels \code{alabama}
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
#' \item{region}{a factor with levels \code{midwest} \code{northeast}
#' \code{south} \code{west}} 
#' \item{sites}{a numeric vector}
#' \item{minority}{a numeric vector} 
#' \item{percent}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Toxic)
#' attach(Toxic)
#' hist(minority,prob=TRUE)
#' lines(density(minority))
#' SIGN.test(sites,conf.level=.98)
#' boxplot(sites~region)
#' kruskal.test(sites~as.factor(region))
#' detach(Toxic)
#' 
NULL





#' Olympic winning times for the men's 1500-meter run
#' 
#' Data for Exercise 1.36
#' 
#' 
#' @name Track15
#' @docType data
#' @format A data frame with 26 observations on the following 2 variables.
#' \describe{ 
#' \item{Year}{a numeric vector} 
#' \item{X1500m}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Track15)
#' attach(Track15)
#' plot(Year,X1500m,type="l",lwd=2,lty=2,col="red",xlab="Year",
#' ylab="1500m Time (seconds)",main="Problem 1.36")       
#' detach(Track15) 
#' 
NULL





#' National Olympic records for women in several races
#' 
#' Data for Exercises 2.97, 5.115, and 9.62
#' 
#' 
#' @name Track
#' @docType data
#' @format A data frame with 55 observations on the following 8 variables.
#' \describe{ 
#' \item{country}{a factor with levels \code{argentina}
#' \code{australia} \code{austria} \code{belgium} \code{bermuda} \code{brazil}
#' \code{burma} \code{canada} \code{chile} \code{china} \code{colombia}
#' \code{cookis} \code{costa} \code{czech} \code{denmark} \code{domrep}
#' \code{dprkorea} \code{finland} \code{france} \code{frg} \code{gbni}
#' \code{gdr} \code{greece} \code{guatemala} \code{hungary} \code{india}
#' \code{indonesia} \code{ireland} \code{israel} \code{italy} \code{japan}
#' \code{kenya} \code{korea} \code{luxembourg} \code{malaysia} \code{mauritius}
#' \code{mexico} \code{netherlands} \code{norway} \code{nz} \code{philippines}
#' \code{png} \code{poland} \code{portugal} \code{rumania} \code{singapore}
#' \code{spain} \code{sweden} \code{switzerland} \code{taipei} \code{thailand}
#' \code{turkey} \code{usa} \code{ussr} \code{wsamoa}} 
#' \item{X100m}{a numeric vector} 
#' \item{X200m}{a numeric vector}
#' \item{X400m}{a numeric vector} 
#' \item{X800m}{a numeric vector} 
#' \item{X1500m}{a numeric vector} 
#' \item{X3000m}{a numeric vector} 
#' \item{marathon}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Track)
#' attach(Track)
#' cor(Track[,2:8])
#' pairs(Track[,2:8])
#' detach(Track)
#' 
NULL





#' Illustrates analysis of variance for three treatment groups
#' 
#' Data for Exercise 10.44
#' 
#' 
#' @name Treatments
#' @docType data
#' @format A data frame with 24 observations on the following 5 variables.
#' \describe{ 
#' \item{treat1}{a numeric vector} 
#' \item{treat2}{a numeric vector} 
#' \item{treat3}{a numeric vector}
#' \item{Treatmnt}{a numeric vector} 
#' \item{Group}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Treatments)
#' attach(Treatments)
#' anova(lm(Treatmnt~as.factor(Group)))
#' detach(Treatments)
#' 
NULL





#' Number of trees in 20 grids
#' 
#' Data for Exercise 1.50
#' 
#' 
#' @name Trees
#' @docType data
#' @format A data frame with 20 observations on the following variable.
#' \describe{ 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Trees)
#' attach(Trees)
#' stem(number)
#' hist(number,breaks=seq(60,110,10),right=FALSE,col="green",main="Problem 1.50")
#' detach(Trees)
#' 
NULL





#' Miles per gallon for standard 4-wheel drive trucks manufactured by
#' Chevrolet, Dodge and Ford
#' 
#' Data for Example 10.2
#' 
#' 
#' @name Trucks
#' @docType data
#' @format A data frame with 15 observations on the following 5 variables.
#' \describe{ 
#' \item{chevy}{a numeric vector} 
#' \item{dodge}{a numeric vector} 
#' \item{ford}{a numeric vector}
#' \item{gas.mileage}{a numeric vector} 
#' \item{truck}{a factor with levels \code{chevy} \code{dodge} \code{ford}} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Trucks)
#' attach(Trucks)
#' anova(lm(gas.mileage~truck))
#' detach(Trucks)
#' 
NULL





#' Percent of students that watch more than 6 hours of TV per day versus
#' national math test scores
#' 
#' Data for Examples 2.1 and 2.7
#' 
#' 
#' @name Tv
#' @docType data
#' @format A data frame with 53 observations on the following 3 variables.
#' \describe{ 
#' \item{State}{a factor with levels \code{Alabama}
#' \code{Alaska} \code{Arizona} \code{Arkansas} \code{California}
#' \code{Colorado} \code{Connecticut} \code{DC} \code{Delaware} \code{Florida}
#' \code{Georgia} \code{Guam} \code{Hawaii} \code{Idaho} \code{Illinois}
#' \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana}
#' \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan}
#' \code{Minnesota} \code{Mississippi} \code{Missour} \code{Montana}
#' \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey}
#' \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota}
#' \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode
#' Island} \code{South Carolina} \code{South Dakota} \code{Tennessee}
#' \code{Texas} \code{Utah} \code{Vermont} \code{Vir Is} \code{Virginia}
#' \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
#' \item{percent}{a numeric vector} 
#' \item{test}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Tv)
#' attach(Tv)
#' plot(percent,test,col="blue")
#' cor(percent,test,use="complete.obs")
#' detach(Tv)
#' 
NULL





#' Intelligence test scores for identical twins in which one twin is given a
#' drug
#' 
#' Data for Exercise 7.54
#' 
#' 
#' @name Twin
#' @docType data
#' @format A data frame with 9 observations on the following 3 variables.
#' \describe{ 
#' \item{TwinA}{a numeric vector} 
#' \item{TwinB}{a numeric vector} 
#' \item{differ}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Twin)
#' attach(Twin)
#' qqnorm(differ)
#' qqline(differ)
#' shapiro.test(differ)
#' t.test(TwinA,TwinB,paired=TRUE)
#' detach(Twin)
#' 
NULL





#' Data set describing a sample of undergraduate students
#' 
#' Data for Exercise 1.15
#' 
#' 
#' @name Undergrad
#' @docType data
#' @format A data frame with 100 observations on the following 6 variables.
#' \describe{ 
#' \item{Gender}{a factor with levels \code{Female}
#' \code{Male}} 
#' \item{Major}{a factor with levels \code{Accounting}
#' \code{Biology} \code{Chemistry} \code{English} \code{Geology} \code{History}
#' \code{Math} \code{Music} \code{Physics} \code{Psychology} \code{Sociology}}
#' \item{Class}{a factor with levels \code{Freshman} \code{Junior}
#' \code{Senior} \code{Sophomore}} 
#' \item{GPA}{a numeric vector}
#' \item{SAT}{a numeric vector} 
#' \item{Drops}{a numeric vector}
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Undergrad)
#' attach(Undergrad)
#' stripchart(GPA~Class,method="stack",col=c("blue","red","green","lightblue"),
#' pch=19,main="GPA versus Class")
#' stripchart(GPA~Gender,method="stack",col=c("red","blue"),pch=19,
#' main="GPA versus Gender")
#' stripchart(SAT~Drops,method="stack",col=c("blue","red","green","lightblue"),
#' pch=19,main="SAT versus Drops")
#' stripchart(Drops~Gender,method="stack",col=c("red","blue"),pch=19,
#' main="Drops versus Gender")
#' detach(Undergrad)
#' 
NULL





#' Number of days of paid holidays and vacation leave for sample of 35 textile
#' workers
#' 
#' Data for Exercise 6.46 and 6.98
#' 
#' 
#' @name Vacation
#' @docType data
#' @format A data frame with 35 observations on the following variable.
#' \describe{ 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Vacation)
#' attach(Vacation)
#' EDA(number)
#' t.test(number,mu=24)
#' detach(Vacation)
#' 
NULL





#' Reported serious reactions due to vaccines in 11 southern states
#' 
#' Data for Exercise 1.111
#' 
#' 
#' @name Vaccine
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{State}{a factor with levels \code{Alabama}
#' \code{Arkansas} \code{Florida} \code{Georgia} \code{Louisiana}
#' \code{Mississippi} \code{North Carolina} \code{Oklahoma} \code{South
#' Carolina} \code{Tennessee} \code{Texas}} 
#' \item{number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Vaccine)
#' attach(Vaccine)
#' fn <- fivenum(number)
#' fn
#' iqr <- IQR(number)
#' ll <- fn[2]-1.5*iqr
#' ul <- fn[4]+1.5*iqr
#' limits <- c(ll,ul)
#' limits
#' boxplot(number)
#' detach(Vaccine)
#' 
NULL





#' Fatality ratings for foreign and domestic vehicles
#' 
#' Data for Exercise 8.34
#' 
#' 
#' @name Vehicle
#' @docType data
#' @format A data frame with 2 observations on the following 6 variables.
#' \describe{ 
#' \item{make}{a factor with levels \code{domestic}
#' \code{foreign}} 
#' \item{A}{a numeric vector} 
#' \item{B}{a numeric vector} 
#' \item{C}{a numeric vector} 
#' \item{D}{a numeric vector} 
#' \item{F}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Vehicle)
#' attach(Vehicle)
#' Vehicle
#' chisq.test(Vehicle[,2:6])
#' detach(Vehicle)
#' 
NULL





#' Verbal test scores and number of library books checked out for 15 eighth
#' graders
#' 
#' Data for Exercise 9.30
#' 
#' 
#' @name Verbal
#' @docType data
#' @format A data frame with 15 observations on the following 4 variables.
#' \describe{ 
#' \item{number}{a numeric vector} 
#' \item{verbal}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Verbal)
#' attach(Verbal)
#' model <- lm(verbal~number)
#' summary(model)
#' detach(Verbal)
#' remove(model)
#' 
NULL





#' Number of sunspots versus mean annual level of Lake Victoria Nyanza from
#' 1902 to 1921
#' 
#' Data for Exercise 2.98
#' 
#' 
#' @name Victoria
#' @docType data
#' @format A data frame with 20 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{a numeric vector} 
#' \item{level}{a numeric vector} 
#' \item{sunspot}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Victoria)
#' attach(Victoria)
#' plot(sunspot,level)
#' model <- lm(level~sunspot)
#' abline(model)
#' cor(sunspot,level)
#' model
#' detach(Victoria)
#' 
NULL





#' Viscosity measurements of a substance on two different days
#' 
#' Data for Exercise 7.44
#' 
#' 
#' @name Viscosit
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{first}{a numeric vector} 
#' \item{second}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Viscosit)
#' attach(Viscosit)
#' t.test(first,second)
#' detach(Viscosit)
#' 
NULL





#' Visual acuity of a group of subjects tested under a specified dose of a drug
#' 
#' Data for Exercise 5.6
#' 
#' 
#' @name Visual
#' @docType data
#' @format A data frame with 18 observations on the following variable.
#' \describe{ 
#' \item{visual}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Visual)
#' attach(Visual)
#' stem(visual)
#' fivenum(visual)
#' boxplot(visual)
#' detach(Visual)
#' 
NULL





#' Reading scores before and after vocabulary training for 14 employees who did
#' not complete high school
#' 
#' Data for Exercise 7.80
#' 
#' 
#' @name Vocab
#' @docType data
#' @format A data frame with 14 observations on the following 2 variables.
#' \describe{ 
#' \item{First}{a numeric vector} 
#' \item{Second}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Vocab)
#' attach(Vocab)
#' DIF <- Second - First
#' qqnorm(DIF)
#' qqline(DIF)
#' shapiro.test(DIF)
#' t.test(Second,First,paired=TRUE)
#' detach(Vocab)
#' remove(DIF)
#' 
NULL





#' Volume of injected waste water from Rocky Mountain Arsenal and number of
#' earthquakes near Denver
#' 
#' Data for Exercise 9.18
#' 
#' 
#' @name Wastewat
#' @docType data
#' @format A data frame with 44 observations on the following 4 variables.
#' \describe{ 
#' \item{gallons}{a numeric vector} 
#' \item{number}{a numeric vector} 
#' \item{ln.no.}{a numeric vector}
#' \item{index}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Wastewat)
#' attach(Wastewat)
#' model <- lm(number~gallons)
#' summary(model)
#' detach(Wastewat)
#' remove(model)
#' 
NULL





#' Weather casualties in 1994
#' 
#' Data for Exercise 1.30
#' 
#' 
#' @name Weather94
#' @docType data
#' @format A data frame with 11 observations on the following 2 variables.
#' \describe{ 
#' \item{Weather.Type}{a factor with levels \code{Extreme
#' Temp} \code{Flash flood} \code{Fog} \code{High wind} \code{Hurricane}
#' \code{Lightning} \code{Other} \code{River flood} \code{Thunderstorm}
#' \code{Tornado} \code{Winter weather}} 
#' \item{Number}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Weather94)
#' attach(Weather94)
#' names(Number) <- Weather.Type
#' barplot(Number,col="lightblue",las=2,cex.names=.65,main="Problem 1.30") 
#' # las=2 places bar names vertically
#' detach(Weather94)
#' 
NULL





#' Price of a bushel of wheat versus the national weekly earnings of production
#' workers
#' 
#' Data for Exercise 2.11
#' 
#' 
#' @name Wheat
#' @docType data
#' @format A data frame with 19 observations on the following 3 variables.
#' \describe{ 
#' \item{year}{a numeric vector} 
#' \item{earnings}{a numeric vector} 
#' \item{price}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Wheat)
#' attach(Wheat)
#' par(mfrow=c(1,2))
#' plot(year,earnings)
#' plot(year,price)
#' par(mfrow=c(1,1))
#' detach(Wheat)
#' 
NULL





#' Direct current produced by different wind velocities
#' 
#' Data for Exercise 9.34
#' 
#' 
#' @name Windmill
#' @docType data
#' @format A data frame with 25 observations on the following 7 variables.
#' \describe{ 
#' \item{velocity}{a numeric vector} 
#' \item{output}{a numeric vector} 
#' \item{SRES1}{a numeric vector}
#' \item{FITS1}{a numeric vector} 
#' \item{X1.velocity}{a numeric vector} 
#' \item{SRES2}{a numeric vector} 
#' \item{FITS2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Windmill)
#' attach(Windmill)
#' summary(lm(output~velocity))
#' detach(Windmill)
#' 
NULL





#' Wind leakage for storm windows exposed to a 50 mph wind
#' 
#' Data for Exercise 6.54
#' 
#' 
#' @name Window
#' @docType data
#' @format A data frame with 9 observations on the following 2 variables.
#' \describe{ 
#' \item{Window}{a numeric vector} 
#' \item{Leakage}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Window)
#' attach(Window)
#' SIGN.test(Leakage,md=.125,alternative="greater")
#' detach(Window)
#' 
NULL





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
#' \item{team}{a factor with levels \code{Atlanta}
#' \code{Chicago} \code{Cincinnati} \code{Houston} \code{Los Angeles}
#' \code{Montreal} \code{New York} \code{Philadelphia} \code{Pittsburgh}
#' \code{San Diego} \code{San Francisco} \code{St. Louis}}
#' \item{wins}{a numeric vector} 
#' \item{batavg}{a numeric vector} 
#' \item{rbi}{a numeric vector} 
#' \item{stole}{a numeric vector} 
#' \item{strkout}{a numeric vector} 
#' \item{caught}{a numeric vector} 
#' \item{errors}{a numeric vector} 
#' \item{era}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Wins)
#' attach(Wins)
#' plot(era,wins)
#' model <- lm(wins~era)
#' abline(model)
#' summary(model)
#' detach(Wins)
#' remove(model)
#' 
NULL





#' Strength tests of two types of wool fabric
#' 
#' Data for Exercise 7.42
#' 
#' 
#' @name Wool
#' @docType data
#' @format A data frame with 10 observations on the following 2 variables.
#' \describe{ 
#' \item{Type.1}{a numeric vector} 
#' \item{Type.2}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Wool)
#' attach(Wool)
#' t.test(Type.1,Type.2,var.equal=TRUE)
#' detach(Wool)
#' 
NULL





#' Monthly sunspot activity from 1974 to 2000
#' 
#' Data for Exercise 2.7
#' 
#' 
#' @name Yearsunspot
#' @docType data
#' @format A data frame with 252 observations on the following 24 variables.
#' \describe{ 
#' \item{X1979}{a numeric vector} 
#' \item{X1980}{a numeric vector} 
#' \item{X1981}{a numeric vector}
#' \item{X1982}{a numeric vector} 
#' \item{X1983}{a numeric vector} 
#' \item{X1984}{a numeric vector} 
#' \item{X1985}{a numeric vector} 
#' \item{X1986}{a numeric vector}
#' \item{X1987}{a numeric vector} 
#' \item{X1988}{a numeric vector} 
#' \item{X1989}{a numeric vector} 
#' \item{X1990}{a numeric vector} 
#' \item{X1991}{a numeric vector}
#' \item{X1992}{a numeric vector} 
#' \item{X1993}{a numeric vector} 
#' \item{X1994}{a numeric vector} 
#' \item{X1995}{a numeric vector} 
#' \item{X1996}{a numeric vector}
#' \item{X1997}{a numeric vector}
#' \item{X1998}{a numeric vector} 
#' \item{X1999}{a numeric vector} 
#' \item{X2000}{a numeric vector} 
#' \item{SSN}{a numeric vector} 
#' \item{year}{a numeric vector} 
#' }
#' @references Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
#' Duxbury
#' @keywords datasets
#' @examples
#' 
#' str(Yearsunspot)
#' attach(Yearsunspot)
#' boxplot(SSN~year,main="Exercise 2.7",col="lightblue")
#' detach(Yearsunspot)
#' 
"Yearsunspot"
#' 








