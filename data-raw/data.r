# Script data rework Start 3/24/16
Abbey <- read.csv('abbey.csv')
devtools::use_data(Abbey, overwrite = TRUE)
#
Abilene <- read.csv('Abilene.csv', colClasses = c("factor", "factor", "numeric"))
devtools::use_data(Abilene, overwrite = TRUE)
#
Abc <- read.csv('Abc.csv')
devtools::use_data(Abc, overwrite = TRUE)
# Create Ability
mat <- matrix(data = c(56, 35, 61, 43, 54, 61, 21, 42, 8, 19), nrow = 2)
dimnames(mat) <- list(gender = c("girls", "boys"), 
                      ability = c("hopeless", "belowavg", "average", 
                                  "aboveavg", "superior"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Ability <- vcdExtra::expand.dft(matDF)
Ability$ability <- factor(Ability$ability, 
                          levels = c("hopeless", "belowavg", "average", 
                                     "aboveavg", "superior"))
Ability$gender <- factor(Ability$gender, levels = c("girls", "boys"))
devtools::use_data(Ability, overwrite = TRUE)
#
Abortion <- read.csv('Abortion.csv')
Abortion$rate <- ifelse(Abortion$lowhigh == 1, "Low", "High")
devtools::use_data(Abortion, overwrite = TRUE)
#
Absent <- read.csv('Absent.csv')
devtools::use_data(Absent, overwrite = TRUE)
#
Achieve <- read.csv('Achieve.csv')
Achieve$gender <- as.factor(ifelse(Achieve$gender == 1, "girls", "boys"))
devtools::use_data(Achieve, overwrite = TRUE)
#
Adsales <- read.csv('Adsales.csv', stringsAsFactors = FALSE)
devtools::use_data(Adsales, overwrite = TRUE)
#
Aggress <- read.csv('Aggress.csv')
devtools::use_data(Aggress, overwrite = TRUE)
#
Aid <- read.csv("Aid.csv")
devtools::use_data(Aid, overwrite = TRUE)
#
Aids <- read.csv("Aids.csv")
devtools::use_data(Aids, overwrite = TRUE)
#
Airdisasters <- read.csv("Airdisasters.csv")
# Fix data set
Airdisasters$decade[Airdisasters$year >= 1930 & Airdisasters$year < 1940] <- "1930s"
Airdisasters$decade[Airdisasters$year >= 1940 & Airdisasters$year < 1950] <- "1940s"
Airdisasters$decade[Airdisasters$year >= 1950 & Airdisasters$year < 1960] <- "1950s"
Airdisasters$decade[Airdisasters$year >= 1960 & Airdisasters$year < 1970] <- "1960s"
Airdisasters$decade[Airdisasters$year >= 1970 & Airdisasters$year < 1980] <- "1970s"
Airdisasters$decade[Airdisasters$year >= 1980 & Airdisasters$year < 1990] <- "1980s"
Airdisasters$decade[Airdisasters$year >= 1990 & Airdisasters$year < 2000] <- "1990s"
devtools::use_data(Airdisasters, overwrite = TRUE)
#
Airline <- read.csv("Airline.csv", stringsAsFactors = FALSE)
devtools::use_data(Airline, overwrite = TRUE)
#
Alcohol <- read.csv("Alcohol.csv")
devtools::use_data(Alcohol, overwrite = TRUE)
# Create Allergy
mat <- matrix(data = c(97, 65, 27, 77, 49, 14, 12, 43, 22), nrow = 3)
dimnames(mat) <- list(event = c("insomnia", "headache", "drowsiness"), 
                      medication = c("seldane-d", "pseudoephedrine", "placebo"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Allergy <- vcdExtra::expand.dft(matDF)
Allergy$event <- factor(Allergy$event, 
                          levels = c("insomnia", "headache", "drowsiness"))
Allergy$medication <- factor(Allergy$medication, 
                        levels = c("seldane-d", "pseudoephedrine", "placebo"))
devtools::use_data(Allergy, overwrite = TRUE)
#
# Anesthet
Anesthet <- read.csv("Anesthet.csv")
devtools::use_data(Anesthet, overwrite = TRUE)
#
# Apolip
Apolipop <- read.csv("Apolipop.csv")
devtools::use_data(Apolipop, overwrite = TRUE)
#
# Append
Append <- read.csv("Append.csv")
devtools::use_data(Append, overwrite = TRUE)
#
# Appendec
Appendec <- read.csv("Appendec.csv")
devtools::use_data(Appendec, overwrite = TRUE)
#
# Aptitude
Aptitude <- read.csv("Aptitude.csv")
devtools::use_data(Aptitude, overwrite = TRUE)
#
# Archaeo
Archaeo <- read.csv("Archaeo.csv")
devtools::use_data(Archaeo, overwrite = TRUE)
#
# Arthriti
Arthriti <- read.csv("Arthriti.csv")
str(Arthriti)
Arthriti$treatment <- as.factor(ifelse(Arthriti$treatment == 1, "A", 
                                       ifelse(Arthriti$treatment == 2, "B", "C")))
devtools::use_data(Arthriti, overwrite = TRUE)
#
# Artifici
Artifici <- read.csv("Artifici.csv")
devtools::use_data(Artifici, overwrite = TRUE)
#
#
Aspirin <- read.csv("Aspirin.csv")
Aspirin$impurity <- factor(Aspirin$impurity, 
                        levels = c("1%", "5%", "10%"))
str(Aspirin)
devtools::use_data(Aspirin, overwrite = TRUE)
#
# Asthmati
Asthmati <- read.csv("Asthmati.csv")
devtools::use_data(Asthmati, overwrite = TRUE)
#
# Attorney
Attorney <- read.csv("Attorney.csv")
devtools::use_data(Attorney, overwrite = TRUE)
#
# Autogear
Autogear <- read.csv("Autogear.csv")
devtools::use_data(Autogear, overwrite = TRUE)
#
# Backtoback
Backtoback <- read.csv("Backtoback.csv")
devtools::use_data(Backtoback, overwrite = TRUE)
#
# Bbsalaries
Bbsalaries <- read.csv("Bbsalaries.csv")
devtools::use_data(Bbsalaries, overwrite = TRUE)
#
# Bigten
Bigten <- read.csv("Bigten.csv")
devtools::use_data(Bigten, overwrite = TRUE)
#
boxplot(rate ~ status, data = subset(Bigten, year = "1993-1994"), 
        horizontal = TRUE, main = "Graduation Rates 1993-1994")
with(data = Bigten,
tapply(rate, list(year, status), mean)
)
