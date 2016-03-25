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

