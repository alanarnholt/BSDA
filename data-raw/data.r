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
Asprin <- read.csv("Aspirin.csv")
Asprin$impurity <- factor(Asprin$impurity, 
                        levels = c("1%", "5%", "10%"))
str(Asprin)
devtools::use_data(Asprin, overwrite = TRUE)
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
#
# Biology
Biology <- read.csv("Biology.csv")
devtools::use_data(Biology, overwrite = TRUE)
#
# Birth
Birth <- read.csv("Birth.csv", colClasses = c("character", "numeric", "factor"))
devtools::use_data(Birth, overwrite = TRUE)
#
rate1998 <- subset(Birth, year == "1998", select = rate, drop = TRUE)
stem(x = rate1998, scale = 2)
hist(rate1998, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.14 in BSDA", col = "pink")
hist(rate1998, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.16 in BSDA", col = "pink", freq = FALSE)
lines(density(rate1998), lwd = 3)
#
# Blackedu
# Create Blackedu
# Create Allergy
mat <- matrix(data = c(486, 496, 659, 530, 691, 435, 208, 134, 96, 65), nrow = 2)
dimnames(mat) <- list(gender = c("Female", "Male"), 
                      education = c("High school dropout", "High school graduate", 
                                    "Some college", "Bachelor's degree", "Graduate degree"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Blackedu <- vcdExtra::expand.dft(matDF)
Blackedu$gender <- factor(Blackedu$gender, 
                        levels = c("Female", "Male"))
Blackedu$education <- factor(Blackedu$education, 
                             levels = c("High school dropout", "High school graduate", 
                                        "Some college", "Bachelor's degree", "Graduate degree"))
# Check
# xtabs(~gender + education, data = Blackedu)  # OK now
devtools::use_data(Blackedu, overwrite = TRUE)
#
# Blood
Blood <- read.csv("Blood.csv")
devtools::use_data(Blood, overwrite = TRUE)
# checks
DIFF <- Blood$machine - Blood$expert
qqnorm(DIFF)
qqline(DIFF)
rm(DIFF)
t.test(Blood$machine, Blood$expert, paired = TRUE)
#
# Board
Board <- read.csv("Board.csv")
devtools::use_data(Board, overwrite = TRUE)
# Checks
boxplot(salary ~ university, data = Board, col = c("red", "blue", "green"),
        ylab = "Income")
tapply(Board$salary, Board$university, summary)
anova(lm(salary ~ university, data = Board))
#
# Bones
Bones <- read.csv("Bones.csv")
devtools::use_data(Bones, overwrite = TRUE)
#
# Books
Books <- read.csv("Books.csv")
devtools::use_data(Books, overwrite = TRUE)
# Examples
plot(spelling ~ book, data = Books)
mod <- lm(spelling ~ book, data = Books)
summary(mod)
abline(mod, col = "blue", lwd = 2)
#
# Bookstor
Bookstor <- read.csv("Bookstor.csv")
devtools::use_data(Bookstor, overwrite = TRUE)
# Checks
boxplot(dollars ~ store, data = Bookstor, 
        col = c("purple", "lightblue", "cyan"))
kruskal.test(dollars ~ store, data = Bookstor)
#
# Brain
Brain <- read.csv("Brain.csv")
devtools::use_data(Brain, overwrite = TRUE)
# Checks
plot(log(brainweight) ~ log(bodyweight), data = Brain,
     pch = 19, col = "blue", main = "Example 2.3")
mod <- lm(log(brainweight) ~ log(bodyweight), data = Brain)
abline(mod, lty = "dashed", col = "blue")
#
# Bumpers
Bumpers <- read.csv("Bumpers.csv")
devtools::use_data(Bumpers, overwrite = TRUE)
# Checks
str(Bumpers)
#
# Bus
# Create Bus
mat <- matrix(data = c(454, 5806, 208, 2112, 491, 3989, 160, 3790, 1599, 10754), nrow = 2)
dimnames(mat) <- list(attendance = c("absent", "present"), 
                      shift = c("am", "noon", "pm", "swing", "split"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Bus <- vcdExtra::expand.dft(matDF)
Bus$attendance <- factor(Bus$attendance, 
                          levels = c("absent", "present"))
Bus$shift <- factor(Bus$shift, levels = c("am", "noon", "pm", "swing", "split"))
# Checks
xtabs(~attendance + shift, data = Bus)
devtools::use_data(Bus, overwrite = TRUE)
#
# Bypass
Bypass <- read.csv("Bypass.csv")
devtools::use_data(Bypass, overwrite = TRUE)
#
# Cabinets
Cabinets <- read.csv("Cabinets.csv")
devtools::use_data(Cabinets, overwrite = TRUE)
# Examples
DIF <- Cabinets$SupplA - Cabinets$SupplB
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
with(data = Cabinets,
t.test(SupplA, SupplB, paired = TRUE)
)
with(data = Cabinets,
wilcox.test(SupplA, SupplB, paired = TRUE)
)
rm(DIF)
#
# Cancer
Cancer <- read.csv("Cancer.csv")
devtools::use_data(Cancer, overwrite = TRUE)
# Examples
str(Cancer)
boxplot(survival ~ type, Cancer)
stomach <- Cancer$survival[Cancer$type == "stomach"]
bronchus <- Cancer$survival[Cancer$type == "bronchus"]
boxplot(stomach, ylab = "Days")
SIGN.test(stomach, md = 100, alternative = "greater")
SIGN.test(bronchus, md = 100, alternative = "greater")
rm(bronchus, stomach)
#
# Carbon
Carbon <- read.csv("Carbon.csv")
devtools::use_data(Carbon, overwrite = TRUE)
# Examples
str(Carbon)
boxplot(CO ~ site, data = Carbon)
kruskal.test(CO ~ site, data = Carbon)
#
# Cat
read.csv("Cat.csv")
devtools::use_data(Cat, overwrite = TRUE)
stem(Cat$score)
fivenum(Cat$score)
boxplot(Cat$score, main = "Problem 1.116", col = "green")
#
# Censored
Censored <- read.csv("Censored.csv")
devtools::use_data(Censored, overwrite = TRUE)
# Examples
str(Censored)
boxplot(survival ~ treatment, data = Censored)
wilcox.test(survival ~ treatment, data = Censored, alternative = "greater")
#
# Challeng
Challeng <- read.csv("Challeng.csv", colClasses = c("character", "character", "numeric", "numeric"))
Challeng$date <- as.Date(Challeng$date, "%m/%d/%y")
devtools::use_data(Challeng, overwrite = TRUE)
# Examples
str(Challeng)
stem(Challeng$temp)
summary(Challeng$temp)
IQR(Challeng$temp)
#
# Chemist
read.csv("Chemist.csv")
devtools::use_data(Chemist, overwrite = TRUE)
#
# Chesapea
read.csv("Chesapea.csv")
devtools::use_data(Chesapea, overwrite = TRUE)
#
# Create Chevy
#
mat <- matrix(data = c(16, 12, 5, 2, 5, 12, 3, 2, 4, 6), nrow = 2)
dimnames(mat) <- list(year = c("1988-90", "1991-93"), 
                      frequency = c("much better than average", "above average", 
                                    "average", "below average", "much worse than average"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Chevy <- vcdExtra::expand.dft(matDF)
Chevy$year <- factor(Chevy$year, 
                         levels = c("1988-90", "1991-93"))
Chevy$frequency <- factor(Chevy$frequency, levels = c("much better than average", "above average", 
                                                     "average", "below average", "much worse than average"))
# Checks
xtabs(~year + frequency, data = Chevy)
devtools::use_data(Chevy, overwrite = TRUE)
#
# Chicken
Chicken <- read.csv("Chicken.csv")
devtools::use_data(Chicken, overwrite = TRUE)
#
# Examples
str(Chicken)
#
# Chipavg
Chipavg <- read.csv("Chipavg.csv")
devtools::use_data(Chipavg, overwrite = TRUE)
#
# Chips
Chips <- read.csv("Chips.csv")
devtools::use_data(Chips, overwrite = TRUE)
#
# Cigar
Cigar <- read.csv("Cigar.csv")
devtools::use_data(Cigar, overwrite = TRUE)
# Examples
#
# Cigarett
Cigarett <- read.csv("Cigarett.csv")
devtools::use_data(Cigarett, overwrite = TRUE)
# Examples
#
# Darwin
Darwin <- read.csv("Darwin.csv")
devtools::use_data(Darwin, overwrite = TRUE)





