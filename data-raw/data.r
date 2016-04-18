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
# 
#
# Citrus
Citrus <- read.csv("Citrus.csv")
devtools::use_data(Citrus, overwrite = TRUE)
#
# Clean
Clean <- read.csv("Clean.csv")
devtools::use_data(Clean, overwrite = TRUE)
# Examples
<<<<<<< HEAD
#
# Darwin
Darwin <- read.csv("Darwin.csv")
devtools::use_data(Darwin, overwrite = TRUE)
=======
str(Clean)
#
# Coaxial
Coaxial <- read.csv("Coaxial.csv")
devtools::use_data(Coaxial, overwrite = TRUE)
# Examples
boxplot(signal ~ cable, data = Coaxial, col = c("red", "green", "yellow"))
kruskal.test(signal ~ cable, data = Coaxial)
#
# Coffee
Coffee <- read.csv("Coffee.csv")
devtools::use_data(Coffee, overwrite = TRUE)
# Examples
differences <- Coffee$with - Coffee$without
qqnorm(differences)
qqline(differences)
shapiro.test(differences)
t.test(Coffee$with, Coffee$without, paired = TRUE, alternative = "greater")
wilcox.test(Coffee$with, Coffee$without, paired = TRUE, alterantive = "greater")
rm(differences)
#
# Coins
Coins <- read.csv("Coins.csv")
devtools::use_data(Coins, overwrite = TRUE)
#
# Commute
Commute <- read.csv("Commute.csv")
devtools::use_data(Commute, overwrite = TRUE)
# Examples
commute <- stack(Commute)
str(commute)
stripplot(ind ~ values, data = commute, jitter = TRUE)
dotplot(ind ~ values, data = commute)
bwplot(ind ~ values, data = commute)
>>>>>>> b00a2bc75db4b07fe2dfe69b95b3e11380c2594b

stripchart(values ~ ind, data = commute, method = "stack", pch = 1, cex = 2, 
           col = c("red", "blue"), group.names = c("1980", "1990"), main = "",
           xlab = "minutes")
title(main = "Commute Time")
boxplot(values ~ ind, data = commute, names=c("1980", "1990"), 
        horizontal = TRUE, las = 1)
rm(commute)
#
# Concept
Concept <- read.csv("Concept.csv")
devtools::use_data(Concept, overwrite = TRUE)
#
str(Concept)
#
# Concrete
Concrete <- read.csv("Concrete.csv")
devtools::use_data(Concrete, overwrite = TRUE)
#
str(Concrete)
#
# Corn
Corn <- read.csv("Corn.csv")
devtools::use_data(Corn, overwrite = TRUE)
#
str(Corn)
#
# Correlat
Correlat <- read.csv("Correlat.csv")
devtools::use_data(Correlat, overwrite = TRUE)
#
#
Counsel <- read.csv("Counsel.csv")
devtools::use_data(Counsel, overwrite = TRUE)
#
str(Counsel)
#
# Cpi
Cpi <- read.csv("Cpi.csv")
devtools::use_data(Cpi, overwrite = TRUE)
#
# Crime
Crime <- read.csv("Crime.csv", colClasses = c("factor", "factor", "numeric"))
devtools::use_data(Crime, overwrite = TRUE)
# 
# Examples
str(Crime)
boxplot(rate ~ year, data = Crime)
#
# Darwin
Darwin <- read.csv("Darwin.csv")
devtools::use_data(Darwin, overwrite = TRUE)

###### Erin Working
##
#
#
# Creating Dealers
mat <- matrix(data = c(19,3,12,8,11,4,2,16,9,13,10,15), nrow = 6)
dimnames(mat) <- list(dealership = c("Honda", "Toyota","Mazda","Ford","Dodge","Saturn"), 
                      service = c("Before Needed","Only When Recommended"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Dealers <- vcdExtra::expand.dft(matDF)
Dealers$dealership <- factor(Dealers$dealership, 
                          levels = c("Honda", "Toyota","Mazda","Ford","Dodge","Saturn"))
Dealers$service <- factor(Dealers$service, levels = c("Before Needed","Only When Recommended"))
devtools::use_data(Dealers, overwrite = TRUE)
# Check
T1 <- xtabs(~dealership+service,data=Dealers)
barplot(t(T1), beside = TRUE)

# Create Defectiv
Defectiv <- read.csv("Defectiv.csv")
devtools::use_data(Defectiv,overwrite=TRUE)
# Check
T1 <- table(Defectiv$C1)
barplot(T1, col = "green", ylab = "Frequency",xlab="Defective Items Produced by Employees",main="Problem 1.27")

# Create Degree
Dmat <- as.matrix(Degree[,2:3])
rownames(Dmat) <- Degree$Field
colnames(Dmat) <- c("1970","1990")
Dmat
devtools::use_data(Degree,overwrite=TRUE)
#Check
barplot(t(Dmat),beside=TRUE,legend=TRUE,cex.names=.5)

# Create Delay
Delay <- read.csv("Delay.csv")
devtools::use_data(Delay,overwrite=TRUE)
#Checks
boxplot(Delay$delay~Delay$carrier)
kruskal.test(Delay$delay~as.factor(Delay$carrier))


# Create Depend
Depend <- read.csv("Depend.csv")
devtools::use_data(Depend,overwrite=TRUE)
# Checks
T1 <- xtabs(~children, data = Depend)
T1
barplot(T1, col = "lightblue", main = "Number of Dependent \nChildren for 50 Families")

# Create Detroit
Detroit <- read.csv("Detroit.csv")
devtools::use_data(Detroit,overwrite=TRUE)

# Create Develop
Develop <- read.csv("Develop.csv")
devtools::use_data(Develop,overwrite=TRUE)
# check
chisq.test(Develop$two.year,Develop$four.yr)

# Create Devmath
Devmath <- read.csv("Devmath.csv")
devtools::use_data(Devmath,overwrite=TRUE)
# checks
EDA(Devmath$score)
t.test(Devmath$score,mu=80,alternative="less")

# Create Dice
Dice <- read.csv("Dice.csv")
devtools::use_data(Dice,overwrite=TRUE)

# Create Diesel
Diesel <- read.csv("Diesel.csv")
devtools::use_data(Diesel,overwrite=TRUE)
#checks 
boxplot(Diesel$NatAvg,Diesel$EstCst,Diesel$Gulf,Diesel$Rocky,Diesel$Calif,col="pink")

# Create Diplomat
Diplomat <- read.csv("Diplomat.csv")
devtools::use_data(Diplomat,overwrite=TRUE)
# checks 
par(mfrow=c(1,2))
names(Diplomat$number) <- Diplomat$country
dotchart(Diplomat$number,main="Number of Tickets",col="blue",pch=1)
names(Diplomat$rate) <- Diplomat$country
dotchart(Diplomat$rate,main="Tickets/Vehicle/Month",col="red",pch=2)
barplot(Diplomat$rate,col="pink")

# Create Disposal
Disposal <- read.csv("Disposal.csv")
devtools::use_data(Disposal,overwrite=TRUE)
# check
stem(Disposal$pounds)
fivenum(Disposal$pounds)
EDA(Disposal$pounds)


#Create Dogs
Dogs <- read.csv("Dogs.csv")
devtools::use_data(Dogs,overwrite=TRUE)

# Create Domestic
Domestic <- read.csv("Domestic.csv")
devtools::use_data(Domestic,overwrite=TRUE)
#checks
names(Domestic$Rate) <- Domestic$Age
barplot(Domestic$Rate,col="gold")
pie(Domestic$Rate)

# Create Dopamine
Dopamine <- read.csv("Dopamine.csv")
devtools::use_data(Dopamine,overwrite=TRUE)
# checks
boxplot(Dopamine$DBH~Dopamine$group,names=c("Non Psychotic","Psychotic"))
t.test(Dopamine$DBH~Dopamine$group,var.equal=TRUE)

# Create Dowjones
Dowjones <- read.csv("Dowjones.csv")
devtools::use_data(Dowjones,overwrite=TRUE)
#checks
plot(Dowjones$year,Dowjones$close,type="l",lty=2,lwd=2,col="blue")
barplot(Dowjones$close,col="blue",las=2,main="Problem 1.35",names.arg=FALSE)

# Create Drink
mat <- matrix(data = c(95,73,12,83,71,46,21,18,8), nrow = 3)
dimnames(mat) <- list(Drink = c("Ok", "Tolerated","Immoral"), View = c("For","Against","Undecided"))
mat
class(mat)
matT <- as.table(mat)
matT
class(matT)
matDF <- as.data.frame(matT)
matDF
class(matDF)
DF <- vcdExtra::expand.dft(matDF)
devtools::use_data(Drink,overwrite=TRUE)
#checks
head(DF)
class(DF)
CT <- xtabs(~Drink + View, data = DF)
CT
chisq.test(CT)


# Create Drug
Drug <- read.csv("Drug.csv")
devtools::use_data(Drug,overwrite=TRUE)
# checks
boxplot(Drug$trials~Drug$group)
wilcox.test(Drug$trials~Drug$group)
