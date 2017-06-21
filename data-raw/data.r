# Script data rework Start 3/24/16
# Updated 6/17/17
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
#################################
Abbey <- read_csv("Abbey.csv")
devtools::use_data(Abbey, overwrite = TRUE)
# Examples
qqnorm(Abbey$price)
qqline(Abbey$price)
t.test(Abbey$price, mu = 300)
#################################
Abc <- read_csv("ABC.csv")
Abc <- Abc %>%
  gather(GroupA, GroupB, GroupC, key = "group", value = "response")
Abc$group <-str_replace(string = Abc$group, pattern= "Group", replacement = "")
Abc
devtools::use_data(Abc, overwrite = TRUE)
# Examples
boxplot(response ~ group, col = c("red", "blue", "green"), data = Abc)
anova(lm(response ~ group, data = Abc))
#################################
Abilene <- read_csv("ABILENE.csv")
Abilene <- Abilene %>%
  gather(`1992`, `1999`, key = "year", value = "number") %>%
  select(crimetype = Crime, year, number)
Abilene$year <- factor(Abilene$year)
devtools::use_data(Abilene, overwrite = TRUE)
# Examples
par(mfrow = c(2, 1))
barplot(Abilene$number[Abilene$year=="1992"],
        names.arg = Abilene$crimetype[Abilene$year == "1992"],
        main = "1992 Crime Stats", col = "red")
barplot(Abilene$number[Abilene$year=="1999"],
        names.arg = Abilene$crimetype[Abilene$year == "1999"],
        main = "1999 Crime Stats", col = "blue")
par(mfrow = c(1, 1))

## Not run: 
library(ggplot2)
ggplot(data = Abilene, aes(x = crimetype, y = number, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
## End(Not run)
#################################
mat <- matrix(data = c(56, 35, 61, 43, 54, 61, 21, 42, 8, 19), nrow = 2)
dimnames(mat) <- list(gender = c("girls", "boys"), 
                      ability = c("hopeless", "belowavg", "average", 
                                  "aboveavg", "superior"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Ability <- vcdExtra::expand.dft(matDF)
rm(mat, matDF, matT)
Ability$ability <- factor(Ability$ability, 
                          levels = c("hopeless", "belowavg", "average", 
                                     "aboveavg", "superior"))
Ability$gender <- factor(Ability$gender, levels = c("girls", "boys"))
Ability <- as_tibble(Ability)
devtools::use_data(Ability, overwrite = TRUE)
# Examples
CT <- xtabs(~gender + ability, data = Ability)
CT
chisq.test(CT)
rm(CT)
#################################
Abortion <- read_csv("ABORTION.csv")
Abortion <- Abortion %>%
  select(state, region, regcode, rate1988 = `88rate`, 
         rate1992 = `92rate`, rate1996 = `96rate`,
         provide1988 = `88provid`, provide1992 = `92provid`, lowhigh)
Abortion$rate <- ifelse(Abortion$rate1996 <= 17, "low", "high")
Abortion
devtools::use_data(Abortion, overwrite = TRUE)
# Examples
T1 <- xtabs(~region + rate, data = Abortion)
T1
chisq.test(T1)
rm(T1)
#################################
Absent <- read_csv("ABSENT.csv")
Absent <- Absent %>%
  select(days)
Absent
devtools::use_data(Absent, overwrite = TRUE)
# Examples
CT <- xtabs(~ days, data = Absent)
CT
barplot(CT, col = "pink")
plot(ecdf(Absent$days), main = "ECDF")
rm(CT)
#################################
Achieve <- read_csv("ACHIEVE.csv")
Achieve <- Achieve %>%
  select(score = Score, gender = Gender)
Achieve$gender <- as.factor(ifelse(Achieve$gender == 1, "girls", "boys"))  
Achieve
devtools::use_data(Achieve, overwrite = TRUE)
# Examples
anova(lm(score ~ gender, data = Achieve))
t.test(score ~ gender, var.equal = TRUE, data = Achieve)
#################################
Adsales <- read_csv("ADSALES.csv")
Adsales <- Adsales %>%
  mutate(month = month.abb[3:8]) %>%
  select(month, ads, sales)
Adsales
devtools::use_data(Adsales, overwrite = TRUE)
# Examples
plot(sales ~ ads, data = Adsales)
mod <- lm(sales ~ ads, data = Adsales)
abline(mod)
summary(mod)
predict(mod, newdata = data.frame(ads = 6), interval = "conf", level = 0.99)
rm(mod)
#################################
Aggress <- read_csv("AGGRESS.csv")
Aggress
devtools::use_data(Aggress, overwrite = TRUE)
# Examples
str(Aggress)
with(data = Aggress,
     EDA(aggres))
# OR
IQR(Aggress$aggres)
diff(range(Aggress$aggres))
#################################
Aid <- read_csv("AID.csv")
Aid <- Aid %>%
  rename(state = State, payment = payment)
devtools::use_data(Aid, overwrite = TRUE)
# Examples
str(Aid)
hist(Aid$payment, xlab = "payment", main = "Your Title Here", 
     col = "lightblue")
boxplot(Aid$payment)
dotplot(state ~ payment, data = Aid)
#################################
Aids <- read_csv("AIDs.csv")
Aids <- Aids %>%
  select(duration, age, group)
devtools::use_data(Aids, overwrite = TRUE)
# Examples
Aids
with(data = Aids,
     EDA(duration)
)
with(data = Aids, 
     t.test(duration, mu = 30, alternative = "greater")
)
with(data = Aids, 
     SIGN.test(duration, md = 24, alternative = "greater")
)
#################################
Airdisasters <- read_csv("AIRDISASTERS.csv")
Airdisasters <- Airdisasters %>%
  select(year, deaths)
Airdisasters$decade <- ifelse(Airdisasters$year >=1930 & Airdisasters$year < 1940,   "30s", 
                       ifelse(Airdisasters$year >=1940 & Airdisasters$year < 1950,   "40s", 
                       ifelse(Airdisasters$year >=1950 & Airdisasters$year < 1960,   "50s", 
                       ifelse(Airdisasters$year >=1960 & Airdisasters$year < 1970,   "60s", 
                       ifelse(Airdisasters$year >=1970 & Airdisasters$year < 1980,   "70s", 
                       ifelse(Airdisasters$year >=1980 & Airdisasters$year < 1990,   "80s", "90s"))))))
devtools::use_data(Airdisasters, overwrite = TRUE)
# Examples
par(las = 1)
stripchart(deaths ~ decade, data = Airdisasters, 
           subset = decade != "1930s" & decade != "1940s", 
           method = "stack", pch = 19, cex = 0.5, col = "red", 
           main = "Aircraft Disasters 1950 - 1990", 
           xlab = "Number of fatalities")
par(las = 0)
#################################
Airline <- read_csv("AIRLINE.csv")
Airline <- Airline %>%
  rename(complaints = complnt)
devtools::use_data(Airline, overwrite = TRUE)
# Examples
with(data = Airline, 
     barplot(complaints, names.arg = airline, col = "lightblue")
)
plot(complaints ~ ontime, data = Airline, pch = 19, col = "red",
     xlab = "On time", ylab = "Complaints")
#################################
Alcohol <- read_csv("ALCOHOL.csv")
devtools::use_data(Alcohol, overwrite = TRUE)
# Examples
qqnorm(Alcohol$age)
qqline(Alcohol$age)
SIGN.test(Alcohol$age, md = 20, conf.level = 0.99)
#################################
mat <- matrix(data = c(97, 65, 27, 77, 49, 14, 12, 43, 22), nrow = 3)
dimnames(mat) <- list(event = c("insomnia", "headache", "drowsiness"), 
                      medication = c("seldane-d", "pseudoephedrine", "placebo"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Allergy <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Allergy$event <- factor(Allergy$event, 
                        levels = c("insomnia", "headache", "drowsiness"))
Allergy$medication <- factor(Allergy$medication, 
                             levels = c("seldane-d", "pseudoephedrine", "placebo"))
Allergy <- as_tibble(Allergy)
devtools::use_data(Allergy, overwrite = TRUE)
# Examples
T1 <- xtabs(~event + medication, data = Allergy)
T1
chisq.test(T1)
rm(T1)
#################################
Anesthet <- read_csv("ANESTHET.csv")
devtools::use_data(Anesthet, overwrite = TRUE)
# Examples
qqnorm(Anesthet$recover)
qqline(Anesthet$recover)
with(data = Anesthet,
     t.test(recover, conf.level = 0.90)$conf
)
#################################
Anxiety <- read_csv("ANXIETY.csv")
devtools::use_data(Anxiety, overwrite = TRUE)
# Examples
plot(math ~ anxiety, data = Anxiety)
with(data = Anxiety,
     cor(math, anxiety)
)
linmod <- lm(math ~ anxiety, data = Anxiety)
abline(linmod)
summary(linmod)
rm(linmod)
#################################
Apolipop <- read_csv("APOLIPOP.csv")
Apolipop <- Apolipop %>%
  select(coffee, apolipB)
devtools::use_data(Apolipop, overwrite = TRUE)
# Examples
plot(apolipB ~ coffee, data = Apolipop)
linmod <- lm(apolipB ~ coffee, data = Apolipop)
summary(linmod)
summary(linmod)$sigma
anova(linmod)
anova(linmod)[2, 3]^.5
par(mfrow = c(2, 2))
plot(linmod)
par(mfrow = c(1, 1))
rm(linmod)
#################################
Append <- read_csv("APPEND.csv")
devtools::use_data(Append, overwrite = TRUE)
# Examples
fee <- Append$fee
ll <- mean(fee) - 2*sd(fee)
ul <- mean(fee) + 2*sd(fee)
limits <-c(ll, ul)
limits
fee[fee < ll | fee > ul]
rm(fee, limits, ll, ul)
#################################
Appendec <- read_csv("APPENDEC.csv")
Appendec$regionc <- ifelse(Appendec$Region == 1, "rural", 
                           ifelse(Appendec$Region == 2, "regional", 
                                  "metropolitan"))
Appendec <- Appendec %>%
  select(cost = Cost, region = regionc)
devtools::use_data(Appendec, overwrite = TRUE)
# Examples
boxplot(cost ~ region, data = Appendec, col = c("red", "blue", "cyan"))
anova(lm(cost ~ region, data = Appendec))
#################################
Aptitude <- read_csv("Aptitude.csv")
Aptitude <- Aptitude %>%
  select(aptitude, product)
devtools::use_data(Aptitude, overwrite = TRUE)
# Examples
plot(product ~ aptitude, data = Aptitude, main = "Exercise 2.1")
model1 <- lm(product ~ aptitude, data = Aptitude)
model1
abline(model1, col = "red", lwd=3)
resid(model1)
fitted(model1)
cor(Aptitude$product, Aptitude$aptitude)
rm(model1)
#################################
Archaeo <- read_csv("Archaeo.csv")
Archaeo <- Archaeo %>%
  select(age, phase)
devtools::use_data(Archaeo, overwrite = TRUE)
# Examples
boxplot(age ~ phase, data = Archaeo, col = "yellow", 
        main = "Example 1.16", xlab = "Ceramic Phase", ylab = "Age")
anova(lm(age ~ phase, data= Archaeo))
#################################
Arthriti <- read_csv("Arthriti.csv")
Arthriti <- Arthriti %>%
  select(time = Time, treatment = Treatmnt)
Arthriti$treatment <- factor(Arthriti$treatment, labels = c("A", "B", "C"))
devtools::use_data(Arthriti, overwrite = TRUE)
# Examples
boxplot(time ~ treatment, data = Arthriti)
anova(lm(time ~ treatment, data = Arthriti))
#################################
Artifici <- read_csv("Artifici.csv")
devtools::use_data(Artifici, overwrite = TRUE)
# Examples
stem(Artifici$duration, 2)
summary(Artifici$duration)
values <- Artifici$duration[Artifici$duration < 6.5]
values
summary(values)
rm(values)
#################################
Asprin <- read_csv("Aspirin.csv")
devtools::use_data(Asprin, overwrite = TRUE)
# Examples
boxplot(time ~ impurity, data = Asprin, 
        col = c("red", "blue", "green"))
#################################
Asthmati <- read_csv("Asthmati.csv")
Asthmati <- Asthmati %>%
  rename(drug = Drug, placebo = Placebo, difference = differ)
devtools::use_data(Asthmati, overwrite = TRUE)
# Examples
qqnorm(Asthmati$difference)
qqline(Asthmati$difference)
shapiro.test(Asthmati$difference)
with(data = Asthmati,
     t.test(placebo, drug, paired = TRUE, mu = 0, 
            alternative = "greater")
)
#################################
Attorney <- read_csv("Attorney.csv")
Attorney <- Attorney %>%
  rename(staff = Staff, convict = Convict, district = District)
devtools::use_data(Attorney, overwrite = TRUE)
# Examples
par(mfrow=c(1, 2))
plot(convict ~ staff, data = Attorney, main = "With Washington, D.C.")
plot(convict[-86] ~staff[-86], data = Attorney, 
     main = "Without Washington, D.C.")
par(mfrow=c(1, 1))
#################################
Autogear <- read_csv("Autogear.csv")
Autogear <- Autogear %>%
  gather(`A`, `B`, key = "manufacturer", value = "defectives") %>%
  select(defectives, manufacturer)
devtools::use_data(Autogear, overwrite = TRUE)
# Examples
t.test(defectives ~ manufacturer, data = Autogear)
wilcox.test(defectives ~ manufacturer, data = Autogear)
t.test(defectives ~ manufacturer, var.equal = TRUE, data = Autogear)
#####################################################################################################################
Backtoback <- read_csv("Backtoback.csv")
Backtoback <- Backtoback %>%
  select(score, group)
devtools::use_data(Backtoback, overwrite = TRUE)
# Examples
wilcox.test(score ~ group, data = Backtoback)
t.test(score ~ group, data = Backtoback)
#################################
Bbsalaries <- read_csv("Bbsalaries.csv")
Bbsalaries <- Bbsalaries %>%
  gather(`ANGLES`, `ORIOLES`, `REDSOXS`, `WHITESOXS`, `INDIANS`, key = "team", value = "salary") %>%
  select(salary, team)
devtools::use_data(Bbsalaries, overwrite = TRUE)
# Examples
par(mar = c(5.1, 8.1, 4.1, 2.1))
stripchart(salary ~ team, data = Bbsalaries, method = "stack", 
           pch = 19, col = "blue", cex = 0.75, las = 1)
title(main = "Major League Salaries")
par(mar = c(5.1, 4.1, 4.1, 2.1))
#################################
Bigten <- read_csv("Bigten.csv")
Bigten1 <- Bigten %>%
  select(school = `School`, `1984-85students`, `1993-94students`) %>% 
  gather(`1984-85students`, `1993-94students`, key = "year", value = "rate")
Bigten1$year <-   str_replace(Bigten1$year, "students", "")
Bigten1$status <- rep("student", 22)
Bigten2 <- Bigten %>%
  select(school = `School`, `1984-85athletes`, `1993-94athletes`) %>% 
  gather(`1984-85athletes`, `1993-94athletes`, key = "year", value = "rate")
Bigten2$year <-   str_replace(Bigten2$year, "athletes", "")
Bigten2$status <- rep("athlete", 22)
Bigten <- bind_rows(Bigten1, Bigten2)
Bigten$year <- factor(Bigten$year)
devtools::use_data(Bigten, overwrite = TRUE)
# Examples
boxplot(rate ~ status, data = subset(Bigten, year = "1993-1994"), 
        horizontal = TRUE, main = "Graduation Rates 1993-1994")
with(data = Bigten,
     tapply(rate, list(year, status), mean)
)
#################################
Biology <- read_csv("Biology.csv")
devtools::use_data(Biology, overwrite = TRUE)
# Examples
hist(Biology$score, breaks = "scott", col = "brown", freq = FALSE, 
     main = "Problem 1.49", xlab = "Test Score")
lines(density(Biology$score), lwd=3)
#################################
Birth <- read_csv("Birth.csv")
Birth <- Birth %>%
  gather(`1990rate`, `1998rate`, key = "year", value = "rate") %>%
  select(state = State, rate, year)
Birth$year <-str_replace(string = Birth$year, pattern= "rate", replacement = "")
Birth$year <- factor(Birth$year)
devtools::use_data(Birth, overwrite = TRUE)
# Examples
rate1998 <- subset(Birth, year == "1998", select = rate)
stem(x = rate1998$rate, scale = 2)
hist(rate1998$rate, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.14 in BSDA", col = "pink")
hist(rate1998$rate, breaks = seq(10.9, 21.9, 1.0), xlab = "1998 Birth Rate",
     main = "Figure 1.16 in BSDA", col = "pink", freq = FALSE)      
lines(density(rate1998$rate), lwd = 3)
rm(rate1998)
#################################
mat <- matrix(data = c(486, 496, 659, 530, 691, 435, 208, 134, 96, 65), nrow = 2)
dimnames(mat) <- list(gender = c("Female", "Male"), 
                      education = c("High school dropout", "High school graduate", 
                                    "Some college", "Bachelor's degree", "Graduate degree"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Blackedu <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Blackedu$gender <- factor(Blackedu$gender, 
                          levels = c("Female", "Male"))
Blackedu$education <- factor(Blackedu$education, 
                             levels = c("High school dropout", "High school graduate", 
                                        "Some college", "Bachelor's degree", "Graduate degree"))
Blackedu <- as_tibble(Blackedu)
devtools::use_data(Blackedu, overwrite = TRUE)
# Examples
T1 <- xtabs(~gender + education, data = Blackedu)
T1
chisq.test(T1)
rm(T1)
#################################
Blood <- read_csv("Blood.csv")
Blood <- Blood %>%
  rename(machine = Machine, expert = Expert)
devtools::use_data(Blood, overwrite = TRUE)
# Examples
DIFF <- Blood$machine - Blood$expert
shapiro.test(DIFF)
qqnorm(DIFF)
qqline(DIFF)
rm(DIFF)
t.test(Blood$machine, Blood$expert, paired = TRUE)
#################################
Board <- read_csv("Board.csv")
Board <- Board %>%
  gather(`UnivA`, `UnivB`, `UnivC`, key = "university", value = "salary")
Board$university <- str_replace(Board$university, "Univ", "")
Board <- Board %>%
  select(salary, university)
Board
devtools::use_data(Board, overwrite = TRUE)
# Examples
boxplot(salary ~ university, data = Board, col = c("red", "blue", "green"), 
        ylab = "Income")
tapply(Board$salary, Board$university, summary)
anova(lm(salary ~ university, data = Board))
Board %>%
  group_by(university) %>%
  summarize(min = min(salary), median = median(salary), max = max(salary))
#################################
Bones <- read_csv("Bones.csv")
Bones <- Bones %>%
  select(density = Density, group) 
Bones$group <- factor(Bones$group, labels = c("active", "nonactice"))
devtools::use_data(Bones, overwrite = TRUE)
# Examples
t.test(density ~ group, data = Bones, alternative = "greater")
t.test(rank(density) ~ group, data = Bones, alternative = "greater")
suppressWarnings(wilcox.test(density ~ group, data = Bones, 
                             alternative = "greater"))
#################################
Books <- read_csv("Books.csv")
devtools::use_data(Books, overwrite = TRUE)
# Examples
plot(spelling ~ book, data = Books)
mod <- lm(spelling ~ book, data = Books)
summary(mod)
abline(mod, col = "blue", lwd = 2)
rm(mod)
#################################
Bookstor <- read_csv("Bookstor.csv")
Bookstor <- Bookstor %>%
  select(dollars = Dollars, store = Store) 
Bookstor$store <- factor(Bookstor$store, labels = c("A", "B", "C"))
devtools::use_data(Bookstor, overwrite = TRUE)
# Examples
boxplot(dollars ~ store, data = Bookstor, 
        col = c("purple", "lightblue", "cyan"))
kruskal.test(dollars ~ store, data = Bookstor)
#################################
Brain <- read_csv("Brain.csv")
Brain <- Brain %>%
  select(species = `Species`, bodyweight = `body wt`, brainweight = `brain wt`)
devtools::use_data(Brain, overwrite = TRUE)
# Examples
plot(log(brainweight) ~ log(bodyweight), data = Brain, 
     pch = 19, col = "blue", main = "Example 2.3")
mod <- lm(log(brainweight) ~ log(bodyweight), data = Brain)      
abline(mod, lty = "dashed", col = "blue")
rm(mod)
#################################
Bumpers <- read_csv("Bumpers.csv")
Bumpers <- Bumpers %>%
  select(car = `Car`, repair)
Bumpers
devtools::use_data(Bumpers, overwrite = TRUE)
# Examples
EDA(Bumpers$repair)
stripchart(Bumpers$repair, method = "stack", pch = 19, col = "blue")
lattice::dotplot(car ~ repair, data = Bumpers)
#################################
mat <- matrix(data = c(454, 5806, 208, 2112, 491, 3989, 160, 3790, 1599, 10754), nrow = 2)
dimnames(mat) <- list(attendance = c("absent", "present"), 
                      shift = c("am", "noon", "pm", "swing", "split"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Bus <- vcdExtra::expand.dft(matDF)
Bus$attendance <- factor(Bus$attendance, 
                         levels = c("absent", "present"))
Bus$shift <- factor(Bus$shift, levels = c("am", "noon", "pm", "swing", "split"))
rm(mat, matT, matDF)
Bus <- as_tibble(Bus)
devtools::use_data(Bus, overwrite = TRUE)
# Examples
T1 <- xtabs(~attendance + shift, data = Bus)
T1
chisq.test(T1)
rm(T1)
rm(T1)
#################################
Bypass <- read_csv("Bypass.csv")
devtools::use_data(Bypass, overwrite = TRUE)
# Examples
EDA(Bypass$charge)
t.test(Bypass$charge, conf.level=.90)$conf
t.test(Bypass$charge, mu = 35000)
#################################
Cabinets <- read_csv("Cabinets.csv")
Cabinets <- Cabinets %>%
  select(home = Home, supplA = SupplA, supplB = SupplB)
devtools::use_data(Cabinets, overwrite = TRUE)
# Examples
DIF <- Cabinets$supplA - Cabinets$supplB
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
with(data = Cabinets, 
     t.test(supplA, supplB, paired = TRUE)
)
with(data = Cabinets,
     wilcox.test(supplA, supplB, paired = TRUE)
)
rm(DIF)
#################################
Cancer <- read_csv("Cancer.csv")
Cancer <- Cancer %>%
  gather(`stomach`, `bronchus`, `colon`, `ovary`, `breast`, key = "type", value = "survival")
devtools::use_data(Cancer, overwrite = TRUE)
# Examples
Cancer
boxplot(survival ~ type, Cancer)
stomach <- Cancer$survival[Cancer$type == "stomach"]
bronchus <- Cancer$survival[Cancer$type == "bronchus"]
boxplot(stomach, ylab = "Days")
SIGN.test(stomach, md = 100, alternative = "greater")
SIGN.test(bronchus, md = 100, alternative = "greater")
rm(bronchus, stomach)
#################################
Carbon <- read_csv("Carbon.csv")
Carbon <- Carbon %>%
  select(CO = monoxide, site = Site)
Carbon$site <- factor(Carbon$site, labels = c("siteA", "siteB", "siteC"))
devtools::use_data(Carbon, overwrite = TRUE)
# Examples
Carbon
boxplot(CO ~ site, data = Carbon)
kruskal.test(CO ~ site, data = Carbon)
#################################
Cat <- read_csv("Cat.csv")
devtools::use_data(Cat, overwrite = TRUE)
# Examples
Cat
stem(Cat$score)
fivenum(Cat$score)
boxplot(Cat$score, main = "Problem 1.116", col = "green")
#################################
Censored <- read_csv("Censored.csv")
Cen1 <- Censored %>%
  gather(`ageA`, `ageB`, key = "treatment", value = "age") %>%
  select(treatment, age)
Cen1 <- na.omit(Cen1)
Cen1$treatment <- factor(str_replace(Cen1$treatment, "age", ""))
# Cen1
Cen2 <- Censored %>%
  select(treatment = group, survival)
Cen2$treatment <- factor(Cen2$treatment, labels = c("A", "B"))
# Cen2
Censored <- data.frame(treatment = Cen1$treatment, age = Cen1$age, survival = Cen2$survival)
Censored <- as_tibble(Censored)
rm(Cen1, Cen2)
devtools::use_data(Censored, overwrite = TRUE)
# Censored
#
# Examples
Censored
boxplot(survival ~ treatment, data = Censored)
wilcox.test(survival ~ treatment, data = Censored, alternative = "greater")
#################################
Challeng <- read_csv("Challeng.csv")
Challeng <- Challeng %>%
  rename(failures = Failures)
Challeng$date <- as.Date(Challeng$date, format = "%m/%d/%y")
devtools::use_data(Challeng, overwrite = TRUE)
# Examples
stem(Challeng$temp)
summary(Challeng$temp)
IQR(Challeng$temp)
quantile(Challeng$temp)
fivenum(Challeng$temp)
stem(sort(Challeng$temp)[-1])
summary(sort(Challeng$temp)[-1])
IQR(sort(Challeng$temp)[-1])
quantile(sort(Challeng$temp)[-1])
fivenum(sort(Challeng$temp)[-1])
par(mfrow=c(1, 2))
qqnorm(Challeng$temp)
qqline(Challeng$temp)
qqnorm(sort(Challeng$temp)[-1])
qqline(sort(Challeng$temp)[-1])
par(mfrow=c(1, 1))
#################################
Chemist <- read_csv("Chemist.csv")
devtools::use_data(Chemist, overwrite = TRUE)
# Examples
hist(Chemist$salary, xlab = "Salary", 
     main = "Chemist Salaries", col = "green")
#################################
Chesapea <- read_csv("Chesapea.csv")
devtools::use_data(Chesapea, overwrite = TRUE)
# Examples
qqnorm(Chesapea$salinity)
qqline(Chesapea$salinity)
shapiro.test(Chesapea$salinity)
t.test(Chesapea$salinity, mu = 7)
#################################
mat <- matrix(data = c(16, 12, 5, 2, 5, 12, 3, 2, 4, 6), nrow = 2)
dimnames(mat) <- list(year = c("1988-90", "1991-93"), 
                      frequency = c("much better than average", "above average", 
                                    "average", "below average", "much worse than average"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Chevy <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Chevy$year <- factor(Chevy$year, 
                     levels = c("1988-90", "1991-93"))
Chevy$frequency <- factor(Chevy$frequency, levels = c("much better than average", "above average", 
                                                      "average", "below average", "much worse than average"))
Chevy <- as_tibble(Chevy)
devtools::use_data(Chevy, overwrite = TRUE)
# Examples
xtabs(~year + frequency, data = Chevy)
#################################
Chicken <- read_csv("Chicken.csv")
Chicken <- Chicken %>%
  gather(`Ration1`, `Ration2`, `Ration3`, key = "feed", value = "gain")
Chicken$feed <- tolower(Chicken$feed)
devtools::use_data(Chicken, overwrite = TRUE)
# Examples
boxplot(gain ~ feed, col = c("red","blue","green"), data = Chicken)
anova(lm(gain ~ feed, data = Chicken))
#################################
Chipavg <- read_csv("Chipavg.csv")
devtools::use_data(Chipavg, overwrite = TRUE)
# Examples
EDA(Chipavg$thickness)
t.test(Chipavg$thickness, mu = 1000)
boxplot(Chipavg$wafer1, Chipavg$wafer2, names = c("Wafer 1", "Wafer 2"))
shapiro.test(Chipavg$wafer1)
shapiro.test(Chipavg$wafer2)
t.test(Chipavg$wafer1, Chipavg$wafer2, var.equal = TRUE)
#################################
Chips <- read_csv("Chips.csv")
devtools::use_data(Chips, overwrite = TRUE)
# Examples
with(data = Chips, 
     boxplot(wafer11, wafer12, wafer13, wafer14, wafer21, wafer22, wafer23, wafer24)
)
#################################
Cigar <- read_csv("Cigar.csv")
Cigar <- Cigar %>%
  select(brandA, brandB, brandC, brandD) %>%
  gather(`brandA`, `brandB`, `brandC`, `brandD`, key = "brand", value = "tar")
devtools::use_data(Cigar, overwrite = TRUE)
# Examples
boxplot(tar ~ brand, data = Cigar)
anova(lm(tar ~ brand, data = Cigar))
#################################
Cigarett <- read_csv("Cigarett.csv")
Cigarett <- Cigarett %>%
  select(cigarettes = cigarett, weight)
devtools::use_data(Cigarett, overwrite = TRUE)
# Examples
plot(weight ~ cigarettes, data = Cigarett)
model <- lm(weight ~ cigarettes, data = Cigarett)
abline(model)
with(data = Cigarett,
     cor(weight, cigarettes)
)
rm(model)
#################################
Citrus <- read_csv("Citrus.csv")
devtools::use_data(Citrus, overwrite = TRUE)
# Examples
model <- lm(percent ~ age, data = Citrus)
summary(model)
anova(model)
rm(model)
#################################
Clean <- read_csv("Clean.csv")
Clean <- Clean %>%
  select(-clean, -agent) %>%
  gather(`A`, `B`, `C`, key = "agent", value = "clean")
devtools::use_data(Clean, overwrite = TRUE)
# Examples
boxplot(clean ~ agent, col = c("red", "blue", "green"), data = Clean)
anova(lm(clean ~ agent, data = Clean))
#################################
Coaxial <- read_csv("Coaxial.csv")
Coaxial <- Coaxial %>%
  select(signal = Signal, cable = Cable)
Coaxial$cable <- factor(Coaxial$cable, 
                        labels = c("typeA", "typeB", "typeC"))
devtools::use_data(Coaxial, overwrite = TRUE)
# Examples
boxplot(signal ~ cable, data = Coaxial, col = c("red", "green", "yellow"))
kruskal.test(signal ~ cable, data = Coaxial)
#################################
Coffee <- read_csv("Coffee.csv")
Coffee <- Coffee %>%
  select(without = Without, with = With, differences = differ)
devtools::use_data(Coffee, overwrite = TRUE)
# Examples
qqnorm(Coffee$differences)
qqline(Coffee$differences)
shapiro.test(Coffee$differences)
t.test(Coffee$with, Coffee$without, paired = TRUE, alternative = "greater")
wilcox.test(Coffee$with, Coffee$without, paired = TRUE, 
            alterantive = "greater")
#################################
Coins <- read_csv("Coins.csv")
Coins <- Coins %>%
  select(return = coins)
devtools::use_data(Coins, overwrite = TRUE)
# Examples
qqnorm(Coins$return)
qqline(Coins$return)
#################################
Commute <- read_csv("Commute.csv")
Commute <- Commute %>%
  gather(`1980`, `1990`, key = "year", value = "time") %>%
  select(city = City, year, time)
devtools::use_data(Commute, overwrite = TRUE)
# Examples
stripplot(year ~ time, data = Commute, jitter = TRUE)
dotplot(year ~ time, data = Commute)
bwplot(year ~ time, data = Commute)
stripchart(time ~ year, data = Commute, method = "stack", pch = 1, cex = 2, 
           col = c("red", "blue"), group.names = c("1980", "1990"), main = "",
           xlab = "minutes")
title(main = "Commute Time")
boxplot(time ~ year, data = Commute, names=c("1980", "1990"), 
        horizontal = TRUE, las = 1)
#################################
Concept <- read_csv("Concept.csv")
devtools::use_data(Concept, overwrite = TRUE)
# Examples
summary(Concept$self)
sd(Concept$self)
diff(range(Concept$self))
IQR(Concept$self)
summary(Concept$self/10)
IQR(Concept$self/10)
sd(Concept$self/10)
diff(range(Concept$self/10))
#################################
Concrete <- read_csv("Concrete.csv")
Concrete <- Concrete %>%
  select(strength = Strength, method = Method)
Concrete$method <- factor(Concrete$method, labels = c("new", "old"))
devtools::use_data(Concrete, overwrite = TRUE)
# Examples
wilcox.test(strength ~ method, data = Concrete, alternative = "greater")
#################################
Corn <- read_csv("Corn.csv")
Corn <- Corn %>%
  select(new = New, standard = Standard, differences = differ)
devtools::use_data(Corn, overwrite = TRUE)
# Examples
boxplot(Corn$differences)
qqnorm(Corn$differences)
qqline(Corn$differences)
shapiro.test(Corn$differences)
t.test(Corn$new, Corn$standard, paired = TRUE, alternative = "greater")
#################################
Correlat <- read_csv("Correlat.csv")
Correlat <- Correlat %>%
  select(x = X, y = Y)
devtools::use_data(Correlat, overwrite = TRUE)
# Examples
plot(y ~ x, data = Correlat)
model <- lm(y ~ x, data = Correlat)
summary(model)
abline(model)
rm(model)
#################################
Counsel <- read_csv("Counsel.csv")
devtools::use_data(Counsel, overwrite = TRUE)
# Examples
EDA(Counsel$score)
t.test(Counsel$score, mu = 70)
#################################
Cpi <- read_csv("Cpi.csv")
Cpi <- Cpi %>%
  select(year = Year, cpi = CPI)
devtools::use_data(Cpi, overwrite = TRUE)
# Examples
plot(cpi ~ year, data = Cpi, type = "l", lty = 2, lwd = 2, col = "red")   
barplot(Cpi$cpi, col = "pink", las = 2, main = "Problem 1.34")   
#################################
Crime <- read_csv("Crime.csv")
Crime <- Crime %>%
  gather(`1983`, `1993`, key = "year", value = "rate") %>%
  select(rate, state = State, year)
devtools::use_data(Crime, overwrite = TRUE)
# Examples
boxplot(rate ~ year, data = Crime)
#################################
Darwin <- read_csv("Darwin.csv")
Darwin <- Darwin %>%
  select(pot, cross, self)
devtools::use_data(Darwin, overwrite = TRUE)
# Examples
differ <- Darwin$cross - Darwin$self
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
wilcox.test(Darwin$cross, Darwin$self, paired = TRUE)
rm(differ)
#################################
# Dealers
mat <- matrix(data = c(19, 3, 12, 8, 11, 4, 2, 16, 9, 13, 10, 15), nrow = 6)
dimnames(mat) <- list(type = c("Honda", "Toyota", "Mazda", "Ford", "Dodge", "Saturn"), 
                      service = c("Replaces unnecessarily", "Follows manufacturer guidelines"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Dealers <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Dealers$type <- factor(Dealers$type, 
                       levels = c("Honda", "Toyota", "Mazda", "Ford", "Dodge", "Saturn"))
Dealers$service <- factor(Dealers$service, levels = c("Replaces unnecessarily", 
                                                      "Follows manufacturer guidelines"))
Dealers <- as_tibble(Dealers)
devtools::use_data(Dealers, overwrite = TRUE)
# Examples
xtabs(~type + service, data = Dealers)
T1 <- xtabs(~type + service, data = Dealers)
T1
addmargins(T1)
pt <- prop.table(T1, margin = 1)
pt
barplot(t(pt),  col = c("red", "skyblue"), legend = colnames(T1))
rm(T1, pt)
#################################
Defectiv <- read_csv("Defectiv.csv")
devtools::use_data(Defectiv, overwrite = TRUE)
# Examples
T1 <- xtabs(~ number, data = Defectiv)
T1
barplot(T1, col = "pink", ylab = "Frequency",
        xlab = "Defective Items Produced by Employees", main = "Problem 1.27")
rm(T1)
#################################
# Degree
mat <- matrix(data = c(78.0, 75.0, 73.4, 43.4, 57.3, 27.8, 8.7, 37.1, 13.6, 0.7, 43.1,
                       84.3, 78.1, 73.4, 71.5, 67.5, 50.7, 46.7, 44.2, 31.2, 13.8, 53.2), 
              nrow = 11)
dimnames(mat) <- list(field = c("Health", "Education", "Foreign Language", "Psychology", 
                                "Fine Arts", "Life Sciences", "Business", "Social Science",
                                "Physical Sciences", "Engineering", "All Fields"), 
                      awarded = c("1970", "1990"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Degree <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Degree$field <- factor(Degree$field, 
                       levels = c("Health", "Education", "Foreign Language", "Psychology", 
                                  "Fine Arts", "Life Sciences", "Business", "Social Science",
                                  "Physical Sciences", "Engineering", "All Fields"))
Degree$awarded <- factor(Degree$awarded, levels = c("1970", "1990"))
Degree <- as_tibble(Degree)
devtools::use_data(Degree, overwrite = TRUE)
# Examples
T1 <- xtabs(~field + awarded, data = Degree)
T1
barplot(t(T1), beside = TRUE, col = c("red", "skyblue"), legend = colnames(T1))
rm(T1)
#################################
Delay <- read_csv("Delay.csv")
Delay$carrier <- factor(Delay$carrier)
devtools::use_data(Delay, overwrite = TRUE)
# Examples
boxplot(delay ~ carrier, data = Delay)
kruskal.test(delay ~ carrier, data = Delay)
#################################
Depend <- read_csv("Depend.csv")
devtools::use_data(Depend, overwrite = TRUE)
# Examples
T1 <- xtabs(~ number, data = Depend)
T1
barplot(T1, col = "lightblue", main = "Problem 1.26",
        xlab = "Number of Dependent Children", ylab = "Frequency")
#################################
Detroit <- read_csv("Detroit.csv")
devtools::use_data(Detroit, overwrite = TRUE)
# Example
EDA(Detroit$educ)
#################################
# Develop
mat <- matrix(data = c(545, 24, 71, 142, 1587, 986, 66, 66, 230, 1939), 
              nrow = 5)
dimnames(mat) <- list(race = c("African American", "American Indian", "Asian",
                               "Latino", "White"), 
                      college = c("Two-year", "Four-year"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Develop <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Develop$race <- factor(Develop$race, 
                       levels = c("African American", "American Indian", "Asian",
                                  "Latino", "White"))
Develop$college <- factor(Develop$college, levels = c("Two-year", "Four-year"))
Develop <- as_tibble(Develop)
devtools::use_data(Develop, overwrite = TRUE)
# Examples
T1 <- xtabs(~race + college, data = Develop)
T1
chisq.test(T1)
rm(T1)
#################################
Devmath <- read_csv("Devmath.csv")
devtools::use_data(Devmath, overwrite = TRUE)
# Examples
EDA(Devmath$score)
t.test(Devmath$score, mu = 80, alternative = "less")
#################################
# Dice
x <- 2L:12L
px <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36
sum(px)
Dice <- data.frame(x = x, px = px)
Dice <- as_tibble(Dice)
rm(x, px)
devtools::use_data(Dice, overwrite = TRUE)
# Examples
roll1 <- sample(1:6, 20000, replace = TRUE)
roll2 <- sample(1:6, 20000, replace = TRUE)
outcome <- roll1 + roll2
T1 <- table(outcome)/length(outcome)
remove(roll1, roll2, outcome)
T1
round(t(Dice), 5)
rm(T1, roll1, roll2)
#################################
Diesel <- read_csv("Diesel.csv")
Diesel$date <- as.Date(Diesel$date, format = "%m/%d/%y")
devtools::use_data(Diesel, overwrite = TRUE)
# Examples
par(las = 2)
boxplot(pricepergallon ~ location, data = Diesel)
boxplot(pricepergallon ~ location, 
        data = droplevels(Diesel[Diesel$location == "EastCoast" | 
                                   Diesel$location == "Gulf" | Diesel$location == "NatAvg" | 
                                   Diesel$location == "Rocky" | Diesel$location == "California", ]), 
        col = "pink", main = "Exercise 2.8")
par(las = 1) 
## Not run: 
ggplot2::ggplot(data = Diesel, aes(x = date, y = pricepergallon, 
                                   color = location)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() + 
  labs(y = "Price per Gallon (in dollars)")
## End(Not run)         
#################################
Diplotmat <- read_csv("Diplomat.csv")
devtools::use_data(Diplomat, overwrite = TRUE)
# Examples
par(las = 2, mfrow = c(2, 2))
stripchart(number ~ country, data = Diplomat, pch = 19, 
           col= "red", vertical = TRUE)
stripchart(rate ~ country, data = Diplomat, pch = 19, 
           col= "blue", vertical = TRUE) 
with(data = Diplomat, 
     barplot(number, names.arg = country, col = "red"))
with(data = Diplomat, 
     barplot(rate, names.arg = country, col = "blue"))           
par(las = 1, mfrow = c(1, 1))
## Not run: 
ggplot2::ggplot(data = Diplomat, aes(x = reorder(country, number), 
                                     y = number)) + 
  geom_bar(stat = "identity", fill = "pink", color = "black") + 
  theme_bw() + labs(x = "", y = "Total Number of Tickets")
ggplot2::ggplot(data = Diplomat, aes(x = reorder(country, rate), 
                                     y = rate)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") + 
  theme_bw() + labs(x = "", y = "Tickets per vehicle per month")

## End(Not run)
#################################
Disposal <- read_csv("Disposal.csv")
devtools::use_data(Disposal, overwrite = TRUE)
# Examples
stem(Disposal$pounds)
fivenum(Disposal$pounds)
EDA(Disposal$pounds)
#################################
Dogs <- read_csv("Dogs.csv")
Dogs$year <- factor(Dogs$year)
devtools::use_data(Dogs, overwrite = TRUE)
# Examples
cor(Dogs$ranking[Dogs$year == "1992"], Dogs$ranking[Dogs$year == "1993"])
cor(Dogs$ranking[Dogs$year == "1997"], Dogs$ranking[Dogs$year == "1998"])
## Not run: 
ggplot2::ggplot(data = Dogs, aes(x = reorder(breed, ranking), y = ranking)) + 
  geom_bar(stat = "identity") + 
  facet_grid(year ~. ) + 
  theme(axis.text.x  = element_text(angle = 85, vjust = 0.5)) 
## End(Not run)
#################################
Domestic <- read_csv("Domestic.csv")
Domestic$age <- factor(Domestic$age)
devtools::use_data(Domestic, overwrite = TRUE)
# Examples
barplot(Domestic$rate, names.arg = Domestic$age)
## Not run: 
ggplot2::ggplot(data = Domestic, aes(x = age, y = rate)) + 
  geom_bar(stat = "identity", fill = "purple", color = "black") + 
  labs(x = "", y = "Domestic violence per 1000 women") + 
  theme_bw()
## End(Not run)
#################################
Dopamine <- read_csv("Dopamine.csv")
devtools::use_data(Dopamine, overwrite = TRUE)
# Examples
boxplot(dbh ~ group, data = Dopamine)
t.test(dbh ~ group, data = Dopamine, var.equal = TRUE)
#################################
Dowjones <- read_csv("Dowjones.csv")
devtools::use_data(Dowjones, overwrite = TRUE)
# Examples
plot(close ~ year, data = Dowjones, type = "l", main = "Exercise 1.35")
## Not run: 
ggplot2::ggplot(data = Dowjones, aes(x = year, y = close)) +
  geom_point(size = 0.5) + 
  geom_line(color = "red") + 
  theme_bw() + 
  labs(y = "Dow Jones Closing Price")
## End(Not run)
#################################
# Drink
mat <- matrix(data = c(95, 73, 12, 83, 71, 46, 21, 18, 8), 
              nrow = 3)
dimnames(mat) <- list(drinking = c("ok", "tolerated", "immoral"), 
                      referendum = c("for", "against", "undecided"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Drink <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Drink$drinking <- factor(Drink$drinking, 
                         levels = c("ok", "tolerated", "immoral"))
Drink$referendum <- factor(Drink$referendum, 
                           levels = c("for", "against", "undecided"))
Drink <- as_tibble(Drink)
devtools::use_data(Drink, overwrite = TRUE)
# Examples
T1 <- xtabs(~drinking + referendum, data = Drink)
T1
chisq.test(T1)
rm(T1)
#################################
Drug <- read_csv("Drug.csv")
devtools::use_data(Drug, overwrite = TRUE)
# Examples
boxplot(trials ~ group, data = Drug)
wilcox.test(trials ~ group, data = Drug)
t.test(rank(trials) ~ group, data = Drug, var.equal = TRUE)
#################################
Dyslexia <- read_csv("Dyslexia.csv")
devtools::use_data(Dyslexia, overwrite = TRUE)
# Examples
plot(height ~ weight, data = Dyslexia)
plot(words ~ factor(handed), data = Dyslexia)
#################################
Earthqk <- read_csv("Earthqk.csv")
devtools::use_data(Earthqk, overwrite = TRUE)
# Examples
EDA(Earthqk$severity)
t.test(Earthqk$severity, mu = 100, alternative = "greater")
#################################
Educat <- read_csv("Educat.csv")
devtools::use_data(Educat, overwrite = TRUE)
# Examples
plot(crime ~ nodegree, data = Educat, 
     xlab = "Percent of population without high school degree",
     ylab = "Violent Crime Rate per 100,000")
#################################
Eggs <- read_csv("Eggs.csv")
devtools::use_data(Eggs, overwrite = TRUE)
# Examples
plot(eggs ~ feed, data = Eggs)
model <- lm(eggs ~ feed, data = Eggs)
abline(model)
summary(model)
rm(model)
#################################
Elderly <- read_csv("Elderly.csv")
devtools::use_data(Elderly, overwrite = TRUE)
# Examples
with(data = Elderly, 
     stripchart(x = list(percent1998, percent1985), method = "stack", pch = 19,
                col = c("red","blue"), group.names = c("1998", "1985"))
)
with(data = Elderly, cor(percent1998, percent1985))
## Not run: 
ggplot2::ggplot(data = Elderly, aes(x = percent1985, y = percent1998)) +
  geom_point() + 
  theme_bw()
## End(Not run)
#################################
Energy <- read_csv("Energy.csv")
devtools::use_data(Energy, overwrite = TRUE)
# Examples
plot(kilowatt ~ size, data = Energy)
with(data = Energy, cor(size, kilowatt))
model <- lm(kilowatt ~ size, data = Energy)
plot(Energy$size, resid(model))
rm(model)
#################################
Engineer <- read_csv("Engineer.csv")
Engineer$university <- factor(Engineer$university)
devtools::use_data(Engineer, overwrite = TRUE)
# Examples
boxplot(salary ~ university, data = Engineer)
kruskal.test(salary ~ university, data = Engineer)
anova(lm(salary ~ university, data = Engineer))
anova(lm(rank(salary) ~ university, data = Engineer))
#################################
Entrance <- read_csv("Entrance.csv")
devtools::use_data(Entrance, overwrite = TRUE)
# Examples
stem(Entrance$score)
stem(Entrance$score, scale = 2)
##################################################################
### More 6/20/17
#################################
Epaminicompact <- read_csv("Epaminicompact.csv")
Epaminicompact <- Epaminicompact %>%
  rename(class = Class, manufacturer = Manufacturer, carline = `carline name`)
devtools::use_data(Epaminicompact, overwrite = TRUE)
# Examples
summary(Epaminicompact$cty)
plot(hwy ~ cty, data = Epaminicompact)
#################################
Epatwoseater <- read_csv("Epatwoseater.csv")
Epatwoseater <- Epatwoseater %>%
  rename(class = Class, manufacturer = Manufacturer, carline = `carline name`)
devtools::use_data(Epatwoseater, overwrite = TRUE)
# Examples
summary(Epatwoseater$cty)
plot(hwy ~ cty, data = Epatwoseater)
boxplot(cty ~ drv, data = Epatwoseater)
#################################
Executiv <- read_csv("Executiv.csv")
Executiv <- Executiv %>%
  rename(age = Age)
devtools::use_data(Executiv, overwrite = TRUE)
# Examples
hist(Executiv$age, xlab = "Age of banking executives", 
     breaks = 5, main = "", col = "gray")
#################################
Exercise <- read_csv("Exercise.csv")
devtools::use_data(Exercise, overwrite = TRUE)
# Examples
stem(Exercise$loss)
#################################
Fabric <- read_csv("Fabric.csv")
Fabric <- Fabric %>%
  gather(With, Without, key = softner, value = softness) %>%
  rename(garment = Type)
Fabric$softner <- tolower(Fabric$softner)
devtools::use_data(Fabric, overwrite = TRUE)
# Examples
wilcox.test(softness ~ softner, data = Fabric, 
            paired = TRUE, alternative = "greater")
## Not run
T7 <- tidyr::spread(Fabric, softner, softness) %>%
  mutate(di = with - without, adi = abs(di), rk = rank(adi), srk = sign(di)*rk)
T7
t.test(T7$srk, alternative = "greater")
##
#################################
Faithful <- read_csv("Faithful.csv")
Faithful <- Faithful %>%
  rename(time = Time, eruption = Eruption)
Faithful$eruption <- factor(Faithful$eruption)
devtools::use_data(Faithful, overwrite = TRUE)
# Examples
t.test(time ~ eruption, data = Faithful)
hist(Faithful$time, xlab = "wait time", main = "", freq = FALSE)
lines(density(Faithful$time))
## Not run
ggplot2::ggplot(data = Faithful, aes(x = time, y = ..density..)) + 
  geom_histogram(binwidth = 5, fill = "pink", col = "black") + 
  geom_density() + 
  theme_bw() +
  labs(x = "wait time")
##
#################################
Family <- read_csv("Family.csv")
Family <- Family %>%
  rename(number = Number, cost = Cost)
devtools::use_data(Family, overwrite = TRUE)
# Examples
plot(cost ~ number, data = Family)
abline(lm(cost ~ number, data = Family))
cor(Family$cost, Family$number)
## Not run
ggplot2::ggplot(data = Family, aes(x = number, y = cost)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()
## End not run
####################################
# Create Ferraro1
mat <- matrix(data = c(245, 140, 115, 205, 160, 135), nrow = 2, byrow = TRUE)
dimnames(mat) <- list(gender = c("Men", "Women"), 
                      candidate = c("Reagan/Bush", "Mondale/Ferraro", "Undecided"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Ferraro1 <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Ferraro1$gender <- factor(Ferraro1$gender, 
                          levels = c("Men", "Women"))
Ferraro1$candidate <- factor(Ferraro1$candidate, 
                             levels = c("Reagan/Bush", "Mondale/Ferraro", "Undecided"))
Ferraro1 <- as_tibble(Ferraro1)
devtools::use_data(Ferraro1, overwrite = TRUE)
Ferraro1
# Examples
T1 <- xtabs(~gender + candidate, data = Ferraro1)
T1
chisq.test(T1)  
rm(T1)
####################################
## Create Ferraro2
mat <- matrix(data = c(245, 155, 100, 185, 235, 80), nrow = 2, byrow = TRUE)
dimnames(mat) <- list(gender = c("Men", "Women"), 
                      candidate = c("Bush", "Ferraro", "Undecided"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Ferraro2 <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Ferraro2$gender <- factor(Ferraro2$gender, 
                          levels = c("Men", "Women"))
Ferraro2$candidate <- factor(Ferraro2$candidate, 
                             levels = c("Bush", "Ferraro", "Undecided"))
Ferraro2 <- as_tibble(Ferraro2)
devtools::use_data(Ferraro2, overwrite = TRUE)
Ferraro2
# Examples
T1 <- xtabs(~gender + candidate, data = Ferraro2)
T1
chisq.test(T1)  
rm(T1)
################################
# Fertility
Fertility <- read_csv("Fertility.csv")
Fertility <- Fertility %>%
  rename(state= State)
devtools::use_data(Fertility, overwrite = TRUE)
# Examples
stem(Fertility$rate)
fivenum(Fertility$rate)
EDA(Fertility$rate)
################################
Firstchi <- read_csv("Firstchi.csv")
devtools::use_data(Firstchi, overwrite = TRUE)
# Examples
EDA(Firstchi$age)
################################
Fish <- read_csv("Fish.csv")
Fish <- Fish %>%
  select(smallmesh, largemesh) %>%
  gather(smallmesh, largemesh, key = "codend", value = "length")
devtools::use_data(Fish, overwrite = TRUE)
## Examples
tapply(Fish$length, Fish$codend, median, na.rm = TRUE)
SIGN.test(Fish$length[Fish$codend == "smallmesh"], conf.level = 0.99)
# NOt run
dplyr::group_by(Fish, codend) %>%
  summarize(MEDIAN = median(length, na.rm = TRUE))
################################
Fitness <- read_csv("Fitness.csv")
Fitness <- Fitness %>%
  mutate(subject = row.names(Fitness)) %>%
  gather(`Before`, `After`, key = "test", value = "number")
devtools::use_data(Fitness, overwrite = TRUE)
## Examples
t.test(number ~ test, data = Fitness, alternative = "greater", paired = TRUE)
# Not run
Wide <- tidyr::spread(Fitness, test, number) %>%
  mutate(diff = After - Before)
Wide
qqnorm(Wide$diff)
qqline(Wide$diff)
t.test(Wide$diff, alternative = "greater")
############################
Florida2000 <- read_csv("Florida2000.csv")
names(Florida2000) <- tolower(names(Florida2000))
Florida2000
devtools::use_data(Florida2000, overwrite = TRUE)
## Examples
plot(buchanan ~ total, data = Florida2000, 
     xlab = "Total votes cast (in thousands)", 
     ylab = "Votes for Buchanan")
##########################
Fluid <- read_csv("Fluid.csv")
Fluid <- Fluid %>%
  select(-response, -group, -`ln(resp)`) %>%
  gather(`26kV`, `28kV`, `30kV`, `32kV`, `34kV`, `36kV`, `38kV`, key = "kilovolts", value = "time")
Fluid <- na.omit(Fluid)
Fluid
devtools::use_data(Fluid, overwrite = TRUE)
## Examples
DF1 <- Fluid[Fluid$kilovolts == "34kV", ]
DF1
# Or
DF2 <- subset(Fluid, subset = kilovolts == "34kV")
DF2
stem(DF2$time)
SIGN.test(DF2$time)
# Not run
DF3 <- dplyr::filter(Fluid, kilovolts == "34kV")
DF3
#
###########################
Food <- read_csv("Food.csv")
Food <- Food %>%
  rename(expenditure = food)
Food
devtools::use_data(Food, overwrite = TRUE)
# Examples
EDA(Food$expenditure)
###########################
Framingh <- read_csv("Framingh.csv")
Framingh
devtools::use_data(Framingh, overwrite = TRUE)
# Examples
stem(Framingh$cholest)
boxplot(Framingh$cholest, horizontal = TRUE)
hist(Framingh$cholest, freq = FALSE)
lines(density(Framingh$cholest))
mean(Framingh$cholest > 200 & Framingh$cholest < 240)

# Not run
ggplot2::ggplot(data = Framingh, aes(x = factor(1), y = cholest)) + 
  geom_boxplot() +                 # boxplot
  labs(x = "") +                   # no x label  
  theme_bw() +                     # black and white theme  
  geom_jitter(width = 0.2) +       # jitter points
  coord_flip()                     # Create horizontal plot
ggplot2::ggplot(data = Framingh, aes(x = cholest, y = ..density..)) +
  geom_histogram(fill = "pink", binwidth = 15, color = "black") + 
  geom_density() + 
  theme_bw()
#
###########################
Freshman <- read_csv("Freshman.csv")
devtools::use_data(Freshman, overwrite = TRUE)
## Examples
BSDA::SIGN.test(Freshman$age, md = 19)
################################
## Create Funeral
mat <- matrix(data = c(15, 60, 25, 20, 38, 42, 34, 44, 22, 12, 40, 48), nrow = 4, byrow = TRUE)
dimnames(mat) <- list(region = c("West", "Central", "South", "East"), 
                      cost = c("less than expected", "about what expected", "more than expected"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Funeral <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Funeral2$region <- factor(Funeral$region, 
                          levels = c("West", "Central", "South", "East"))
Funeral$cost <- factor(Funeral$cost, 
                       levels = c("less than expected", "about what expected", "more than expected"))
Funeral <- as_tibble(Funeral)
Funeral
devtools::use_data(Funeral, overwrite = TRUE)
# Examples
T1 <- xtabs(~region + cost, data = Funeral)
T1
chisq.test(T1)  
rm(T1)
####################################
Galaxie <- read_csv("Galaxie.csv")
devtools::use_data(Galaxie, overwrite = TRUE)
## Examples
EDA(Galaxie$velocity)
#####################################
## Creating Gallup
mat <- matrix(data = c(43, 52, 5, 42, 53, 5, 44, 51, 5, 
                       30, 67, 3, 45, 50, 5, 58, 33, 9, 
                       27, 67, 6, 26, 70, 4, 45, 52, 3, 
                       54, 39, 7, 49, 47, 4, 39, 55, 6), 
              nrow = 12, byrow = TRUE)
dimnames(mat) <- list(demographics = c("National","Gender: Male","Gender: Female",
                                       "Education: College","Education: High School",
                                       "Education: Grade School",
                                       "Age: 18-24", "Age: 25-29", "Age: 30-49", 
                                       "Age: 50-older", "Religion: Protestant",
                                       "Religion: Catholic"), 
                      opinion = c("Criminal", "Not Criminal", "No Opinion"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Gallup <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Gallup$demographics <- factor(Gallup$demographics, 
                              levels = c("National","Gender: Male","Gender: Female",
                                         "Education: College","Education: High School",
                                         "Education: Grade School",
                                         "Age: 18-24", "Age: 25-29", "Age: 30-49", 
                                         "Age: 50-older", "Religion: Protestant",
                                         "Religion: Catholic"))
Gallup$opinion <- factor(Gallup$opinion, 
                         levels = c("Criminal", "Not Criminal", "No Opinion"))
Gallup <- as_tibble(Gallup)
Gallup
devtools::use_data(Gallup, overwrite = TRUE)
## Examples
T1 <- xtabs(~demographics + opinion, data = Gallup)
T1
t(T1[c(2, 3), ])
barplot(t(T1[c(2, 3), ]))
barplot(t(T1[c(2, 3), ]), beside = TRUE)
# not run
dplyr::filter(Gallup, demographics == "Gender: Male" | demographics == "Gender: Female") %>%
  ggplot2::ggplot(aes(x = demographics, fill = opinion)) + 
  geom_bar() + 
  theme_bw() + 
  labs(y = "Fraction")
##
