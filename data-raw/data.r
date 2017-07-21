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
#######################################################
Gasoline <- read_csv("Gasoline.csv")
Gasoline
devtools::use_data(Gasoline, overwrite = TRUE)
# Examples
stem(Gasoline$price)
######################################################
German <- read_csv("German.csv")
German <- German %>%
  mutate(student = row.names(German)) %>%
  select(-differ, -sgnrnks) %>%
  gather(Before, After, key = "when", value = "errors")
German
devtools::use_data(German, overwrite = TRUE)
## Examples
t.test(errors ~ when, data = German, paired = TRUE)
wilcox.test(errors ~ when, data = German)
## Not run
T8 <- tidyr::spread(German, when, errors) %>%
  mutate(di = After - Before, adi = abs(di), rk = rank(adi), srk = sign(di)*rk)
T8
qqnorm(T8$di)
qqline(T8$di)
t.test(T8$srk)
##
######################################################
Golf <- read_csv("Golf.csv")
Golf
devtools::use_data(Golf, overwrite = TRUE)
## Examples
stem(Golf$yards)
qqnorm(Golf$yards)
qqline(Golf$yards)
## Not run
ggplot2::ggplot(data = Golf, aes(sample = yards)) +
  geom_qq() + 
  theme_bw()
##
#######################################################
Governor <- read_csv("Governor.csv")
Governor <- Governor %>%
  gather(`1994salary`, `1999salary`, key = "year", value = "salary") %>%
  rename(state = State)
Governor$year <- str_replace(string = Governor$year, pattern= "salary", replacement = "")
Governor$year <- factor(Governor$year)
Governor
devtools::use_data(Governor, overwrite = TRUE)
## Examples
boxplot(salary ~ year, data = Governor)
## Not run
ggplot2::ggplot(data = Governor, aes(x = salary)) + 
  geom_density(fill = "pink") + 
  facet_grid(year ~ .) + 
  theme_bw()
##
#######################################################
Gpa <- read_csv("Gpa.csv")
Gpa <- Gpa %>%
  rename(hsgpa = HSGPA, collgpa = CollGPA)
Gpa
devtools::use_data(Gpa, overwrite = TRUE)
## Examples
plot(collgpa ~ hsgpa, data = Gpa)
mod <- lm(collgpa ~ hsgpa, data = Gpa)
abline(mod)               # add line
yhat <- predict(mod)      # fitted values
e <- resid(mod)           # residuals
cbind(Gpa, yhat, e)       # Table 2.1
cor(Gpa$hsgpa, Gpa$collgpa)
## Not run
ggplot2::ggplot(data = Gpa, aes(x = hsgpa, y = collgpa)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()
##
# Gpa <- Gpa %>%
#   mutate(student = row.names(Gpa)) %>%
#   gather(`HSGPA`, `CollGPA`, key = "school", value = "gpa")
# Gpa
# Wide <- Gpa %>%
#   tidyr::spread(school, gpa)
# Wide
######################################################
Grades <- read_csv("Grades.csv")
devtools::use_data(Grades, overwrite = TRUE)
## Examples
hist(Grades$grades, main = "", xlab = "Test grades", right = FALSE)
## Not run
ggplot2::ggplot(data = Grades, aes(x = grades, y = ..density..)) + 
  geom_histogram(fill = "pink", binwidth = 5, color = "black") +
  geom_density(lwd = 2, color = "red") +
  theme_bw() 
##  
######################################################
Graduate <- read_csv("Graduate.csv")
Graduate <- Graduate %>%
  rename(school = School, code = Code, percent = Percent)
Graduate
devtools::use_data(Graduate, overwrite = TRUE)
## Examples
barplot(Graduate$percent, names.arg = Graduate$school, 
        las = 2, cex.names = 0.7, col = "tomato")
######################################################
Greenriv <- read_csv("Greenriv.csv")
Greenriv
devtools::use_data(Greenriv, overwrite = TRUE)
## Examples
stem(Greenriv$thick)
SIGN.test(Greenriv$thick, md = 7.3, alternative = "greater")
######################################################
Grnriv2 <- read_csv("Grnriv2.csv")
Grnriv2
devtools::use_data(Grnriv2, overwrite = TRUE)
## Examples
stem(Grnriv2$thick)
t.test(Grnriv2$thick, mu = 8, alternative = "less")
######################################################
Groupabc <- read_csv("Groupabc.csv")
Groupabc <- Groupabc %>%
  gather(`GroupA`, `GroupB`, `GroupC`, key = "group", value = "response")
Groupabc$group <- factor(str_replace(Groupabc$group, pattern = "Group", replace = ""))
Groupabc
devtools::use_data(Groupabc, overwrite = TRUE)
## Examples
boxplot(response ~ group, data = Groupabc, 
        col = c("red", "blue", "green"))
anova(lm(response ~ group, data = Groupabc))
######################################################
Groups <- read_csv("Groups.csv")
Groups <- Groups %>%
  gather(`GroupA`, `GroupB`, `GroupC`, key = "group", value = "response")
Groups$group <- factor(str_replace(Groups$group, pattern = "Group", replace = ""))
Groups
devtools::use_data(Groups, overwrite = TRUE)
## Examples
boxplot(response ~ group, data = Groups, 
        col = c("red", "blue", "green"))
anova(lm(response ~ group, data = Groups))
##
######################################################
Gym <- read_csv("Gym.csv")
Gym <- Gym %>%
  select(age, number)
Gym
devtools::use_data(Gym, overwrite = TRUE)
## Examples
plot(number ~ age, data = Gym)
model <- lm(number ~ age, data = Gym)
abline(model)
summary(model)
######################################################
Habits <- read_csv("Habits.csv")
Habits
devtools::use_data(Habits, overwrite = TRUE)
## Examples
shapiro.test(Habits$differ)
qqnorm(Habits$differ)
qqline(Habits$differ)
wilcox.test(Habits$B, Habits$A, paired = TRUE, alternative = "less")
t.test(Habits$signrks, alternative = "less")
## Not run
ggplot2::ggplot(data = Habits, aes(x = differ)) + 
  geom_dotplot(fill = "blue") + theme_bw()
##
######################################################
Haptoglo <- read_csv("Haptoglo.csv")
Haptoglo
devtools::use_data(Haptoglo, overwrite = TRUE)
## Example
shapiro.test(Haptoglo$concent)
t.test(Haptoglo$concent, mu = 2, alternative = "less")
##
######################################################
Hardware <- read_csv("Hardware.csv")
Hardware
devtools::use_data(Hardware, overwrite = TRUE)
## Examples
stem(Hardare$receipt)
######################################################
Hardwood <- read_csv("Hardwood.csv")
Hardwood
devtools::use_data(Hardwood, overwrite = TRUE)
## Example
plot(tensile ~ hardwood, data = Hardwood)
model <- lm(tensile ~ hardwood, data = Hardwood)
abline(model)
plot(model, which = 1)
####################################################
## Creating Heat
mat <- matrix(data = c(16, 48, 51, 22, 9, 6, 19, 25, 26,
                       6, 9, 12, 34, 8, 4, 4, 1, 1), byrow = TRUE, 
              nrow = 6)
dimnames(mat) <- list(fuel = c("Utility gas", "LP bottled gas", "Electricity", "Fuel oil", "Wood", "Other"), 
                      location = c("American Indians on reservation", "All U.S. households", "American Indians not on reservations")
)
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Heat <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Heat$fuel <- factor(Heat$fuel, levels =  c("Utility gas", "LP bottled gas", "Electricity", "Fuel oil", "Wood", "Other"))
Heat$location <- factor(Heat$location, levels = c("American Indians on reservation", "All U.S. households", "American Indians not on reservations"))
Heat <- as_tibble(Heat)
Heat
devtools::use_data(Heat, overwrite = TRUE)
## Examples

T1 <- xtabs(~ fuel + location, data = Heat)
T1
barplot(t(T1), beside = TRUE, legend = TRUE)
## Not run
ggplot2::ggplot(data = Heat, aes(x = fuel, fill = location)) + 
  geom_bar(position = "dodge") + 
  labs(y = "percent") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
##
######################################################
## 
Heating <- read_csv("Heating.csv")
Heating <- Heating %>%
  select(TypeA, TypeB, TypeC) %>%
  gather(`TypeA`, `TypeB`, `TypeC`, key = "type", value = "efficiency")
Heating$type <- factor(str_replace(Heating$type, "Type", ""))
Heating
devtools::use_data(Heating, overwrite = TRUE)
## Examples
boxplot(efficiency ~ type, data = Heating, col = c("red", "blue", "green"))
kruskal.test(efficiency ~ type, data = Heating)
##
#######################################################
## Creating Hodgkin
mat <- matrix(data = c(74, 18, 12, 
                       68, 16, 12, 
                       154, 54, 58, 
                       18, 10, 44), nrow = 4, byrow = TRUE)
dimnames(mat) <- list(type = c("LP", "NS", "MC", "LD"), 
                      response = c("Positive", "Partial", "None"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Hodgkin <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Hodgkin$type <- factor(Hodgkin$type, 
                       levels = c("LP", "NS", "MC", "LD"))
Hodgkin$response <- factor(v, levels = c("Positive", "Partial", "None"))
Hodgkin <- as_tibble(Hodgkin)
Hodgkin
devtools::use_data(Hodgkin, overwrite = TRUE)
## Examples
T1 <- xtabs(~type + response, data = Hodgkin)
T1
barplot(t(T1), legend = TRUE, beside = TRUE)
## nor run
ggplot2::ggplot(data = Hodgkin, aes(x = type, fill = response)) + 
  geom_bar(position = "dodge") + 
  theme_bw()
##
#####################################################
Homes <- read_csv("Homes.csv")
Homes <- Homes %>%
  select(city = City, `1994`, region = Region, `2000`) %>%
  gather(`1994`, `2000`, key = "year", value = "price")
Homes$year <- factor(Homes$year)
Homes
devtools::use_data(Homes, overwrite = TRUE)
# Examples Chap 5 Stat insight
tapply(Homes$price, Homes$year, mean)
tapply(Homes$price, Homes$region, mean)
##
p2000 <- subset(Homes, year == "2000")
p1994 <- subset(Homes, year == "1994")
## Not run
ggplot2::ggplot(data = Homes, aes(x = region, y = price)) + 
  geom_boxplot() +
  theme_bw() + 
  facet_grid(year ~ .)
##
##############################################################
Homework <- read_csv("Homework.csv")
Homework <- Homework %>%
  rename(private = Private, public = Public) %>%
  gather(`private`, `public`, key = "school", value = "time")
Homework
devtools::use_data(Homework, overwrite = TRUE)
## Examples
boxplot(time ~ school, data = Homework, 
        ylab = "Hours per week spent on homework")
t.test(time ~ school, data = Homework)
##
##############################################################
Honda <- read_csv("Honda.csv")
Honda
devtools::use_data(Honda, overwrite = TRUE)
## Examples
t.test(Honda$mileage, mu = 40, alternative = "less")
##############################################################
Hostile <- read_csv("Hostile.csv")
Hostile <- Hostile %>%
  select(Rural, Suburban, Urban) %>%
  gather(`Rural`, `Suburban`, `Urban`, key = "location", value = "hostility")
Hostile$location <- factor(Hostile$location)
Hostile
devtools::use_data(Hostile, overwrite = TRUE)
## Examples
boxplot(hostility ~ location, data = Hostile, 
        col = c("red", "blue", "green"))
kruskal.test(hostility ~ location, data = Hostile)
##############################################################
Housing <- read_csv("Housing.csv")
Housing <- Housing %>%
  rename(city = City) %>%
  gather(`1984`, `1993`, key = "year", value = "price")
Housing$year <- factor(Housing$year)
Housing
devtools::use_data(Housing, overwrite = TRUE)
## Examples
stripchart(price ~ year, data = Housing, method = "stack", 
           pch = 1, col = c("red", "blue"))
## Not run
ggplot2::ggplot(data = Housing, aes(x = price, fill = year)) + 
  geom_dotplot() + 
  facet_grid(year ~ .) + 
  theme_bw()
##
##############################################################
Hurrican <- read_csv("Hurrican.csv")
Hurrican <- Hurrican %>%
  rename(elnino = ElNino) %>%
  select(-code)
Hurrican$elnino <- factor(Hurrican$elnino)
Hurrican
devtools::use_data(Hurrican, overwrite = TRUE)
## Examples
T1 <- xtabs(~hurrican, data = Hurrican)
T1
barplot(T1, col = "blue", main = "Problem 1.38",
        xlab = "Number of hurricanes", 
        ylab = "Number of seasons")
boxplot(storms ~ elnino, data = Hurrican, 
        col = c("blue", "yellow", "red"))
anova(lm(storms ~ elnino, data = Hurrican))
rm(T1)
##############################################################
Iceberg <- read_csv("Iceberg.csv")
Iceberg <- Iceberg %>%
  rename(month = Month, Newfoundland = Newfound, `Grand Banks` = GrandBk)
Iceberg
devtools::use_data(Iceberg, overwrite = TRUE)
## Examples
plot(Newfoundland ~ `Grand Banks`, data = Iceberg)
abline(lm(Newfoundland ~ `Grand Banks`, data = Iceberg))
##
##############################################################
Income <- read_csv("Income.csv")
Income <- Income %>%
  select(state = State, percent_change = income)
Income
devtools::use_data(Income, overwrite = TRUE)
## Examples
Income$class <- cut(Income$percent_change, breaks = c(-Inf, 0.5, 1.0, 1.5, 2.0, Inf))
T1 <- xtabs(~class, data = Income)
T1
barplot(T1, col = "pink")
## Not run
DF <- as.data.frame(T1)
DF
ggplot2::ggplot(data = DF,  aes(x = class, y = Freq)) + 
  geom_bar(stat = "identity", fill = "purple") + 
  theme_bw()
##
##############################################################
Independent <- read_csv("Independent.csv")
Independent <- Independent %>%
  select(score, group)
Independent$group <- factor(Independent$group, labels = c("A", "B"))
Independent
devtools::use_data(Independent, overwrite = TRUE)
## Examples
qqnorm(Independent$score[Independent$group=="A"])
qqline(Independent$score[Independent$group=="A"])
qqnorm(Independent$score[Independent$group=="B"])
qqline(Independent$score[Independent$group=="B"])
boxplot(score ~ group, data = Independent)
wilcox.test(score ~ group, data = Independent)
##
##############################################################
Indian <- read_csv("Indian.csv")
Indian <- Indian %>%
  rename(reservation = Reserv, `percent high school` = highsch, `per capita income` = income, `poverty rate` = poverty)
Indian
devtools::use_data(Indian, overwrite = TRUE)
## Examples
par(mfrow = c(1, 2))
plot(`per capita income` ~ `percent high school`, data = Indian, 
     xlab = "Percent high school graudates", ylab = "Per capita income")
plot(`poverty rate` ~ `percent high school`, data = Indian,
     xlab = "Percent high school graudates", ylab = "Percent poverty")
par(mfrow = c(1, 1))
##
##############################################################
Indiapol <- read_csv("Indiapol.csv")
Indiapol <- Indiapol %>%
  select(year, speed)
Indiapol
devtools::use_data(Indiapol, overwrite = TRUE)
## Examples
plot(speed ~ year, data = Indiapol, type = "b")
##############################################################
Indy500 <- read_csv("Indy500.csv")
Indy500
devtools::use_data(Indy500, overwrite = TRUE)
## Examples Ex 7.16 7.36
stripchart(qualif ~ group, data = Indy500, method = "stack",
           pch = 19, col = c("red", "blue"))
boxplot(qualif ~ group, data = Indy500)
t.test(qualif ~ group, data = Indy500)
## Not run
ggplot2::ggplot(data = Indy500, aes(sample = qualif)) + 
  geom_qq() + 
  facet_grid(group ~ .) + 
  theme_bw()
##
##############################################################
##############################################################
Inflatio <- read_csv("Inflatio.csv")
Inflatio <- Inflatio %>%
  select(year, pay, increase, inflation)
Inflatio
devtools::use_data(Inflatio, overwrite = TRUE)
## Examples Ex 7.16 7.36
## Examples
plot(increase ~ inflation, data = Inflatio)
cor(Inflatio$increase, Inflatio$inflation, use = "complete.obs")

##
##############################################################
Inletoil <- read_csv("Inletoil.csv")
Inletoil
devtools::use_data(Inletoil, overwrite = TRUE)
## Examples
hist(Inletoil$temp, breaks = 3)
qqnorm(Inletoil$temp)
qqline(Inletoil$temp)
t.test(Inletoil$temp)
t.test(Inletoil$temp, mu = 98, alternative = "less")
##
##############################################################
## Creating Inmate
mat <- matrix(data = c(407, 106, 4525, 2825,
                       1156, 2513, 4439, 442,
                       1314, 348, 7297, 2675), 
              byrow = TRUE, nrow = 3)
dimnames(mat) <- list(race = c("white", "black", "hispanic"), 
                      drug = c("heroin", "crack", "cocaine", "marijuana"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Inmate <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Inmate$race <- factor(Inmate$race, 
                      levels = c("white", "black", "hispanic"))
Inmate$drug <- factor(Inmate$drug, levels = c("heroin", "crack", "cocaine", "marijuana"))
Inmate <- as_tibble(Inmate)
Inmate
devtools::use_data(Inmate, overwrite = TRUE)
## Examples 
T1 <- xtabs(~race + drug, data = Inmate)
T1
chisq.test(T1)
rm(T1)
##
####################################################
## Inspect <- read_csv("Inspect.csv")
## Inspect
##
## Create Inspect
mat <- matrix(data = c(1, 8, 4, 
                       16, 13, 7, 
                       16, 11, 6, 
                       19, 21, 6, 
                       4, 4, 4, 
                       1, 5, 28), byrow = TRUE, nrow = 6)
dimnames(mat) <- list(station = c("auto inspection", "auto repair", "tire store", "gas station", 
                                  "car care center", "new car dealer"), 
                      passed = c("less than 70%", "between 70% and 84%", "more than 85%"))
mat
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Inspect <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Inspect$station <- factor(Inspect$station, 
                          levels = c("auto inspection", "auto repair", "tire store", "gas station", 
                                     "car care center", "new car dealer"))
Inspect$passed <- factor(Inspect$passed, levels = c("less than 70%", "between 70% and 84%", "more than 85%"))
Inspect <- as_tibble(Inspect)
Inspect
devtools::use_data(Inspect, overwrite = TRUE)
## Examples
T1 <- xtabs(~ station + passed, data = Inspect)
T1
barplot(T1, beside = TRUE, legend = TRUE)
chisq.test(T1)
rm(T1)
## Not run
ggplot2::ggplot(data = Inspect, aes(x = passed, fill = station)) + 
  geom_bar(position = "dodge") + 
  theme_bw()
##
####################################################
Insulate <- read_csv("Insulate.csv")
Insulate
devtools::use_data(Insulate, overwrite = TRUE)
# Examples Ex 9.50
plot(loss ~ temp, data = Insulate)
model <- lm(loss ~ temp, data = Insulate)
abline(model, col = "blue")
summary(model)
## Not run
ggplot2::ggplot(data = Insulate, aes(x = temp, y = loss)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()
##
#
####################################################
Iqgpa <- read_csv("Iqgpa.csv")
Iqgpa <- Iqgpa %>%
  select(iq = IQ, gpa = GPA)
Iqgpa
devtools::use_data(Iqgpa, overwrite = TRUE)
## Examples
plot(gpa ~ iq, data = Iqgpa, col = "blue", pch = 19)
model <- lm(gpa ~ iq, data = Iqgpa)
summary(model)
rm(model)
##
##
####################################################
# Irises <- read_csv("Irises.csv")
Irises <- iris
Irises <- Irises %>%
  rename(sepal_length = Sepal.Length, sepal_width = Sepal.Width, 
         petal_length = Petal.Length, petal_width = Petal.Width,
         species = Species)
Irises <- as_tibble(Irises)
Irises
devtools::use_data(Irises, overwrite = TRUE)
## Examples
tapply(Irises$sepal_length, Irises$species, mean)
t.test(Irises$sepal_length[Irises$species == "setosa"], conf.level = 0.99)
hist(Irises$sepal_length[Irises$species == "setosa"], 
     main = "Sepal length for\n Iris Setosa",
     xlab = "Length (in cm)")
boxplot(sepal_length ~ species, data = Irises)
####################################################
Jdpower <- read_csv("Jdpower.csv")
Jdpower <- Jdpower %>%
  rename(car = Car)
Jdpower
devtools::use_data(Jdpower, overwrite = TRUE)
#Examples Ex 2.14, 2.17, 2.31, 2.33, and 2.40
model <- lm(`1995` ~ `1994`, data = Jdpower)
summary(model)
plot(`1995` ~ `1994`, data = Jdpower)
abline(model, col = "red")
rm(model)
###################################################################
Jobsat <- read_csv("Jobsat.csv")
Jobsat <- Jobsat %>%
  rename(wspt = WSPT, satisfaction = satisfac)
Jobsat
devtools::use_data(Jobsat, overwrite = TRUE)
#Examples Ex 9.60 (Wilson Stress Profile for Teachers)
plot(satisfaction ~ wspt, data = Jobsat)
model <- lm(satisfaction ~ wspt, data = Jobsat)
abline(model)
summary(model)
rm(model)
###########################
Kidsmoke <- read_csv("Kidsmoke.csv")
Kidsmoke$gender <- ifelse(Kidsmoke$gender == 0, "female", "male")
Kidsmoke$smoke <- ifelse(Kidsmoke$smoke == 0, "no", "yes")
Kidsmoke
#Examples Ex 4.85
T1 <- xtabs(~smoke + gender, data = Kidsmoke)
T1
prop.table(T1)
prop.table(T1, 1)
prop.table(T1, 2)
###########################
Kilowatt <- read_csv("Kilowatt.csv")
Kilowatt <- Kilowatt %>%
  rename(state = State)
Kilowatt
#Examples Exp 5.9
BSDA::EDA(Kilowatt$rate)
###########################
Kinder <- read_csv("Kinder.csv")
Kinder <- Kinder %>%
  rename(pair = Pair, kinder = Kinder, nokinder = NoKinder)
Kinder
#Examples Ex 7.68
boxplot(Kinder$kinder, Kinder$nokinder)
diff <- Kinder$kinder - Kinder$nokinder
qqnorm(diff)
qqline(diff)
shapiro.test(diff)
t.test(Kinder$kinder, Kinder$nokinder, paired = TRUE)
# Or
t.test(diff)
rm(diff)
##########################
Laminect <- read_csv("Laminect.csv")
Laminect <- Laminect %>%
  select(Rural, Regional, Metropol)%>%
  gather(Rural, Regional, Metropol, key = "area", value = "cost")
Laminect
#Examples Ex 10.18
boxplot(cost ~ area, data = Laminect, col = topo.colors(3))
anova(lm(cost ~ area, data = Laminect))
##########################
Lead <- read_csv("Lead.csv")
Lead <- Lead %>%
  select(exposed, control) %>%
  gather(exposed, control, key = "group", value = "lead")
Lead
#Examples Exp 1.17 (micrograms/dl?)
boxplot(lead ~ group, data = Lead)
#########################
Leader <- read_csv("Leader.csv")
Leader <- Leader %>%
  gather(under35, over35, key = "age", value = "score")
Leader <- na.omit(Leader)
Leader
#Examples Ex 7.31
boxplot(score ~ age, data = Leader, col = c("gray", "green"))
t.test(score ~ age, data = Leader)
########################
Lethal <- read_csv("Lethal.csv")
Lethal
#Examples Exp 6.12 (seconds)
BSDA::SIGN.test(Lethal$survival, md = 45, alternative = "less")
#######################
Life <- read_csv("Life.csv")
Life <- Life %>%
  rename(men = Men, women = Women)
Life
#Examples Ex 1.31
plot(men ~ year, type = "l", ylim = c(min(men, women), max(men, women)), col = "blue",
     main = "Life Expectancy vs Year", ylab = "Age", xlab = "Year", data = Life)
lines(women ~ year, col = "red", data = Life)
text(1955, 65, "Men", col = "blue")
text(1955, 70, "Women", col = "red")
#######################
Lifespan <- read_csv("Lifespan.csv")
Lifespan <- Lifespan %>%
  select(heat, life)
Lifespan
#Examples Ex 2.4, 2.37, and 2.49 (heat = C, life = hours)
plot(life ~ heat, data = Lifespan)
model <- lm(life ~ heat, data = Lifespan)
abline(model)
resid(model)
sum((resid(model))^2)
anova(model)
rm(model)
######################
Ligntmonth <- read_csv("Ligntmonth.csv")
Ligntmonth <- Ligntmonth %>%
  rename(month = Month)
Ligntmonth$month <- as.Date(Ligntmonth$month, format = "%m/%d/%y")
Ligntmonth
#Examples Ex 2.6
plot(deaths ~ damage, data = Ligntmonth)
model = lm(deaths ~ damage, data = Ligntmonth)
abline(model)
rm(model)
######################
Lodge <- read_csv("Lodge.csv")
Lodge <- Lodge %>%
  select(traffic = Traffic, site = Site, ranks = Ranks)
Lodge
#Examples Ex 10.33
boxplot(traffic ~ site, data = Lodge, col = cm.colors(3))
anova(lm(traffic ~ factor(site), data = Lodge))
######################
Longtail <- read_csv("Longtail.csv")
Longtail <- Longtail %>%
  select(score, group = Group, ranks = Ranks)
Longtail
#Examples Ex 10.45
boxplot(score ~ group, data = Longtail, col = heat.colors(3))
kruskal.test(score ~ factor(group), data = Longtail)
anova(lm(score ~ factor(group), data = Longtail))
######################
Lowabil <- read_csv("Lowabil.csv")
Lowabil <- Lowabil %>%
  rename(pair = Pair, experiment = Experimt, control = Control)
Lowabil
#Examples Exp 7.18
diff = Lowabil$experiment - Lowabil$control
qqnorm(diff)
qqline(diff)
shapiro.test(diff)
t.test(Lowabil$experiment, Lowabil$control, paired = TRUE)
# OR
t.test(diff)
rm(diff)
######################
Magnesiu <- read_csv("Magnesiu.csv")
Magnesiu <- Magnesiu %>%
  select(distance, magnesium = magnesiu)
Magnesiu
#Examples Ex 9.9
plot(magnesium ~ distance, data = Magnesiu)
model = lm(magnesium ~ distance, data = Magnesiu)
abline(model)
summary(model)
rm(model)
######################
Malpract <- read_csv("Malpract.csv")
Malpract
#Examples Ex 5.73 (in $1000)
BSDA::SIGN.test(Malpract$award, conf.level = 0.90)
######################
Manager <- read_csv("Manager.csv")
Manager
#Examples Ex 5.81
stem(Manager$salary)
BSDA::SIGN.test(Manager$salary)
######################
Marked <- read_csv("Marked.csv")
Marked
#Examples Ex 6.100
BSDA::EDA(Marked$percent)
BSDA::SIGN.test(Marked$percent, md = 60, alternative = "greater")
t.test(Marked$percent, mu = 60, alternative = "greater")
######################
Math <- read_csv("Math.csv")
Math
#Examples Ex 1.69
stem(Math$math)
hist(Math$math, main = "Math Scores", xlab = "score", freq = FALSE)
lines(density(Math$math), col = "red")
CharlieZ <- (62 - mean(Math$math))/sd(Math$math)
CharlieZ
scale(Math$math)[which(Math$math == 62)]
######################
Mathcomp <- read_csv("Mathcomp.csv")
Mathcomp
#Examples Ex 5.26
stem(Mathcomp$score)
BSDA::EDA(Mathcomp$score)
######################
Mathpro <- read_csv("Mathpro.csv")
Mathpro <- Mathpro %>%
  select(state, `Sat-M`, Profic, Group)%>%
  rename(`sat_math` = `Sat-M`, profic = Profic, group = Group)
Mathpro
#Examples Exp 9.1 and 9.6, Ex 9.24
model <- lm(sat_math ~ profic, data = Mathpro)
plot(sat_math ~ profic, data = Mathpro, ylab = "SAT", xlab = "proficiency")
abline(model)
summary(model)
rm(model)
######################
Maze <- read_csv("Maze.csv")
Maze <- Maze %>%
  select(score, condition)
Maze
#Examples Ex 10.13
boxplot(score ~ condition, data = Maze, col = rainbow(4))
anova(lm(score ~ condition, data = Maze))
#####################
Median <- read_csv("Median.csv")
Median <- Median %>%
  gather(Sample1, Sample2, Sample3, key = "sample", value = "value")
Median
#Examples Ex 10.52
boxplot(value ~ sample, data = Median, col = rainbow(3))
anova(lm(value ~ sample, data = Median))
kruskal.test(value ~ factor(sample), data = Median)
#####################
Mental <- read_csv("Mental.csv")
Mental
#Examples Ex 6.52 (age = mental age)
BSDA::SIGN.test(Mental$age, md = 100)
#####################
Mercury <- read_csv("Mercury.csv")
Mercury
#Examples Exp 1.9 (mercury measured in parts per million)
stem(Mercury$mercury)
#####################
Metrent <- read_csv("Metrent.csv")
Metrent
#Examples Ex 5.117 (monthly rent in dollars)
boxplot(Metrent$rent)
t.test(Metrent$rent, conf.level = 0.99)$conf
#####################
Miller <- read_csv("Miller.csv")
Miller
#Examples Ex 5.7 not Exp 5.7 (miller = Scores on Miller Personality Test)
stem(Miller$miller)
fivenum(Miller$miller)
boxplot(Miller$miller)
qqnorm(Miller$miller,col = "blue")
qqline(Miller$miller, col = "red")
#####################
Miller1 <- read_csv("Miller1.csv")
Miller1
#Examples Ex 1.41 (miller = Scores on Miller Personality Test)
stem(Miller1$miller)
stem(Miller1$miller, scale = 2)
#####################
Moisture <- read_csv("Moisture.csv")
Moisture
#Examples Ex 9.32 not 9.37 (moisture is g of water per 100 g of dried sediment)
plot(moisture ~ depth, data = Moisture)
model <- lm(moisture ~ depth, data = Moisture)
abline(model)
plot(resid(model) ~ depth, data = Moisture)
rm(model)
#####################
Monoxide <- read_csv("Monoxide.csv")
Monoxide <- Monoxide %>%
  rename(manufacturer = manufac, competitor = compet) %>%
  gather(manufacturer, competitor, key = "company", value = "emission")
Monoxide <- na.omit(Monoxide)
Monoxide
#Examples Ex 7.45
boxplot(emission ~ company, data = Monoxide, col = topo.colors(2))
t.test(emission ~ company, data = Monoxide)
wilcox.test(emission ~ company, data = Monoxide)
## Not run
ggplot2::ggplot(data = Monoxide, aes(x = company, y = emission)) + 
  geom_boxplot() + 
  theme_bw()
##
#####################
Movie <- read_csv("Movie.csv")
Movie <- Movie %>%
  rename(before = Before, after = After)
Movie
#Examples Ex 7.53
qqnorm(Movie$differ)
qqline(Movie$differ)
shapiro.test(Movie$differ)
t.test(Movie$after, Movie$before, paired = TRUE, conf.level = 0.99)
wilcox.test(Movie$after, Movie$before, paired = TRUE)
#####################
Music <- read_csv("Music.csv")
Music <- Music %>%
  rename(method1 = Method1, method2 = Method2)
Music
#Examples Ex 7.59
qqnorm(Music$differ)
qqline(Music$differ)
shapiro.test(Music$differ)
t.test(Music$method1, Music$method2, paired = TRUE)
# Or
t.test(Music$differ)
## Not run
ggplot2::ggplot(data = Music, aes(x = differ)) + 
  geom_dotplot() + 
  theme_bw()
##
#####################
Name <- read_csv("Name.csv")
Name <- Name %>%
  rename(brand = Brand)
Name
#Examples Exp 2.8 and Ex 2.28 (value and revenue billions of dollars)
plot(value ~ revenue, data = Name)
model <- lm(value ~ revenue, data = Name)
abline(model)
cor(Name$value, Name$revenue)
summary(model)
rm(model)
#####################
Nascar <- read_csv("Nascar.csv")
Nascar <- Nascar%>%
  select(time = Time, team = Team, ranks = Ranks)
Nascar
#Examples Ex 10.53
boxplot(time ~ team, data = Nascar)
model <- lm(time ~ factor(team), data = Nascar)
summary(model)
anova(model)
rm(model)
#####################
Nervous <- read_csv("Nervous.csv")
Nervous
#Examples Exp 10.3
boxplot(react ~ drug, data = Nervous, col = rainbow(4))
model <- aov(react ~ factor(drug), data = Nervous)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model), las = 1)
#####################
Newsstand <- read_csv("Newsstand.csv")
Newsstand
#Examples Ex 1.43 (daily profit in dollars)
stem(Newsstand$profit)
stem(Newsstand$profit, scale = 3)
#####################
Nfldraf2 <- read_csv("Nfldraf2.csv")
Nfldraf2 <- Nfldraf2%>%
  rename(rating = Rating)
Nfldraf2
#Examples Ex 9.63 (forty = time in secondds for 40 yard dash)
plot(rating ~ forty, data = Nfldraf2)
summary(lm(rating ~ forty, data = Nfldraf2))
#####################
Nfldraft <- read_csv("Nfldraft.csv")
Nfldraft <- Nfldraft %>%
  rename(rating = Rating)
Nfldraft
#Examples Ex 9.10 and 9.16
plot(rating ~ forty, data = Nfldraft)
cor(Nfldraft$rating, Nfldraft$forty)
summary(lm(rating ~ forty, data = Nfldraft))
#####################
Nicotine <- read_csv("Nicotine.csv")
Nicotine
#Examples Ex 9.21 (nicotine in milligrams, sales in $100,000 dollars)
model <- lm(sales ~ nicotine, data = Nicotine)
plot(sales ~ nicotine, data = Nicotine)
abline(model)
summary(model)
predict(model, newdata = data.frame(nicotine = 1), 
        interval = "confidence", level = 0.99)
#####################
Orange <- read_csv("Orange.csv")
Orange
#Examples Ex 9.61 (harvest in millions of boxes - price is average price charged 
# by California growers for for 75-pound box of navel oranges - values are for 6 consecutive years)
plot(price ~ harvest, data = Orange)
model <- lm(price ~ harvest, data = Orange)
abline(model)
summary(model)
rm(model)
#####################
Orioles <- read_csv("Orioles.csv")
Orioles
#Examples Exp 1.3
stripchart(Orioles$`1999salary`, method = "stack", pch = 19)
## Not run
ggplot2::ggplot(data = Orioles, aes(x = `1999salary`)) + 
  geom_dotplot(dotsize = 0.5) + 
  labs(x = "1999 Salary") +
  theme_bw()
##
#####################
Oxytocin <- read_csv("Oxytocin.csv")
Oxytocin <- Oxytocin %>%
  rename(subject = Subject, before = Before, after = After)
Oxytocin
#Examples Ex 7.86 (mean arterial blood pressure of 11 subjects
# before and after they received oxytocin.)

diff = Oxytocin$after - Oxytocin$before
qqnorm(diff)
qqline(diff)
shapiro.test(diff)
t.test(Oxytocin$after, Oxytocin$before, paired = TRUE)
rm(diff)
#####################
Parented <- read_csv("Parented.csv")
Parented <- Parented %>%
  rename(educat = Educat, mother = Mother, father = Father)
Parented
#Examples Ex 1.32
mat <- cbind(Parented$mother, Parented$father)
row.names(mat) <- Parented$educat
barplot(t(mat), beside = TRUE, legend = TRUE, col = c("blue", "red"))
rm(mat)
#####################
Patrol <- read_csv("Patrol.csv")
Patrol <- Patrol %>%
  select(tickets, years, log_tickets = `ln(tickets)`)
Patrol
#Examples Exp 9.3 (tickets per week)
model <- lm(tickets ~ years, data = Patrol)
summary(model)
confint(model, level = 0.98)
####################
Pearson <- read_csv("Pearson.csv")
Pearson
#Examples Ex 2.20 (inches)
plot(brother ~ sister, data = Pearson)
cor(Pearson$brother, Pearson$sister)
####################
Phone <- read_csv("Phone.csv")
Phone
#Examples Ex 6.95
qqnorm(Phone$time)
qqline(Phone$time)
shapiro.test(Phone$time)
BSDA::SIGN.test(Phone$time, md = 5, alternative = "greater")
####################
Poison <- read_csv("Poison.csv")
type <- c(rep("Drugs", 150857), rep("Cleaning Agent", 22347), 
          rep("Plants", 22326), rep("Cosmetics", 13192), 
          rep("Alcohol", 9201), rep("Insecticides", 8438))
Poison <- data.frame(type)
Poison <- as_tibble(Poison)
Poison
#Examples Ex 1.113
T1 <- xtabs(~type, data = Poison)
T1
par(mar = c(5.1 + 2, 4.1, 4.1, 2.1))
barplot(sort(T1, decreasing = TRUE), las = 2, col = rainbow(6))
par(mar = c(5.1, 4.1, 4.1, 2.1))
rm(T1)
## Not run
ggplot2::ggplot(data = Poison, aes(x = type, fill = type)) + 
  geom_bar() + 
  theme_bw() + 
  guides(fill = FALSE)
##
####################
Politic <- read_csv("Politic.csv")
Politic <- Politic %>%
  rename(party = Party, gender = Gender)
Politic$party <- ifelse(Politic$party == 1, "democrat",
                        ifelse(Politic$party == 2, "republican", "other"))
Politic$gender <- ifelse(Politic$gender == 1, "female", "male")
Politic
#Examples Exp 8.3
T1 <- xtabs(~party + gender, data = Politic)
T1
chisq.test(T1)
rm(T1)
###################
Pollutio <- read_csv("Pollutio.csv")
Pollutio
#Examples Ex 5.59 (air pollution index for a major western city on 15 randomly selected summer days)
stem(Pollutio$inde)
t.test(Pollutio$inde, conf.level = 0.98)$conf
##################
Porosity <- read_csv("Porosity.csv")
Porosity
#Examples Ex 5.86 (percent)
stem(Porosity$porosity)
fivenum(Porosity$porosity)
boxplot(Porosity$porosity)
##################
## Alan stopped here 7/19 @ 9:09am

Poverty <- read_csv("Poverty.csv")
Poverty <- Poverty %>%
  rename(city = City, poverty = Poverty, crime = Crime)
Poverty <- Poverty %>%
  select(city, poverty, crime, population = popu)
Poverty
#Examples Ex 9.11 and 9.17 (Crime rate per 1000, poverty = % in poverty)
plot(poverty ~ crime, data = Poverty)
model <- lm(poverty ~ crime, data = Poverty)
abline(model)
summary(model)
rm(model)
##################
Precinct <- read_csv("Precinct.csv")
Precinct
#Examples Ex 2.2 and 2.38 (robbery rate (robberies per 1000 people?), percent low income residents)
plot(rate ~ income, data = Precinct)
model <- (lm(rate ~ income, data = Precinct))
abline(model)
rm(model)
##################
Prejudic <- read_csv("Prejudic.csv")
Prejudic
#Examples Exp 5.10 (not Ex 5.22)
stem(Prejudic$prejud)
BSDA::EDA(Prejudic$prejud)
##################
Presiden <- read_csv("Presiden.csv")
Presiden <- Presiden %>%
  rename(first_initial = firs, last_name = Presiden, birth_state = Birt, inaugural_age = Inaugage, death_age = Deathage)
Presiden
#Examples Ex 1.126
pie(xtabs(~birth_state, data = Presiden))
stem(Presiden$inaugural_age)
stem(Presiden$death_age)
par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
stripchart(x=list(Presiden$inaugural_age, Presiden$death_age), 
           method = "stack",
           group.names = c("Inaugural Age", "Death Age"),
           col = c("green","brown"), pch = 19, las = 1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
##
##################
Press <- read_csv("Press.csv")
Press <- Press %>%
  select(education_yrs = educat, confidence = confid)
Press
#Examples Ex 9.55 (educat)
plot(confidence ~ education_yrs, data = Press)
model <- lm(confidence ~ education_yrs, data = Press)
abline(model, col = "purple")
summary(model)
rm(model)
##################
Prognost <- read_csv("Prognost.csv")
Prognost <- Prognost %>%
  rename(kprs_score = score)
Prognost
#Examples Ex 6.61
BSDA::EDA(Prognost$kprs_score)
t.test(Prognost$kprs_score, mu = 9)
##################
Program <- read_csv("Program.csv")
Program <- Program %>%
  rename(method1 = Method1, method2 = Method2, method3 = Method3, method4 = Method4)%>%
  gather(method1, method2, method3, method4, key = "method", value = "score")
Program
#Examples Ex 10.17
boxplot(score ~ method, col = c("red", "blue", "green", "yellow"), data = Program)
anova(lm(score ~ method, data = Program))
TukeyHSD(aov(score ~ method, data = Program))
par(mar = c(5.1, 4.1 + 4, 4.1, 2.1))
plot(TukeyHSD(aov(score ~ method, data = Program)), las = 1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
##################
Psat <- read_csv("Psat.csv")
Psat <- Psat %>%
  select(psat, sat)
Psat
#Examples Ex 2.50
model <- lm(sat ~ psat, data = Psat)
par(mfrow = c(1, 2))
plot(Psat$psat, resid(model))
plot(model, which = 1)
rm(model)
par(mfrow = c(1, 1))
##################
Psych <- read_csv("Psych.csv")
Psych
#Examples Ex 1.42 (score = number of correct repsonses in a psychology experiment)
stem(Psych$score)
BSDA::EDA(Psych$score)
##################
Puerto <- read_csv("puerto.csv")
Puerto
#Examples Ex 5.22 and 5.65 (income = weekly family income in dollars)
stem(Puerto$income)
boxplot(Puerto$income)
t.test(Puerto$income,conf.level = .90)$conf
##################
Quail <- read_csv("Quail.csv")
Quail <- Quail%>%
  rename(treatment = treatmen) %>%
  gather(placebo, treatment, key = "group", value = "level")
Quail
#Examples Ex 1.53, 1.77, 1.88, 5.66, and 7.50 (level = low-density lipoprotein (LDL) cholestrol level)
boxplot(level ~ group, data = Quail, horizontal = TRUE, xlab = "LDL Level",
        col = c("yellow", "lightblue"))
##################
Quality <- read_csv("Quality.csv")
Quality <- Quality %>%
  gather(Process1, Process2, key = "process", value = "score")
Quality <- na.omit(Quality)
Quality
# Examples Ex 7.81 (score = results of a quality control test)
boxplot(score ~ process, data = Quality)
t.test(score ~ process, data = Quality)

##################
Rainks <- read_csv("Rainks.csv")
Rainks
#Examples Ex 9.8 (page 465)
cor(Rainks)
model <- lm(rain ~ x2, data = Rainks)
summary(model)
##################
Randd <- read_csv("Randd.csv")
Randd
Randd <- Randd %>%
  select(rd, sales)
Randd
#Examples Exp 9.8 and Ex 9.37 (not 9.36) (all numbers in million dollars) 
# (rd = research and development in millions of dollars, sales = sales in millions of dollars)
plot(sales ~ rd, data = Randd)
model <- lm(sales ~ rd, data = Randd)
abline(model)
summary(model)
plot(model, which = 1)
rm(model)
##################
Rat <- read_csv("Rat.csv")
Rat
Rat <- Rat %>%
  rename(survival_time = `survival time`)
Rat
#Examples Ex 1.52, 1.76, 5.62, and 6.44 
# (survival_time = survival time in weeks for rats exposed to a high level of radiation)
hist(Rat$survival_time)
qqnorm(Rat$survival_time)
qqline(Rat$survival_time)
summary(Rat$survival_time)
t.test(Rat$survival_time)
t.test(Rat$survival_time, mu = 100, alternative = "greater")
##################
Ratings <- read_csv("Ratings.csv")
Ratings <- Ratings %>%
  select(rating = Rating, gpa = GPA)
Ratings
#Examples Exp 2.6 (page 83) 
# (gpa = students' grade point average, rating = instructor rating (A-F))
boxplot(gpa ~ rating, data = Ratings, xlab = "Student rating of instructor", 
        ylab = "Student GPA")
## Not run
ggplot2::ggplot(data = Ratings, aes(x = rating, y = gpa, fill = rating)) +
  geom_boxplot() + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Student rating of instructor", y = "Student GPA")
##
##################
Reaction <- read_csv("Reaction.csv")
Reaction
# Examples Example 6.11 not Exercise 
# (page 330, time = threshold reaction time (in seconds) for persons
# subjected to emotional stress)
stem(Reaction$time)
BSDA::SIGN.test(Reaction$time, md = 15, alternative = "less")

##################
Reading <- read_csv("Reading.csv")
Reading <- Reading %>%
  rename(score = reading)
Reading
#Examples Ex 1.72 (but not Ex 2.10)
# (score = standardized reading test score)
hist(Reading$score, main = "Exercise 1.72", 
     col = "lightgreen", xlab = "Standardized reading score")
summary(Reading$score)
sd(Reading$score)
##################
Readiq <- read_csv("Readiq.csv")
Readiq <- Readiq%>%
  rename(iq = IQ)
Readiq
#Examples Ex 2.53, page  ( reading = reading achievement score, iq = IQ score)
plot(reading ~ iq, data = Readiq)
model <- lm(reading ~ iq, data = Readiq)
abline(model)
predict(model, newdata = data.frame(iq = c(100, 120)))
residuals(model)[c(6, 7)]
rm(model)
##################
# Referend <- read_csv("Referend.csv")
# Create Referend
mat <- matrix(data = c(24, 29, 7, 68, 39, 12, 47, 8, 3), byrow = TRUE, 
              nrow = 3)
dimnames(mat) <- list(choice = c("A", "B", "C"), 
                      response = c("for", "against", "undecided"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Referend <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Referend$choice <- factor(Referend$choice, 
                          levels = c("A", "B", "C"))
Referend$response <- factor(Referend$response, 
                            levels = c("for", "against", "undecided"))
Referend <- as_tibble(Referend)
Referend 
#Examples Ex 8.20 page 436 (choice = response to question (A, B, or C), 
# response = opinion on referendum (for, against, undecided) )
T1 <- xtabs(~choice + response, data = Referend)
T1
chisq.test(T1)
chisq.test(T1)$expected
##################
Region <- read_csv("Region.csv")
Region <- Region %>%
  select(pollution = Index, region = Region, ranks = Ranks)
Region$region <- ifelse(Region$region == 1, "west",
                        ifelse(Region$region == 2, "central", "east"))
Region
#Examples Ex 10.26 
# (pollution = pollution index for a county, region = region of a county (west, central, east))
boxplot(pollution ~ region, data = Region)
anova(lm(pollution ~ region, data = Region))
##################
Register <- read_csv("Register.csv")
Register
Register <- Register %>%
  select(age, cost)
Register
#Examples Ex 2.3, 2.39, and 2.54 
# (cost = maintenance cost of cash register in dollars, age = age of cash register in years)
plot(cost ~ age, data = Register)
model <- lm(cost ~ age, data = Register)
abline(model)
predict(model, newdata = data.frame(age = c(5, 10)))
plot(model, which = 1)
rm(model)
##################
Rehab <- read_csv("Rehab.csv")
Rehab <- Rehab %>%
  select(psych1 = Psych1, psych2 = Psych2, differ)
Rehab$inmate <- 1:20
Rehab <- Rehab %>%
  select(inmate, psych1, psych2, differ)
Rehab
#Examples Ex 7.61 (inmate = inmate identification number, 
# psch1 = rating from first psychiatrist on the inmates rehabilative potential, 
# psych2 = rating from second psychiatrist on the inmates rehabilative potential)
boxplot(Rehab$differ)
qqnorm(Rehab$differ)
qqline(Rehab$differ)
t.test(Rehab$differ)
# Or
t.test(Rehab$psych1, Rehab$psych2, paired = TRUE)
##################
Remedial <- read_csv("Remedial.csv")
Remedial <- Remedial %>% 
  gather(female, male, key = "gender", value = "score")
Remedial
#Examples Ex 7.43, page 389, (score = math placement score, gender = female, male)
boxplot(score ~ gender, data = Remedial, 
        col = c("purple", "blue"))
t.test(score ~ gender, data = Remedial, conf.level = 0.98)
t.test(score ~ gender, data = Remedial, conf.level = 0.98)$conf
wilcox.test(score ~ gender, data = Remedial, 
            conf.int = TRUE, conf.level = 0.98)
##################
Rentals <- read_csv("Rentals.csv")
Rentals
#Examples Ex 1.122, page 70 (rent = weekly apartment rental price (in dollars))
stem(Rentals$rent)
sum(Rentals$rent < mean(Rentals$rent) - 3*sd(Rentals$rent) | 
      Rentals$rent > mean(Rentals$rent) + 3*sd(Rentals$rent))
##################
Repair <- read_csv("Repair.csv")
Repair
#Examples Ex 5.77 (time = time to repair a wrecked in car (in hours))
stem(Repair$time)
BSDA::SIGN.test(Repair$time, conf.level = 0.98)
##################
Retail<- read_csv("Retail.csv")
Retail
# Examples Ex 9.59, page 509, (months = length of employment (in months), 
# sales = employee gross sales (in dollars))
plot(sales ~ months, data = Retail)
model <- lm(sales ~ months, data = Retail)
abline(model)
summary(model)
##################
Ronbrown1 <- read_csv("Ronbrown1.csv")
Ronbrown1 <- Ronbrown1 %>%
  select(depth, temperature = downtemp1)
Ronbrown1  
#Examples Ex 2.9, Example 2.4 uses Ronbrown2
# (depth = ocen depth (in meters),
# temperature = ocean temperature (in centigrade))
plot(temperature ~ depth, data = Ronbrown1, ylab = "Temperature")
##################
Ronbrown2 <- read_csv("Ronbrown2.csv")
Ronbrown2 <- Ronbrown2 %>%
  select(depth, temperature = primarytemp, salinity = primarysalinity)
Ronbrown2
#Examples Exp 2.4 and Ex 2.56
# (depth = ocen depth (in meters),
# temperature = ocean temperature (in centigrade))
# salinity = ocean salinity level
plot(salinity ~ depth, data = Ronbrown2)
model <- lm(salinity ~ depth, data = Ronbrown2)
summary(model)
plot(model, which = 1)
rm(model)
##################
Rural <- read_csv("Rural.csv")
Rural <- Rural%>%
  select(score, area = code)
Rural$area <- ifelse(Rural$area == 1, "rural", "city")
Rural
#Examples Exp 7.16, page 383
# score = childs social adjustment score
# area = character variable with values city and rural
boxplot(score ~ area, data = Rural)
wilcox.test(score ~ area, data = Rural)
## Not run
Rural <- dplyr::mutate(Rural, r = rank(score))
Rural
t.test(r ~ area, data = Rural)
##################
Salary <- read_csv("Salary.csv")
Salary
#Examples Ex 3.66, page 182
# salary = starting salary for Ph.D. psycholgists
qqnorm(Salary$salary, pch = 19, col = "purple")
qqline(Salary$salary, col = "blue")
##################
Salinity <- read_csv("Salinity.csv")
Salinity
# Examples Ex 5.27 and 5.64
# salinity = surface-water salinity value
stem(Salinity$salinity)
qqnorm(Salinity$salinity, pch = 19, col = "purple")
qqline(Salinity$salinity, col = "blue")
t.test(Salinity$salinity, conf.level = 0.99)
t.test(Salinity$salinity, conf.level = 0.99)$conf
##################
Sat <- read_csv("Sat.csv") 
Sat <- Sat %>%
  select(-code)
Sat
names(Sat)
Sat94 <- Sat %>%
  select(state, verbal = verbal94, math = math94, total = total94, percent = percent94, expend = expend94)
Sat94
Sat95 <- Sat %>%
  select(state, verbal = verbal95, math = math95, total = total95)
Sat95
Sat99 <- Sat %>%
  select(state, verbal = verbal99, math = math99, total = total99, percent = percent99,  expend = expend99)
Sat99
Sat <- rbind(Sat94, Sat99)
Sat$year <- factor(rep(c(1994, 1999), each = 51))
Sat
# Examples Stat Insight Ch 9, page 456
# state = US state
# verbal = verbal SAT score
# math = math SAT score
# total = combined verbal and math SAT score
# percent = percent of high school seniors taking the SAT
# expend = state expenditure per student (in dollars)
# year = year
Sat94 <- Sat[Sat$year == 1994, ]
Sat94
Sat99 <- subset(Sat, year == 1999)
Sat99
stem(Sat99$total)
plot(total ~ percent, data = Sat99)
model <- lm(total ~ percent, data = Sat99)
abline(model, col = "blue")
summary(model)
rm(model)
##################
Saving <- read_csv("Saving.csv")
Saving <- Saving %>% 
  select(par = PAR, state, ranks)
Saving$state <- ifelse(Saving$state == 1, "California",
                       ifelse(Saving$state == 2, "NewYork", "Texas"))
Saving <- na.omit(Saving)
Saving$test <- rank(Saving$par)
Saving
Saving <- Saving %>%
  select(par, state)
Saving
#Examples Ex 10.34 and 10.49, page 551,
# par = problem-asset-ratio for Savings & Loans that wer listed as being financially troubled in 1992 
boxplot(par ~ state, data = Saving)
boxplot(par ~ state, data = Saving, log = "y")
model <- aov(par ~ state, data = Saving)
summary(model)
plot(TukeyHSD(model))
kruskal.test(par ~ factor(state), data = Saving)
##################
##################
Scales <- read_csv("Scales.csv")
Scales <- Scales %>%
  rename(brand = Brand)
Scales
#Examples Ex 1.89, page 61
# brand = variable indicating brand of bathroom scale (A, B, C, or D)
# reading = recorded value (in pounds) of a 100 pound weight
boxplot(reading ~ brand, data = Scales, col = rainbow(4), 
        ylab = "Weight (lbs)")
## Not run
ggplot2::ggplot(data = Scales, aes(x = brand, y = reading, fill = brand)) + 
  geom_boxplot() + 
  labs(y = "weight (lbs)") +
  theme_bw() + 
  theme(legend.position = "none") 
##
##################
Schizop2 <- read_csv("Schizop2.csv")
Schizop2
#Examples Ex 6.99, page 348
# Learning exhibited by patients with schizophrenia after they take a specified does of a tranquilizer.
# score = schizophrenics score on a second standardized exam
hist(Schizop2$score, xlab = "score on standardized test after a tranquilizer", 
     main = "Exercise 6.99", breaks = 10, col = "orange")
BSDA::EDA(Schizop2$score)
BSDA::SIGN.test(Schizop2$score, md = 22, alternative = "greater")
##################
Schizoph <- read_csv("Schizoph.csv")
Schizoph
#Examples Exp 6.10
# Learning exhibited by patients with schizophrenia after they take a specified does of a tranquilizer.
# score = schizophrenics score on a standardized exam one hour after recieving a specified dose of a tranqilizer.
hist(Schizoph$score, xlab = "score on standardized test", 
     main = "Example 6.10", breaks = 10, col = "orange")
BSDA::EDA(Schizoph$score)
t.test(Schizoph$score, mu = 20)
##################
## Seatbelt <- read_csv("Seatbelt.csv")
## Create Seatbelt
mat <- matrix(data = c(12813, 647, 359, 42, 65963, 4000, 2642, 303), byrow = TRUE,  nrow = 2)
dimnames(mat) <- list(seatbelt = c("Yes", "No"), 
                      injuries = c("None", "Minimal", "Minor", "Major"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Seatbelt <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Seatbelt$seatbelt <- factor(Seatbelt$seatbelt, 
                            levels = c("Yes", "No"))
Seatbelt$injuries <- factor(Seatbelt$injuries, 
                            levels = c("None", "Minimal", "Minor", "Major"))
Seatbelt <- as_tibble(Seatbelt)
Seatbelt
#Examples Ex 8.24
# Prior to enactment of seat belt legislation in Alberta, Canada, data
# were collected on 86, 769 automobile accidents reports to determine the effectiveness
# of seat belts in preventing injury.
# seatbelt = a factor with levels Yes or No indicating whether the driver was wearing a seatbelt
# injuries = a factor with levels None, Minimal, Minor, or Major indicating the extent of the drivers injuries.
#
T1 <- xtabs(~seatbelt + injuries, data = Seatbelt)
T1
chisq.test(T1)
rm(T1)
################## ATA stopped 6/30/17 @ 6:12 am here
#########################################################
Selfdefe <- read_csv("Selfdefe.csv")
Selfdefe <- Selfdefe %>%
  rename(woman = Woman, before = Before, after = After)
Selfdefe
#Examples Exp 7.19, page 397
# A group of women in a large city were given instruction on self defense.  
# Prior to the course, they were tested to determine their self-confidence.  
# After the course they were given the same test.  A high score on the test
# indicates a high degree of self-confidence.
# woman = number identifying the woman
# before = before the course self-confidence score
# after = after the course self-confidence score
Selfdefe$differ <- Selfdefe$after - Selfdefe$before
Selfdefe
t.test(Selfdefe$differ, alternative = "greater")
t.test(Selfdefe$after, Selfdefe$before, 
       paired = TRUE, alternative = "greater")
##################
Senior <- read_csv("Senior.csv")
Senior
#Examples Ex 1.83 and 3.67, page 59
# Reaction times for senior citizens
# Sent Larry email about units 7/20/16...as seconds does not make sense
# reaction = reaction time for senior citizens applying for a driver's license renewal
stem(Senior$reaction)
fivenum(Senior$reaction)
boxplot(Senior$reaction, main = "Problem 1.83, part d",
        horizontal = TRUE, col = "purple")
#################
Sentence <- read_csv("Sentence.csv")
Sentence
#Examples Ex 1.123, page 71
# months = sentence length in months for prisoners convicted of homocide
stem(Sentence$months)
ll <- mean(Sentence$months)-2*sd(Sentence$months)
ul <- mean(Sentence$months)+2*sd(Sentence$months)
limits <- c(ll, ul)
limits
rm(ul, ll, limits)
#################
Shkdrug <- read_csv("Shkdrug.csv")
Shkdrug <- Shkdrug %>%
  select(treatment = Treatment, response = Response)
Shkdrug
# Examples Ex 10.11-12, page 532
# Effect of drug and electroshock therapy on a subject's ability to solve simple tasks.
# treatment = type of treament Drug/NoS, Drug/Shk, NoDg/NoS, or NoDrug/S
# response = number of tasks completed in a 10-minute period
boxplot(response ~ treatment, data = Shkdrug)
model <- lm(response ~ treatment, data = Shkdrug)
anova(model)
rm(model)
#################
Shock <- read_csv("Shock.csv")
Shock <- Shock %>%
  gather(key = "group", value = "attempts")
Shock
#Examples Ex 10.50, page 561
# Effect of shock treatment on the amount of time one takes to complete 
# a difficult task.
# group = grouping variable with values of Group1 (no shock), Group2 (medium shock), 
# and Group3 (sever shock)
# attempts = number of attempts to complete a task
boxplot(attempts ~ group, data = Shock)
model <- lm(attempts ~ group, data = Shock)
anova(model)
rm(model)
#################
#################
## Alan stopped here for lunch 7/20/17 @ 12:08
#################
Shoplift <- read_csv("Shoplift.csv")
Shoplift
#Examples Ex 9.58, pg 509, (sales = sales in $1000, loss in $100) 
plot(loss ~ sales, data = Shoplift)
model <- lm(loss ~ sales, data = Shoplift)
summary(model)
rm(model)
################
Short <- read_csv("Short.csv")
Short <- Short %>% 
  select(sample = Sample, parallax = Parallax of the sun)
Short 
#Examples Ex 6.65, pg 341 ( sample = sample number, parallax = )
hist(Short$parallax, main = "Problem 6.65", 
     xlab = "", col = "orange")
BSDA::SIGN.test(Short$parallax, md = 8.798)
t.test(Short$parallax, mu = 8.798)
################
Shuttle <- read_csv("Shuttle.csv")
Shuttle <- Shuttle %>%
  select(users = shuttle, autos)
Shuttle
#Examples Ex 9.20, pg 483 - (users = number of shuttle riders, 
# autos = number of automobiles in the downtown area)
plot(autos ~ users, data = Shuttle)
model <- lm(autos ~ users, data = Shuttle)
summary(model)
rm(model)
################
Simpson <- read_csv("Simpson.csv")
Simpson <- Simpson%>%
  select(gpa, sport = spor, gender)
Simpson$sport <- ifelse(Simpson$sport == 1, "basketball",
                        ifelse(Simpson$sport == 2, "track", "soccer"))
Simpson$gender <- ifelse(Simpson$gender == 1, "male", "female")          
Simpson 
#Examples Examples 1.18, page 56 - (gpa = grade point average,
# sport = sport played (basketball, soccer, or track))
# gender = athlete sex (male, female)
boxplot(gpa ~ gender, data = Simpson)
boxplot(gpa ~ sport, data = Simpson)
## Not run
ggplot2::ggplot(data = Simpson, aes(x = gender, y = gpa, 
                                    fill = gender)) +
  geom_boxplot() + 
  facet_grid(.~sport) + theme_bw()
##
################
Situp <- read_csv("Situp.csv")
Situp
# Examples Ex 1.47, page 30
# number = maximum number of situps completed in an 
# exercise class after 1 month in the program.
stem(Situp$number)
hist(Situp$number, breaks = seq(0, 70, 10), right = FALSE)
hist(Situp$number, breaks = seq(0, 70, 10), right = FALSE, 
     freq = FALSE, col = "pink", main = "Problem 1.47", 
     xlab = "Maximum number of situps")
lines(density(Situp$number), col = "red")
################
Skewed <- read_csv("Skewed.csv")
Skewed
#Examples Ex 7.65, page 412 (C1 is a random sample of size 16 from a population, 
# C2 is random sample of size 14 from a population)
boxplot(Skewed$C1, Skewed$C2, col = c("pink", "lightblue"))
wilcox.test(Skewed$C1, Skewed$C2)
################ ATA stopped here 4:25 pm 6/29/17
##################################################################
##################################################################
Skin <- read_csv("Skin.csv")
Skin$patient <- 1:11
Skin <- Skin %>%
  select(patient, close, poor) %>%
  mutate(differ = close - poor)
Skin
# Examples Ex 5.20, page 244
# patient = patient identification number
# close = graft survival time in days for a closely matched skin graft on the same burn patient
# poor = graft survival time in days for a poorly matched skin graft on the same burn patient
# differ = difference between close and poor (in days) 
stem(Skin$differ)
boxplot(Skin$differ)
summary(Skin$differ)
#####################################################
Slc <- read_csv("Slc.csv")
Slc <- Slc %>%
  rename(slc = SLC)
Slc
# Examples Ex 5.116, page 296
# slc = Red blood cell sodium-lithium contertransport
BSDA::EDA(Slc$slc)
hist(Slc$slc, freq = FALSE, xlab = "sodium lithium countertransport",
     main = "", col = "lightblue")
lines(density(Slc$slc), col = "purple")
################
Smokyph <- read_csv("Smokyph.csv")
Smokyph <- Smokyph %>%
  select(waterph, code, elev)
Smokyph
# Examples Ex 6.40, 6.59, 7.10, 7.35, page 325
# waterph = water sample pH level
# code = charater variable with values low (elevation below 0.6 miles), and
# high (elevation above 0.6 miles)
# elev = elevation in miles

## code 0 = elevation below 0.6 miles, 1 = elevation above 0.6 miles
summary(Smokyph$waterph)
tapply(Smokyph$waterph, Smokyph$code, mean)
stripchart(waterph ~ code, data = Smokyph, method = "stack",
           pch = 19, col = c("red", "blue"))
t.test(Smokyph$waterph, mu = 7)
BSDA::SIGN.test(Smokyph$waterph, md = 7)
t.test(waterph ~ code, data = Smokyph, alternative = "less")
t.test(waterph ~ code, data = Smokyph, conf.level = 0.90)
## Not run
ggplot2::ggplot(data = Smokyph, aes(x = waterph, fill = code)) + 
  geom_dotplot() + 
  facet_grid(code ~ .) + 
  guides(fill = FALSE)
##
#########################################################
########## ATA stopped here (Smokyph) @ 10:44 06/29/17
## Snore <- read_csv("Snore.csv")
## Create Snore
mat <- matrix(data = c(24, 35, 21, 30, 1355, 603, 192, 224), byrow = TRUE, nrow = 2)
dimnames(mat) <- list(heartdisease = c("yes", "no"), 
                      snore = c("nonsnorer", "occassional snorer", "nearly every night", "snores every night"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Snore <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Snore$heart <- factor(Snore$heartdisease, 
                      levels = c("yes", "no"))
Snore$snore <- factor(Snore$snore, 
                      levels = c("nonsnorer", "occassional snorer", "nearly every night", "snores every night"))
Snore <- as_tibble(Snore)
Snore #rename variables?
## Examples Ex 8.21, page 437
# snore = factor with levels nonsnorer, ocassional snorer, nearly every night, and snores every night.
# heartdisease = factor indicating whether the indiviudal has heart disease (no or yes)
T1 <- xtabs(~ heartdisease + snore, data = Snore)
T1
chisq.test(T1)
rm(T1)
################
Snow <- read_csv("Snow.csv")
Snow <- Snow %>%
  select(concent, site)
Snow$site <- ifelse(Snow$site == 1, "Antarctica", "Greenland")
Snow
#Examples Ex 7.87, page 417
# conentration = concentration of microparticles from melted snow (in parts per billion)
# site = location of snow sample (Antarctica or Greenland)
boxplot(concent ~ site, data = Snow, col = c("lightblue", "lightgreen"))
################
Soccer <- read_csv("Soccer.csv")
Soccer
# Examples Ex 1.46, page 30
# weight = soccer players weight (in pounds)
stem(Soccer$weight, scale = 2)
hist(Soccer$weight, breaks = seq(110, 210, 10), col = "orange",
     main = "Problem 1.46 \n Weights of Soccer Players", 
     xlab = "weight (lbs)", right = FALSE)
################
Social <- read_csv("Social.csv")
Social
# Examples Ex 6.63, page 340
# income = annual income (in dollars) of North Carolina social 
# workers with less than five years experience.
BSDA::SIGN.test(Social$income, md = 27500, alternative = "less")
################
Sophomor <- read_csv("Sophomor.csv")
Sophomor <- Sophomor %>%
  rename(student = Student, gpa = GPA, sat = SAT, exam = Exam)
Sophomor
# Examples Ex 2.42, page 110
# student = student's identification number
# gpa = student's grade point average
# sat = student's SAT math score
# exam = student's final exam grade in college algebra 
cor(Sophomor)
plot(exam ~ gpa, data = Sophomor)
## Not run
ggplot2::ggplot(data = Sophomor, aes(x = gpa, y = exam)) + 
  geom_point()
ggplot2::ggplot(data = Sophomor, aes(x = sat, y = exam)) + 
  geom_point()
##
################
South <- read_csv("South.csv")
South
# Examples Ex 1.84, page 59
# rate = murder rate per 100,000 people
boxplot(South$rate, col = "gray", ylab = "Murder rate per 100,000 people")
################
Speed <- read_csv("Speed.csv")
Speed <- Speed %>%
  rename(before = Before, after = After) %>%
  mutate(signranks = sign(differ)*rank(abs(differ))) %>%
  select(before, after, differ, signranks)
Speed
#Examples Ex 7.58, page 404
# before = reading comprehension score before taking a speed-reading course
# after = reading comprehension score after taking a speed-reading course
# differ = after - before (comprehension reading scores)
# signranks = signed ranked differences
t.test(Speed$differ, alternative = "greater")
t.test(Speed$signranks, alternative = "greater")
wilcox.test(Speed$after, Speed$before, paired = TRUE, alternative = "greater")
################
Spellers <- read_csv("Spellers.csv")
Spellers <- Spellers %>%
  select(Fourth, Colleague = Colleag) %>%
  gather(Fourth, Colleague, key = "teacher", value = "score")
Spellers
# Examples Ex 7.82, page 416
# teacher = character variable with values Fourth and Colleague
boxplot(score ~ teacher, data = Spellers)
t.test(score ~ teacher, data = Spellers)
################
Spelling <- read_csv("Spelling.csv")
Spelling <- Spelling %>%
  rename(before = Before, after = After)
Spelling
# Examples Ex 7.56, page 403
# before = spelling score before a 2-week course of instruction
# after = spelling score after a 2-week course of instruction
# differ = after - before (spelling score)
qqnorm(Spelling$differ)
qqline(Spelling$differ)
shapiro.test(Spelling$differ)
t.test(Spelling$before, Spelling$after, paired = TRUE)
t.test(Spelling$differ)
################
## Sports <- read_csv("Sports.csv")
## Create Sports
mat <- matrix(data = c(33, 38, 24, 5, 38, 21, 15, 26), byrow = TRUE, nrow = 2)
dimnames(mat) <- list(gender = c("male", "female"), 
                      sport = c("football", "basketball", "baseball", "tennis"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Sports <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Sports$gender <- factor(Sports$gender, 
                        levels = c("male", "female"))
Sports$sport <- factor(Sports$sport, 
                       levels = c("football", "basketball", "baseball", "tennis"))
Sports <- as_tibble(Sports)
Sports
#Examples 8.32, page 444
# gender = a factor with levels male and female
# sport a factor with levels football, basketball, baseball, and tennis
T1 <- xtabs(~gender + sport, data = Sports)
T1
chisq.test(T1)
rm(T1)
################
## Spouse <- read_csv("Spouse.csv")
## Create Spouse
mat <- matrix(data = c(35, 35, 146, 87, 130, 69, 7, 31), byrow = TRUE,  nrow = 4)
dimnames(mat) <- list(result = c("not prosecuted", "pleaded guilty",
                                 "convicted", "acquitted"), 
                      spouse = c("husband", "wife"))
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Spouse <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Spouse$result <- factor(Spouse$result, 
                        levels = c("not prosecuted", "pleaded guilty",
                                   "convicted", "acquitted"))
Spouse$spouse <- factor(Spouse$spouse, 
                        levels = c("husband", "wife"))
Spouse <- as_tibble(Spouse)
Spouse
# Examples Ex 8.33, page 444
# result = a factor with levels not prosecuted, pleaded guilty, convicted, and acquited.
# spouse = a factor with levels husband and wife
T1 <- xtabs(~result + spouse, data = Spouse)
T1
chisq.test(T1)
rm(T1)
###############
Stable <- read_csv("Stable.csv")
Stable
# Examples Ex 6.93, page 345
# time = time (in seconds) for horse to run 1 mile
BSDA::SIGN.test(Stable$time, md = 98.5, alternative = "greater")
###############
Stamp <- read_csv("Stamp.csv")
Stamp <- Stamp%>%
  select(thickness)
Stamp
# thickness = stamp thickness (in mm)
#Examples Stat Insight Ch 1 and Ex 5.110 (thickness in mm)
hist(Stamp$thickness, freq = FALSE, col = "lightblue", 
     main = "", xlab = "stamp thickness (mm)")
lines(density(Stamp$thickness), col = "blue")
t.test(Stamp$thickness, conf.level = 0.99)
###############
Statclas <- read_csv("Statclas.csv")
Statclas <- Statclas%>%
  gather(`9am`, `2pm`, key = "class", value = "score")
Statclas
# Examples Ex 7.30, page 372
# class = class meeting time (9am or 2pm)
# score = grade for an introductory statistics class
str(Statclas)
boxplot(score ~ class, data = Statclas)
t.test(score ~ class, data = Statclas)
###############
Statelaw <- read_csv("Statelaw.csv")
Statelaw <- Statelaw %>%
  rename(state = State)
Statelaw
#Examples Ex 6.62, page 339
# state = US state
# cost = dollars spent per resident on law enforcement
BSDA::EDA(Statelaw$cost)
BSDA::SIGN.test(Statelaw$cost, md = 8, alternative = "less")
###############
Statisti <- read_csv("Statisti.csv")
Statisti <- Statisti%>%
  gather(Class1, Class2, key = "class", value = "score")
Statisti
#Examples Ex 1.70 and 1.87, page 47
# class = character variable with values Class1 and Class2
# score = test score for an introductory statistics test
boxplot(score ~ class, data = Statisti)
tapply(Statisti$score, Statisti$class, summary, na.rm = TRUE)
## Not run
dplyr::group_by(Statisti, class) %>%
  summarize(Mean = mean(score, na.rm = TRUE), 
            Median = median(score, na.rm = TRUE), 
            SD = sd(score, na.rm = TRUE),
            RS = IQR(score, na.rm = TRUE))
##
###############
Step <- read_csv("Step.csv")
Step
# Examples Ex 6.79, page 344
# score = State test of educational progress (STEP) science test score
BSDA::EDA(Step$score)
t.test(Step$score, mu = 80, alternative = "less")
wilcox.test(Step$score, mu = 80, alternative = "less")
###############
Stress <- read_csv("Stress.csv")
Stress <- Stress %>%
  rename(prestress = Prestre, poststress = Poststre)
Stress
# Examples Exp 7.20, page 398
# prestress = short term memory score before being exposed to a stressful situation
# poststress = short term memory score after being exposed to a stressful situation
diff <- Stress$prestress - Stress$poststress
qqnorm(diff)
qqline(diff)
t.test(diff)
t.test(Stress$prestress, Stress$poststress, paired = TRUE)
## Not run
wilcox.test(Stress$prestress, Stress$poststress, paired = TRUE)
## Stopped here 06/28/17 with examples @ 10:23am ATA
###############
Study <- read_csv("Study.csv")
Study
#Examples Ex 5.25, page 247
# hours = number of hours a week freshmen reported studying for their courses
stem(Study$hours)
hist(Study$hours)
summary(Study$hours)
###############
###############
## Alan stopped here 7/20/17 @ 9:49pm
###############
###############
Submarin <- read_csv("Submarin.csv")
Submarin <- Submarin %>%
  rename(month = Month)
Submarin
#Examples Ex 2.16 and 2.45, page 88
# reported = number of submarines reported sunk by U.S. Navy
# actual = number of submarines actually sunk by U.S. Navy
model <- lm(actual ~ reported, data = Submarin)
summary(model)
plot(actual ~ reported, data = Submarin)
abline(model, col = "red")
rm(model)
###############
Subway <- read_csv("Subway.csv")
Subway
#Examples Ex 5.19, page 243
# time = time (in minutes) it takes a subway to travel from the airport to downtown
hist(Subway$time, main = "Exercise 5.19", 
     xlab = "Time (in minutes)", col = "purple")
summary(Subway$time)
###############
Sunspot <- read_csv("Sunspot.csv")
Sunspot
#Examples Exp 1.7, page 15
# year = year
# sunspots = average number of sunspots for the year
plot(sunspots ~ year, data = Sunspot, type = "l")
## Not run
lattice::xyplot(sunspots ~ year, data = Sunspot, 
                main = "Yearly sunspots", type = "l")
lattice::xyplot(sunspots ~ year, data = Sunspot, type = "l", 
                main = "Yearly sunspots", aspect = "xy")
ggplot2::ggplot(data = Sunspot, aes(x = year, y = sunspots)) + 
  geom_line() + 
  theme_bw()
##
###############
Superbowl <- read_csv("Superbowl.csv")
Superbowl
Superbowl <- Superbowl %>%
  rename(winning_team = `Winning team`, winner_score = `winner score`, 
         losing_team = `Losing team`, loser_score = `loser score`, 
         victory_margin = margin)
Superbowl
#Examples Ex 1.54, page 32
# winning_team = name of Suberbowl winning team
# winning_score = winning score for the Superbowl
# losing_team = name of Suberbowl losing team
# loser_score = score of losing team
# victory_margin = winner_score - loser_score
stem(Superbowl$victory_margin)
###############
Supercar <- read_csv("Supercar.csv")
Supercar <- Supercar %>%
  select(speed, car)
Supercar$car <- ifelse(Supercar$car == 1,   "Acura", 
                       ifelse(Supercar$car == 2,   "Ferrari", 
                              ifelse(Supercar$car == 3,   "Lotus", 
                                     ifelse(Supercar$car == 4,   "Porsche", "Viper"))))
Supercar
#Examples Stat Insight Ch 10, page 512
# speed = top speed of car without redlining
# car = nmae of sports car
boxplot(speed ~ car, data = Supercar, col = rainbow(6),
        ylab = "Speed (mph)")
summary(aov(speed ~ car, data = Supercar))
anova(lm(speed ~ car, data = Supercar))
###############
Tablrock <- read_csv("Tablrock.csv")
Tablrock <- Tablrock %>%
  rename(ozone = `3`)
Tablrock
# 
dia <- rep(seq(as.Date("1992/09/01"), by = "day", length = 30), each = 24)
dia <- dia[-720]
Tablrock$day <- dia
Tablrock
#Examples Ex 5.63, page 272
# day = date
# hour = time of day
# ozone = ozone concentration
# tmp = temperature (in Celcius)
# Ask Larry what the rest means 7/21/17---sent email
summary(Tablrock$ozone)
boxplot(Tablrock$ozone)
qqnorm(Tablrock$ozone)
qqline(Tablrock$ozone)
par(mar = c(5.1 - 1, 4.1 + 2, 4.1 - 2, 2.1))
boxplot(ozone ~ day, data = Tablrock, 
        horizontal = TRUE, las = 1, cex.axis = 0.7)
par(mar = c(5.1, 4.1, 4.1, 2.1))
## Not run
ggplot2::ggplot(data = Tablrock, aes(sample = ozone)) + 
  geom_qq() + 
  theme_bw()
ggplot2::ggplot(data = Tablrock, aes(x = as.factor(day), y = ozone)) + 
  geom_boxplot(fill = "pink") + 
  coord_flip() + 
  labs(x = "") + 
  theme_bw()
##
###############
Teacher <- read_csv("Teacher.csv")
Teacher <- Teacher %>%
  rename(state = State) %>%
  gather(`1973-74`,`1983-84`,`1993-94`, key = "year", value = "salary")
Teacher
# Examples Ex 5.114, page 294
# state = U.S. state
# year = academic year
# salary = avaerage salary (in dollars)
par(mfrow = c(3, 1))
hist(Teacher$salary[Teacher$year == "1973-74"],
     main = "Teacher salary 1973-74", xlab = "salary",
     xlim = range(Teacher$salary, na.rm = TRUE))
hist(Teacher$salary[Teacher$year == "1983-84"],
     main = "Teacher salary 1983-84", xlab = "salary",
     xlim = range(Teacher$salary, na.rm = TRUE))
hist(Teacher$salary[Teacher$year == "1993-94"],
     main = "Teacher salary 1993-94", xlab = "salary",
     xlim = range(Teacher$salary, na.rm = TRUE))
par(mfrow = c(1, 1))
## Not run
ggplot2::ggplot(data = Teacher, aes(x = salary)) + 
  geom_histogram(fill = "purple", color = "black") + 
  facet_grid(year ~ .) + 
  theme_bw()
##
###############
Tenness <- read_csv("Tenness.csv")
Tenness
#Examples Ex 6.56, page 336
# score = Tennessee Self-Concept Scale score
hist(Tenness$score, freq= FALSE, main = "", col = "green",
     xlab = "Tennessee Self-Concept Scale score")
lines(density(Tenness$score))
## Not run
ggplot2::ggplot(data = Tenness, aes(x = score, y = ..density..)) + 
  geom_histogram(binwidth = 2, fill = "purple", color = "black") +
  geom_density(color = "red", fill = "pink", alpha = 0.3) + 
  theme_bw()
##
###############
Tensile <- read_csv("Tensile.csv")
Tensile <- Tensile %>%
  select(tensile = Tensile, run = Run)
Tensile$run <- factor(Tensile$run)
Tensile
#Examples Exp 7.11, page 368
# tensile = plastic bag tensile strength (pounds per square inch?)
# run = factor with run number (1 or 2)
boxplot(tensile ~ run, data = Tensile, 
        col = c("purple", "cyan"))
t.test(tensile ~ run, data = Tensile)
###############
Test1 <- read_csv("Test1.csv")
Test1 <- Test1 %>%
  rename(score = test1)
Test1
#Examples Ex 5.80, page 281
# score = score on first statistics exam
stem(Test1$score)
boxplot(Test1$score)
################  Stopped here 06/27/17 @ 12:02
Thermal <- read_csv("Thermal.csv")
Thermal <- Thermal %>%
  select(temp, loss)
Thermal
#Examples Exp 9.5page 473 (temp = degrees Celcius) (heat loss (BTUs))
model <- lm(loss ~ temp, data = Thermal)
summary(model)
plot(loss ~ temp, data = Thermal)
abline(model)
rm(model)
#################
Tiaa <- read_csv("Tiaa.csv")
Tiaa$date <- as.Date(Tiaa$date, format = "%m/%d/%y")
Tiaa
#Examples (find in book)
#################
Ticket <- read_csv("Ticket.csv")
Ticket
#Examples Ex 5.18, page 243
# time = time (in seconds) to check out a reservation
BSDA::EDA(Ticket$time)
#################
Toaster <- read_csv("Toaster.csv")
Toaster
#Examples Ex 9.36 not 9.35 as in app and help file, apge 501
# toaster = name of toaster
# score = Consumer Reports score
# cost = price of toaster (in dollars)
plot(cost ~ score, data = Toaster)
model <- lm(cost ~ score, data = Toaster)
summary(model)
names(summary(model))
summary(model)$r.squared
plot(model, which = 1)
################# ----
# Create Tonsils 
mat <- matrix(data = c(19, 497, 29, 560, 24, 269), nrow = 3, byrow = TRUE)
mat
dimnames(mat) <- list(size = c("Normal", "Large", "Very Large"), 
                      status = c("Carrier", "Non-carrier"))
mat
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Tonsils <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Tonsils$size <- factor(Tonsils$size, 
                       levels = c("Normal", "Large", "Very Large"))
Tonsils$status <- factor(Tonsils$status,
                         levels = c("Carrier", "Non-carrier"))
Tonsils <- as_tibble(Tonsils)
Tonsils
# Examples #Ex 2.78, page 127
# size = a factor with levels Normal, Large, and Very Large
# status = a factor with levels Carrier and Non-carrier
##
T1 <- xtabs(~size + status, data = Tonsils)
T1
prop.table(T1, 1)
prop.table(T1, 1)[2, 1]
barplot(t(T1), legend = TRUE, beside = TRUE, col = c("red", "green"))
## Not run
NDF <- dplyr::count(Tonsils, size, status) 
ggplot2::ggplot(data = NDF, aes(x = size, y = n, fill = status)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("red", "green")) + 
  theme_bw()
#################
Tort <- read_csv("Tort.csv")
Tort <- Tort %>%
  rename(population = populat)
Tort
# Examples Ex 5.13, page 235
# county = U.S. county
# months = average number of months to process a tort
# population = population of the county
# torts = number of torts
# rate = rate per 10,000 residents
BSDA::EDA(Tort$months)
#################
Toxic <- read_csv("Toxic.csv")
Toxic
# Examples Ex 1.55, 5.108, 5.109, 8.58, 10.35
# page 33, 454, 
# state = U.S. state
# region = U.S. region
# sites = number of commercial hazardous waste sites
# minority = ???percent of minorities living in communities with commercial
# hazardous waste sites (?)
# percent = ???

hist(Toxic$sites)
hist(Toxic$minority)
qqnorm(Toxic$minority)
qqline(Toxic$minority)
boxplot(sites ~ region, data = Toxic)
tapply(Toxic$sites, Toxic$region, median)
kruskal.test(sites ~ factor(region), data = Toxic)
#################
Track <- read_csv("Track.csv")
Track
#Examples Ex 2.97, 5.15, 9.62
# country = athletes country
# `100m` = time in seconds for 100 m
# `200m` = time in seconds for 200 m
# `400m` = time in seconds for 400 m
# `800m` = time in minutes for 800 m
# `1500m` = time in minutes for 1500 m
# `3000m` = time in minutes for 3000 m
# marathon = time in minutes for marathon

plot(`200m` ~ `100m`, data = Track)
plot(`400m` ~ `100m`, data = Track)
plot(`400m` ~ `200m`, data = Track)
cor(Track[, 2:8])
#################
Track15 <- read_csv("Track15.csv")
Track15 <- Track15 %>%
  rename(year = Year, time = `1500m`)
Track15
#Examples Ex 1.36, page 21
# year = Olympic year
# time = Olympic winning time (in seconds) for the 1500-meter run
plot(time~ year, data = Track15, type = "b", pch = 19,
     ylab = "1500m time in seconds", col = "green")
#################
Treatments <- read_csv("Treatments.csv")
Treatments <- Treatments%>%
  select(score = Treatmnt, group = Group)
Treatments$group <- factor(Treatments$group)
Treatments
#Examples Ex 10.44, page 556
# score = score from an experiment
# group = factor with levels 1, 2, and 3
boxplot(score ~ group, data = Treatments)
summary(aov(score ~ group, data = Treatments))
summary(lm(score ~ group, data = Treatments))
anova(lm(score ~ group, data = Treatments))
##################
Trees <- read_csv("Trees.csv")
Trees
#Examples Ex 1.50,
# number = number of trees in a grid
stem(Trees$number)
hist(Trees$number, main = "Exercise 1.50", xlab = "number",
     col = "brown")
##################
Trucks <- read_csv("Trucks.csv")
Trucks <- Trucks %>%
  select(mpg = `gas mileage`, truck)
Trucks
#Examples Table 10.2, page 514
# mpg = miles per gallon
# truck = character variable with values chevy, dodge, and ford
boxplot(mpg ~ truck, data = Trucks, horizontal = TRUE, las = 1)
summary(aov(mpg ~ truck, data = Trucks))
##################
Tv <- read_csv("Tv.csv")
Tv <- Tv %>%
  rename(state = State)
Tv
# Examples 2.1 and 2.7, page 78
# state = U.S. state
# percent = percent of students who watch more than six hours of TV a day
# test = state average on national math test
plot(test ~ percent, data = Tv, col = "blue")
cor(Tv$test, Tv$percent)
###################
Twin <- read_csv("Twin.csv")
Twin <- Twin %>%
  rename(twinA = TwinA, twinB = TwinB)
Twin
# Examples 7.54, page 402
# twinA = score on intelligence test without drug
# twinB = score on intelligence test after taking drug
# differ = twinA - twinB
qqnorm(Twin$differ)
qqline(Twin$differ)
shapiro.test(Twin$differ)
t.test(Twin$twinA, Twin$twinB, paired = TRUE)
###################
Undergrad <- read_csv("Undergrad.csv") 
Undergrad <- Undergrad %>%
  select(gender = Gender, major = Major, class = Class, gpa = GPA, sat = SAT, drops = Drops)
Undergrad
# Examples Ex 1.15, pgs 3, and 10
# gender = character variable with values Female and Male
# major = college major
# class = college year group classification
# gpa = grade point average
# sat = Scholastic Assessment Test score
# drops = numberof courses dropped
stripchart(gpa ~ class, data = Undergrad, method = "stack", col = c("blue","red","green","lightblue"),
           pch = 19, main = "GPA versus Class")
stripchart(gpa ~ gender, data = Undergrad, method = "stack", col = c("red", "blue"), pch = 19,
           main = "GPA versus Gender")
stripchart(sat ~ drops, data = Undergrad, method = "stack", col = c("blue", "red", "green", "lightblue"),
           pch = 19, main = "SAT versus Drops")
stripchart(drops ~ gender, data = Undergrad, method = "stack", col = c("red", "blue"), pch = 19,
           main = "Drops versus Gender")
## Not run
ggplot2::ggplot(data = Undergrad, aes(x = sat, y = drops, fill = factor(drops))) + 
  facet_grid(drops ~.) +
  geom_dotplot() +
  guides(fill = FALSE)
##
###################
Vacation <- read_csv("Vacation.csv")
Vacation
# Examples Ex 6.46 and not Ex 6.98, page 327
# number = number of days of paid holidays and vacation leave taken
boxplot(Vacation$number)
hist(Vacation$number, main = "Exercise 6.46", col = "blue",
     xlab = "number of days of paid holidays and vacation leave taken")
t.test(Vacation$number, mu = 24)
###################
Vaccine <- read_csv("Vaccine.csv")
Vaccine <- Vaccine %>%
  rename(state = "State")
Vaccine
# Examples # Ex 1.111, page 66
# state = U.S. state
# number = number of reported serious reactions per million doses of a vaccine
stem(Vaccine$number, scale = 2) 
fn <- fivenum(Vaccine$number)
fn
iqr <- IQR(Vaccine$number)
iqr
###################
# Create Vehicle
mat <- matrix(data = c(11, 0, 10, 4, 12, 30, 9, 38, 7, 30), nrow = 2, byrow = TRUE)
mat
dimnames(mat) <- list(make = c("Foreign", "Domestic"), 
                      rating = c("Much better than average", "Above average", "Average", "Below average", "Much worse than average"))
mat
matT <- as.table(mat)
matDF <- as.data.frame(matT)
Vehicle <- vcdExtra::expand.dft(matDF)
rm(mat, matT, matDF)
Vehicle$make <- factor(Vehicle$make, 
                       levels = c("Foreign", "Domestic"))
Vehicle$rating <- factor(Vehicle$rating, 
                         levels =  c("Much better than average", "Above average", "Average", "Below average", "Much worse than average"))
Vehicle <- as_tibble(Vehicle)
Vehicle
# Examples Ex 8.34, page 445 df 151 * 2
# make = factor with levels Foreign and Domestic
# rating = factor with levels Much better than average, Above average, Average, 
# Below average, and Much worse than average
T1 <- xtabs(~make + rating, data = Vehicle)
T1
chisq.test(T1)
###################
Verbal <- read_csv("Verbal.csv")
Verbal <- Verbal %>%
  select(number, verbal)
Verbal
# Examples Ex 9.30, page 495
# number = number of library books checked out
# verbal = verbal test score
plot(verbal ~ number, data = Verbal)
abline(lm(verbal ~ number, data = Verbal))
summary(lm(verbal ~ number, data = Verbal))
#################
Victoria <- read_csv("Victoria.csv")
# Victoria$year <- seq(as.Date("1902/01/01"), by = "year", length = 20)
Victoria
# Examples Ex 2.98, page 134
# year = year
# level = mean annual level of Lake Victoria Nyanza
# sunspot = number of sunspots

plot(level ~ sunspot, data = Victoria)
model <- lm(level ~ sunspot, data = Victoria)
summary(model)
rm(model)
#################
Viscosit <- read_csv("Viscosit.csv")
Viscosit
# Examples Ex 7.44, page 389
# first = viscosity measurement for a certain substance on day one
# second = viscosity measurement for a certain substance on day two
boxplot(Viscosit$first, Viscosit$second)
t.test(Viscosit$first, Viscosit$second, var.equal = TRUE)
#################
Visual <- read_csv("Visual.csv")
Visual
# Examples Ex 5.6
# visual = visual acuity measurement (units?)
stem(Visual$visual)
boxplot(Visual$visual, col = "purple")
#################
Vocab <- read_csv("Vocab.csv")
Vocab <- Vocab %>%
  select(first = First, second = Second)
Vocab
#Examples Ex 7.80, page 416
# first = reading test score before formal vocabulary training
# second = reading test score after formal vocabulary training
t.test(Vocab$first, Vocab$second, paired = TRUE)
#################
Wastewat <- read_csv("Wastewat.csv")
Wastewat <- Wastewat %>%
  select(gallons, number)
Wastewat
# Examples Ex 9.18, page 480
# gallons = injected water (in million gallons)
# number = number of earthqueakes detected in Denver
plot(number ~ gallons, data = Wastewat)
model <- lm(number ~ gallons, data = Wastewat)
summary(model)
anova(model)
plot(model, which = 2)
#################
#Weather94 <- read_csv("Weather94.csv")
type <- c(rep("Flash Flood", 59), rep("River Flood", 32), 
          rep("Lightning", 69), rep("Tornado", 69), 
          rep("Hurricane", 9), rep("Extreme Temp", 81), 
          rep("Winter Weather", 31), rep("Thunderstorm", 17), 
          rep("High Wind", 12), rep("Fog", 3), rep("Other", 6))
Weather94 <- data.frame(type)
Weather94 <- as_tibble(Weather94)
Weather94
#Examples Ex 1.30, page 19
# type = factor with levels Extreme Temp, Flash Flood, Fog, HIgh Wind, Hurricane, 
# Lighting, Other, River Flood, Thunderstorm, Tornado, and Winter Weather.

T1 <- xtabs(~type, data = Weather94)
T1
par(mar = c(5.1 + 2, 4.1 - 1, 4.1 - 2, 2.1))
barplot(sort(T1, decreasing = TRUE), las = 2, col = rainbow(11))
par(mar = c(5.1, 4.1, 4.1, 2.1))
## Not run
T2 <- as.data.frame(T1)
T2
ggplot2::ggplot(data =T2, aes(x = reorder(type, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "purple") +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle = 55, vjust = 0.5)) + 
  labs(x = "", y = "count")
##
#################
Wheat <- read_csv("Wheat.csv")
Wheat
#Examples Ex 2.11, page 87
# year = year
# earnings =  national weekly earnings (in dollars) for production workers
# price = price for a bushel of wheat (in dollars)
par(mfrow = c(1, 2))
plot(earnings ~ year, data = Wheat)
plot(price ~ year, data = Wheat)
par(mfrow = c(1, 1))
#################
Windmill <- read_csv("Windmill.csv")
Windmill <- Windmill %>%
  select(velocity, output)
Windmill
# Examples Ex 9.35 not 9.34, page 499
# velocity = 
summary(lm(output ~ velocity, data = Windmill))
anova(lm(output ~ velocity, data = Windmill))
#########
Window <- read_csv("Window.csv")
Window <- Window %>%
  select(window = Window, leakage = Leakage)
Window
#Examples Ex 6.54
BSDA::SIGN.test(Window$leakage, md = 0.125, alternative = "greater")
#########
Wins <- read_csv("Wins.csv")
Wins
#Examples Ex 9.23
plot(wins ~ era, data = Wins)
## Not run
ggplot2::ggplot(data = Wins, aes(x = era, y = wins)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()
##
#########
Wool <- read_csv("Wool.csv")
Wool <- Wool%>%
  gather(`Type 1`, `Type 2`, key = "type", value = "strength")
Wool
#Examples Ex 7.42
boxplot(strength ~ type, data = Wool, col = c("blue", "purple"))
t.test(strength ~ type, data = Wool, var.equal = TRUE)
########
Yearsunspot <- read_csv("Yearsunspot.csv")
YEARS <- seq(as.Date("1979/01/01"), by = "month", length = 252)
Yearsunspot <- Yearsunspot %>%
  select(SSN, year) %>%
  rename(number = SSN)
Yearsunspot$year <- YEARS
Yearsunspot
#Examples Ex 2.7
plot(number ~ year, data = Yearsunspot)
########