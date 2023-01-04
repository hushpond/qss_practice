resume <- read.csv(system.file("resume.csv", package = "qss"))
library("devtools")
remotes::install_github("kosukeimai/qss-package", force = TRUE, auth_token = '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')
force = TRUE
library("foreign")
dir(system.file("extdata", "data_files", package = "qss"))
load(system.file("extdata", "data_files", "UNpop.RData", package = "qss"))
read.csv(system.file("extdata", "data_files", "UNpop.csv", package = "qss"))
read.dta(system.file("extdata", "data_files", "UNpop.dta", package = "qss"))

data("resume", package = "qss")
dim(resume)

race.call.tab <- table(race = resume$race, call = resume$call)
race.call.tabo

addmargins(race.call.tab)

## overall callback rate: total callbacks divided by the sample size
sum(race.call.tab[, 2]) / nrow(resume)

## callback rate for black-sounding names
mean(resume$call[resume$race == "black"])

## subset blacks only
resumeB <- resume[resume$race == "black", ]
dim(resumeB)

resumeBf <- subset(resume, select = c("call", "firstname"),
                   subset = (race == "black" & sex == "female"))
head(resumeBf)

resume$BlackFemale <- ifelse(resume$race == "black" &
                               resume$sex == "female", 1, 0)
table(race = resume$race, sex = resume$sex,
      BlackFemale = resume$BlackFemale)


pop.first <- c(2525779, 3026003, 3691173)
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second)
pop.all
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop[2]
pop.rate <- world.pop / world.pop[1]
pop.rate
pop.increase <- world.pop[-1] - world.pop[-7]
percent.increase <- (pop.increase / world.pop[-7]) * 100
percent.increase

social <- read.csv(system.file("social.csv", package = "qss"))

data(package = "qss")
data(elections, package = "qss")
vignette(package = "qss")
vignette("causality", package = "qss")
#-----------------------------------------------
#인과 추론의 근본적 문제 = 인과효과를 추정하기 위해서는 
# Counterfactual 결과를 추론해야 한다. 따라서 각 개인에 대한 
# 처치효과의 타당한 추정값을 얻을 수 없다. 하지만 처치 할당을
# 무작위화함으로써 평균 처치효과를 추정할 수 있다.

# 무작위 대조시험에서 각 구성 단위는 처치 또는 통제그룹에 무작위로 할당된다.
# 이는 처치그룹과 통제그룹 간의 평균 결과 차이가 오로지 처치에서만 기인한다는 사실을 보장한다.
# 허나 RCT는 이를 통해 내적 타당성은 보장하겠으나, 외적 타당성은 보장하기 어렵다. 
# ``resume
data("resume", package = "qss")
dim(resume)
summary(resume)
race.call.tab <- table(race = resume$race, call = resume$call)
race.call.tab
addmargins(race.call.tab)

sum(race.call.tab[,2]/nrow(resume))
mean(resume$call)

mean(resume$call[resume$race == "black"])
resumeBf <- subset(resume, select = c("call", "firstname"), subset = (race == "black" & sex == "female"))
head(resumeBf)

resume$BlackFemale <- ifelse(resume$race == "black" & resume$sex == "female", 1, 0)
table(race = resume$race, sex=resume$sex, BlackFemale = resume$BlackFemale)

class(resume$type)
resume$type
resume$type <- NA
resume$type[resume$race == "black" & resume$sex == "female"] <- "BlackFemale"
resume$type[resume$race == "black" & resume$sex == "male"] <- "BlackMale"
resume$type[resume$race == "white" & resume$sex == "female"] <- "WhiteFemale"
resume$type[resume$race == "white" & resume$sex == "male"] <- "WhiteMale"

resume$type <- as.factor(resume$type)
levels(resume$type)
tapply(resume$call, resume$type, mean)

resume$firstname <- as.factor(resume$firstname)
callback.name <- tapply(resume$call, resume$firstname, mean)
sort(callback.name)

social <- read.csv("social.csv")
social <- read.csv(system.file("social.csv", package = "qss"))
data("social", package = "qss")
social <- read.csv("social.csv")
summary(social)

tapply(social$primary2006, social$primary2004, mean)
tapply(social$primary2006, social$messages, mean)
mean(social$primary2006[social$messages == "Control"])
tapply(social$primary2006, social$messages, mean) - mean(social$primary2006[social$messages == "Control"])

social$age <- 2006 - social$yearofbirth
tapply(social$age, social$messages, mean)
tapply(social$primary2004, social$messages, mean)

#----------------------------
data("minwage", package = "qss")
dim(minwage)
summary(minwage)
head(minwage)

minwageNJ <- subset(minwage, subset = (location != "PA"))
minwagePA <- subset(minwage, subset = (location == "PA"))

mean(minwageNJ$wageBefore < 5.05)
mean(minwageNJ$wageAfter < 5.05)
mean(minwagePA$wageBefore < 5.05)
mean(minwagePA$wageAfter < 5.05)

prop.table(table(minwageNJ$chain))
prop.table(table(minwagePA$chain))
minwageNJ.bk.subset <- subset(minwageNJ.bk, subset = ((location != "shoreNJ")&(location!="centralNJ")))
minwageNJ.bk <- subset(minwageNJ, subset = (chain == "burgerking"))

median(minwage$fullPropafter) - median(minwagePA$fullPropAfter)

#-----------------------------------
data("afghan", package = "qss")
prop.table(table(ISAF = afghan$violent.exp.ISAF,
                 Taliban = afghan$violent.exp.taliban))
head(is.na(afghan$income), n = 10)
sum(is.na(afghan$income))
afghan.sub <- na.omit(afghan)
nrow(afghan.sub)
length(na.omit(afghan$income))

ISAF.ptable <- prop.table(table(ISAF = afghan$violent.exp.ISAF,
                                exclude = NULL))
ISAF.ptable
barplot(ISAF.ptable,
        names.arg = c("피해 없음", "피해 있음", "무응답"),
        main = "ISAF에 의한 민간인 희생",
        xlab = "응답 분류",
        ylab = "응답 비율", ylim = c(0, 0.7))
hist(afghan$age, freq=FALSE, ylim = c(0,0.04), xlab="Age", 
     main = "Distribution of respondent's age")

hist(afghan$educ.years, freq = FALSE,
     breaks = seq(from = -0.5, to = 18.5, by = 1),
     xlab = "교육 연수",
     main = "응답자 교육 수준 분포")
text(x = 3, y = 0.5, "median")
abline(v = median(afghan$educ.years))
lines(x = rep(media))

tapply(afghan$violent.exp.taliban, afghan$province, mean, na.rm = TRUE)
tapply(afghan$violent.exp.ISAF, afghan$province, mean, na.rm = TRUE)

pdf(file = "educ.pdf", height = 5, width = 5)
boxplot(educ.years ~ province, data = afghan,
        main = "주별 교육 수준", ylab = "교육 연수")
dev.off()

data("afghan.village", package = "qss")
boxplot(altitude~village.surveyed, data = afghan.village,
        ylab = "Altitude (meters)", names = c("Nonsampled", "Sampled"))

data("congress", package = "qss")
rep <- subset(congress, subset = (party == "Republican"))
dem <- subset[congress$party == "Democrat", ]
dem <- congress[congress$party == "Democrat", ]

rep80 <- subset(rep, subset = (congress == 80))

data("congress", package = "qss")

## subset the data by party
rep <- subset(congress, subset = (party == "Republican"))
dem <- congress[congress$party == "Democrat", ] # another way to subset

## 80th and 112th congress
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

lim <- c(-1.5, 1.5)
xlab <- "Economic liberalism/conservatism"
ylab <- "Racial liberalism/conservatism"

par(cex = 1.5)
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "80th Congress") # democrats
points(rep80$dwnom1, rep80$dwnom2, pch = 17, col = "red") # republicans
text(-0.75, 1, "Democrats")
text(1, -1, "Republicans")

## party median for each congress
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

par(cex = 1.5)
## Democrats
plot(names(dem.median), dem.median, col = "blue", type = "l",
     xlim = c(80, 115), ylim = c(-1, 1), xlab = "Congress",
     ylab = "DW-NOMINATE score (1st dimension)")
## add Republicans
lines(names(rep.median), rep.median, col = "red")
text(110, -0.6, "Democratic\n Party")
text(110, 0.85, "Republican\n Party")

# ---------------------------
data("gini", package = "qss")
gini <- read.csv("USGini.csv")

library(autoleg)
install.packages("autoleg")


autoReg_0.1.0.tar.gz	

packageurl <- "http://cran.r-project.org/src/contrib/Archive/autoReg/autoReg_0.1.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

library(autoReg)
library(survival)
library(survminer)
install.packages("ftExtra")
library(flextable)
library(tidyverse)
install.packages("howto")
library(howto)
library(magrittr)
library(dplyr)
data=subset(anderson, rx==0)
library(autoReg)
data=subset(anderson, rx==0)

anderson %>%  
  group_by(rx) %>% 
  dplyr::summarize(T=mean(time), h=sum(status)/sum(time))
install.packages("interpretCI")

data("anderson", package = "autoReg")
?autoReg
??autoReg
anderson

remove.packages("autoReg")
install.packages("autoReg")
anderson

#------------------------
values <- c(2,4,6)
n <- length(values)
results <- rep(NA,n)
for(i in 1:n){
  results[i] <- values[i]*2
  cat(values[i], "times 2 is equal to", results[i], "\n")
}
results

i<-1
x<-values[i]*2
cat(values[i], "times 2 is equal to", x, "\n")

data <- data.frame("a"=1:2, "b"=c("hi", "hey"), "c"=3:4)

results <- rep(NA,3)
for(i in 1:3){
  cat("iteration", i, "\n")
  results[i] <- median(data[,i])
}
results

operation <- "add"
if(operation=="add"){
  cat("I will perform addition 4+4\n")
  4+4
}
if(operation == "multiply"){
  cat("I will perform multiplication 4*4\n")
  4*4
}

operation <- "multiply"
operation <- "multiply"
if (operation == "add") {
  cat("I will perform addition 4 + 4")
  4 + 4
} else {
  cat("I will perform multiplication 4 * 4")
  4 * 4
}

data("pres08", package = "qss")
data("polls08", package = "qss")

y <- as.Date("2008/9/1")
x - y
polls08$middate <- as.Date(polls08$middate)
polls08$DaysToElection <- as.Date("2008-11-04") - polls08$middate
poll.pred <- rep(NA,51)
st.names <- unique(polls08$state)
names(poll.pred) <- as.character(st.names)
## convert to a Date object
polls08$middate <- as.Date(polls08$middate)

## computer the number of days to the election day
polls08$DaysToElection <- as.Date("2008-11-04") - polls08$middate
poll.pred <- rep(NA, 51) # initialize a vector place holder

## extract unique state names which the loop will iterate through
st.names <- unique(polls08$state)

## add state names as labels for easy interpretation later on
names(poll.pred) <- as.character(st.names)

## loop across 50 states plus DC
for (i in 1:51){
  ## subset the ith state
  state.data <- subset(polls08, subset = (state == st.names[i]))
  ## further subset the latest polls within the state
  latest <- subset(state.data, DaysToElection == min(DaysToElection))
  ## compute the mean of latest polls and store it
  poll.pred[i] <- mean(latest$margin)
}
errors <- pres08$margin - poll.pred
names(errors) <- st.names # add state names
mean(errors) # mean prediction error


par(cex = 1.5)
## histogram
hist(errors, freq = FALSE, ylim = c(0, 0.08),
     main = "Poll prediction error",
     xlab = "Error in predicted margin for Obama (percentage points)")
## add mean
abline(v = mean(errors), lty = "dashed", col = "red")
text(x = -7, y = 0.07, "average error", col = "red")


par(cex = 1.5)
## type = "n" generates "empty" plot
plot(poll.pred, pres08$margin, type = "n", main = "", xlab = "Poll results",
     xlim = c(-40, 90), ylim = c(-40, 90), ylab = "Actual election results")
## add state abbreviations
text(x = poll.pred, y = pres08$margin, labels = pres08$state, col = "blue")
## lines
abline(a = 0, b = 1, lty = "dashed") # 45 degree line
abline(v = 0)  # vertical line at 0
abline(h = 0)  # horizontal line at 0

data("face", package = "qss")
face$d.share <- face$d.votes / (face$d.votes + face$r.votes)
face$r.share <- face$r.votes / (face$d.votes + face$r.votes)
face$diff.share <- face$d.share - face$r.share

par(cex = 1.5)
plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red", "blue"),
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")

cor(face$d.comp, face$diff.share)

fit <- lm(diff.share ~ d.comp, data=face)
coef(fit)
head(fitted(fit))
plot(face$d.comp, face$diff.share, xlim = c(0,1.05), ylim = c(-1,1),
     xlab = "민주당 능지점수",
     ylab = "민주당 득표율차이",
     main = "외모 능력 점수와 득표율")
abline(fit)
abline(v=0, lty = "dashed")
epsilon.hat <- resid(fit)
sqrt(mean(epsilon.hat^2))

pres<-merge(pres08, pres12, by.x = "state", by.y = "state.abb")
pres1 <- cbind(pres08, pres12)
summary(pres1)

pres$Obama2008.z <- scale(pres$Obama.x)
pres$Obama2012.z <- scale(pres$Obama.y)

fit1 <- lm(Obama2012.z ~ Obama2008.z, data = pres)
fit1

plot(pres$Obama2008.z, pres$Obama2012.z, xlim = c(-4,4), ylim=c(-4,4), xlab = "2008년 Obama 표준화 득표율",
     ylab = "2012년 오바마 표준화 득표율")
abline(fit1)
par("mar")

par(mar=c(3,3,3,3))

# ---------------------------------------

data("social", package = "qss")
levels(social$messages)

fit <- lm(primary2008 ~ messages, data=social)

data("MPs", package = "qss")
MPs.labour <- subset(MPs, subset = (party =="labour"))
MPs.tory <- subset(MPs, subset = (party == "tory"))

labour.fit1 <- lm(ln.net ~ margin, 
                  data = MPs.labour[MPs.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin,
                  data = MPs.labour[MPs.labour$margin > 0, ])


tory.fit1 <- lm(ln.net ~ margin, 
                data = MPs.tory[MPs.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin,
                data = MPs.tory[MPs.tory$margin > 0, ])

## Labour: range of predictions
y1l.range <- c(min(MPs.labour$margin), 0) # min to 0
y2l.range <- c(0, max(MPs.labour$margin)) # 0 to max

## prediction
y1.labour <- predict(labour.fit1, newdata = data.frame(margin = y1l.range))
y2.labour <- predict(labour.fit2, newdata = data.frame(margin = y2l.range))

## Tory: range of predictions
y1t.range <- c(min(MPs.tory$margin), 0) # min to 0
y2t.range <- c(0, max(MPs.tory$margin)) # 0 to max

## predict outcome
y1.tory <- predict(tory.fit1, newdata = data.frame(margin = y1t.range))
y2.tory <- predict(tory.fit2, newdata = data.frame(margin = y2t.range))

plot(MPs.labour$margin, MPs.labour$ln.net, main = "Labour",
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")
lines(y1t.range, y1.tory, col = "blue")
lines(y2t.range, y2.tory, col = "blue")

tory.MP <- exp(y2.tory[1])
tory.MP