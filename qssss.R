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
race.call.tab

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
