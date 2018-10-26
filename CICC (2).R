library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

#### Read data

cidata <- read.csv("carInsurance_train.csv", header=T)

#### EDA

summary(cidata)
str(cidata)
ggplot(data = cidata) + geom_density(aes(x= cidata$Age), fill = "grey50")

summary(cidata$Job)
summary(cidata$CallStart)
summary(cidata$CallEnd)

cidata$DaysPassed

## string으로 된 시간데이터 정수형으로 바꾸기
starttime <- str_split(string = cidata$CallStart, pattern=":")
starttime <- data.frame(Reduce(rbind, starttime)) 
starttime$X1 <- as.numeric(as.character(starttime$X1))
starttime$X2 <- as.numeric(as.character(starttime$X2))
starttime$X3 <- as.numeric(as.character(starttime$X3))
startfactor <- c("starthour", "startminute", "startsecond")
names(starttime) <- startfactor
row.names(starttime) <- NULL
starttime


endtime <- str_split(string = cidata$CallEnd, pattern=":")
endtime <- data.frame(Reduce(rbind, endtime)) 
endtime$X1 <- as.numeric(as.character(endtime$X1))
endtime$X2 <- as.numeric(as.character(endtime$X2))
endtime$X3 <- as.numeric(as.character(endtime$X3))
endfactor <- c("endhour", "endminute", "endsecond")
names(endtime) <- endfactor
row.names(endtime) <- NULL
endtime


## Start와 End를 이용, 통화시간 구하기
dif <- endtime - starttime
dif <- (dif$endhour * 3600) + (dif$endminute * 60) + (dif$endsecond)
dif
summary(dif)

cidata <- cbind(cidata, starttime, endtime)
cidata$dif <- dif
cidata <- cidata[,-c(17,18)]


## 나이 데이터 범주화, 독일의 법적기준에 따라 분류함
summary(cidata$Age)
ggplot(cidata, aes(y=cidata$Age, x=1)) + geom_violin()

cidata<- transform(cidata,
                   early_working_age = as.factor(ifelse(cidata$Age<=24, 1, 0)),
                   prime_working_age = as.factor(ifelse(cidata$Age>24 & cidata$Age<=54 , 1, 0)),
                   mature_working_age = as.factor(ifelse(cidata$Age>54 & cidata$Age<=64, 1, 0)),
                   elderly = as.factor(ifelse(cidata$Age>64, 1, 0)))


## https://www.indexmundi.com/germany/age_structure.html

cidata <- cidata[,-2]


## Missing Value 처리하기

sum(is.na(cidata$Job)) # 19개는 그냥 버림 귀찮음
cidata <- cidata[!c(is.na(cidata$Job)),] # 4000 -> 3981 obs

summary(cidata$Marital)


# Education Missing value
# NA -> idk
sum(is.na(cidata$Education))

cidata$Education_ <- addNA(cidata$Education)
levels(cidata$Education_) <- c(levels(cidata$Education), 'idk')

cidata$Educationf
cidata$Education <- NULL


# DaysPassed Missing value
# 1년 이상(>=365)인 경우 365로 초기화
# -1 인 신규고객 경우 역시 365
summary(cidata$DaysPassed)

ggplot(cidata, aes(y=cidata$DaysPassed, x=1)) + geom_violin()
ggplot(data = cidata) + geom_density(aes(x= cidata$DaysPassed), fill = "grey50")

cidata$DaysPassed[c(cidata$DaysPassed > 365)] <- 365
cidata$DaysPassed[c(cidata$DaysPassed == -1)] <- 365

summary(cidata$DaysPassed)
ggplot(data = cidata) + geom_density(aes(x= cidata$DaysPassed), fill = "grey50")

summary(cidata$Default)


# 이진 변수들 팩터화
cidata$Default <- as.factor(cidata$Default)
cidata$CarLoan <- as.factor(cidata$CarLoan)
cidata$CarInsurance <- as.factor(cidata$CarInsurance)
cidata$HHInsurance <- as.factor(cidata$HHInsurance)

# 이해 불가능한 두 변수 제거
cidata$LastContactDay <- NULL
cidata$LastContactMonth <- NULL

# Communication Missing value
# NA -> Missing
cidata$Communication_ <- addNA(cidata$Communication)
levels(cidata$Communication_) <- c(levels(cidata$Communication), 'Missing')

cidata$Communication <- NULL


# 의미 없는 데이터 제거
cidata$startminute <- NULL
cidata$startsecond <- NULL
cidata$endminute <- NULL
cidata$endsecond <- NULL

# 시간 팩터화
cidata$starthour <- as.factor(cidata$starthour)
cidata$endhour <- as.factor(cidata$endhour)

# Outocme Missing value
# NA -> Missing
cidata$Outcome_ <- addNA(cidata$Outcome)
levels(cidata$Outcome_) <- c(levels(cidata$Outcome), 'Missing')

cidata$Outcome <- NULL

## Balance 데이터 처리
# minusBalance 데이터 추가: Balance가 0 및 음수인 경우 1, 아닌경우 0
# Balance <= 1인경우 1로 초기화 후 Log변환하여 값을 모음

summary(cidata$Balance)

ggplot(data = cidata) + geom_density(aes(x= cidata$Balance), fill = "grey50")

cidata<- transform(cidata,
                   minusbalance = as.factor(ifelse(cidata$Balance<=1, 1, 0)))
                
cidata$logBalance <- cidata$Balance   
cidata$logBalance[c(cidata$Balance <= 1)] <- 1
summary(cidata$logBalance)

cidata$logBalance <- log10(cidata$logBalance)
ggplot(data = cidata) + geom_density(aes(x= cidata$logBalance), fill = "grey50")
cidata$Balance <- NULL

summary(cidata$Job)

sapply(cidata, class) ## class check for the dataframe at once

cidata$DaysPassed


## 전처리 끝

