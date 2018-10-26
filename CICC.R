library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)


cidata <- read.csv("Data/carInsurance_train.csv", header=T)

summary(cidata)
str(cidata)
ggplot(data = cidata) + geom_density(aes(x= cidata$Age), fill = "grey50")


summary(cidata$Job)
summary(cidata$CallStart)
summary(cidata$CallEnd)

cidata$DaysPassed

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

dif <- endtime - starttime
dif <- (dif$endhour * 3600) + (dif$endminute * 60) + (dif$endsecond)
summary(dif)

cidata <- cbind(cidata, starttime, endtime)
cidata$dif <- dif
cidata <- cidata[,-c(17,18)]

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
sum(is.na(cidata$Job)) ## 19개는 그냥 버림 귀찮음
cidata <- cidata[!c(is.na(cidata$Job)),] ## 4000 -> 3981

summary(cidata$Marital)

sum(is.na(cidata$Education))

cidata$Education_ <- addNA(cidata$Education)
levels(cidata$Education_) <- c(levels(cidata$Education), 'idk')

cidata$Educationf
cidata$Education <- NULL

summary(cidata$DaysPassed)

ggplot(cidata, aes(y=cidata$DaysPassed, x=1)) + geom_violin()
ggplot(data = cidata) + geom_density(aes(x= cidata$DaysPassed), fill = "grey50")

cidata$DaysPassed[c(cidata$DaysPassed > 365)] <- 365
cidata$DaysPassed[c(cidata$DaysPassed == -1)] <- 365

summary(cidata$DaysPassed)
ggplot(data = cidata) + geom_density(aes(x= cidata$DaysPassed), fill = "grey50")

summary(cidata$Default)

cidata$Default <- as.factor(cidata$Default)
cidata$CarLoan <- as.factor(cidata$CarLoan)
cidata$CarInsurance <- as.factor(cidata$CarInsurance)
cidata$HHInsurance <- as.factor(cidata$HHInsurance)

cidata$LastContactDay <- NULL
cidata$LastContactMonth <- NULL

cidata$Communication_ <- addNA(cidata$Communication)
levels(cidata$Communication_) <- c(levels(cidata$Communication), 'Missing')

cidata$Communication <- NULL

cidata$startminute <- NULL
cidata$startsecond <- NULL
cidata$endminute <- NULL
cidata$endsecond <- NULL

cidata$starthour <- as.factor(cidata$starthour)
cidata$endhour <- as.factor(cidata$endhour)

cidata$Outcome_ <- addNA(cidata$Outcome)
levels(cidata$Outcome_) <- c(levels(cidata$Outcome), 'Missing')

cidata$Outcome <- NULL

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

summary(cidata)


