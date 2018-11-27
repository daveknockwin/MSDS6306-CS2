data <- read.csv("./Data/API_17_DS2_en_csv_v2_10226244.csv", skip=4)
library(reshape2)
data2 <- reshape(data, varying = 5:62, sep="", direction='long')
colnames(data2) <- c("Country","Code","Indicator","IndicatorCode","Value","Year","Id")
survial.percent.male <- data2[data2$Indicator == "Survival to age 65, male (% of cohort)",]
literacy.youth.male <- data2[data2$Indicator == "Literacy rate, youth male (% of males ages 15-24)",]
survival.v.litrate.raw <- merge(survial.percent.male, literacy.youth.male, by=c("Code", "Year"))
survival.v.litrate <- survival.v.litrate.raw[c(3,1,2,6,11)]
colnames(survival.v.litrate) <- c("Country","Code","Year","survival.percent","literate.rate")
survival.v.literate <- na.omit(survival.v.litrate)
survival.v.literate$log.survival.percent <- log(survival.v.literate$survival.percent)
survival.v.literate$log.literate.rate <- log(survival.v.literate$literate.rate)
# survival.v.literate$sqrt.survival.percent <- sqrt(survival.v.literate$survival.percent)
# survival.v.literate$sqrt.literate.rate <- sqrt(survival.v.literate$literate.rate)
library(ggplot2)
ggplot(survival.v.literate, aes(x=log.literate.rate, y=log.survival.percent)) + geom_point(shape=1) + geom_smooth(method=lm)
