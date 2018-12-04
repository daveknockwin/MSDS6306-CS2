---
title: "Finding a New Home for the Bill and Melinda Foundation"
author: "David Nguyen, Austin Simeone, John Rodgers, Hannah Kosinovsky"
date: "December 4, 2018"
output: 
  html_document: 
    keep_md: yes
---




```r
library(reshape2)
library(ggplot2)
library(pander)
library(rcompanion)
library(GGally)
```

```
## 
## Attaching package: 'GGally'
```

```
## The following object is masked from 'package:pander':
## 
##     wrap
```

```r
library(corrplot)
```

```
## corrplot 0.84 loaded
```
####Survival Rate and Literacy:
We first examined the relationship between literacy in males and the the survial rate of male. After cleaning the data, applying a regression, we found a linear relationship. The subsequent chunks show the process

```r
# load data from file in the data directory
data <- read.csv("./Data/API_17_DS2_en_csv_v2_10226244.csv", skip=4)
# generate rows for values for each year
data2 <- reshape(data, varying = 5:62, sep="", direction='long')
# rename columns
colnames(data2) <- c("Country","Code","Indicator","IndicatorCode","Value","Year","Id")
# isolate survival rate data into dataframe
survial.percent.male <- data2[data2$Indicator == "Survival to age 65, male (% of cohort)",]
# isolate literacy rate data into dataframe
literacy.youth.male <- data2[data2$Indicator == "Literacy rate, youth male (% of males ages 15-24)",]
# merge survival rate and literacy rate dataframes
survival.v.litrate.raw <- merge(survial.percent.male, literacy.youth.male, by=c("Code", "Year"))
# isolate specific variables from raw merged data
survival.v.literate <- survival.v.litrate.raw[c(3,1,2,6,11)]
# rename columns
colnames(survival.v.literate) <- c("Country","Code","Year","survival.percent","literate.rate")
# exclude na values
survival.v.literate <- na.omit(survival.v.literate)
# generate log of survival percent
survival.v.literate$log.survival.percent <- log(survival.v.literate$survival.percent)
# generate log of literate rate
survival.v.literate$log.literate.rate <- log(survival.v.literate$literate.rate)
```

The variables used for linear regression at literacy rate in youth males (age 15-24) and the survival rate of males to age 65. A summary of those variables and their distribution among the sample are below.

```r
pander(summary(literacy.youth.male$Value))
```


-----------------------------------------------------------
 Min.    1st Qu.   Median   Mean    3rd Qu.   Max.   NA's  
------- --------- -------- ------- --------- ------ -------
 22.38    78.13    89.94    86.92    97.85    100    13099 
-----------------------------------------------------------

```r
plotNormalHistogram(literacy.youth.male$Value)
```

![](CS2_files/figure-html/variable-summaries-1.png)<!-- -->

```r
pander(summary(survial.percent.male$Value))
```


----------------------------------------------------------
 Min.    1st Qu.   Median   Mean    3rd Qu.   Max.   NA's 
------- --------- -------- ------- --------- ------ ------
 1.477    47.55    60.86    58.88    70.61    90.4   1632 
----------------------------------------------------------

```r
plotNormalHistogram(survial.percent.male$Value)
```

![](CS2_files/figure-html/variable-summaries-2.png)<!-- -->

A plot of the log of the survival rate and the log of the literacy rate shows that the two variables have a linear correlation.

```r
# plot surivival rate versus literate rate
ggplot(survival.v.literate, aes(x=log.literate.rate, y=log.survival.percent)) + geom_point(shape=1) + geom_smooth(method=lm)
```

![](CS2_files/figure-html/plot-1.png)<!-- -->

All of the assumptions associated with linear regression were met below.

```r
# generate linear model of survival rate vs literate rate
s.v.l.lm <- lm(log.survival.percent ~ log.literate.rate, data=survival.v.literate)
# generate residuals
s.v.l.m.res <- resid(s.v.l.lm)
# plot residuals
plot(survival.v.literate$log.literate.rate, s.v.l.m.res, ylab="Residuals",xlab="log.literate.rate", main="Residuals vs log of literate rate")
```

![](CS2_files/figure-html/residuals-1.png)<!-- -->


```r
s.v.l.m.stud <- rstudent(s.v.l.lm)
hist(s.v.l.m.stud, freq=FALSE, main="Distribution of Studentized Residuals", xlab="Studentized Residuals")
xfit <- seq(min(s.v.l.m.stud)-1,max(s.v.l.m.stud)+1,length=40)
yfit <- dnorm(xfit)
lines(xfit,yfit)
```

![](CS2_files/figure-html/studentresiduals-1.png)<!-- -->

```r
par(mfrow=c(1,2))
qqnorm(s.v.l.m.stud)
qqline(s.v.l.m.stud)
```

![](CS2_files/figure-html/qqplot-1.png)<!-- -->

```r
pander(summary(s.v.l.lm))
```


--------------------------------------------------------------------
        &nbsp;           Estimate   Std. Error   t value   Pr(>|t|) 
----------------------- ---------- ------------ --------- ----------
    **(Intercept)**       0.1257     0.08567      1.467     0.1424  

 **log.literate.rate**    0.8935     0.01923      46.47       0     
--------------------------------------------------------------------


-------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$   Adjusted $R^2$ 
-------------- --------------------- ------- ----------------
     2205             0.1509          0.495       0.4947     
-------------------------------------------------------------

Table: Fitting linear model: log.survival.percent ~ log.literate.rate
$\hat{\mu}(log(Survival Percent)|log(Literate Rate)) = 0.126 + 0.894 * log(Literate Rate)$

The doubling of the literate rate if male youth results in a change of 85% increase in the rate of males surviving to 65. 
<br>
About 49.5% of the variation in the log of Survival Rate is explained by the log of the Literate Rate.
<br>
Next we applied a multiple regressin on female life expectany as a function of 3 other varibales. We found that 75% of the variability in life expectancy in females can be attributed to these variables.

```r
mothers.postion.maternity <- data2[data2$IndicatorCode == 'SG.MMR.LEVE.EP',]

female.life.expectancy<- data2[data2$IndicatorCode == "SP.DYN.LE00.FE.IN",]

male.life.expectancy<- data2[data2$IndicatorCode =="SP.DYN.LE00.MA.IN",]

female.participation <- data2[data2$IndicatorCode =="IC.FRM.FEMO.ZS",]

female.manager <- data2[data2$IndicatorCode =="IC.FRM.FEMM.ZS",]

data3 <- Reduce(function(x,y) merge(x = x, y = y, by = c("Code", "Year")), 
       list(mothers.postion.maternity, 
            female.life.expectancy,
            male.life.expectancy,
            female.participation,
            female.manager))
```

```
## Warning in merge.data.frame(x = x, y = y, by = c("Code", "Year")): column
## names 'Country.x', 'Indicator.x', 'IndicatorCode.x', 'Value.x', 'Id.x',
## 'Country.y', 'Indicator.y', 'IndicatorCode.y', 'Value.y', 'Id.y' are
## duplicated in the result

## Warning in merge.data.frame(x = x, y = y, by = c("Code", "Year")): column
## names 'Country.x', 'Indicator.x', 'IndicatorCode.x', 'Value.x', 'Id.x',
## 'Country.y', 'Indicator.y', 'IndicatorCode.y', 'Value.y', 'Id.y' are
## duplicated in the result
```

```r
data3 <- data3[complete.cases(data3),]

names(data3)[c(6,11,16,21,26)] <- c('a','b','c','d','e')

summary(data3[c(6,11,16,21,26)])
```

```
##        a              b               c               d        
##  Min.   :0.00   Min.   :50.59   Min.   :48.25   Min.   : 4.50  
##  1st Qu.:0.00   1st Qu.:68.68   1st Qu.:63.30   1st Qu.:25.40  
##  Median :1.00   Median :74.96   Median :68.53   Median :33.10  
##  Mean   :0.63   Mean   :72.74   Mean   :67.20   Mean   :34.12  
##  3rd Qu.:1.00   3rd Qu.:78.37   3rd Qu.:71.62   3rd Qu.:43.52  
##  Max.   :1.00   Max.   :83.90   Max.   :80.30   Max.   :69.40  
##        e        
##  Min.   : 0.30  
##  1st Qu.:11.68  
##  Median :18.75  
##  Mean   :17.97  
##  3rd Qu.:24.70  
##  Max.   :36.40
```

```r
lapply(data3[c(6,11,16,21,26)], plotNormalHistogram)
```

![](CS2_files/figure-html/summary_of_life_expectancy-1.png)<!-- -->![](CS2_files/figure-html/summary_of_life_expectancy-2.png)<!-- -->![](CS2_files/figure-html/summary_of_life_expectancy-3.png)<!-- -->![](CS2_files/figure-html/summary_of_life_expectancy-4.png)<!-- -->![](CS2_files/figure-html/summary_of_life_expectancy-5.png)<!-- -->

```
## $a
## NULL
## 
## $b
## NULL
## 
## $c
## NULL
## 
## $d
## NULL
## 
## $e
## NULL
```

```r
#MULTICOLINEARITY
explanatory_variables <- data3[c(6,11,16,21,26)]

#visually 
ggpairs(explanatory_variables)
```

![](CS2_files/figure-html/summary_of_life_expectancy-6.png)<!-- -->

```r
#complete data only
#correlation of each feature and another
m <- cor(explanatory_variables)

#plots correlation matrix
corrplot(m, method = c('number'))
```

![](CS2_files/figure-html/summary_of_life_expectancy-7.png)<!-- -->

```r
fit <- lm(b~a+d+e, data = data3)
```






