
################  IMPORT LIBRARIES ################ 
rm(list=ls(all=TRUE))
# install.packages(c('foreign,"pastecs","moments","nortest","sm","vioplot","car","corrplot","sjPlot","ggpubr","caret","olsrr","tidyr","reshape2","ggplot2","faraway","factoextra"))
library(readxl)
library(pastecs)
library(moments)
library(nortest)
library(sm)
library(vioplot) 
library(EnvStats)
library(car)
library(corrplot)
library(sjPlot)
library(ggpubr)
library(caret)
library(olsrr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(lmtest)
library(faraway)
library(factoextra)
library(VIM) # visual inspection of missing data
library(mice) # for missing data
library(dplyr) # for summary by group
library(ggpubr) # for ggboxplot
library(gmodels)# for contingency tables


################ IMPORT THE DATA ################ 
# A study conducted to explore various characteristics of a cohort of students
# 50 respondents, male and female, of various ages
library(foreign)
setwd("~/Postgraduate Diploma in Data Analytics/Statistics for Data Analytics/Individual project 2/Fundamentals of Statistics")
data <- read.spss("CollegeStudentData.sav",to.data.frame=TRUE)
attach(data)

################ DESCRIPTIVE STATS ###############
names(data)
# check final structure of data
str(data) 
# 50 obs. of  18 variables: 6 numeric and 12 categorical variables
summary(data)
# categorical variables with 2 groups:  gender, children, tvsitcom, tvmovies, tvsports, tvnews
# categorical variables with more than 2 groups:  age and marital 
# list rows of data that have missing values
data[!complete.cases(data),]
# 3 entries are not available in the first three rows of the columns:
# "marital status" (marital), "positive evaluation, major" (evalprog) and "weekly working hours" (hrswork)
# Due to the categorical nature of marital status and evaluation, missing value cannot be replaced by an average or 
# the most frequent entry as it will be biased. In order to assess the possibility of omitting missing entries, the 
# percentage of missing values is calculated. For small datasets, a safe maximum threshold is often 2.5% of the total. 
# For this reason, we will omit these three lines even if we won't apply t-test and chi-square test using these variables.

# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(data,2,pMiss)

aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=1,
                  gap=3, ylab=c("Histogram of missing data","Pattern"))
# The plot indicates that 94% of the observations/samples do not have any missing information, 2% are missing 
# the marital status, positive evaluation and weekly working hours.

# Neither imputation not deletion was decided as our final choice based on 2 facts.
# The pairwise or listwise deletion of observations suffer from loss of information whereas it would
# be a biased decision to replace them with either averages or most frequent values. For this reason,
# a predictive model would be the best option if analysis of the whole dataset was our project goal.
# In order to avoid losing the number of observations, both of tests, one sample t-test and 
# one chi-squared test will be applied only on the remaining variables.

################ ANALYSIS OF REMAINING VARIABLES ###############
# create new dataset without 3 variables with missing data
newdata <- data[,c(1:3,5:14,16:17)]

# split the data into numeric and categorical tables for further analysis
data.num <- newdata[, sapply(newdata, class) == "numeric"]
data.fac <- newdata[, sapply(newdata, class) == "factor"]

################  NUMERIC VARIABLES ################ 
options(scipen=100)
options(digits=2)
stat.desc(data.num, basic=F) # nbr.val=number of observations = 50 students/observations
stat.desc(data.num, desc=F)  # nbr.na=number of missing values # no missing values

# NORMALITY #
# For applying t-tests, there are no assumptions for Independent variables but it's useful to understand their distribution
# in order to find influential points or outliers or concentrated values. For this reason, we will apply tests and graphs for Normality as well as 
# Violplots for assessing the values concentration. In this way, we could manage symmetrical distributions by suggesting possible transformations for highly skewed variables.

# Create QQ-plots and run the Shapiro Test for checking Normality of numeric variables:

par(mfrow=c(2,3))
qqnorm(data.num$height,main="Student's height")
qqline(data.num$height,col=2)  
qqnorm(data.num$pheight,main="Parent's height")
qqline(data.num$pheight,col=2)
qqnorm(data.num$hrstv,main="Weekly tv-watching hours")
qqline(data.num$hrstv,col=2)
qqnorm(data.num$hrsstudy,main="Weekly study hours")
qqline(data.num$hrsstudy,col=2)
qqnorm(data.num$currgpa,main="Student's current GPA")
qqline(data.num$currgpa,col=2)
title("Normal Q-Q Plots", outer = TRUE)


# "P-Values of Shapiro Test for Normality" 
y <- data.num
for (i in 1:length(y)){
  print(c(names(y)[i],shapiro.test(y[,i])$p.value))
}
# "height"             "0.0560297773661378"
# "pheight"            "0.00383235569624087"
# "hrstv"               "0.00155559135932099"
# "hrsstudy"            "0.00253114386489483"
# "currgpa"             "0.0997606609090022"

# If p-value<0.05 then reject the Null hypothesis which means that our data is not normal.
# Fortunately, Normality holds for 2 variables, Student height and Current GPA. 

p<-ncol(y)
par(mfrow=c(2,3))
for (i in 1:p){
  hist(y[,i], main=names(y)[i], probability=TRUE)
  lines(density(y[,i]), col=2)
  index <- seq( min(y[,i]), max(y[,i]), length.out=100)
  ynorm <- dnorm( index, mean=mean(y[,i]), sd(y[,i]) )
  lines( index, ynorm, col=3, lty=3, lwd=3 )
}

# Compare Raw with Log-transformed
# par(mfrow=c(2,5))
# for (i in 1:(p)){
#   qqnorm(data.num[,i], main=names(data.num)[i] )
#   qqline(data.num[,i])
#   qqnorm(log(data.num[,i]), main=paste('Log of', names(data.num)[i]  ) ) 
#   qqline(log(data.num[,i]))
# }

# Coefficient of Variation for the 5 numeric variables CV=s/x?? 
for (i in 1:length(data.num)){
  print(c(names(data.num)[i],round(sd(data.num[,i])/mean(data.num[,i]),3)))
}
# All variables' CV<1 which means that they show low variability. For our t-test we will 
# proceed with the variables Student height and Current GPA. 

# DETECTING OUTLIERS IN THE 2 CANDIDATE VARIABLES #
library(grDevices)
X1 <- data.num$height
X2 <- data.num$currgpa
which(X1 %in% boxplot.stats(X1)$out) # no outliers
X1[which(X1 %in% boxplot.stats(X1)$out)]
which(X2 %in% boxplot.stats(X2)$out) # 12th and 27th observations
X2[which(X2 %in% boxplot.stats(X2)$out)]

par(mfrow=c(1,2))
outlier_values1 <- boxplot.stats(X1)$out
boxplot(X1, main="Student Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values1, collapse=", ")), cex=0.6)
outlier_values2 <- boxplot.stats(X2)$out
boxplot(X2, main="Current GPA", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values2, collapse=", ")), cex=0.6)


################  CATEGORICAL VARIABLES ################ 
# Before decide on the independent variable for the t-test, we test which of the 2-groups
# categorical variables does not violate the homogeneity of variances using Levene's test.

summary(data.fac)
# variables with 2 independent categories are: gender, children, tvsitcom, tvmovies, tvsports and tvnews
two.fac <- data.fac[,c(1,3:7)]

################  ASSUMPTIONS FOR INDEPENDENT T-TEST ################ 
complete.data <- data[,c(1,3,8:11)]

# 1. Dependent variable "student height" is measured at interval level using a continuous scale.
# 2. Categorical variable with 2 independent groups
# 3. No outliers for independent variable
# 4. The dependent variable should have equal population variances for the 2 groups.
# The null hypothesis of equal variances will be rejected if p-value < 0.05.
# In our case, p-values for all independent variables are greater than a = 0.05, which means that
# we fail to reject the null hypothesis as there is not enough evidence to suggest that the null 
# hypothesis is false at the 95% confidence level.

# "P-Values of Levene's Test for Homogeneity of variances" 
for (i in 2:length(complete.data)){
  print(names(complete.data)[i])
  group <- as.factor(complete.data[,i])
  print(leveneTest(height~group,complete.data))
}
# keep all two-groups variables.

# 5. Approximately normal distribution of "student height" in each group and
# between the groups of independent variable

# separate Shapiro-Wilk tests for all groups of observations
for (i in 2:length(complete.data)){
  print(names(complete.data)[i])
  group <- as.factor(complete.data[,i])
  print(do.call("rbind", with(complete.data, tapply(height,group,
                                              function(x) unlist(shapiro.test(x)[c("statistic", "p.value")])))))
}
# The null hypothesis of normality will be rejected if p-value < 0.05.
# In our case, p-values with all independent variables are greater than a = 0.05, which means that
# we fail to reject the null hypothesis as there is not enough evidence to suggest that the null 
# hypothesis is false at the 95% confidence level. The normality of height holds for all the 
# levels of the factor variables separately.

# Another approach is the normality of residuals of the linear regression height~group variables. 
# Shapiro-Wilk tests the null hypothesis that the residuals in all groups come from 
# the same normal distribution.

for (i in 2:length(complete.data)){
  ## linear model 
  group <- as.factor(complete.data[,i])
  fm <- lm(height ~ group, data = complete.data)
  ## Shapiro for residuals
  print(shapiro.test(residuals(fm)))
}
# All p-values are greater than alpha=0.05 which confirms what we observed from the
# previous Shapiro-Wilk tests.

# 6. Independence of within-group and between-groups observations.
# For the confirmation of the last assumption, let's think the way this study was designed.
# This cohort of students consists of 50 respondents, male and female, of various ages.
# This means that the observations come from 50 persons who were systematically assigned
# to the groups, set at the design stage of the study.

################  INDEPENDENT T-TEST ################ 

# Our question which will be replied by the independent t-test is:
# "Does the average height differ between males and females?"
# "Is there any significant difference between males and females heights?"
my_data1 <- complete.data[,c(1,2)]

# Before answering our question, let's describe our data:
# Summary statistics by gender:
group_by(my_data1, gender) %>%
summarise(
count = n(),
mean = mean(height, na.rm = TRUE),
sd = sd(height, na.rm = TRUE),
se = sd/sqrt(count)
)
# The volumes of groups are almost the same with 2 more males. 

# Visualize groups' heights using box plots
# Plot height by group and color by group
ggboxplot(my_data1, x = "gender", y = "height", 
          fill = "gender", palette = "Set1",
          ylab = "Height", xlab = "Groups")

# From the boxplot, the difference of the height between the 2 genders is clear.

library(knitr)
#Make Table
library(dplyr)
# Summarize data by groups
d <- data.frame(my_data1,gender)
md <- d %>% # "Start with the data set we imported, d 
  group_by(gender) %>% # Then group d by IV
  summarize(N = length(height), # Then summarize each group
            Mean = mean(height),
            SD = sd(height),
            SE = SD/sqrt(N)) 

td <- d %>% summarize(IV = "Total",
                      N = length(height), 
                      Mean = mean(height),
                      SD = sd(height),
                      SE = SD/sqrt(N))

dd <- rbind(md,td)
kable(td,
      digits = 2,
      caption = "Table 1: Descriptive Statistics for DV",
      align = "c")


# Let's check the assumptions before the Independent t-test:
# 1. The two samples are independent as the samples from males and females are not related.
# For the independence in each group, let's think the way this study was designed.
# This cohort of students consists of 50 respondents, male and female, of various ages.
# This means that the observations come from 50 persons who were systematically assigned
# to the groups, set at the design stage of the study.

# 2. Normality of dependent variable in each gender
# Shapiro-Wilk normality test for Men's heights
with(my_data1, shapiro.test(height[gender == "males"]))# p = 0.1 (0.11)
# Shapiro-Wilk normality test for Women's heights
with(my_data1, shapiro.test(height[gender == "females"])) # p = 0.3 (0.29)

# The two p-values (males: 0.1, females: 0.3) are greater than the significance level a=0.05 
# implying that the null hypothesis of normality cannot be rejected and as a result
# the distribution of the data are not significantly different from the normal distribution. 
# In other words, we can assume the normality.

# 3. Homogeneity of variances: same variances for the two populations?
# Performing F-test using the function var.test().
res.ftest <- var.test(height ~ gender, data = my_data1)
res.ftest$p.value
# From the output, the p-value of F-test is 0.15 which is greater than the significance level alpha = 0.05.
# That means that there is no significant difference between the variances of the two sets
# of data. In conclusion, we can use the independent t-test which assumes equality variances.

# Compute t-test
res <- t.test(height ~ gender, data = my_data1, var.equal = TRUE)
res
# RESULTS:
# t is the t-test statistic value (t = 9)
# df is the degrees of freedom (df= 50)
# p-value is the significance level of the t-test (p-value = 0.00000000002).
# conf.int is the confidence interval of the mean at 95% (conf.int = [4.7, 7.5])
# sample estimates is the mean value of each sample (mean = 70, 64)

# 2 ALTERNATIVES
# Test whether the average women’s height is less than the average men’s height:
# t.test(height ~ gender, data = my_data1,var.equal = TRUE, alternative = "less")
# Test whether the average women’s height is greater than the average men’s weight:
# t.test(height ~ gender, data = my_data1,var.equal = TRUE, alternative = "greater")

# The p-value of t-test is <<0.001, which is less than the significance level alpha = 0.05. 
# In conclusion, males has significantly different average height from females.

################  CHI-SQUARE TEST FOR INDEPENDENCE ################ 

"Is there any relationship between gender and tvmovies variables?"
"Is there any difference between the two genders and event of watching or not movies on TV? "
my_data2 <- complete.data[,c(2,4)]

# Let's check the assumptions before the Chi-square test for Independence:
# 1. Both of these two variables are nominal, and specifically dichotomous, as they consist of
# two categories with no intrinsic order or rank.
# 2. Both variables consists of two independent categories/levels.

# tab1<-table(my_data2$gender, my_data2$tvmovies )
# tab1
# prop.table(tab1) 
# prop.table(tab1,1)
# prop.table(tab1,2)
# round(100*prop.table(tab1,1),1)
# chisq.test( tab1  )

# SUMMARY OF CONTINGENCY TABLE
CrossTable(my_data2$gender, my_data2$tvmovies, digits=1, format='SPSS',chisq = T)
library(sjPlot)
sjt.xtab( my_data2$tvmovies, my_data2$gender, digits = 1,
         show.cell.prc = TRUE, show.row.prc = TRUE, show.col.prc = TRUE, 
         show.exp = FALSE,
         var.labels = c('Watching Television shows-movies','Gender'),
         encoding = "Windows-1253",use.viewer = TRUE,
         CSS = list(css.table = "border: 2px solid;",
                    css.tdata = "border: 1px solid;",
                    css.horline = "border-bottom: double blue;"),
         show.legend = TRUE)

chisq.test(my_data2$tvmovies,my_data2$gender,correct = TRUE)
# In the crosstab, it is observed that women are more likely to be in favour 
# of watching television movies than men. Chi-square is 27.3, degrees of freedom equal to 1 and p-value is << 0.001,
# which is much less than the significance level a = 0.05. As a conclusion, gender and tvmovies 
# are dependent at alpha = 0.05.


