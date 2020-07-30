################  IMPORT LIBRARIES ################ 
rm(list=ls(all=TRUE))
# install.packages(c("pastecs","moments","nortest","sm","vioplot","car","corrplot","sjPlot","ggpubr","caret","olsrr","tidyr","reshape2","ggplot2","faraway","factoextra"))
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
library(tidyr) # long to wide format of data
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
library(rstatix) # for outlier detection identify_outliers


################ IMPORT THE DATA ################ 
setwd("~/Postgraduate Diploma in Data Analytics/Statistics for Data Analytics/Individual project 2/ANOVA_MANOVA")
death_data <- read_excel("DeathsbyCause.xlsx",sheet = 3,range = cell_cols(3:13),col_names = TRUE)
death_data <- as.data.frame(death_data[,c(1:4,11)])

################ DATA WRANGLING ################ 
names(death_data) <- c("Region","Country","Sex","Cause","Deaths")
death_data$Region <- as.factor(death_data$Region)
death_data$Sex <- as.factor(death_data$Sex)
death_data$Cause <- as.factor(death_data$Cause)

## Create an ID variable for Cause:
# new factor group (copy of Cause)
death_data[,"CauseID"] <- death_data[,"Cause"]
# rename the levels into "TOTAL", "LRI", "TBLC", "IHD", "STR", "COPD":
levels(death_data[,"CauseID"]) <- list("TOTAL" = "Total",
                                       "LRI" = "Lower respiratory infections",
                                       "TBLC" = "Trachea, bronchus, lung cancers",
                                       "IHD" = "Ischaemic heart disease",
                                       "STR" = "Stroke", 
                                       "COPD" = "Chronic obstructive pulmonary disease")
# control
head(death_data)

# keep deaths for "Both sexes" and discard "Total" from Cause:
newdata <- death_data[death_data$Sex=='Both sexes',]
newdata <- newdata[newdata$Cause!='Total',]
newdata <- droplevels(newdata)
levels(newdata$Sex)
levels(newdata$Cause)
levels(newdata$CauseID)
summary(newdata)

# check final structure of data
str(newdata)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
data_wide <- spread(newdata[,-4], CauseID, Deaths)
head(data_wide)

# An interesting question that arizes from our data is if the 5 diseases cause similar average 
# number of deaths independently of the country, region or sex.
# This will be replied using an one way ANOVA by comparing the five means from these independent
# diseases using the F-distribution. 
# The null hypothesis is that the five means are equal.
# However, if the p-value is less than the significance level alpha=0.05,
# this means that at least two diseases' means are different from each other.
# But it does not tell which is this pair.

# By setting a seed, create a random sample with 1 random entry by Country and 36 by Disease:
set.seed(20)
sampled_deaths <- newdata %>% group_by(newdata$Country) %>% sample_n(1) 
sampled_deaths <- sampled_deaths[,-7]
sampled_deaths <- sampled_deaths %>% group_by(sampled_deaths$Cause) %>% sample_n(36,replace = TRUE)


################ DATA EXPLORATION ################ 

# Summary Stats
group_by(sampled_deaths, CauseID) %>%
  summarise(
    count = n(),
    mean = mean(Deaths, na.rm = TRUE),
    sd = sd(Deaths, na.rm = TRUE)
  )

group_by(sampled_deaths, Cause) %>%
  summarise(
    count = n(),
    mean = mean(Deaths, na.rm = TRUE),
    sd = sd(Deaths, na.rm = TRUE)
  )

# Visualize data
# Box plots
# ++++++++++++++++++++
# Plot deaths by cause and color by cause
ggboxplot(sampled_deaths, x = "CauseID", y = "Deaths", 
          fill  = "Cause", palette = "Set1" ,
          # order = c("ctrl", "trt1", "trt2"),
          ylab = "Deaths", xlab = "CauseID")

# c("#00AFBB", "#E7B800", "#FC4E07","#A4A4A4", "#FC4E07")
################ ANOVA ASSUMPTIONS ################ 

# 1. Dependent variable is measured at ratio level using a continuous scale as zero deaths is meaningful.
# 2. Approximately normal distribution of deaths in each group/disease
do.call("rbind", with(sampled_deaths, tapply(Deaths,CauseID,
                                             function(x) unlist(lillie.test(x)[c("statistic", "p.value")]))))
# The normality is rejected as the p-values of each group is less than the significance level.

# 3. The dependent variable should have equal population variances for the 5 diseases.
# Levene's test is an alternative to the Bartlett test and less sensitive to
# departures from normality. 
# The null hypothesis of equal variances will be rejected if p-value < 0.05.
leveneTest(Deaths~CauseID,sampled_deaths)
# From Levene's test, p-value is greater than a = 0.05, which means that
# we fail to reject the null hypothesis as there is not enough evidence to suggest that the null 
# hypothesis is false at the 95% confidence level. We can conclude that the variance of the dependent
# variable is equal for the 5 groups.


# 4. Independence of within-group and between-groups observations.
# For the confirmation of the last assumption, let's think the way we designed the data.
# Our sampling was stratified in order to achieve 1 random entry by Country and 36 by Disease. 
# The observations come from 180 different countries, meaning that there are not repeating 
# "individuals" and, by design, the deaths caused by one disease are not affected by other disease
# achieving independence between the groups. 


# DETECTING OUTLIERS IN DEATHS BY DISEASE #
library(grDevices)
X1 <- sampled_deaths$Deaths
X2 <- sampled_deaths$CauseID

mat <- matrix(c(1,2,3,4,5), nrow=1, ncol=5)
layout(mat)
# outlier_values <- boxplot.stats(X1)$out
for (i in 1:length(levels(X2))){
  boxplot(X1[X2==levels(X2)[i]]~X2[X2==levels(X2)[i]], main="Deaths",xlab=levels(X2)[i],ylab='Deaths')
}
par(mfrow=c(1,1))
boxplot(X1~X2, main="Deaths",xlab='Disease',ylab='Deaths')

outliers_group <- sampled_deaths %>% group_by(CauseID) %>% identify_outliers("Deaths")

################ ONE-WAY ANOVA ################ 

################ Kruskal-Wallis ################ 
# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test.
# It’s recommended in our case as one of the ANOVA assumptions, the Normality, is not met. 
kruskal.test(Deaths ~ CauseID, data = sampled_deaths)
# As the p-value is less than the significance level 0.05, we can conclude that
# there are significant differences between deaths caused by the diseases.

################ Multiple pairwise-comparison between groups ################ 
# From the above output, we know that there is a significant difference between diseases,
# but we don’t know which exact pairs of diseases are different.
# For this reason, pairwise comparisons between disease levels will be applied using a post hoc procedure 
# that is similar with the HSD for ANOVAs with corrections for multiple testing.
# A non-parametric alternative called  Wilcoxon signed-rank test is used as our data are not normally distributed. 

# Bonferroni correction in which the p-values are multiplied by the number of comparisons.
pairwise.wilcox.test(sampled_deaths$Deaths, sampled_deaths$CauseID,p.adjust.method = "bonferroni")

# Benjamini & Hochberg (1995) adjustment
pairwise.wilcox.test(sampled_deaths$Deaths, sampled_deaths$CauseID,p.adjust.method = "BH")
# The pairwise comparison indicates the following significant differences (p < 0.05):
#   1. TBLC - LRI
#   2. TBLC - IHD
#   3. TBLC - STR
#   4. COPD - LRI
#   5. COPD - IHD
#   6. COPD - STR
# As observed, the differences in the deaths across the diseases are driven by two diseases/groups
# which are TBLC and COPD. The differences come from the comparison of these two diseases with the 
# remaining groups. An important result is the fact that when there are not significant differences
# in the number of deaths neither between the pairwise comparisons of LRI - IHD - STR nor between
# the pair COPD and TBLC, (lowest number of deaths) something that it was obvious from both the individual
# boxplots and the group statistics.






