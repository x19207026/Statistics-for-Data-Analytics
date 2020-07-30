
####### FUNCTIONS #######

model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}
##########

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
library(tidyr)
library(reshape2)
library(ggplot2)
library(lmtest)
library(faraway)
library(factoextra)

########## IMPORT DATA ########## 
setwd("~/Multiple regression") # change with your directory
GDP_regress <- read_excel("GDP_regressors.xlsx",sheet = 1,range = "B2:O30",col_names = TRUE)
# rownames(GDP_regress) <- as.character(t(GDP_regress[,1]))
GDP_regress <- as.data.frame(GDP_regress)
names(GDP_regress) <- c("Geo_Codes","GDP","Empl_Rt","Unemp_Rt","HICP","REER","FDI","Pov_Exc","Population","Lab_Force","Vol_Ratio","Min_Wages","Tax_Rt","Net_Earn")
# check final structure of data
str(GDP_regress)

# Descriptive Statistics of the numeric variables
options(scipen=100)
options(digits=2)
stat.desc(GDP_regress, basic=F) # nbr.val=number of observations = 28 countries/observations
stat.desc(GDP_regress, desc=F)  # nbr.na=number of missing values # 6 NAs for Minimum Wages
summary(GDP_regress)

##########  MODELLING AND PERMANENT EXCLUSIONS ########## 
# 1. All variables are numeric except for the geographical codes, so we keep this variable outside from our dataset.
GDP_num <- GDP_regress[,-1]
countries <- GDP_regress[,1]
# 2. Instead of omitting 6 from 28 countries, we will exclude the Minimum Wages from our reference datasets as the Pearson correlation with GDP is very small(-0.17)
GDP_new <- na.omit(GDP_num) 
cor(GDP_new$GDP,GDP_new$Min_Wages) 
GDP_final <- GDP_num[,-11]

# Coefficient of Variation for the 12 numeric variables CV=s/x?? 
for (i in 1:length(GDP_final)){
  print(c(names(GDP_final)[i],round(sd(GDP_final[,i])/mean(GDP_final[,i]),3)))
}
# The CVs measures the data dispersion in relation to the mean of the population.
# The advantage of CV is that it can be expressed as percentage and compares variables with and without different measurement scales.
# CV < 1 indicates low-variance distributions, whereas CV > 1 shows high-variance.
# High-variability: GDP,Unemp_Rt, FDI, Pov_Exc, Population and Lab_Force
# Low-variability: Empl_Rt, HICP, REER, Vol_Ratio, Tax_Rt and Net_Earn

##########  TESTING FOR NORMALITY ########## 
# There are no assumptions for Independent variables but it's useful to understand their distribution
# in order to find influential points or outliers or concentrated values. For this reason, we will apply tests and graphs for Normality as well as 
# Violplots for assessing the values concentration. In this way, we could manage symmetrical distributions by suggesting possible transformations for highly skewed variables.

# Create QQ-plots and run the Lilliefors Test for checking Normality of numeric variables:
par(mfrow=c(3,4))
qqnorm(GDP_final$GDP,main="GDP")
qqline(GDP_final$GDP,col=2)  
qqnorm(GDP_final$Empl_Rt,main="Employment Rate")
qqline(GDP_final$Empl_Rt,col=2)
qqnorm(GDP_final$Unemp_Rt,main="Unemployment Rate")
qqline(GDP_final$Unemp_Rt,col=2)
qqnorm(GDP_final$HICP,main="HICP")
qqline(GDP_final$HICP,col=2)
qqnorm(GDP_final$REER,main="REER")
qqline(GDP_final$REER,col=2)
qqnorm(GDP_final$FDI,main="FDI")
qqline(GDP_final$FDI,col=2)
qqnorm(GDP_final$Pov_Exc,main="Risk of Poverty/Exclusion")
qqline(GDP_final$Pov_Exc,col=2)
qqnorm(GDP_final$Population,main="Total Population")
qqline(GDP_final$Population,col=2)
qqnorm(GDP_final$Lab_Force,main="Labour Force")
qqline(GDP_final$Lab_Force,col=2)
qqnorm(GDP_final$Vol_Ratio,main="Export/Import Ratio")
qqline(GDP_final$Vol_Ratio,col=2)
qqnorm(GDP_final$Tax_Rt,main="Tax Rate")
qqline(GDP_final$Tax_Rt,col=2)
qqnorm(GDP_final$Net_Earn,main="Net Earnings")
qqline(GDP_final$Net_Earn,col=2)
title("Normal Q-Q Plots", outer = TRUE)

# "P-Values of Lilliefors Test for Normality" ??? check other tests
for (i in 1:length(GDP_final)){
  print(c(names(GDP_final)[i],lillie.test(GDP_final[,i])$p.value))
}
# If p-value<0.05 then reject the Null hypothesis which means that our data is not normal.
# Reject Normality for: 
# GDP, Unemp_Rt, FDI, Pov_Exc, Population, Lab_Force

# "GDP"         "0.0000000000451380535032028"
# "Empl_Rt"     "0.341070402579081"
# "Unemp_Rt"    "0.00000000226937071916235"
# "HICP"        "0.609117232070103"
# "REER"        "0.361339382147722"
# "FDI"         "0.00000000000000617760490823492"
# "Pov_Exc"     "0.000000470142714397453"
# "Population"  "0.0000000202683749769199"
# "Lab_Force"   "0.0000000157873051493739"
# "Vol_Ratio"   "0.648246922610136"
# "Tax_Rt"      "0.127952255634648"
# "Net_Earn"    "0.0511132961357281"

# A log transformation would suffice for non-normal variables in this small population/sample.
# However, we will apply it only to the highly skewed variables in order to avoid losing the meaningful explanation of the effects on the dependent variable.

# Kurtosis & Skewness
# kurtosis ≈3 (excess ≈0) is called mesokurtic
# kurtosis <3 (excess kurtosis <0) is called platykurtic
# kurtosis >3 (excess kurtosis >0) is called leptokurtic
# skewness < −1 or > +1 is highly skewed.
# skewness is between −1 and -1/2 or between +1/2 and +1 is moderately skewed.
# skewness is between -1/2 and +1/2 is approximately symmetric.

for (i in 1:length(GDP_final)){
  print(c("Kurtosis and Skewness for",names(GDP_final)[i]))
  print(kurtosis(GDP_final[,i]))
  print(skewness(GDP_final[,i]))
  print(agostino.test(GDP_final[,i])) # a = 0.05
}
# GDP
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# Empl_Rt
# mesokurtic and moderately skewed
# rejects Null hypothesis --> Not-normal
# Unemp_Rt
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# HICP
# mesokurtic and approximately symmetric
# Normal
# REER
# leptokurtic and moderately skewed
# Normal
# FDI
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# Pov_Exc
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# Population
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# Lab_Force
# leptokurtic and highly skewed
# rejects Null hypothesis --> Not-normal
# Vol_Ratio
# mesokurtic and approximately symmetric
# Normal
# Tax_Rt
# mesokurtic and approximately symmetric
# Normal
# Net_Earn
# platykurtic and approximately symmetric
# Normal

##########  RESPONSE Transformation ########## 
# Due to an outlier/influential point (17th observation) in the GDP, the relationships between the response variables and 
# the possible predictors are driven by this outlier. As the outlier belongs to the response variable, instead of dropping it,
# we are choosing to transform GDP, something which will make assumptions work better.
# For identifying the best transformation for our response variable, we will run a Box-Cox test in a fitted additive multiple regression model
# with response GDP and each of the other variables as predictors.

raw_model <- lm(GDP ~ Empl_Rt + Unemp_Rt + HICP + REER + FDI + Pov_Exc + Population + Lab_Force + Vol_Ratio+Tax_Rt, data = GDP_final)
boxcox(raw_model, plotit=TRUE)
transform_model <- lm(log(GDP) ~ Empl_Rt + Unemp_Rt + HICP + REER + FDI + Pov_Exc + Population + Lab_Force + Vol_Ratio+Tax_Rt, data = GDP_final)

par(mfrow=c(1,2))
plot(fitted(raw_model), resid(raw_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Raw GDP model")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(transform_model), resid(transform_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Log GDP model")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
title("Fitted versus Residuals",outer = TRUE)

# Looking at the second plot,it seems that the assumptions of the model are no longer violated.
# Breusch-Pagan and Shapiro-Wilk tests verify it.

bptest(transform_model)
shapiro.test(resid(transform_model))
# p-values very large, fail to reject the null hypothesis, normality

GDP_mod <- GDP_final
GDP_mod[,1] <- log(GDP_mod[,1])
names(GDP_mod)[1]<-"Log_GDP"

##########  PREDICTORS Transformation ########## 
GDP_mod %>%
  gather(-Log_GDP, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Log_GDP)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  ggtitle("Simple Linear Regression with Log(GDP) and each predictor", subtitle = "Fit Linear, Logarithmic and Polynomial Lines")

# No linear relationships between Log_GDP and the half of the predictors.   
# Possibly needed transformation of the predictors in order to treat outliers.
# We apply SLR:
log_gdp_empl <- lm(Log_GDP~Empl_Rt,data=GDP_mod)
log_gdp_unemp <- lm(Log_GDP~Unemp_Rt,data=GDP_mod)
log_gdp_FDI <- lm(Log_GDP~FDI,data=GDP_mod)
log_gdp_REER <- lm(Log_GDP~REER,data=GDP_mod)
log_gdp_HICP <- lm(Log_GDP~HICP,data=GDP_mod)
log_gdp_Earn <- lm(Log_GDP~Net_Earn,data=GDP_mod)
log_gdp_Pop <- lm(Log_GDP~Population,data=GDP_mod)
log_gdp_Force <- lm(Log_GDP~Lab_Force,data=GDP_mod)
log_gdp_Vol_R <- lm(Log_GDP~Vol_Ratio,data=GDP_mod)
log_gdp_Tax <- lm(Log_GDP~Tax_Rt,data=GDP_mod)

par(mfrow = c(3, 4))
plot(fitted(log_gdp_empl), resid(log_gdp_empl), col = "dodgerblue",
       pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Empl_Rt")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_unemp), resid(log_gdp_unemp), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Unemp_Rt")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_FDI), resid(log_gdp_FDI), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs FDI")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_REER), resid(log_gdp_REER), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs REER")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_HICP), resid(log_gdp_HICP), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs HICP")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_Earn), resid(log_gdp_Earn), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Net_Earn")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_Pop), resid(log_gdp_Pop), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Population")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_Force), resid(log_gdp_Force), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Lab_Force")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_Vol_R), resid(log_gdp_Vol_R), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Vol_Ratio")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(log_gdp_Tax), resid(log_gdp_Tax), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals",main="Log_GDP vs Tax_Rt")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
title("Fitted vs Residuals",outer=TRUE)

# Based on the skewness values, highly skewed Unemp_Rt, FDI, Pov_Exc, Population and Lab_Force need to be transformed or trim some of the influential data points/outliers.
# We are not choosing to remove outliers as our sample size is small and each variable's outliers refer to different countries.
# As a result from the SLR graphs, we will apply log transformation to 4 from 5 variables as FDI cannot be log transformed due to its negative values.
# After further investigation, we will keep raw FDI without transforming it as negative values indicate that outflows of investment exceed inflows.
# This is an important feature that we don't want to lose.

GDP_norm <- GDP_mod
GDP_norm[,c(3,7,8,9)] <- log(GDP_norm[,c(3,7,8,9)])
names(GDP_norm)[c(3,7,8,9)]<-paste(c("Log_Unemp_Rt","Log_Pov_Exc","Log_Pop","Log_Lab_Force"))
GDP_norm %>%
  gather(-Log_GDP, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Log_GDP)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  ggtitle("Simple Linear Regression with Log(GDP) and each predictor", subtitle = "Fit Linear, Logarithmic and Polynomial Lines")

norm_model <- lm(Log_GDP ~ Empl_Rt + Log_Unemp_Rt + HICP + REER + FDI + Log_Pov_Exc + Log_Pop + Log_Lab_Force + Vol_Ratio + Tax_Rt, data = GDP_norm)
par(mfrow = c(1, 2))
plot(fitted(norm_model), resid(norm_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(norm_model), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(norm_model), col = "dodgerblue", lwd = 2)


# Fitted vs Residuals plot for 3 steps of transformation #
par(mfrow=c(1,3),oma=c(2, 3, 5, 2))
plot(fitted(raw_model), resid(raw_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Raw GDP model")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(transform_model), resid(transform_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Log GDP model")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
plot(fitted(norm_model), resid(norm_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Log GDP model & transformed predictors")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
title("Fitted versus Residuals",line=3,outer = TRUE)
title("Steps of transformation",line=1,outer = TRUE)

##########  MULTICOLLINEARITY ########## 
# As we want to fit a Multiple Regression Model for measuring GDP, we want to remove any Multicollinearity issues
# among the predictors. But we care only for the variables with significant correlation (small p-value) with our response variable Log_GDP. 
# For this reason, we will calculate the correlation values and their p-values among all the variables 
# Because our variables are log transformed, we will apply Spearman's Rank correlation which is invariant under monotone transformations.

sjt.corr(GDP_norm,corr.method = "spearman",show.p=TRUE,p.numeric = TRUE,fade.ns = TRUE,digits = 3,triangle = "lower",remove.spaces = TRUE)
# This table shows the Spearman's rank correlation, as well as the p-value of the independent variables
# analyzed against GDP. Six from Eleven variables have a significant relationship with GDP because their p-values are smaller than the 0.01 significance level.
# These variables are: Log_Unemp_Rt, REER, Log_Pov_Exc, Log_Pop, Log_Lab_Force and Tax_Rt
GDP_norm %>%
  gather(-Log_GDP, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Log_GDP)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  ggtitle("Simple Linear Regression with Log(GDP) and each predictor")

GDP_end <- GDP_norm[c(1,3,5,7,8,9,11)]
pairs(GDP_end[,-1], col = "dodgerblue")
title("Scatterplots between pairs of predictors",line=3,outer=TRUE,cex.main=1)
round(cor(GDP_end,method = "spearman"), 2)

########## QUICK PRINCIPAL COMPONENT ANALYSIS ##########
gdp.pca <- prcomp(GDP_end[,-1], center = TRUE,scale. = TRUE)
summary(gdp.pca)
fviz_eig(gdp.pca)
# From the plot, it seems that most of the variability (99%) will be explained by 3 principal components.
# Eigenvalues
eig.val <- get_eigenvalue(gdp.pca)
eig.val
# Results for Variables
res.var <- get_pca_var(gdp.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Finally, the variables with the greatest contribution to the first three principal components are:
# Labour_Force(Unemployment Rate, Poverty/Exclusion, Population), REER, Tax_Rt  
# It seems that we will have a multicollinearity problem with Labour_Force, Unemployment Rate, Poverty/Exclusion and Population variables.
# For assessing the multicollinearity issue, we will use VIF values.   

########## CHECK VIF AND REDUCE THE NUMBER OF PREDICTORS ##########
# Split randomly the data into training (70%) and test set (30%). The training set will be used for building the predictive model while the model will be evaluated using the test set.
set.seed(123) # for reproducibility
train.samples <- GDP_end$Log_GDP %>% createDataPartition(p = 0.7,list = FALSE)
train.data  <- GDP_end[train.samples, ]
test.data <- GDP_end[-train.samples, ]
#========================== STEP 1 ==========================#
mfull<-lm(Log_GDP~.,data=train.data)
# Make predictions
predictions <- mfull %>% predict(test.data)
# Model performance of the full model
model <- mfull
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)

# Detect multicollinearity in the full regression model
vif(mfull)
#========================== STEP 2 ==========================#
# Build a model excluding the Population variable
model1 <- lm(Log_GDP ~. -Log_Pop, data = train.data)
# Make predictions
predictions <- model1 %>% predict(test.data)
# Model performance
model <- model1
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the REDUCED regression model
vif(model1)
#========================== STEP 3 ==========================#
# Build a model excluding the Population and Poverty/Exclusion variables
model2 <- lm(Log_GDP ~. -Log_Pop - Log_Pov_Exc , data = train.data)
# Make predictions
predictions <- model2 %>% predict(test.data)
# Model performance
model <- model2
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the DECREASED regression model
vif(model2)
#========================== STEP 4A =========================#
# Build a model excluding the Population, Poverty/Exclusion and Labour Force variables
model3 <- lm(Log_GDP ~. -Log_Pop - Log_Pov_Exc -Log_Lab_Force , data = train.data)
# Make predictions
predictions <- model3 %>% predict(test.data)
# Model performance
model <- model3
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the DECREASED regression model
vif(model3)
#========================== STEP 4B =========================#
# Build a model excluding the Population, Poverty/Exclusion and Unemployment Rate variables
model4 <- lm(Log_GDP ~. -Log_Pop - Log_Pov_Exc -Log_Unemp_Rt , data = train.data)
# Make predictions
predictions <- model4 %>% predict(test.data)
# Model performance
model <- model4
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the DECREASED regression model
vif(model4)

# library(broom)
# glance(mfull) # use this to summarize in latex
# Metrics for model 1
# glance(mfull) %>%
#   dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
# 
# ldply(list(mfull), model_fit_stats)

########## IMPROVEMENT OF THE BEST MODEL AND ASSESSMENT OF INTERACTIONS ##########
# We will perform ANOVA to check if the 2-way and 3-way interaction terms are significant.

#========================== ANOVA 1 =========================#
all_int_mod <- lm(Log_GDP ~ Log_Lab_Force*REER*Tax_Rt , data = train.data) # Same with Model 4 but with 2 and 3-way interactions
two_way_int_mod <- lm(Log_GDP ~ (Log_Lab_Force+REER+Tax_Rt)^2 , data = train.data) # Same with Model 4 but with 2 interactions
anova(two_way_int_mod,all_int_mod)
# As p-value is extremely large, we fail to reject the Null hypothesis of the F-test, which means that the 3-way interaction is not preferred.

####### Calculation of R2 and RMSE for all_int_mod #######
# Make predictions
predictions1 <- all_int_mod %>% predict(test.data)
# Model performance
model <- all_int_mod
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions1, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions1, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in full regression model
vif(all_int_mod)

#========================== ANOVA 2 =========================#
full_no_int_mod <- lm(Log_GDP ~ Log_Lab_Force+REER+Tax_Rt , data = train.data) # Same with Model 4 
anova(full_no_int_mod,two_way_int_mod)
# As p-value is extremely large, we fail to reject the Null hypothesis of the F-test, which means that we prefer the smaller less flexible model without any interaction.

####### Calculation of R2 and RMSE for two_way_int_mod #######
# Make predictions
predictions2 <- two_way_int_mod %>% predict(test.data)
# Model performance
model <- two_way_int_mod
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions2, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions2, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the 2-way regression model
vif(two_way_int_mod)

####### Calculation of R2 and RMSE for full_no_int_mod #######
# Make predictions
predictions3 <- full_no_int_mod %>% predict(test.data)
# Model performance
model <- full_no_int_mod
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions3, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions3, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the regression model without interactions
vif(full_no_int_mod)

# OUTCOME
# However, the 2-way model has a "better fit" (smaller RMSE) and larger R-squared value for the test set. Also, the difference with the model without any interaction seems to be large. 
# For this reason, we will make a try to assess if we can keep some of the 2-way interactions instead of all.   

########## ASSESSMENT OF 2-WAY INTERACTIONS ########## 

two_way_int_mod <- lm(Log_GDP ~ (Log_Lab_Force+REER+Tax_Rt)^2 , data = train.data) # Same with Model 4 but with 2 interactions
two_way_int_1 <- lm(Log_GDP ~ Log_Lab_Force+REER+Tax_Rt +Log_Lab_Force:REER , data = train.data)
two_way_int_2 <- lm(Log_GDP ~ Log_Lab_Force+REER+Tax_Rt +REER:Tax_Rt , data = train.data)
two_way_int_3 <- lm(Log_GDP ~ Log_Lab_Force+REER+Tax_Rt +Log_Lab_Force:Tax_Rt , data = train.data)

anova(two_way_int_1,two_way_int_mod)
anova(two_way_int_2,two_way_int_mod)
anova(two_way_int_3,two_way_int_mod)

predictions <- two_way_int_3 %>% predict(test.data)
# Model performance
model <- two_way_int_3
data.frame(
  RMSE.train = RMSE(fitted(model), train.data$Log_GDP),
  RMSE.test = RMSE(predictions, test.data$Log_GDP),
  R2.train = R2(fitted(model), train.data$Log_GDP,form="traditional"),
  R2adj.train = summary(model)$adj.r.squared,
  R2.test = R2(predictions, test.data$Log_GDP,form="traditional"), # or form = "corr"
  R2pred.train = pred_r_squared(model),
  PRESS.train = PRESS(model),
  RSE = summary(model)$sigma,
  AIC = AIC(model), 
  BIC = BIC(model)
)
# Detect multicollinearity in the regression model without interactions
vif(two_way_int_3)

anova(full_no_int_mod,two_way_int_1)
anova(full_no_int_mod,two_way_int_2)
anova(full_no_int_mod,two_way_int_3)

# Implementation of the coefficients and coefficient of determination
best.model <- lm(Log_GDP ~ Log_Lab_Force+REER+Tax_Rt , data = GDP_end) # Same with Model 4 but in full dataset
coef(best.model)["Log_Lab_Force"]
coef(best.model)["REER"]
coef(best.model)["Tax_Rt"]

summary(best.model)
(exp(coef(best.model)["REER"]) - 1) * 100 
(exp(coef(best.model)["Tax_Rt"]) - 1) * 100 

# REGRESSION DIAGNOSTICS #
par(mfrow=c(2,2))
plot(best.model, which=1:4)

par(mfrow=c(1,1))
qqnorm(best.model$resid)
qqline(best.model$resid,col="red")
lillie.test(best.model$resid) # normal residuals??
# With a Scale-Location plot, we will check the important assumption of constant variance of the errors. 
plot(best.model, which = 3)
# Due to an outlier, the standardized residuals are trending downward initially.
# After this, the trend line is almost even and the residuals are uniformly scattered. This probably indicates that the constant variance assumption holds. 
# Partial residual plots 
crPlot(best.model, variable = "Log_Lab_Force")
crPlot(best.model, variable = "REER")
crPlot(best.model, variable = "Tax_Rt")

# OUTCOME #
# From the Regression Diagnostics' Plots, we would suggest to keep the best.model

# lm.hat <- hatvalues(best.model)
# id.lm.hat <- which(lm.hat > (2*(4+1)/nrow(GDP_end))) ##  hatvalue > #2*(k+1)/n
# lm.hat[id.lm.hat]




