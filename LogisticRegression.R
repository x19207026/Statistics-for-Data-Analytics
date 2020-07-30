################  IMPORT LIBRARIES ################ 
rm(list=ls(all=TRUE))
# install.packages(c("pastecs","moments","nortest","sm","vioplot","car","corrplot","sjPlot","ggpubr","caret","olsrr","tidyr","reshape2","ggplot2","faraway","factoextra"))
# install_github("kassambara/factoextra")
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
library(VIM) # visual inspection of missing data
library(mice) # for missing data
library(dplyr) # for summary by group
library(ggpubr) # for ggboxplot
library(gmodels)# for contingency tables
library(rstatix) # for outlier detection identify_outliers
library(Amelia) # for missmap 
library(tibble)
library(tidyverse) # for drop_na
library(rpart) # for decision tree
library(devtools)
library(factoextra) # PCA visualization
library(ROSE) # for the ROC curve
library(psych) # for pairplots
library(broom) # for standardized residuals (.std.resid) and Cook’s distance (.cooksd) 
# devtools::install_github("laresbernardo/lares")
library(lares) # for significant ranked-cross correlations
library(REdaS) # for Bartlett's Test of Sphericity
library(ipred) # for confusion matrix

################ FUNCTIONS ################ 

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '0', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '0', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

################ IMPORT THE DATA ################ 
setwd("~/Postgraduate Diploma in Data Analytics/Statistics for Data Analytics/Individual project 2/1. Datasets")
OVERWEIGHT_data <- read_excel("Prevalence of overweight among adults _ BMI greater than 25 _ crude.xls",sheet = 4,range = "A1:C192",col_names = TRUE) # Dependent variable
CHE_data <- read_excel("Current health expenditure (CHE) per capita in US$.xls",sheet = 2,range = "A1:B191",col_names = TRUE)
UV_data <- read_excel("Exposure to solar ultraviolet (UV) radiation.xls",sheet = 2,range = "A1:B192",col_names = TRUE)
VIOLENCE_data <- read_excel("Intimate partner violence prevalence.xls",sheet = 3,range = "A1:B108",col_names = TRUE)
MAT_MOR_data <- read_excel("Maternal mortality.xls",sheet = 2,range = "A1:B184",col_names = TRUE)
VEHICLE_data <- read_excel("Registered vehicles.xls",sheet = 2,range = "A1:B180",col_names = TRUE)
ROAD_SAFE_data <- read_excel("Road safety demographics.xls",sheet = 2,range = "A1:B182",col_names = TRUE)
SUICIDE_data <- read_excel("Suicide rate estimates, age-standardized.xls",sheet = 2,range = "A1:C184",col_names = TRUE)
POISON_data <- read_excel("Unintentional poisoning_burden of disease.xls",sheet = 2,range = "A1:C184",col_names = TRUE)
SDG_data <- read_excel("Health expenditure greater than 25 percent of total (percent pop).xls",sheet = 2,range = "A1:B154",col_names = TRUE)
DOC_data <- read_excel("Medical doctors.xls",sheet = 2,range = "A1:B194",col_names = TRUE)
POVERTY_data <- read_excel("Population under poverty (%).xls",sheet = 2,range = "A1:B158",col_names = TRUE)
NEWBORN_data <- read_excel("Neonatal mortality rate per 1000 live births.xls",sheet = 2,range = "A1:B195",col_names = TRUE)

################ DATASETS TRANSFORMATION ################ 

# 1. OVERWEIGHT_data
OVERWEIGHT_data <- as.data.frame(OVERWEIGHT_data)
countries <- c(OVERWEIGHT_data[,1]) # 191 countries
OVERWEIGHT_data$success <- c()  # dependent variable
for (i in 1:length(countries)){
  if(as.numeric(OVERWEIGHT_data$Female)[i]>as.numeric(OVERWEIGHT_data$Male[i])){
    OVERWEIGHT_data$success[i] <- 1  
  }else{
    OVERWEIGHT_data$success[i] <- 0 # this is the reference level in R
  }
}
OVERWEIGHT_data$success <- as.factor(OVERWEIGHT_data$success)
# OVERWEIGHT_data$success <- relevel(OVERWEIGHT_data$success, ref = "1") # now, the reference level has changed into 1

# 2. CHE_data
# keep only 191 countries, round the data and change the column name:
CHE_data <- as.data.frame(CHE_data)[is.element(CHE_data$`Country`, countries),]
CHE_data[,2] <- as.numeric(CHE_data[,2])

# 3. UV_data
# keep only 191 countries, round the data and change the column name:
UV_data <- as.data.frame(UV_data)[is.element(UV_data$`Country`, countries),]
UV_data[,2] <- as.numeric(UV_data[,2])

# 4. VIOLENCE_data
VIOLENCE_data <- as.data.frame(VIOLENCE_data)[is.element(VIOLENCE_data$`Countries`, countries),]
VIOLENCE_data[,2] <- as.numeric(VIOLENCE_data[,2])
names(VIOLENCE_data)[1]<-'Country'

# 5. MAT_MOR_data
MAT_MOR_data <- as.data.frame(MAT_MOR_data)[is.element(MAT_MOR_data$`Country`, countries),]
MAT_MOR_data[,2] <- as.numeric(MAT_MOR_data[,2])

# 6. VEHICLE_data
VEHICLE_data <- as.data.frame(VEHICLE_data)[is.element(VEHICLE_data$`Country`, countries),]
VEHICLE_data[,2] <- as.numeric(VEHICLE_data[,2])

# 7. ROAD_SAFE_data
ROAD_SAFE_data <- as.data.frame(ROAD_SAFE_data)[is.element(ROAD_SAFE_data$`Country`, countries),]
ROAD_SAFE_data[,2] <- as.numeric(ROAD_SAFE_data[,2])

# 8. SUICIDE_data
SUICIDE_data <- as.data.frame(SUICIDE_data)[is.element(SUICIDE_data$`Country`, countries),]
SUICIDE_data$suicide_f_to_m <- c()  # ratio female to male suicide rates
for (i in 1:dim(SUICIDE_data)[1]){
  SUICIDE_data$suicide_f_to_m[i] <- round(as.numeric(SUICIDE_data$Females[i])/as.numeric(SUICIDE_data$Males[i]),2)
}
SUICIDE_data$suicide_f_to_m[which(SUICIDE_data$suicide_f_to_m == 'Inf')] <- NA # Replace Infimum with NA

# 9. POISON_data
POISON_data <- as.data.frame(POISON_data)[is.element(POISON_data$`Country`, countries),]
POISON_data$poison_f_to_m <- c()  # ratio female to male mortality rates due to unintesional poisoning
for (i in 1:dim(POISON_data)[1]){
  POISON_data$poison_f_to_m[i] <- round(as.numeric(POISON_data$Females[i])/as.numeric(POISON_data$Males[i]),2)
}

# 10. SDG_data
SDG_data <- as.data.frame(SDG_data)[is.element(SDG_data$`Country`, countries),]
SDG_data$SDG25 <- as.numeric(SDG_data$SDG25)

# 11. DOC_data
DOC_data <- as.data.frame(DOC_data)[is.element(DOC_data$`Country`, countries),]
DOC_data$doctors <- round(as.numeric(DOC_data$doctors),0)

# 12. POVERTY_data
POVERTY_data <- as.data.frame(POVERTY_data)[is.element(POVERTY_data$`Country`, countries),]
POVERTY_data$pop_under_pov <- as.numeric(POVERTY_data$pop_under_pov)

# 13. NEWBORN_data
NEWBORN_data <- as.data.frame(NEWBORN_data)[is.element(NEWBORN_data$`Country`, countries),]
NEWBORN_data$neo_deaths <- as.numeric(NEWBORN_data$neo_deaths)


################ DATA ANALYSIS TABLE ################ 
# Join tables using pipes with dplyr library
my_data <- OVERWEIGHT_data %>% 
  left_join(CHE_data, by = c("Country")) %>%
  left_join(UV_data, by = c("Country")) %>%
  left_join(VIOLENCE_data, by = c("Country")) %>%
  left_join(MAT_MOR_data, by = c("Country")) %>%
  left_join(VEHICLE_data, by = c("Country")) %>%
  left_join(ROAD_SAFE_data, by = c("Country")) %>%
  left_join(SUICIDE_data, by = c("Country")) %>%
  left_join(POISON_data, by = c("Country")) %>%
  left_join(SDG_data, by = c("Country")) %>%
  left_join(DOC_data, by = c("Country")) %>%
  left_join(POVERTY_data, by = c("Country")) %>%
  left_join(NEWBORN_data, by = c("Country")) %>%
  select(Country, success, health_exp_cap,UV_rad,Sex_viol_w,mat_death,reg_vehicle,gincome_cap,
         suicide_f_to_m,poison_f_to_m,SDG25,doctors,pop_under_pov,neo_deaths) 

##########  MODELLING AND PERMANENT EXCLUSIONS ########## 

str(my_data) # all numeric (ratio) variables apart from country (character) and dependent variable which is a factor with 2 levels (binary)
summary(my_data)
missmap(my_data, main = "Missing values vs observed") # 9% missing entries among all variables

# Due to the fact that the variable Violence towards women is not available for all countries and
# is only populated for 84 over 191 around 44% of them, its final exclusion from the analysis dataset has beed decided
# in order not to discard additional information given from the remaining variables.
my_data <- my_data[,-5]
summary(my_data)
dim(my_data)[1]
missmap(my_data) # 6.08% missing entries among all variables = 6.0814*(191*13)/100 = 151

# Descriptive Statistics for Numeric variables
options(scipen=100)
options(digits=2)
stat.desc(my_data[,-c(1,2)], basic=F) # nbr.val=number of observations = 191 countries/observations
stat.desc(my_data[,-c(1,2)], desc=F)  # nbr.na=number of missing values # Multiple missing values
dim(my_data[!complete.cases(my_data),])[1] # 66 rows with missing values

# The variables with an alarming rate of missing values are a) SDG25 (40), b) pop_under_pov (36) and c) reg_vehicle (22),
# justifying 98/151 total missing values.
# At this point, we should note that the missing values that seem to exist in the remaining variables have 
# been occured due to the join of all variables on the Country. This means that, initially, there were 
# WHO tables with smaller number of country entries, but, by joining them based on a key column, additional 
# missings have been created. For this reason, before replacing them with an average or omitting these 66 countries,
# the coefficient of variation is calculated for each variable which measures how much the data disperses from 
# the population mean. This coefficient has the advantage of being expressed as a percentage and comparing 
# variables with or without different measurements scales. 

for (i in 1:length(my_data[,-c(1,2)])){
  test <- na.omit(my_data[,-c(1,2)][,i])
  print(c(names(my_data[,-c(1,2)])[i],round(sd(test)/mean(test),3)))
}
# "health_exp_cap" "1.666"         
# "UV_rad" "0.352" 
# "mat_death" "3.756"    
# "reg_vehicle" "3.175"      
# "gincome_cap" "1.417"      
# "suicide_f_to_m" "0.618"           
# "poison_f_to_m" "0.841"        
# "SDG25" "1.358"
# "doctors" "0.896"  
# "pop_under_pov" "1.472"        
# "neo_deaths" "0.809"     

# CV < 1 indicates low-variance distributions, whereas CV > 1 shows high-variance.
# High-variability: health_exp_cap, mat_death, reg_vehicle, gincome_cap, SDG25, pop_under_pov
# Low-variability: UV_rad, suicide_f_to_m, poison_f_to_m, doctors, neo_deaths
# For saving the given information from the other variables, based on the high CV values, the 
# a) SDG25, b) pop_under_pov and c) reg_vehicle will be excluded from the data analysis dataset as,
# due to their extreme variation, they would cause biased estimates in case of averaging the missing countries.
newdata <- my_data[,-c(6,10,12)]

# number of rows of data that have missing values from the rest variables
dim(newdata[!complete.cases(newdata),])[1] # 28 rows with missing values
options(scipen=100)
options(digits=2)
stat.desc(newdata[,-c(1,2)], basic=F) # nbr.val=number of observations = 191 countries/observations
stat.desc(newdata[,-c(1,2)], desc=F)  # nbr.na=number of missing values # Multiple missing values

# As the data points refer to different countries with multiple characteristics with great variability (high CV),
# it would not seem correct to replace all missing values with averages.
# Two variables with enough missing entries and a very high CV are a) mat_death (10 NAs/CV>>1) and b) gincome_cap (16 NAs/CV>>1).
# After expert thought, we omitted those 21 countries that referred to NAs and replaced the remaining ones by each  
# column's average.
data_wna <- newdata %>% drop_na("mat_death") %>% drop_na("gincome_cap")
data_wna[sapply(data_wna, is.numeric)] <- lapply(data_wna[sapply(data_wna, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
stat.desc(data_wna[,-c(1,2)], basic=F) # nbr.val=number of observations = 191 countries/observations
stat.desc(data_wna[,-c(1,2)], desc=F)  # nbr.na=number of missing values # Multiple missing values
# The statistics have not changed much with this treatment phase.
rownames(data_wna) = 1:170 # reset the indices

# For a better understanding of how the categorical variables have been dealt as dummy variables by R, we used  
# contrasts() function
contrasts(data_wna$success)
# For the success variable, the case where "men overweight rate is greater than females" is used as the reference.

################ DATA EXPLORATION ################ 
# Linearity assumption of the Logistic regression 
# Here, we’ll check the linear relationship between continuous predictor variables and the
# logit of the outcome. This can be done by visually inspecting the scatter plot between each 
# predictor and the logit values.

# 1. Fit the logistic regression model
model <- glm(success ~., data = data_wna[,-1], family = binomial)

# 2. Predict the probability (p) of success positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# 3. Remove qualitative variables from the original data frame and bind the logit values to the data:
# Select only numeric predictors
model_data <- data_wna %>% dplyr::select_if(is.numeric) 
predictors <- colnames(model_data)
# Bind the logit and tidying the data for plot
model_data <- model_data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# 4. Create the scatter plots:
ggplot(model_data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# The smoothed scatter plots show that the variables doctors, gincome_cap, health_exp_cap, neo_deaths and UV_rad are all quite 
# linearly associated with the female overweight outcome in logit scale.
# The other variables are not linear and would probably need a transformation. In order to avoid it, we will apply Principal Component Analysis.

################ PRINCIPAL COMPONENT ANALYSIS ################ 
# Instead of running an exploratory analysis in our data, a quicker approach called Principal Component Analysis
# will be used for identifying the subset of 8 predictors that captures as much information and explains the maximum
# variability in the data. However, part of the initial attestation process involves confirmation that the data can 
# be analysed using PCA in order to get a valid result. This consists of checking five important assumptions:

# 1.  PCA can be applied only on numerical data with multiple predictors measured at continuous level. In our case,
#     all predictors are ratio variables apart from the response and identification variables which will be omitted
#     from the PCA  
# 2.  Requiring an adequate sample size, the Kaiser-Meyer-Olkin (KMO) was measured for (a) the overall data set and
#     (b) each individual variable.
# 3.  For detecting if our data is suitable for data reduction, Bartlett's test of sphericity should be used.
# 4.  No significant outliers should appear in the data, meaning influential values that can alter the quality of the
#     PCA and the logistic regression model in the end. This assumption will be examined by visualizing the Cook’s distance values.
# 5.  If there are no linear relationships between all variables, the analysis has little chance not to provide misleading
#     information on principal components.

# STEP 1: Variables Exploration AND SIGNIFICANT CORRELATIONS ##
# Since we have p=8 predictors, p(p-1)/2=28 scatter plots could be created. For their creation, we will use the R function
# function pairs.panels() where our binary response variable will be excluded from the calculation of the Spearman's correlation.

# Check for Multicollinearity of predictors
sjt.corr(data_wna[,c(3:10)],corr.method = "spearman",show.p=TRUE,p.numeric = TRUE,fade.ns = TRUE,digits = 3,triangle = "lower",remove.spaces = TRUE)
# However, variables show multicollinearity with significant |spearman's coefficients| > 0.3.

# pairs.panels(data_wna[,-c(1,2)], 
#              method = "spearman", # correlation method as it's not robust to outliers
#              hist.col = "#00AFBB",
#              density = TRUE,  # show density plots
#              ellipses = TRUE # show correlation ellipses)
# By looking at the scatter plots, there are influential data points as well as variables with lower correlation between them.

# Correlation panel
# panel.cor <- function(x, y){
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- round(cor(x, y,method = "spearman"), digits=2)
#   txt <- paste0("R = ", r)
#   cex.cor <- 0.9/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }

panel.cor <- function(x, y, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y, method = "spearman"), digits=2)
  txt <- paste0("R = ", r)
  test <- cor.test(x,y, method = "spearman")
  Signif <- ifelse(round(test$p.value,2)<0.05,"p<0.001",paste("p=",round(test$p.value,2)))  
  text(0.5, 0.25, txt)
  text(.5, .75, Signif)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="steelblue", ...)
}

# Customize upper panel
my_cols <- c("#00AFBB", "#FC4E07")  
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[data_wna$success])
}
# Create the pair plots
pairs(data_wna[,-c(1,2)], lower.panel = panel.cor, upper.panel = upper.panel,diag.panel = panel.hist)

# Create the pair plots
corr_cross(data_wna[,-c(1,2)], # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           #top = 10  display top 10 couples of variables (by correlation coefficient)
           method = "spearman"
)
# From the above histogram, it is clear that there are 22 from 28 significant correlations.
# Although there is a material indication of multicollinearity, there are two variables, poison_f_to_m and suicide_f_to_m, 
# that are not much correlated with the others. The variable suicide_f_to_m shows an absolute spearman correlation coefficient 
# smaller than 0.3 with most of its combinations. In order to apply PCA, these two variables will be excluded from the 
# analysis dataset.

data_wna1 <- data_wna[,-c(7,8)]

# STEP 2: Outlier Detection and Removal#
model_od <- glm(success ~., data = data_wna1[,-1], family = binomial)
# (A) Cook's distance measures the influence of each observation by excluding points when fitting a model.
# In general, observations with cook’s distance greater than 4 times the mean may be classified as influential. 
cooksd <- cooks.distance(model_od) 
par(mfrow=c(1,1))
plot(cooksd, pch="*", cex=2, main="Influential Observations by Cook's Distance", ylab="Cook's distance", xlab="Country Index")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
data_wna1[influential, ]  # 10 influential observations.
# 3 largest values: # 9: Bahamas, 40: Georgia and 148: Syrian Arab Republic
# There is no specific reason why these rows turned out influential as a combination of extreme values appears to multiple variables.

# (B) STANDARDIZED RESIDUALS (NO!!!)
# Because of the multiplicity of influential values, our decision could be based on the standardized residuals' plot 
# and we will use the sample standard deviation as a cut-off for identifying outliers.
# Any data point that falls outside of 2 standard deviations (95%) will be considered as influential and will be removed.
# Extract model results
model.data <- augment(model) %>% mutate(index = 1:n()) 
# The data for the top 9 largest values, according to the Cook’s distance, can be displayed as follow:
# model.data %>% top_n(9, .cooksd)
# Plot the standardized residuals:
ggplot(model.data, aes(index, .std.resid)) + geom_point(aes(color = success), alpha = .5) + theme_bw()
# Filter potential influential data points with abs(.std.res) > 2:
model.data %>% filter(abs(.std.resid) > 2) # 3 influential data points: 21,117,127

# Because of the multiplicity of influential values, our decision will focus on the influence of outliers.
# Cook's distance combines residual size with leverage to detect the points that have the biggest impact on the regression line.
# That is the reason why we will exclude the 10 influential countries (5,9% <10% of dataset) from our analysis dataset.
data_wna2 <- data_wna1[-influential,]

# STEP 3: ADEQUACY AND DATA REDUCTION ABILITY OF THE FINAL SAMPLE SIZE #
KMO(data_wna2[,-c(1,2)])
# Kaiser-Meyer-Olkin (KMO) measure ranges from 0.71 to 0.89 for each item and 0.77 overall indicating sampling adequacy.
# Overall MSA =  0.77
# MSA for each item = 
# health_exp_cap      UV_rad      mat_death    gincome_cap        doctors       neo_deaths 
#       0.71           0.89           0.73           0.72           0.81           0.79 

bart_spher(data_wna2[,-c(1,2)], use = c("everything"))
# cortest.bartlett(data_wna2[,-c(1,2)], n = NULL,diag=TRUE)
# Bartlett’s test null hypothesis is that all true correlations between variables in the data set are zero.
# As it was expected, the p-value is much lower than 0.05 and as a result this null hypothesis is rejected, confirming 
# the data reduction ability of our final sample size.

# Pre-steps for PCA:
# 1. The first component captures the largest variability among the components.
# There is no other component with greater variability than the first principal component.
# 2. The original predictors should be normalized in order to avoid insane choices for principal components, caused by their different scales.
# 3. In order not to hammer down the model capability of generalization, training and test sets will be separately kept.
# PCA implementation on training data and prediction using these components on test data.

# After imputing missing values with the mean and discarding influential data points, we remove the dependent
# variable (response) and the country which is also an identifier variable.
pca_data <- subset(data_wna2, select = -c(success, Country))     
# check available variables
colnames(pca_data)
str(pca_data) # all predictors are numeric variables

# Split randomly the data into training (75%) and test set (25%).
# The training set will be used for the PCA implementation followed by the prediction on the test set using these components.
set.seed(123) # for same results in case of reproducibility in the future
# Now Selecting 75% of data as sample from total 'n' rows of the data   
train.samples <- sample.int(n = nrow(pca_data), size = floor(.75*nrow(pca_data)), replace = F)
pca.train <- pca_data[train.samples, ]
pca.test  <- pca_data[-train.samples, ]

# Normalization: 
# By using R function prcomp(,scale. = T), all predictors are centered with mean equals to zero and standard deviation equals to 1.
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

# The five measures refer to :
# a) respective mean and standard deviation, denoted as center and scale, of the pre-normalized variables.
prin_comp$center # mean
prin_comp$scale #standard deviation

# b) The most important measure called rotation refers to the principal component loading. 
# Each column of rotation matrix contains the principal component loading vector.
prin_comp$rotation
# In our case, rotation returns 6 principal components loadings with maximum = min(n-1,p) = min(120-1,6) = 6

# 3. Instead of manually multiplying the loading with the data for computing the principal component score vector,
# the measure x gives all the scores in a matrix with 120x6 dimension.
dim(prin_comp$x)

# 4. standard deviation of each principal component
std_dev <- prin_comp$sdev
pr_var <- std_dev^2 # variance or eigenvalue is the same
pr_var
# or 
eig.val <- get_eigenvalue(prin_comp)
eig.val
# Having the aim to retain as much information as possible, we keep the components which explain the maximum variance 
# by dividing the variance by the sum of total variance.
prop_varex <- pr_var/sum(pr_var) # proportion of variance explained
prop_varex

summary(prin_comp)
# Showing material difference, 2 components explain 61.3% and 16.2% variance respectively
# with the rest ones explaning around or less than 10% of the total variance.

################# PCA Visualization ###############

# Scree plot helps to access which components explain the most of variability in the data.
par(mfrow=c(1,2))
#scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b", main="Scree plot")
# fviz_eig(prin_comp,addlabels = TRUE,main= "Scree plot: Eigenvalues of Principal components",linecolor ="red") + theme_minimal()
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b",main="Cumulative scree plot")

# Scree plot suggests that 2 components explain around 77.5% variance in the data, meaning that using PCA the 6 predictors are reduced to 2 without making compromisies on explained variance.
# A clearer picture is given by the cumulative variance plot where the four last components do not make huge difference. 
# For this reason, the selected number of components, which will be considered as predictor variables, is 2 [PC1 to PC2] and proceed to the modelling stage.

# With the following plots, we assess the contribution of variables to the principal components.
# 1. Plot of the variables/ biplot
# This plot shows the relationship between variables, the quality of their representation on the factor map, the correlations between variables and the dimensions.
# The variables with sum of cos2 close to 1 are well represented by 2 dimensions.
fviz_famd_var(prin_comp, choice="var", repel = TRUE, ggtheme=theme(axis.text=element_text(size=10)), col.var = "cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),title="Correlation Graph for Representativeness")
# biplot(prin_comp, scale = 0) # ensure scaled arrows for the representation of loadings by scale = 0 
# focus on the extreme ends (top, bottom, left, right) of this graph
fviz_famd_var(prin_comp, choice="var", repel = TRUE, col.var = "contrib", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),title="Correlation Graph for Contribution")
# From the plot above, it is suggested that the variables with the greatest contribution in the first dimension
# are gincome_cap, doctors and health_exp_cap, while mat_death is contributing mostly to the second dimension.

# 2. Contribution to the first dimension/PC1
fviz_contrib(prin_comp,"var",axes=1)
# The red dashed line indicates the expexted average in case of uniform contributions.
# 3. Contribution to the second dimension/PC2
fviz_contrib(prin_comp,"var",axes=2)
# The red dashed line indicates the expexted average in case of uniform contributions.

################ PREDICTIVE MODELLING WITH PCA COMPONENTS ################ 
# Once you have done the analysis with PCA, you may want to look into whether the new variables
# can predict some phenomena well. This is kinda like machine learning: Whether features can 
# classify the data well. 

# keep only training and test data from the data analysis dataset without NAs
train.set <- data_wna2[train.samples,] 
test.set <- data_wna2[-train.samples,]

# 1. Fitting Logistic Regression to the Training set

# add a training set with principal components
train.data <- data.frame(success = train.set$success, prin_comp$x)

# we are interested in first 2 PCAs
train.data <- train.data[,1:3]

model1 <- glm(success ~ PC1 + PC2, family = binomial,data=train.data)
summary(model1)

# 2. Predicting the Test set results
#transform test set into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- data.frame(test.set$success,test.data)
#select the first 2 components
test.data <- test.data[,1:3]
#make prediction on test data
prob_pred = predict(model1, type = 'response', newdata = test.data[-1])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

###### MODEL EVALUATION ######
# Making the Confusion Matrix
cm = table(test.data[,1], y_pred)
library(ipred)
library(caret)
confusionMatrix(cm)

# Visualize confusion matrix
cm_v <- confusionMatrix(data = factor(y_pred), reference = factor(test.data[,1]))
draw_confusion_matrix(cm_v)
# Accuracy rate = Correct/Total = (7+29)/40 = 36/40 = 90%
# Type I Error = FP/actual no = 2/9 = 0.22 = 22%
# Type II Error = FN/actual yes = 2/32 = 0.062 = 6.2%

# Precision: Precision is about being precise, i.e., how accurate your model is. 
# In other words, you can say, when a model makes a prediction, how often it is correct. 
# In OUR prediction case, when our Logistic Regression model predicted customers will buy 
# the magazine 69% of the time.
# Recall or Sensitivity: If there are women that bought the magazine in test data and 
# your Logistic Regression model can identify it 85% of the time.

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

# ROC Curve
# Receiver Operating Characteristic(ROC) curve is a plot of the true positive rate(Recall) 
# against the false positive rate. It shows the tradeoff between sensitivity and specificity.
par(mfrow=c(1,1))
roc.curve(test.data[,1], y_pred, plotit = TRUE, add.roc = TRUE, n.thresholds=100)
# AUC(Area Under Curve) score for our case is 0.857. AUC score 1 represents perfect classifier, 
# and 0.5 represents a worthless classifier.









#############################################################################################
# DETECTING OUTLIERS OR EXTREME VALUES
# Values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers.
# Values above Q3 + 3xIQR or below Q1 - 3xIQR are considered as extreme points (or extreme outliers).
# Q1 and Q3 are the first and third quartile, respectively. IQR is the interquartile range (IQR = Q3 - Q1).

data_wna %>% identify_outliers(health_exp_cap) # 21 outliers, 18 extreme values
data_wna %>% identify_outliers(UV_rad) # 1 outlier, 0 extreme values
data_wna %>% identify_outliers(mat_death) # 28 outliers, 10 extreme values
data_wna %>% identify_outliers(gincome_cap) # 23 outliers, 6 extreme values
data_wna %>% identify_outliers(suicide_f_to_m) # 13 outliers, 6 extreme values
data_wna %>% identify_outliers(poison_f_to_m) # 11 outliers, 8 extreme values
data_wna %>% identify_outliers(doctors) # 2 outliers, 0 extreme values
data_wna %>% identify_outliers(neo_deaths) # 0 outliers, 0 extreme values










