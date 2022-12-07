# STAT 7110 Big Data Analysis 
# 11/28/2022
# Mia Ruppel

rm(list=ls())
setwd('~/Documents/GitHub/STAT7110Project')

# load in data 
bigdata <- read.table('fat.dat.txt')

# convert case numbers to row names
rownames(bigdata) = bigdata[ ,1]
bigdata <- bigdata[ ,-1] 

# rename columns 
col_name <- c('%BF_Brozek', '%BF_Siri', 'Density', 'Age', 'Weight', 'Height', 
              'Adip_Index', 'Fat_Free_Weight','Neck_Circum', 'Chest_Circum', 'Abd_Circum', 'Hip_Circum', 
              'Thigh_Circum', 'Knee_Circum', 'Ankle_Circum', 'Ext_Bicep_Circum', 
              'Forearm_Circum', 'Wrist_Circum')
colnames(bigdata) <- col_name

# peaking at the data 
str(bigdata)

# summary statistics 
summary(bigdata)

# correlation matrix for all variables 
pairs(bigdata)

# distribution plot for BF% 
hist(bigdata$`%BF_Brozek`, breaks=10, col="blue", border="white", prob=T,
     main="Brozek's Body Fat Percentage", xlab="Brozek's Body Fat Percentage")
lines(density(bigdata$`%BF_Brozek`))

# scatter plots with trend lines
library(ggplot2)
# BF% and Adipose Index
ggplot(data = bigdata, mapping = aes(x = bigdata$`%BF_Brozek`, y = Adip_Index)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 3) +
  geom_smooth(method = "lm") +
  labs(title = "Brozek's Body Fat Percentage vs Adipose Tissue Index",
       x = "Brozek's Body Fat (%)",
       y = "Adipose Index (kg/m^2)") 
# BF% and Weight 
ggplot(data = bigdata, mapping = aes(x = bigdata$`%BF_Brozek`, y = Weight)) +
  geom_point(color = "dark green", alpha = .7, size = 3) +
  geom_smooth(method = "lm") +
  labs(title = "Brozek's Body Fat Percentage vs Weight",
       x = "Brozek's Body Fat (%)",
       y = "Weight (lbs)") 
# BF% and Abdomen Circumference 
ggplot(data = bigdata, mapping = aes(x = bigdata$`%BF_Brozek`, y = Abd_Circum)) +
  geom_point(color = "purple", alpha = .7, size = 3) +
  geom_smooth(method = "lm") +
  labs(title = "Brozek's Body Fat Percentage vs Abdomen Circumference",
       x = "Brozek's Body Fat (%)",
       y = "Abdomen Circumference (cm)") 

# covariance of variables 
cov(bigdata)
#correlation of variables
cor(bigdata)

#  simple linear regression model 
fit1 <- lm(`%BF_Brozek` ~ Adip_Index, data=bigdata)
summary(fit1) # adj R^2 of 0.5281

fitted(fit1)
residuals(fit1)

plot(bigdata$Adip_Index, bigdata$`%BF_Brozek`, 
     xlab="Adipose Index (kg/m^2)", 
     ylab="Body Fat Percentage (%)")
abline(fit1) # fairly linear, no need for polynomials most likely 

# multiple linear regression with interaction 
fit2 <- lm(`%BF_Brozek` ~ Adip_Index + Weight + Adip_Index:Weight, data=bigdata)
summary(fit2) # adj R^2 of 0.575 (not much better)

# multiple linear regression
fit3 <- lm(`%BF_Brozek` ~ Adip_Index + Weight + Abd_Circum + Chest_Circum, data=bigdata)
summary(fit3) # adj R^2 of 0.7153 (pretty good!)

par(mfrow=c(2,2))
plot(fit3) # seems to fit assumptions of normality, linearity, and constant variance 

# identifying  outliers (high difference between fitted and residual)
library(car)
library(leaps)
outlierTest(fit3) # one outlier, subject 39 

# identifying high leverage points (outliers with regards to other predictors)
hat.plot <- function(fit, mydata) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  text(hatvalues(fit), labels=rownames(mydata), cex=0.9, font=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
par(mfrow=c(1,1))
hat.plot(fit3, bigdata) # also shows subject 39 causing issues 

# identifying influential observations (have a disproportionate impact on the value of the model)
# Cook's distance
( cutoff <- 4/(nrow(bigdata)-length(fit$coefficients)-2) )
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red") # showing subject 39 being abnormal as well

# VERDICT: remove subject 39 from data 
bigdata_final <- bigdata[!(row.names(bigdata) %in% "39"), ]

# selecting the best fit model 
fit4 <- lm(`%BF_Brozek` ~ Adip_Index + Weight + Abd_Circum + Chest_Circum + Fat_Free_Weight + 
             Neck_Circum + Hip_Circum + Thigh_Circum + Knee_Circum + Ankle_Circum + Age +
             Ext_Bicep_Circum + Forearm_Circum + Wrist_Circum, data=bigdata_final)
summary(fit4) # adj R^2  of 0.9717

leaps <-regsubsets(`%BF_Brozek` ~ Adip_Index + Weight + Abd_Circum + Chest_Circum + Fat_Free_Weight + 
                     Neck_Circum + Hip_Circum + Thigh_Circum + Knee_Circum + Ankle_Circum + Age +
                     Ext_Bicep_Circum + Forearm_Circum + Wrist_Circum, data=bigdata_final, nbest=14)
plot(leaps, scale="adjr2") # shows with combination of predictive variables results in highest R^2
title('All Subsets Regression')

fit5 <- lm(`%BF_Brozek` ~ Adip_Index + Weight + Abd_Circum + Fat_Free_Weight + Chest_Circum +
            Thigh_Circum + Ankle_Circum + Ext_Bicep_Circum + Forearm_Circum, data=bigdata_final)
summary(fit5) # adj R^2 of 0.9718

# accessing normality 
qqPlot(fit5, simulate=TRUE, labels=row.names(bigdata_final),
       id=list(method="identify"), main="Q-Q Plot")

# accessing linearity 
crPlots(fit5)

# accessing constant variance 
ncvTest(fit5) # large p value = constant variance





