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






