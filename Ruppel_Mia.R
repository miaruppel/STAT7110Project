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




