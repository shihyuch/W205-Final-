library(car)
library(dplyr)
# load the data 
# C:\Data Science App\Data Retrival\Final Project\Unemployment LR

dataDir="C:\\Data Science App\\Data Retrival\\Final Project\\Unemployment LR\\Combined.csv"
df <- read.csv(dataDir, header=TRUE)
Hgih_School_Percentage <- df$HgihSchoolPercentage
pre_Hgih_School_Percentage = Hgih_School_Percentage[1:60]
inn_Hgih_School_Percentage = Hgih_School_Percentage[61:96]
aft_Hgih_School_Percentage = Hgih_School_Percentage[97:144]


Min_Wage <- df$MinWage
pre_Min_Wage = Min_Wage[1:60]
inn_Min_Wage = Min_Wage[61:96]
aft_Min_Wage = Min_Wage[97:144]

G_D_P <- df$GDP
pre_G_D_P = G_D_P[1:60]
inn_G_D_P = G_D_P[61:96]
aft_G_D_P = G_D_P[97:144]

Unemployment_Rate <- df$UnemploymentRate
pre_Unemployment_Rate = Unemployment_Rate[1:60]
inn_Unemployment_Rate = Unemployment_Rate[61:96]
aft_Unemployment_Rate = Unemployment_Rate[97:144]

### Education and Unemployment ### 

### pre recession 
pre_LR_model = lm(pre_Hgih_School_Percentage ~ pre_Unemployment_Rate, data = df)

summary(pre_LR_model)

plot(pre_Hgih_School_Percentage, pre_Unemployment_Rate, xlab="pre_Hgih_School_Percentage",
     ylab="pre_Unemployment_Rate") 
abline(pre_LR_model) 

### inn recession 
inn_LR_model = lm(inn_Hgih_School_Percentage ~ inn_Unemployment_Rate, data = df)

summary(inn_LR_model)

plot(inn_Hgih_School_Percentage, inn_Unemployment_Rate, xlab="inn_Hgih_School_Percentage",
     ylab="inn_Unemployment_Rate") 
abline(inn_LR_model) 

### aft recession 
aft_LR_model = lm(aft_Hgih_School_Percentage ~ aft_Unemployment_Rate, data = df)

summary(aft_LR_model)

plot(aft_Hgih_School_Percentage, aft_Unemployment_Rate, xlab="aft_Hgih_School_Percentage",
     ylab="aft_Unemployment_Rate") 
abline(aft_LR_model) 


### SingleR_model ### 
### pre recession

LR_model = lm(aft_Hgih_School_Percentage ~ aft_Unemployment_Rate, data = df)

summary(LR_model)

plot(aft_Hgih_School_Percentage, aft_Unemployment_Rate, xlab="aft_Hgih_School_Percentage",
     ylab="pre_Unemployment_Rate") 
 

### MultiR_model ### 
### pre recession

pre_MultiR_model = lm(pre_Unemployment_Rate ~ pre_Min_Wage + pre_Hgih_School_Percentage + pre_G_D_P, data = df)

summary(pre_MultiR_model)


### inn recession

inn_MultiR_model = lm(inn_Unemployment_Rate ~ inn_Min_Wage + inn_Hgih_School_Percentage + inn_G_D_P, data = df)

summary(inn_MultiR_model)

### aft recession

aft_MultiR_model = lm(aft_Unemployment_Rate ~ aft_Min_Wage + aft_Hgih_School_Percentage + aft_G_D_P, data = df)

summary(aft_MultiR_model)

#plot(aft_Min_Wage, aft_Unemployment_Rate, xlab="aft_Min_Wage",
#     ylab="pre_Unemployment_Rate")    
