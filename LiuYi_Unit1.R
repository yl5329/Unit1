# Script to analyze and extract data from Picarro peaks

# Load Library
library(plyr)
library(ggplot2)
library(intervals)
library(memisc)
library(ggpubr)
library(cowplot)

# minimum detectable slopes (ppm/s) from Picrro G2201-i
CH4_min <- 0.0000033
CO2_min <- 0.001

# minimum detectable slopes (ppm/hr)
CH4_min_hr <- 0.0000033*(60*60)
CO2_min_hr <- 0.001*(60*60)

# Time zone correction for Picarro in hours (from GMT).
#In other words how many hours is the Picarro off from GMT.
TZ <- 4

# Create a master file with all Picarro data
Cmin <- list.files(pattern="*DataLog_User.dat", full.names=TRUE)
## create vector of total file number
num_raw_data <- length(Cmin)

# make sure pattern matches end of your config file name
params <- read.csv(list.files(pattern="*_config.csv"), header=T, stringsAsFactor=T)
names(params) <- c("Batch", "Assay_Time_hr",	"Start",	"End")

# make sure horizontal lines indicated steady state
plot(final2.df$HR_12CH4_dry ~ final2.df$diffEPOCH)
plot(final2.df$X12CO2_dry ~ final2.df$diffEPOCH)

#find the mean of that identified "steady state"#
analysisCO2 <- ddply(final2.df, c("UID"), summarise,
                     CO2_N    = sum(!is.na(X12CO2_dry)),
                     CO2_median = median(X12CO2_dry),
                     CO2_mean = mean(X12CO2_dry),
                     CO2_sd = sd(X12CO2_dry),
                     CO2_se   = CO2_sd / sqrt(CO2_N), 
                     CO2_min = min(X12CO2_dry),
                     CO2_max = max (X12CO2_dry),
                     time = mean(peakEpoch))


analysisCH4 <- ddply(final2.df, c("UID"), summarise,
                     CH4_N    = sum(!is.na(HR_12CH4_dry)),
                     CH4_median = median(HR_12CH4_dry),
                     CH4_mean = mean(HR_12CH4_dry),
                     CH4_sd = sd(HR_12CH4_dry),
                     CH4_se   = CH4_sd / sqrt(CH4_N), 
                     CH4_min = min(HR_12CH4_dry),
                     CH4_max = max (HR_12CH4_dry))

analysisCO2i <- ddply(final2.df, c("UID"), summarise,
                      CO2i_N    = sum(!is.na(Delta_Raw_iCO2)),
                      CO2i_median = median(Delta_Raw_iCO2),
                      CO2i_mean = mean(Delta_Raw_iCO2),
                      CO2i_sd = sd(Delta_Raw_iCO2),
                      CO2i_se   = CO2i_sd / sqrt(CO2i_N), 
                      CO2i_min = min(Delta_Raw_iCO2),
                      CO2i_max = max (Delta_Raw_iCO2))

analysisCH4i <- ddply(final2.df, c("UID"), summarise,
                      CH4i_N    = sum(!is.na(HR_Delta_iCH4_Raw)),
                      CH4i_median = median(HR_Delta_iCH4_Raw),
                      CH4i_mean = mean(HR_Delta_iCH4_Raw),
                      CH4i_sd = sd(HR_Delta_iCH4_Raw),
                      CH4i_se   = CH4i_sd / sqrt(CH4i_N), 
                      CH4i_min = min(HR_Delta_iCH4_Raw),
                      CH4i_max = max (HR_Delta_iCH4_Raw))

# visualization of data..
hist(analysisCO2$CO2_se)
boxplot(analysisCO2$CO2_se)
boxplot(analysisCO2$CO2_mean)
hist(analysisCH4$CH4_se)
boxplot(analysisCH4$CH4_se)
boxplot(analysisCH4$CH4_mean)
plot(analysisCH4$CH4_se~analysisCH4$CH4_mean)

# Create Output Files
#create models and extract summaries for 
#parameters of interest
lmCO2umol <- ddply(final5.df, c("SampleID"), summarise,
                   mCO2_slope_umol_hr =format(coef(lm(umolCO2~TIME_hr))[2], digits=3),
                   mCO2_intercept=format(coef(lm(umolCO2~TIME_hr))[1], digits=3),
                   mCO2_r2=format(summary(lm(umolCO2~TIME_hr))$adj.r.squared, digits=2))

lmCH4umol <- ddply(final5.df, c("SampleID"), summarise,
                   mCH4_slope_umol_hr =format(coef(lm(umolCH4~TIME_hr))[2], digits=3),
                   mCH4_intercept=format(coef(lm(umolCH4~TIME_hr))[1], digits=3),
                   mCH4_r2=format(summary(lm(umolCH4~TIME_hr))$adj.r.squared, digits=2))



lmCO2umol.df=lmCO2umol
lmCH4umol.df=lmCH4umol

MDCD <- ddply(final5.df, c("SampleID"), summarise,
              CO2_ppm_slope =format(coef(lm(CO2_mean~TIME_hr))[2], digits=3),
              CH4_ppm_slope=format(coef(lm(CH4_mean~TIME_hr))[2], digits=3),
              CO2_umol_pvalue=format(summary(lm(umolCO2~TIME_hr))$coefficients[2,4], digits=2),
              CH4_umol_pvalue=format(summary(lm(umolCH4~TIME_hr))$coefficients[2,4], digits=2))  
MDCD.df=MDCD
