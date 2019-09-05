library(reshape2)
library(tidyverse)
library(psych)
library(GPArotation)
library(corrplot)
library(nFactors)
library(dplyr)
library(nnet)
library(MASS)

mke_data <- read.csv("/Users/Joey/Desktop/Snowden/MKE Factors Update_for Shion and Joe.csv")
chi_data <- read.csv("/Users/Joey/Desktop/Snowden/CHI Factors Update_for Shion and Joe.csv")
metadata <- read.csv("/Users/Joey/Desktop/Snowden/CHI_MKE_Metadata.csv")

# Removing first NA column so full dataset operations dont fail
mke_data$D_R <- NULL
chi_data$D_R <- NULL

#
# MILWAUKEE MODELS
#

# Loadings found through factor analysis
# affluence, immigration measure, economic disadvantage, residential stability
#
disadvantage_mke_fl <- c(0.9, 0.9, 0.8, 0.7, -0.7, 0.7)
immigration_mke_fl <- c(1, 0.8)
resStability_mke_fl <- c(0.9, 0.7)
affluence_mke_fl <- c(0.7, 0.5)

disadvantage_mke <- rowMeans(mke_data %>% dplyr::select(BelowProp, SSIProp, MandFHH_ch, Unemployed, Inc75K_, 
                                                        PAIProp) * disadvantage_mke_fl)
immigration_mke <- rowMeans(mke_data %>% dplyr::select(HispProp, ForeignPro) * immigration_mke_fl)
resStability_mke <- rowMeans(mke_data %>% dplyr::select(OwnerProp, SameProp) * resStability_mke_fl)
affluence_mke <- rowMeans(mke_data %>% dplyr::select(BAup, OccuProp) * affluence_mke_fl)

# creates population density vector
#
populationDensity_mke <- numeric()
for( x in 1:nrow(mke_data) ){
  popDensity <- mke_data$Total_Pop[x] / mke_data$SqMi[x]
  populationDensity_mke[x] <- popDensity
}

#
# Creating the models
#

# building new dataframe of factors + other needed variables
# dataframe contains respective crime, latent factors of economic disadvantage, immigration measure, residential stability, 
#   affluence, population density, respective lag variable, and EthnHerter
#
mke_property_modelFrame <- data.frame(mke_data$Property, mke_data$pclag, disadvantage_mke, immigration_mke, 
                                      resStability_mke, affluence_mke, populationDensity_mke, mke_data$EthnHerter)
mke_violent_modelFrame <- data.frame(mke_data$Violent, mke_data$vclag, disadvantage_mke, immigration_mke, 
                                     resStability_mke, affluence_mke, populationDensity_mke, mke_data$EthnHerter)

# linear models
mke_property_model_lm <- lm(mke_property_modelFrame$mke_data.Property ~ disadvantage_mke + immigration_mke + 
                              resStability_mke + affluence_mke + populationDensity_mke + mke_data.pclag + 
                              mke_data.EthnHerter, data=mke_property_modelFrame)
mke_violent_model_lm <- lm(mke_violent_modelFrame$mke_data.Violent ~ disadvantage_mke + immigration_mke + 
                             resStability_mke + affluence_mke + populationDensity_mke + mke_data.vclag + 
                             mke_data.EthnHerter, data=mke_violent_modelFrame)
summary(mke_property_model_lm)
summary(mke_violent_model_lm)

# negative binomial models
#
# throws an error: does not like the 0 values in certain socioeconomic categories
# rows 1, 24, 238, & 239 have 0.0000 in all socioeconomic categories of the mke_data dataset
#
mke_property_model_nb <- glm.nb(mke_property_modelFrame$mke_data.Property ~ disadvantage_mke + immigration_mke +
                                  resStability_mke + affluence_mke+populationDensity_mke + mke_data.pclag + 
                                  mke_data.EthnHerter, data=mke_property_modelFrame)
mke_violent_model_nb <- glm.nb(mke_violent_modelFrame$mke_data.Violent ~ disadvantage_mke + immigration_mke +
                                 resStability_mke + affluence_mke + populationDensity_mke + mke_data.vclag + 
                                 mke_data.EthnHerter, data=mke_violent_modelFrame)
summary(mke_property_model_nb)
summary(mke_violent_model_nb)


#
# CHICAGO MODELS
#

# Loadings found through factor analysis
# affluence, immigration measure, economic disadvantage, residential stability
#
disadvantage_chi_fl <- c(0.8, 0.8, 0.7, 0.6)
affluence_chi_fl <- c(0.8, 0.7, 0.7, -0.5)
immigration_chi_fl <- c(0.9, 0.8)
resStability_chi_fl <- c(0.8, 0.7)

# row means
disadvantage_chi <- rowMeans(chi_data %>% dplyr::select(BelowProp, MandFHH_ch, SSIProp, PAIProp) * disadvantage_chi_fl)
affluence_chi <- rowMeans(chi_data %>% dplyr::select(BAup, OccuProp, Inc75K_, Unemployed) * affluence_chi_fl)
immigration_chi <- rowMeans(chi_data %>% dplyr::select(HispProp, ForeignPro) * immigration_chi_fl)
resStability_chi <- rowMeans(chi_data %>% dplyr::select(OwnerProp, SameProp) * resStability_chi_fl)

# creates population density vector
#
populationDensity_chi <- numeric()
for( x in 1:nrow(chi_data) ){
  popDensity <- chi_data$Total_Pop[x] / chi_data$SqMi[x]
  populationDensity_chi[x] <- popDensity
}

#
# Creating the models
#

# building new dataframe of factors + other needed variables
# dataframe contains respective crime, latent factors of economic disadvantage, immigration measure, residential stability, 
#   affluence, population density, respective lag variable, and EthnHerter
#
chi_property_modelFrame <- data.frame(chi_data$Property, chi_data$pclag, disadvantage_chi, immigration_chi, 
                                      resStability_chi, affluence_chi, populationDensity_chi, chi_data$EthnHerter)
chi_violent_modelFrame <- data.frame(chi_data$Violent, chi_data$vclag, disadvantage_chi, immigration_chi, 
                                     resStability_chi, affluence_chi, populationDensity_chi, chi_data$EthnHerter)

# linear models
chi_property_model_lm <- lm(chi_property_modelFrame$chi_data.Property ~ disadvantage_chi + immigration_chi + 
                              resStability_chi + affluence_chi + populationDensity_chi, + chi_data.pclag + 
                              chi_data.EthnHerter, data=chi_property_modelFrame)
chi_violent_model_lm <- lm(chi_violent_modelFrame$chi_data.Violent ~ disadvantage_chi + immigration_chi + 
                             resStability_chi + affluence_chi + populationDensity_chi, + chi_data.vclag + 
                             chi_data.EthnHerter, data=chi_violent_modelFrame)
summary(chi_property_model_lm)
summary(chi_violent_model_lm)

# negative binomial models
chi_property_model_nb <- glm.nb(chi_property_modelFrame$chi_data.Property ~ disadvantage_chi + immigration_chi +
                                  resStability_chi + affluence_chi + populationDensity_chi + chi_data.pclag + 
                                  chi_data.EthnHerter, data=chi_property_modelFrame)
chi_violent_model_nb <- glm.nb(chi_violent_modelFrame$chi_data.Violent ~ disadvantage_chi + immigration_chi +
                                 resStability_chi + affluence_chi + populationDensity_chi + chi_data.vclag + 
                                 chi_data.EthnHerter, data=chi_violent_modelFrame)
summary(chi_property_model_nb)
summary(chi_violent_model_nb)
