library(reshape2)
library(tidyverse)
library(psych)
library(GPArotation)
library(corrplot)
library(nFactors)
library(dplyr)
library(nnet)
library(MASS)

rm(list=ls())

metadata <- read.csv("/Users/Joey/Desktop/Snowden/Data/CHI_MKE_Metadata.csv")
new_chi <- read.delim("/Users/Joey/Desktop/Snowden/Data/CHI Factors Update_for Shion and Joe_corrected.csv")
new_mke <- read.csv("/Users/Joey/Desktop/Snowden/Data/MKE Factors Update_for Shion and Joe_corrected.csv")

# Removing first NA column so full dataset operations dont fail
new_chi$D_R <- NULL
new_mke$D_R <- NULL

#
# MILWAUKEE MODELS
#

# Loadings found through factor analysis
# affluence, immigration measure, economic disadvantage, residential stability
#
disadvantage_mke_fl <- c(0.8, 0.8, 0.8, 0.7, -0.7, 0.7, -0.6)
immigration_mke_fl <- c(1, 0.9)
resStability_mke_fl <- c(0.9, 0.8)
affluence_mke_fl <- c(0.6)

mke_data <- new_mke

disadvantage_mke <- rowMeans(mke_data %>% dplyr::select(BelowProp, MandFHH_ch, SSIProp, Unemployment_fixed, Inc75K_, 
                                                        PAIProp, BAup) * disadvantage_mke_fl)
immigration_mke <- rowMeans(mke_data %>% dplyr::select(HispProp, ForeignPro) * immigration_mke_fl)
resStability_mke <- rowMeans(mke_data %>% dplyr::select(OwnerProp, SameProp) * resStability_mke_fl)
affluence_mke <- rowMeans(mke_data %>% dplyr::select(OccuProp) * affluence_mke_fl)


# creates population density vector
#
populationDensity_mke <- numeric()
for( x in 1:nrow(mke_data) ){
  if (mke_data$SqMi[x] == 0.0){
    populationDensity_mke[x] <- 0
  }else{
    popDensity <- mke_data$Total_Pop[x] / mke_data$SqMi[x]
    populationDensity_mke[x] <- popDensity
  }
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

# negative binomial models
#
mke_property_model_nb <- glm.nb(mke_data.Property ~ disadvantage_mke + immigration_mke +
                                resStability_mke + affluence_mke + populationDensity_mke + mke_data.pclag +
                                mke_data.EthnHerter, data=mke_property_modelFrame)

mke_violent_model_nb <- glm.nb(mke_data.Violent ~ disadvantage_mke + immigration_mke +
                               resStability_mke + affluence_mke + populationDensity_mke  + mke_data.vclag+ 
                               mke_data.EthnHerter, data=mke_violent_modelFrame)
summary(mke_property_model_nb)
summary(mke_violent_model_nb)

#
# CHICAGO MODELS
#

# Loadings found through factor analysis
# affluence, immigration measure, economic disadvantage, residential stability
#
disadvantage_chi_fl <- c(0.8, 0.8, 0.7, 0.7, -0.7, 0.7)
affluence_chi_fl <- c(0.8, 0.7)
immigration_chi_fl <- c(0.8, 0.8)
resStability_chi_fl <- c(0.8, 0.7)

chi_data <- new_chi

# row means
disadvantage_chi <- rowMeans(chi_data %>% dplyr::select(BelowProp, MandFHH_ch, SSIProp, Unemployed_Fixed, Inc75K_, PAIProp) * disadvantage_chi_fl)
affluence_chi <- rowMeans(chi_data %>% dplyr::select(BAup, OccuProp) * affluence_chi_fl)
immigration_chi <- rowMeans(chi_data %>% dplyr::select(ForeignPro, HispProp) * immigration_chi_fl)
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

# negative binomial models
chi_property_model_nb <- glm.nb(chi_data.Property ~ disadvantage_chi + immigration_chi +
                                  resStability_chi + affluence_chi + populationDensity_chi + chi_data.pclag + 
                                  chi_data.EthnHerter, data=chi_property_modelFrame)
chi_violent_model_nb <- glm.nb(chi_data.Violent ~ disadvantage_chi + immigration_chi +
                                 resStability_chi + affluence_chi + populationDensity_chi + chi_data.vclag + 
                                 chi_data.EthnHerter, data=chi_violent_modelFrame)
summary(chi_property_model_nb)
summary(chi_violent_model_nb)


# helper functions for renaming the tables
#
changeNamesProp <- function(table){
  colnames(table) <- c('Intercept', 'econ_Disadvantage', 'Conc_Immigration', 
                       'Res_Stability', 'Conc_Affluence', 'Pop_Density', 'pclag', 'EthnHerter')
  rownames(table) <- c('MKE','CHI')
  table <- as.table(table)
  return(table)
}
changeNamesPropPval <- function(table){
  colnames(table) <- c('Intercept', 'econ_Disadvantage', 'Conc_Immigration', 
                       'Res_Stability', 'Conc_Affluence', 'Pop_Density', 'pclag', 'EthnHerter')
  rownames(table) <- c('MKE','Signif_MKE','CHI','Signif_CHI')
  table <- as.table(table)
  return(table)
}
changeNamesViolent <- function(table){
  colnames(table) <- c('Intercept', 'Econ_Disadvantage', 'Conc_Immigration', 
                       'Res_Stability', 'Conc_Affluence', 'Pop_Density', 'pclag', 'EthnHerter')
  rownames(table) <- c('MKE','CHI')
  table <- as.table(table)
  return(table)
}
changeNamesViolentPval <- function(table){
  colnames(table) <- c('Intercept', 'Econ_Disadvantage', 'Conc_Immigration', 
                       'Res_Stability', 'Conc_Affluence', 'Pop_Density', 'pclag', 'EthnHerter')
  rownames(table) <- c('MKE','Signif_MKE','CHI','Signif_CHI')
  table <- as.table(table)
  return(table)
}

# builds matrices of the coefficients from the 4 negative binomial models above
#
# coefficient estimate
#
mke_prop_coef <- coef(summary(mke_property_model_nb))[,1]
mke_violent_coef <- coef(summary(mke_violent_model_nb))[,1]
chi_prop_coef <- coef(summary(chi_property_model_nb))[,1]
chi_violent_coef <- coef(summary(chi_violent_model_nb))[,1]
property_coef <- matrix(c(mke_prop_coef, chi_prop_coef), ncol=8, byrow=TRUE)
violent_coef <- matrix(c(mke_violent_coef, chi_violent_coef), ncol=8, byrow=TRUE)
property_coefEstimate <- changeNamesProp(property_coef)
violent_coefEstimate <- changeNamesViolent(violent_coef)

property_coefEstimate
violent_coefEstimate

# std error
#
mke_prop_stderr <- coef(summary(mke_property_model_nb))[,2]
mke_violent_stderr <- coef(summary(mke_violent_model_nb))[,2]
chi_prop_stderr <- coef(summary(chi_property_model_nb))[,2]
chi_violent_stderr <- coef(summary(chi_violent_model_nb))[,2]
property_stderr <- matrix(c(mke_prop_stderr, chi_prop_stderr), ncol=8, byrow=TRUE)
violent_stderr <- matrix(c(mke_violent_stderr, chi_violent_stderr), ncol=8, byrow=TRUE)
property_stderr <- changeNamesProp(property_stderr)
violent_stderr <- changeNamesViolent(violent_stderr)

property_stderr
violent_stderr

# t value
#
mke_prop_tval <- coef(summary(mke_property_model_nb))[,3]
mke_violent_tval <- coef(summary(mke_violent_model_nb))[,3]
chi_prop_tval <- coef(summary(chi_property_model_nb))[,3]
chi_violent_tval <- coef(summary(chi_violent_model_nb))[,3]
property_tval <- matrix(c(mke_prop_tval, chi_prop_tval), ncol=8, byrow=TRUE)
violent_tval <- matrix(c(mke_violent_tval, chi_violent_tval), ncol=8, byrow=TRUE)
property_tval <- changeNamesProp(property_tval)
violent_tval <- changeNamesViolent(violent_tval)

property_tval
violent_tval

# p value
#
mke_prop_pval <- coef(summary(mke_property_model_nb))[,4]
mke_violent_pval <- coef(summary(mke_violent_model_nb))[,4]
chi_prop_pval <- coef(summary(chi_property_model_nb))[,4]
chi_violent_pval <- coef(summary(chi_violent_model_nb))[,4]
signif_mke_prop <- c('***','*',' ','.',' ','***','***','*')
signif_mke_violent <- c('***',' ',' ','***',' ','***','***','**')
signif_chi_prop <- c('***',' ',' ','***',' ','***','***',' ')
signif_chi_violent <- c('***',' ',' ','***','***','***','***','.')
property_pval <- matrix(c(mke_prop_pval, signif_mke_prop, chi_prop_pval, signif_chi_prop), ncol=8, byrow=TRUE)
violent_pval <- matrix(c(mke_violent_pval, signif_mke_violent, chi_violent_pval, signif_chi_violent), ncol=8, byrow=TRUE)
property_pval <- changeNamesPropPval(property_pval)
violent_pval <- changeNamesViolentPval(violent_pval)

property_pval
violent_pval


