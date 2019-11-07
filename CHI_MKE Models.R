library(reshape2)
library(formattable)
library(tidyverse)
library(data.table)
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


#
# comparing the models
#

anova(mke_property_model_nb, mke_violent_model_nb, test='Chisq')
anova(chi_property_model_nb, chi_violent_model_nb, test='Chisq')


# creating table for MKE property NB model
#
mke_prop_est <- coef(summary(mke_property_model_nb))[,1] # estimate
mke_prop_stdErr <- coef(summary(mke_property_model_nb))[,2] # std error
mke_prop_zVal <- coef(summary(mke_property_model_nb))[,3] # z value
mke_prop_pVal <- coef(summary(mke_property_model_nb))[,4] # p value
mke_prop_signifCodes <- vector() # signif codes for easier p value viewing
for( i in 1:length(coef(summary(mke_property_model_nb))[,4])){
  val <- coef(summary(mke_property_model_nb))[,4][i]
  if (val < 1 && val > 0.1){
    mke_prop_signifCodes[i] <- ' '
  }else if(val < 0.1 && val > 0.05){
    mke_prop_signifCodes[i] <- '.'
  }else if(val < 0.05 && val > 0.01){
    mke_prop_signifCodes[i] <- '*'
  }else if(val < 0.01 && val > 0.001){
    mke_prop_signifCodes[i] <- '**'
  }else if(val < 0.001){
    mke_prop_signifCodes[i] <- '***'
  }
  else{
    mke_prop_signifCodes[i] <- 'NA'
  }
}
mke_prop_table_nb <- data.table('Variables'=c('(Intercept)','Disadvantage','Immigration',
                                              'ResStability','Affluence','PopDensity','pclag','EthnHerter'),
                                'Estimate'=mke_prop_est,
                                'Std. Error'=mke_prop_stdErr,
                                'z value'=mke_prop_zVal,
                                'p value'=mke_prop_pVal,
                                'Signif. codes'=mke_prop_signifCodes)
mke_prop_table_nb <- mke_prop_table_nb %>% arrange(match(Variables, c('(Intercept)','pclag','EthnHerter',
                                                                      'PopDensity','Disadvantage','Immigration',
                                                                      'ResStability','Affluence')))
formattable(mke_prop_table_nb, align=c('l'),
            list('Variables' = formatter("span",
                                         style = x ~ style(color = ifelse(x=='pclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black')))))))



# creating table for MKE violent NB model
mke_viol_est <- coef(summary(mke_violent_model_nb))[,1] # estimate
mke_viol_stdErr <- coef(summary(mke_violent_model_nb))[,2] # std error
mke_viol_zVal <- coef(summary(mke_violent_model_nb))[,3] # z value
mke_viol_pVal <- coef(summary(mke_violent_model_nb))[,4] # p value
mke_viol_signifCodes <- vector() # signif codes for easier p value viewing
for( i in 1:length(coef(summary(mke_violent_model_nb))[,4])){
  val <- coef(summary(mke_violent_model_nb))[,4][i]
  if (val < 1 && val > 0.1){
    mke_viol_signifCodes[i] <- ' '
  }else if(val < 0.1 && val > 0.05){
    mke_viol_signifCodes[i] <- '.'
  }else if(val < 0.05 && val > 0.01){
    mke_viol_signifCodes[i] <- '*'
  }else if(val < 0.01 && val > 0.001){
    mke_viol_signifCodes[i] <- '**'
  }else if(val < 0.001){
    mke_viol_signifCodes[i] <- '***'
  }
  else{
    mke_viol_signifCodes[i] <- 'NA'
  }
}
mke_viol_table_nb <- data.table('Variables'=c('(Intercept)','Disadvantage','Immigration',
                                              'ResStability','Affluence','PopDensity','vclag','EthnHerter'),
                                'Estimate'=mke_viol_est,
                                'Std. Error'=mke_viol_stdErr,
                                'z value'=mke_viol_zVal,
                                'p value'=mke_viol_pVal,
                                'Signif. codes'=mke_viol_signifCodes)
mke_viol_table_nb <- mke_viol_table_nb %>% arrange(match(Variables, c('(Intercept)','vclag','EthnHerter',
                                                                      'PopDensity','Disadvantage','Immigration',
                                                                      'ResStability','Affluence')))
formattable(mke_viol_table_nb, align=c('l'),
            list('Variables' = formatter("span",
                                         style = x ~ style(color = ifelse(x=='vclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black')))))))


# creating table for CHI property NB model
chi_prop_est <- coef(summary(chi_property_model_nb))[,1] # estimate
chi_prop_stdErr <- coef(summary(chi_property_model_nb))[,2] # std error
chi_prop_zVal <- coef(summary(chi_property_model_nb))[,3] # z value
chi_prop_pVal <- coef(summary(chi_property_model_nb))[,4] # p value
chi_prop_signifCodes <- vector() # signif codes for easier p value viewing
for( i in 1:length(coef(summary(chi_property_model_nb))[,4])){
  val <- coef(summary(chi_property_model_nb))[,4][i]
  if (val < 1 && val > 0.1){
    chi_prop_signifCodes[i] <- ' '
  }else if(val < 0.1 && val > 0.05){
    chi_prop_signifCodes[i] <- '.'
  }else if(val < 0.05 && val > 0.01){
    chi_prop_signifCodes[i] <- '*'
  }else if(val < 0.01 && val > 0.001){
    chi_prop_signifCodes[i] <- '**'
  }else if(val < 0.001){
    chi_prop_signifCodes[i] <- '***'
  }
  else{
    chi_prop_signifCodes[i] <- 'NA'
  }
}
chi_prop_table_nb <- data.table('Variables'=c('(Intercept)','Disadvantage','Immigration',
                                              'ResStability','Affluence','PopDensity','pclag','EthnHerter'),
                                'Estimate'=chi_prop_est,
                                'Std. Error'=chi_prop_stdErr,
                                'z value'=chi_prop_zVal,
                                'p value'=chi_prop_pVal,
                                'Signif. codes'=chi_prop_signifCodes)
chi_prop_table_nb <- chi_prop_table_nb %>% arrange(match(Variables, c('(Intercept)','pclag','EthnHerter',
                                                                      'PopDensity','Disadvantage','Immigration',
                                                                      'ResStability','Affluence')))
formattable(chi_prop_table_nb, align=c('l'),
            list('Variables' = formatter("span",
                                         style = x ~ style(color = ifelse(x=='pclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black')))))))

# table for CHI violent NB model
chi_viol_est <- coef(summary(chi_violent_model_nb))[,1] # estimate
chi_viol_stdErr <- coef(summary(chi_violent_model_nb))[,2] # std error
chi_viol_zVal <- coef(summary(chi_violent_model_nb))[,3] # z value
chi_viol_pVal <- coef(summary(chi_violent_model_nb))[,4] # p value
chi_viol_signifCodes <- vector() # signif codes for easier p value viewing
for( i in 1:length(coef(summary(chi_violent_model_nb))[,4])){
  val <- coef(summary(chi_violent_model_nb))[,4][i]
  if (val < 1 && val > 0.1){
    chi_viol_signifCodes[i] <- ' '
  }else if(val < 0.1 && val > 0.05){
    chi_viol_signifCodes[i] <- '.'
  }else if(val < 0.05 && val > 0.01){
    chi_viol_signifCodes[i] <- '*'
  }else if(val < 0.01 && val > 0.001){
    chi_viol_signifCodes[i] <- '**'
  }else if(val < 0.001){
    chi_viol_signifCodes[i] <- '***'
  }
  else{
    chi_viol_signifCodes[i] <- 'NA'
  }
}
chi_viol_table_nb <- data.table('Variables'=c('(Intercept)','Disadvantage','Immigration',
                                              'ResStability','Affluence','PopDensity','vclag','EthnHerter'),
                                'Estimate'=chi_viol_est,
                                'Std. Error'=chi_viol_stdErr,
                                'z value'=chi_viol_zVal,
                                'p value'=chi_viol_pVal,
                                'Signif. codes'=chi_viol_signifCodes)
chi_viol_table_nb <- chi_viol_table_nb %>% arrange(match(Variables, c('(Intercept)','vclag','EthnHerter',
                                                                      'PopDensity','Disadvantage','Immigration',
                                                                      'ResStability','Affluence')))
formattable(chi_viol_table_nb, align=c('l'),
            list('Variables'=formatter('span',
                                       style = x~style(color = ifelse(x=='vclag','red',ifelse(x=='PopDensity','green',ifelse(x=='EthnHerter','blue','black')))))))
summary(chi_violent_model_nb)


