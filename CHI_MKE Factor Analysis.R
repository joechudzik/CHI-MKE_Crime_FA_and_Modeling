library(reshape2)
library(tidyverse)
library(psych)
library(GPArotation)
library(corrplot)
library(nFactors)
library(dplyr)
library(nnet)
library(MASS)

# cleans global library
rm(list=ls())

mke_data <- read.csv("/Users/Joey/Desktop/Snowden/Data/MKE Factors Update_for Shion and Joe.csv")
chi_data_orig <- read.csv("/Users/Joey/Desktop/Snowden/Data/CHI Factors Update_for Shion and Joe.csv")
chi_unemployed_fixedCol <- read.csv("/Users/Joey/Data/Desktop/Snowden/Unemployed CHI (Fixed).csv")
metadata <- read.csv("/Users/Joey/Desktop/Snowden/Data/CHI_MKE_Metadata.csv")

# found wrong values in original CHI dataset. This next line contains the corrected CHI dataset
#
chi_data_new <- read.delim("/Users/Joey/Desktop/CHI Factors Update_for Shion and Joe_corrected.csv")

# Removing first NA column so full dataset operations dont fail
mke_data$D_R <- NULL
chi_data_orig$D_R <- NULL
chi_data_new$D_R <- NULL


#
# MILWAUKEE DATA
#

# shows correlation of variables in full MKE dataset
corrplot(cor(mke_data, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# building new dataframes to include only socioeconomic variables along with pclag and vclag variables
#
mke_data.props <- mke_data[14:24]
mke_data.props$SameProp <- mke_data$SameProp

mke_data_withCrime <- mke_data.props
mke_data_withCrime$Violent <- mke_data$Violent
mke_data_withCrime$Property <- mke_data$Property
mke_data_withCrime$vclag <- mke_data$vclag
mke_data_withCrime$pclag <- mke_data$pclag
mke_data_withCrime$EthnHerter <- mke_data$EthnHerter

# shows correlation of below 4 groups in a heatmap type plot
corrplot(cor(mke_data_withCrime, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# finding number of factors
parallel_mke <- fa.parallel(mke_data.props, fm="minres", fa="fa")
# perform factor analysis
fourfactor_mke <- fa(mke_data.props, nfactors=4, rotate="varimax", fm="minres")
print(fourfactor_mke)
# show plot, dividing the dimensions into 4 factors
fa.diagram(fourfactor_mke) 
# print loadings over 0.5
print(fourfactor_mke$loadings, cutoff=0.5)


#
# CHICAGO DATA
#

# correlation of variables in full CHI dataset
corrplot(cor(new_chi, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# building new dataframes to include only socioeconomic variables along with pclag and vclag variables
#

chi_data.props <- chi_data_new[14:26]
chi_data.props <- chi_data.props[ -c(6) ]

# chi_data.props <- chi_data[14:18]
# chi_data.props$HispProp <- chi_data$HispProp
# chi_data.props$Unemployed <- chi_data$Unemployed
# chi_data.props$Inc75K_ <- chi_data$Inc75K_
# chi_data.props$BAup <- chi_data$BAup
# chi_data.props$SSIProp <- chi_data$SSIProp
# chi_data.props$BelowProp <- chi_data$BelowProp
# chi_data.props$SameProp <- chi_data$SameProp

chi_data_withCrime <- chi_data.props
chi_data_withCrime$Violent <- chi_data$Violent
chi_data_withCrime$Property <- chi_data$Property
chi_data_withCrime$vclag <- chi_data$vclag
chi_data_withCrime$pclag <- chi_data$pclag
chi_data_withCrime$EthnHerter <- chi_data$EthnHerter


# shows correlation of variables across the modified prop dataset
corrplot(cor(chi_data_withCrime, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# finding number of factors
parallel_chi <- fa.parallel(chi_data.props, fm="minres", fa="fa")
# perform factor analysis
fourfactor_chi <- fa(chi_data.props, nfactors=4, rotate="varimax", fm="minres")
print(fourfactor_chi)
# show plot dividing the dimensions into 4 factors
fa.diagram(fourfactor_chi) 
# print loadings over 0.5
print(fourfactor_chi$loadings, cutoff=0.5)

