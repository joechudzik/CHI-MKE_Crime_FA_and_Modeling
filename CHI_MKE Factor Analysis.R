library(reshape2)
library(tidyverse)
library(psych)
library(GPArotation)
library(corrplot)
library(nFactors)
library(dplyr)
library(nnet)
library(MASS)
library(reshape2)

# cleans global library
rm(list=ls())

metadata <- read.csv("/Users/Joey/Desktop/Snowden/Data/CHI_MKE_Metadata.csv")

# found wrong values in original CHI dataset. This next line contains the corrected CHI dataset
#
chi_data_new <- read.delim("/Users/Joey/Desktop/Snowden/Data/CHI Factors Update_for Shion and Joe_corrected.csv")
mke_data_new <- read.csv("/Users/Joey/Desktop/Snowden/Data/MKE Factors Update_for Shion and Joe_corrected.csv")

# Removing first NA column so full dataset operations dont fail
mke_data_new$D_R <- NULL
chi_data_new$D_R <- NULL


#
# MILWAUKEE DATA
#
mke_data <- mke_data_new
# shows correlation of variables in full MKE dataset
corrplot(cor(mke_data, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# building new dataframes to include only socioeconomic variables along with pclag and vclag variables
#
mke_data.props <- mke_data_new[14:26]
mke_data.props <- mke_data.props[ -c(12) ]

mke_data_withCrime <- mke_data.props
mke_data_withCrime$Violent <- mke_data_new$Violent
mke_data_withCrime$Property <- mke_data_new$Property
mke_data_withCrime$vclag <- mke_data_new$vclag
mke_data_withCrime$pclag <- mke_data_new$pclag
mke_data_withCrime$EthnHerter <- mke_data_new$EthnHerter

# shows correlation of below 4 groups in a heatmap type plot
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}
colnames(mke_data_withCrime)[colnames(mke_data_withCrime)=='Unemployment_fixed'] <- 'Unemployed'
mke_cormat <- cor(mke_data_withCrime, use='complete.obs')
mke_cormat <- reorder_cormat(mke_cormat)
mke_upper_tri <- get_lower_tri(mke_cormat)
mke_melted_cormat <- melt(mke_upper_tri, na.rm=TRUE)
mke_heatmap <- ggplot(data=mke_melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color='white')+
  geom_text(aes(Var1, Var2, label=format(round(value, 2), nsmall=2)), color='black',size=3)+
  scale_fill_gradient2(low='red', high='blue', mid='white', midpoint=0, limit=c(-1,1), space='Lab')+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=65, vjust=0.65, size=10), axis.title.x=element_blank(), axis.title.y=element_blank())+
  coord_fixed()
mke_heatmap + theme(
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1,0),
  legend.position = c(0.4,0.8),
  legend.direction = 'horizontal')+
  guides(fill = guide_colorbar(barwidth=7, barheight=1, title.position = 'top', title.hjust=0.5))

#corrplot(cor(mke_data_withCrime, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

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
chi_data <- chi_data_new
chi_data.props <- chi_data_new[14:26]
chi_data.props <- chi_data.props[ -c(6) ]

chi_data_withCrime <- chi_data.props
chi_data_withCrime$Violent <- chi_data$Violent
chi_data_withCrime$Property <- chi_data$Property
chi_data_withCrime$vclag <- chi_data$vclag
chi_data_withCrime$pclag <- chi_data$pclag
chi_data_withCrime$EthnHerter <- chi_data$EthnHerter


# shows correlation of variables across the modified prop dataset
colnames(chi_data_withCrime)[colnames(chi_data_withCrime)=='Unemployed_Fixed'] <- 'Unemployed'
chi_cormat <- cor(chi_data_withCrime, use='complete.obs')
chi_cormat <- reorder_cormat(chi_cormat)
chi_upper_tri <- get_lower_tri(chi_cormat)
chi_melted_cormat <- melt(chi_upper_tri, na.rm=TRUE)
chi_heatmap <- ggplot(data=chi_melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile(color='white')+
  geom_text(aes(Var1, Var2, label=format(round(value, 2), nsmall=2)), color='black',size=3)+
  scale_fill_gradient2(low='red', high='blue', mid='white', midpoint=0, limit=c(-1,1), space='Lab')+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=65, vjust=0.65, size=10), axis.title.x=element_blank(), axis.title.y=element_blank())+
  coord_fixed()
chi_heatmap + theme(
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1,0),
  legend.position = c(0.4,0.8),
  legend.direction = 'horizontal')+
  guides(fill = guide_colorbar(barwidth=7, barheight=1, title.position = 'top', title.hjust=0.5))

#corrplot(cor(chi_data_withCrime, use="complete.obs"), order="hclust", tl.col="black", tl.ce=.75)

# finding number of factors
parallel_chi <- fa.parallel(chi_data.props, fm="minres", fa="fa")
# perform factor analysis
fourfactor_chi <- fa(chi_data.props, nfactors=4, rotate="varimax", fm="minres")
print(fourfactor_chi)
# show plot dividing the dimensions into 4 factors
fa.diagram(fourfactor_chi) 
# print loadings over 0.5
print(fourfactor_chi$loadings, cutoff=0.5)

