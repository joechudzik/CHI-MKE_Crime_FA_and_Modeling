library(randomForest)
library(dplyr)
library(data.table)
library(formattable)
library(tidyr)

rm(list=ls())

mke_data <- read.csv("/Users/Joey/Desktop/Snowden/Data/MKE Factors Update_for Shion and Joe_corrected.csv")

# Removing first NA column so full dataset operations dont fail
mke_data$D_R <- NULL

populationDensity_mke <- numeric()
for( x in 1:nrow(mke_data) ){
  if (mke_data$SqMi[x] == 0.0){
    populationDensity_mke[x] <- 0
  }else{
    popDensity <- mke_data$Total_Pop[x] / mke_data$SqMi[x]
    populationDensity_mke[x] <- popDensity
  }
}

# MKE without latent factor stuff
#
mke_prop_wVars <- mke_data[14:26]
mke_prop_wVars$Property <- mke_data$Property
mke_prop_wVars$pclag <- mke_data$pclag
mke_prop_wVars$PopDensity <- populationDensity_mke

mke_viol_wVars <- mke_data[14:26]
mke_viol_wVars$Violent <- mke_data$Violent
mke_viol_wVars$vclag <- mke_data$vclag
mke_viol_wVars$PopDensity <- populationDensity_mke

# latent factor stuff - building the dataframe
#
disadvantage_mke_fl <- c(0.8, 0.8, 0.8, 0.7, -0.7, 0.7, -0.6)
immigration_mke_fl <- c(1, 0.9)
resStability_mke_fl <- c(0.9, 0.8)
affluence_mke_fl <- c(0.6)

disadvantage_mke <- rowMeans(mke_data %>% dplyr::select(BelowProp, MandFHH_ch, SSIProp, Unemployment_fixed, Inc75K_, 
                                                        PAIProp, BAup) * disadvantage_mke_fl)
immigration_mke <- rowMeans(mke_data %>% dplyr::select(HispProp, ForeignPro) * immigration_mke_fl)
resStability_mke <- rowMeans(mke_data %>% dplyr::select(OwnerProp, SameProp) * resStability_mke_fl)
affluence_mke <- rowMeans(mke_data %>% dplyr::select(OccuProp) * affluence_mke_fl)

mke_prop_modelFrame <- data.frame(mke_data$Property, mke_data$pclag, disadvantage_mke, immigration_mke, 
                                      resStability_mke, affluence_mke, populationDensity_mke, mke_data$EthnHerter)
colnames(mke_prop_modelFrame) <- c('Property','pclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter')
mke_viol_modelFrame <- data.frame(mke_data$Violent, mke_data$vclag, disadvantage_mke, immigration_mke, 
                                     resStability_mke, affluence_mke, populationDensity_mke, mke_data$EthnHerter)
colnames(mke_viol_modelFrame) <- c('Violent','vclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter')


#
# MKE Property RandomForest begin
#

# random forest with latent factors
#
# split into train and validation sets; training set : validation set = 70 : 30 (random)
set.seed(100)
mke_prop_wLF_sample <- sample(nrow(mke_prop_modelFrame), 0.7*nrow(mke_prop_modelFrame), replace=FALSE)
mke_prop_wLF_trainSet <- mke_prop_modelFrame[mke_prop_wLF_sample,]
mke_prop_wLF_validSet <- mke_prop_modelFrame[-mke_prop_wLF_sample,]

# create random forest model with default parameters 
mke_prop_forest_wLF <- randomForest(Property ~ ., data = mke_prop_wLF_trainSet, importance=TRUE)

#mke_prop_forest_wLF <- randomForest(Property ~ ., data=mke_prop_modelFrame, importance=TRUE)


# random forest with variables (without binning into latent factors)
#
mke_prop_wVars_sample <- sample(nrow(mke_prop_wVars), 0.7*nrow(mke_prop_wVars), replace=FALSE)
mke_prop_wVars_trainSet <- mke_prop_wVars[mke_prop_wVars_sample,]
mke_prop_wVars_validSet <- mke_prop_wVars[-mke_prop_wVars_sample,]
mke_prop_forest_wVars <- randomForest(Property ~ ., data=mke_prop_wVars_trainSet, importance=TRUE)

#mke_prop_forest_wVars <- randomForest(Property ~ ., data=mke_prop_wVars, importance=TRUE)

# MKE Violent
#
# random forest with latent factors
#
mke_viol_wLF_sample <- sample(nrow(mke_viol_modelFrame), 0.7*nrow(mke_viol_modelFrame), replace=FALSE)
mke_viol_wLF_trainSet <- mke_viol_modelFrame[mke_viol_wLF_sample,]
mke_viol_wLF_validSet <- mke_viol_modelFrame[-mke_viol_wLF_sample,]
mke_viol_forest_wLF <- randomForest(Violent ~ ., data=mke_viol_wLF_trainSet, importance=TRUE)

mke_viol_forest_wLF <- randomForest(Violent ~ ., data=mke_viol_modelFrame, importance=TRUE)

# random forest with variables (without binning into latent factors)
#
mke_viol_wVars_sample <- sample(nrow(mke_viol_wVars), 0.7*nrow(mke_viol_wVars), replace=FALSE)
mke_viol_wVars_trainSet <- mke_viol_wVars[mke_viol_wVars_sample,]
mke_viol_wVars_validSet <- mke_viol_wVars[-mke_viol_wVars_sample,]
mke_viol_forest_wVars <- randomForest(Violent ~ ., data=mke_viol_wVars_trainSet, importance=TRUE)

#mke_viol_forest_wVars <- randomForest(Violent ~ ., data=mke_viol_wVars, importance=TRUE)

varImpPlot(mke_viol_forest_wLF)

anova(mke_property_model_nb, mke_violent_model_nb)
mke_prop_forest_wLF
mke_viol_forest_wLF



#
# creates table visualizations
#
# PROPERTY CHART
#
# initializes columns for MSEs later used in the charts
#
mke_prop_wLF_z <- c(rep(0, 15))
mke_prop_wVars_z <- c(rep(0, 7))

mke_prop_wLF <- importance(mke_prop_forest_wLF)[1:7]
mke_prop_wVars <- importance(mke_prop_forest_wVars)[1:15]

for (v in mke_prop_wLF_z){
  mke_prop_wLF <- c(mke_prop_wLF, v)
}
for (v in mke_prop_wVars){
  mke_prop_wVars_z <- c(mke_prop_wVars_z, v)
}
mke_prop_wVars <- mke_prop_wVars_z

# create the tables
#
mke_prop_table <- data.table('Variables'=c('pclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter',
                                           'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                           'SSIProp','BelowProp','BAup','EthnHerter','SameProp','pclag','PopDensity'),
                             'IncMSE1' = mke_prop_wLF,
                             'IncMSE2' = mke_prop_wVars)
fix1 <- aggregate(IncMSE1 ~ Variables, data=mke_prop_table, FUN=sum)
fix2 <- aggregate(IncMSE2 ~ Variables, data=mke_prop_table, FUN=sum)
fix1$IncMSE2 <- fix2$IncMSE2
mke_prop_table <- fix1
mke_prop_table <- mke_prop_table %>% arrange(match(Variables, c('pclag','EthnHerter','PopDensity',
                                                                'Disadvantage','Immigration','ResStability','Affluence',
                                                                'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                                                'SSIProp','BelowProp','BAup','SameProp')))

# rounds off the decimals at 3 decimal places to make the table easier to read & removes 0.000 to clean up table
#
for (i in 1:length(mke_prop_table$IncMSE1)){
  mke_prop_table$IncMSE1[i] <- format(round(as.double(mke_prop_table$IncMSE1[i]), 3), nsmall=3)
  mke_prop_table$IncMSE2[i] <- format(round(as.double(mke_prop_table$IncMSE2[i]), 3), nsmall=3)
  if (mke_prop_table$IncMSE1[i] == 0.000){
    mke_prop_table$IncMSE1[i] <- ' '
  }
  else if (mke_prop_table$IncMSE2[i] == 0.000){
    mke_prop_table$IncMSE2[i] <- ' '
  }
}

# "if the variable name is pclag, color it red. if variable is pop density, color it green. if variable is ethnherter, color it blue"
#
setnames(mke_prop_table, c('Variables','%IncMSE1','%IncMSE2'))
formattable(mke_prop_table,
            align = c('l'),
            list('Variables' = formatter("span",style = x ~ style(color = ifelse(x=='pclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black'))))),
                 '%IncMSE1' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black'))),
                 '%IncMSE2' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black')))))


# VIOLENT CHART
#
# initializes columns for MSEs later used in the charts
#
mke_viol_wLF_z <- c(rep(0, 15))
mke_viol_wVars_z <- c(rep(0, 7))

mke_viol_wLF <- importance(mke_viol_forest_wLF)[1:7]
mke_viol_wVars <- importance(mke_viol_forest_wVars)[1:15]

for (v in mke_viol_wLF_z){
  mke_viol_wLF <- c(mke_viol_wLF, v)
}
for (v in mke_viol_wVars){
  mke_viol_wVars_z <- c(mke_viol_wVars_z, v)
}
mke_viol_wVars <- mke_viol_wVars_z

# create the tables
#
mke_viol_table <- data.table('Variables'=c('vclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter',
                                           'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                           'SSIProp','BelowProp','BAup','EthnHerter','SameProp','vclag','PopDensity'),
                             'IncMSE1' = mke_viol_wLF,
                             'IncMSE2' = mke_viol_wVars)
fix1 <- aggregate(IncMSE1 ~ Variables, data=mke_viol_table, FUN=sum)
fix2 <- aggregate(IncMSE2 ~ Variables, data=mke_viol_table, FUN=sum)
fix1$IncMSE2 <- fix2$IncMSE2
mke_viol_table <- fix1
mke_viol_table <- mke_viol_table %>% arrange(match(Variables, c('vclag','EthnHerter','PopDensity',
                                                                'Disadvantage','Immigration','ResStability','Affluence',
                                                                'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                                                'SSIProp','BelowProp','BAup','SameProp')))

# rounds off the decimals at 3 decimal places to make the table easier to read & removes 0.000 to clean up table
#
for (i in 1:length(mke_viol_table$IncMSE1)){
  mke_viol_table$IncMSE1[i] <- format(round(as.double(mke_viol_table$IncMSE1[i]), 3), nsmall=3)
  mke_viol_table$IncMSE2[i] <- format(round(as.double(mke_viol_table$IncMSE2[i]), 3), nsmall=3)
  if (mke_viol_table$IncMSE1[i] == 0.000){
    mke_viol_table$IncMSE1[i] = ''
  }
  if (mke_viol_table$IncMSE2[i] == 0.000){
    mke_viol_table$IncMSE2[i] = ''
  }
}

# "if the variable name is vclag, color it red. if variable is pop density, color it green. if variable is ethnherter, color it blue"
#
setnames(mke_viol_table, c('Variables','%IncMSE1','%IncMSE2'))
formattable(mke_viol_table,
            title = 'mke violent',
            align = c('l'),
            list('Variables' = formatter("span",style = x ~ style(color = ifelse(x=='vclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black'))))),
                 '%IncMSE1' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black'))),
                 '%IncMSE2' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black')))))
