library(randomForest)
library(dplyr)
library(data.table)
library(formattable)
library(tidyr)

rm(list=ls())

chi_data <- read.delim("/Users/Joey/Desktop/Snowden/Data/CHI Factors Update_for Shion and Joe_corrected.csv")

chi_data$D_R <- NULL

populationDensity_chi <- numeric()
for( x in 1:nrow(chi_data) ){
  popDensity <- chi_data$Total_Pop[x] / chi_data$SqMi[x]
  populationDensity_chi[x] <- popDensity
}

# CHI without latent factor stuff
#
chi_prop_wVars <- chi_data[14:26]
chi_prop_wVars$Property <- chi_data$Property
chi_prop_wVars$pclag <- chi_data$pclag
chi_prop_wVars$PopDensity <- populationDensity_chi

chi_viol_wVars <- chi_data[14:26]
chi_viol_wVars$Violent <- chi_data$Violent
chi_viol_wVars$vclag <- chi_data$vclag
chi_viol_wVars$PopDensity <- populationDensity_chi

# latent factor stuff - building the dataframe
#
disadvantage_chi_fl <- c(0.8, 0.8, 0.7, 0.7, -0.7, 0.7)
affluence_chi_fl <- c(0.8, 0.7)
immigration_chi_fl <- c(0.8, 0.8)
resStability_chi_fl <- c(0.8, 0.7)

disadvantage_chi <- rowMeans(chi_data %>% dplyr::select(BelowProp, MandFHH_ch, SSIProp, Unemployed_Fixed, Inc75K_, PAIProp) * disadvantage_chi_fl)
affluence_chi <- rowMeans(chi_data %>% dplyr::select(BAup, OccuProp) * affluence_chi_fl)
immigration_chi <- rowMeans(chi_data %>% dplyr::select(ForeignPro, HispProp) * immigration_chi_fl)
resStability_chi <- rowMeans(chi_data %>% dplyr::select(OwnerProp, SameProp) * resStability_chi_fl)

chi_prop_modelFrame <- data.frame(chi_data$Property, chi_data$pclag, disadvantage_chi, immigration_chi, 
                                      resStability_chi, affluence_chi, populationDensity_chi, chi_data$EthnHerter)
colnames(chi_prop_modelFrame) <- c('Property','pclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter')
chi_viol_modelFrame <- data.frame(chi_data$Violent, chi_data$vclag, disadvantage_chi, immigration_chi, 
                                     resStability_chi, affluence_chi, populationDensity_chi, chi_data$EthnHerter)
colnames(chi_viol_modelFrame) <- c('Violent','vclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter')

#
# CHI Property RandomForest begin
#

# random forest with latent factors
#
# split into train and validation sets; training set : validation set = 70 : 30 (random)
set.seed(100)
chi_prop_wLF_sample <- sample(nrow(chi_prop_modelFrame), 0.7*nrow(chi_prop_modelFrame), replace=FALSE)
chi_prop_wLF_trainSet <- chi_prop_modelFrame[chi_prop_wLF_sample,]
chi_prop_wLF_validSet <- chi_prop_modelFrame[-chi_prop_wLF_sample,]

# create random forest model with default parameters 
chi_prop_forest_wLF <- randomForest(Property ~ ., data = chi_prop_wLF_trainSet, importance=TRUE)

#chi_prop_forest_wLF <- randomForest(Property ~ ., data=chi_prop_modelFrame, importance=True)


# random forest with variables (without binning into latent factors)
#
chi_prop_wVars_sample <- sample(nrow(chi_prop_wVars), 0.7*nrow(chi_prop_wVars), replace=FALSE)
chi_prop_wVars_trainSet <- chi_prop_wVars[chi_prop_wVars_sample,]
chi_prop_wVars_validSet <- chi_prop_wVars[-chi_prop_wVars_sample,]
chi_prop_forest_wVars <- randomForest(Property ~ ., data=chi_prop_wVars_trainSet, importance=TRUE)

#chi_prop_forest_wVars <- randomForest(Property ~., data=chi_prop_wVars, importance=True)

# CHI Violent
#
# random forest with latent factors
#
chi_viol_wLF_sample <- sample(nrow(chi_viol_modelFrame), 0.7*nrow(chi_viol_modelFrame), replace=FALSE)
chi_viol_wLF_trainSet <- chi_viol_modelFrame[chi_viol_wLF_sample,]
chi_viol_wLF_validSet <- chi_viol_modelFrame[-chi_viol_wLF_sample,]
chi_viol_forest_wLF <- randomForest(Violent ~ ., data=chi_viol_wLF_trainSet, importance=TRUE)

#chi_viol_forest_wLF <- randomForest(Violent ~ ., data=chi_viol_modelFrame, importance=TRUE)

# random forest with variables (without binning into latent factors)
#
chi_viol_wVars_sample <- sample(nrow(chi_viol_wVars), 0.7*nrow(chi_viol_wVars), replace=FALSE)
chi_viol_wVars_trainSet <- chi_viol_wVars[chi_viol_wVars_sample,]
chi_viol_wVars_validSet <- chi_viol_wVars[-chi_viol_wVars_sample,]
chi_viol_forest_wVars <- randomForest(Violent ~ ., data=chi_viol_wVars_trainSet, importance=TRUE)

#chi_viol_forest_wVars <- randomForest(Violent ~ ., data=chi_viol_wVars, importance=TRUE)



# creates table visualizations
#
#

# PROPERTY CHART
#
# initializes columns for MSEs later used in the charts
#
chi_prop_wLF_z <- c(rep(0, 15))
chi_prop_wVars_z <- c(rep(0, 7))

chi_prop_wLF <- importance(chi_prop_forest_wLF)[1:7]
chi_prop_wVars <- importance(chi_prop_forest_wVars)[1:15]

for (v in chi_prop_wLF_z){
  chi_prop_wLF <- c(chi_prop_wLF, v)
}
for (v in chi_prop_wVars){
  chi_prop_wVars_z <- c(chi_prop_wVars_z, v)
}
chi_prop_wVars <- chi_prop_wVars_z

# create the tables
#
chi_prop_table <- data.table('Variables'=c('pclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter',
                                           'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                           'SSIProp','BelowProp','BAup','EthnHerter','SameProp','pclag','PopDensity'),
                             'IncMSE1' = chi_prop_wLF,
                             'IncMSE2' = chi_prop_wVars)
fix1 <- aggregate(IncMSE1 ~ Variables, data=chi_prop_table, FUN=sum)
fix2 <- aggregate(IncMSE2 ~ Variables, data=chi_prop_table, FUN=sum)
fix1$IncMSE2 <- fix2$IncMSE2
chi_prop_table <- fix1
chi_prop_table <- chi_prop_table %>% arrange(match(Variables, c('pclag','EthnHerter','PopDensity',
                                                                'Disadvantage','Immigration','ResStability','Affluence',
                                                                'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                                                'SSIProp','BelowProp','BAup','SameProp')))

# rounds off the decimals at 3 decimal places to make the table easier to read & removes 0.000 to clean up table
#
for (i in 1:length(chi_prop_table$IncMSE1)){
  chi_prop_table$IncMSE1[i] <- format(round(as.double(chi_prop_table$IncMSE1[i]), 3), nsmall=3)
  chi_prop_table$IncMSE2[i] <- format(round(as.double(chi_prop_table$IncMSE2[i]), 3), nsmall=3)
  if (chi_prop_table$IncMSE1[i] == 0.000){
    chi_prop_table$IncMSE1[i] <- ''
  }
  if (chi_prop_table$IncMSE2[i] == 0.000){
    chi_prop_table$IncMSE2[i] <- ''
  }
}

# "if the variable name is pclag, color it red. if variable is pop density, color it green. if variable is ethnherter, color it blue"
#
setnames(chi_prop_table, c('Variables','%IncMSE1','%IncMSE2'))
formattable(chi_prop_table,
            align = c('l'),
            list('Variables' = formatter("span",style = x ~ style(color = ifelse(x=='pclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black'))))),
                 '%IncMSE1' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black'))),
                 '%IncMSE2' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black')))))


# VIOLENT CHART
#
# initializes columns for MSEs later used in the charts
#
chi_viol_wLF_z <- c(rep(0, 15))
chi_viol_wVars_z <- c(rep(0, 7))

chi_viol_wLF <- importance(chi_viol_forest_wLF)[1:7]
chi_viol_wVars <- importance(chi_viol_forest_wVars)[1:15]

for (v in chi_viol_wLF_z){
  chi_viol_wLF <- c(chi_viol_wLF, v)
}
for (v in chi_viol_wVars){
  chi_viol_wVars_z <- c(chi_viol_wVars_z, v)
}
chi_viol_wVars <- chi_viol_wVars_z

# create the tables
#
chi_viol_table <- data.table('Variables'=c('vclag','Disadvantage','Immigration','ResStability','Affluence','PopDensity','EthnHerter',
                                           'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                           'SSIProp','BelowProp','BAup','EthnHerter','SameProp','vclag','PopDensity'),
                             'IncMSE1' = chi_viol_wLF,
                             'IncMSE2' = chi_viol_wVars)
fix1 <- aggregate(IncMSE1 ~ Variables, data=chi_viol_table, FUN=sum)
fix2 <- aggregate(IncMSE2 ~ Variables, data=chi_viol_table, FUN=sum)
fix1$IncMSE2 <- fix2$IncMSE2
chi_viol_table <- fix1
chi_viol_table <- chi_viol_table %>% arrange(match(Variables, c('vclag','EthnHerter','PopDensity',
                                                                'Disadvantage','Immigration','ResStability','Affluence',
                                                                'PAIProp','OccuProp','OwnerProp','Inc75K_','MandFHH_ch','Unemployed','ForeignPro','HispProp',
                                                                'SSIProp','BelowProp','BAup','SameProp')))

# rounds off the decimals at 3 decimal places to make the table easier to read & removes 0.000 to clean up the table
#
for (i in 1:length(chi_viol_table$IncMSE1)){
  chi_viol_table$IncMSE1[i] <- format(round(as.double(chi_viol_table$IncMSE1[i]), 3), nsmall=3)
  chi_viol_table$IncMSE2[i] <- format(round(as.double(chi_viol_table$IncMSE2[i]), 3), nsmall=3)
  if (chi_viol_table$IncMSE1[i] == 0.000){
    chi_viol_table$IncMSE1[i] <- ''
  }
  if (chi_viol_table$IncMSE2[i] == 0.000){
    chi_viol_table$IncMSE2[i] <- ''
  }
}

# "if the variable name is vclag, color it red. if variable is pop density, color it green. if variable is ethnherter, color it blue"
#
setnames(chi_viol_table, c('Variables','%IncMSE1','%IncMSE2'))
formattable(chi_viol_table,
            align = c('l'),
            list('Variables' = formatter("span",style = x ~ style(color = ifelse(x=='vclag', 'red',ifelse(x=='PopDensity', 'green',ifelse(x=='EthnHerter', 'blue', 'black'))))),
                 '%IncMSE1' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black'))),
                 '%IncMSE2' = formatter("span",style = x ~ style(color = ifelse(x=='0.000', 'white','black')))))

