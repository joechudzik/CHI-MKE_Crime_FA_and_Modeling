# CHI-MKE_Crime_FA_and_Modeling

This repository is a research project done at Marquette University. 

The project consisted of first building datasets of various statistics within the census tract boundaries of Milwaukee, WI and Chicago, IL. (Please see the CHI_MKE_Metadata file for the entire variable list of which describes the variables as well)

Factor analysis was then performed on both datasets to uncover 4 distinct latent factors with the following predictors:
- Concentrated Disadvantage Measure
- Concentrated Immigration Measure
- Concentrated Affluence Measure
- Residential Stability

These factors above were used in conjunction with ethnic heterogeneity, population density (Total_Pop/SqMi), and the crime type's respective lag variable as separate variables to build 4 different negative binomial models. In total, each model consisted of 7 different variables which described either property or violent crimes of Milwaukee or Chicago. The following specific models were created:
- Property crimes in Milwaukee
- Property crimes in Chicago
- Violent crimes in Milwaukee
- Violent Crimes in Chicago

Analysis to be continued...
