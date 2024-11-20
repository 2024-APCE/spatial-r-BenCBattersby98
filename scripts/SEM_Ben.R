#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of APCE 2007 dataset
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSr7ZdAjDsL97wmXc3iI-OgxR0kcTxongFpNtrlWUvWVPoyfdMkql_OmXdgmUvSrSygVdCnz7K87_I/pub?gid=653791228&single=true&output=csv")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
library(lavaan)

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1nrDCutpAfO5iKC5mueuOmhHgIXnwrsvzRsy3vxiLKng/edit?gid=1936062943#gid=1936062943") data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSr7ZdAjDsL97wmXc3iI-OgxR0kcTxongFpNtrlWUvWVPoyfdMkql_OmXdgmUvSrSygVdCnz7K87_I/pub?gid=653791228&single=true&output=csv")
SEMdata
# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata,
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd,
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
names(SEMdatastd)
multreg_std<-lm(woody~dist2river+elevation+rainfall+cec+burnfreq+hills,data=SEMdatastd)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSr7ZdAjDsL97wmXc3iI-OgxR0kcTxongFpNtrlWUvWVPoyfdMkql_OmXdgmUvSrSygVdCnz7K87_I/pub?gid=653791228&single=true&output=csv")

# Make a lavaan model as hypothesized in the SEMdata and fit the model 
woody_model<-'woody~burnfreq+hills
woody~dist2river+elevation
woody~dist2river+rainfall
woody~rainfall+cec
woody~elevation+rainfall'
woody_model
woody_model<-lavaan::sem(woody_model,SEMdatastd)

n# show the model results
summary(woody_model,standardized=T,fit.measures=T,rsquare=T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

library(semPlot)



# Make a lavaan model as hypothesized in the SEMdata and fit the model 
woody_model<-'woody~hills
woody~dist2river+elevation
woody~dist2river+cec
woody~rainfall+cec
woody~elevation+rainfall'
woody_model
woody_model<-lavaan::sem(woody_model,SEMdatastd)

n# show the model results
summary(woody_model,standardized=T,fit.measures=T,rsquare=T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR




# Make a lavaan model as hypothesized in the SEMdata and fit the model 
woody_model<-'dist2river~elevation+woody
cec~rainfall+woody
woody~burnfreq+hills
woody~elevation+rainfall'
woody_model
woody_model<-lavaan::sem(woody_model,SEMdatastd)

n# show the model results
summary(woody_model,standardized=T,fit.measures=T,rsquare=T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# also explore the models as shown in fig 5b and 5c of the  SEMdatasd paper
# so repeat the model for leaf P content
