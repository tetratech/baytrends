## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----noRunCode1, eval=FALSE, echo=TRUE, results='asis'------------------------
#  library(baytrends)
#  df_in   <- loadData(file = "ExampleData.csv")
#  df_Surv <- makeSurvDF(df_in)

## ----noRunCode2, eval=FALSE, echo=TRUE, results='asis'------------------------
#  df_in$tdp <- survival::Surv(df_in$tdp_lo, df_in$tdp_hi, type = "interval2")

## ----noRunCode4, eval=FALSE, echo=TRUE, results='asis'------------------------
#  library(baytrends)
#  
#  # Load the first 10 rows of the built-in baytrends dataframe
#  # dataCensored into a dataframe and just a few fields.
#  df0 <- dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
#  df0
#  
#  # Convert the dataframe, df0, that has Surv objects to a
#  # dataframe where censored data are represented with
#  # "_lo/_hi" field name syntax.
#  df1 <- unSurvDF(df0)
#  df1
#  
#  # Save dataframe, df1, where censored data are represented with
#  # "_lo/_hi" field name syntax to a csv file in the working
#  # directory.
#  saveDF(df1, folder = '.')
#  
#  # Load data from csv file where censored data are represented
#  # with "_lo/_hi“ field name syntax to a dataframe of same
#  # structure.
#  df2 <- loadData("*.csv" ,folder = '.')
#  df2
#  
#  # Convert dataframe, df2, where censored data are represented
#  # with "_lo/_hi“ field name syntax to a dataframe with Surv objects.
#  df3 <- makeSurvDF(df2)
#  df3

## ----RunCode4, eval=TRUE, echo=TRUE, results='markup'-------------------------
library(baytrends)

# Load the first 10 rows of the built-in baytrends dataframe
# dataCensored into a dataframe and just a few fields. 
df0 <- dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
df0

# Convert the dataframe, df0, that has Surv objects to a 
# dataframe where censored data are represented with 
# "_lo/_hi" field name syntax. 
df1 <- unSurvDF(df0)
df1

# Save dataframe, df1, where censored data are represented with
# "_lo/_hi" field name syntax to a csv file in the working 
# directory.
saveDF(df1, folder = '.')

# Load data from csv file where censored data are represented
# with "_lo/_hi“ field name syntax to a dataframe of same 
# structure.
df2 <- loadData("*.csv" ,folder = '.')
df2

# Convert dataframe, df2, where censored data are represented
# with "_lo/_hi“ field name syntax to a dataframe with Surv objects.
df3 <- makeSurvDF(df2)
df3

## ----noRunCode5, eval=FALSE, echo=TRUE, results='asis'------------------------
#  library(baytrends)
#  #library(dplyr)
#  
#  # Create two dataframes with Surv objects.
#  df11 <- dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
#  df11
#  
#  df12 <- dataCensored[11:20,c("station","date","layer","tss","tdp","tp")]
#  df12
#  
#  # Combine the two dataframe into one dataframe
#  df13 <- dplyr::bind_rows(df11,df12)
#  df13

## ----RunCode5, eval=TRUE, echo=TRUE, results='markup'-------------------------
library(baytrends)
#library(dplyr)

# Create two dataframes with Surv objects. 
df11 <- dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
df11

df12 <- dataCensored[11:20,c("station","date","layer","tss","tdp","tp")]
df12

# Combine the two dataframe into one dataframe
df13 <- dplyr::bind_rows(df11,df12)
df13

## ----RunCode6, eval=TRUE, echo=TRUE, results='markup'-------------------------
dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
unSurvDF(dataCensored[1:10,c("station","date","layer","tss","tdp","tp")])
unSurv(dataCensored[1:10,"tdp"])

## ----RunCode7, eval=TRUE, echo=TRUE, results='markup'-------------------------
dataCensored[1:10,c("station","date","layer","tss","tdp","tp")]
imputeDF(dataCensored[1:10,c("station","date","layer","tss","tdp","tp")])
impute(dataCensored[1:10,"tdp"])

