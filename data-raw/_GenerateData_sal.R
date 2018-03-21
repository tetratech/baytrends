# Generate salinity data
#
# Erik.Leppo@tetratech.com
# 20180321
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Will be hidden in final package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 0. Prep####
# assume wd is package directory
wd <- file.path(getwd(),"data-raw")

# 1. Generate Data####
# Load data (sal)
load("salinity_1984to2016.rdata")

# Subset DF
# * down select input data set to those stations in baytrends list ----
df <- sal[sal$Station %in% unique(dataCensored$station)
             , c("Station", "Sample_Date", "Layer", "MeasureValue")]

# ** clean up data structure ----
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
df$Station     <- trim(as.character(df$Station))
df$Layer       <- trim(as.character(df$Layer))
df$Sample_Date <- trim(as.character(df$Sample_Date))
df$Sample_Date <- as.POSIXct(strptime(df$Sample_Date, "%m/%d/%Y"))

# rename columns
names(df) <- c(names(df)[1:3], "salinity")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
sal <- df
#setwd("..")
devtools::use_data(sal, overwrite = TRUE)

