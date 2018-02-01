## ----Vignette_Data, eval=FALSE-------------------------------------------
#  # 1. change wd to vignettes
#  setwd(file.path("C:","Users","Erik.Leppo","OneDrive - Tetra Tech, Inc"
#                  ,"MyDocs_OneDrive","GitHub","ContDataQC","vignettes"))
#  #
#  # 2. Add data for vignette examples
#  # Parameters
#  Selection.Operation <- c("GetGageData","QCRaw", "Aggregate", "SummaryStats")
#  Selection.Type      <- c("Air","Water","AW","Gage","AWG","AG","WG")
#  Selection.SUB <- c("Data1_RAW","Data2_QC","Data3_Aggregated","Data4_Stats")
#  myDir.BASE <- getwd()
#  #
#  # Create data directories
#  myDir.create <- paste0("./",Selection.SUB[1])
#    ifelse(dir.exists(myDir.create)==FALSE,dir.create(myDir.create),"Directory already exists")
#  myDir.create <- paste0("./",Selection.SUB[2])
#    ifelse(dir.exists(myDir.create)==FALSE,dir.create(myDir.create),"Directory already exists")
#  myDir.create <- paste0("./",Selection.SUB[3])
#    ifelse(dir.exists(myDir.create)==FALSE,dir.create(myDir.create),"Directory already exists")
#  myDir.create <- paste0("./",Selection.SUB[4])
#    ifelse(dir.exists(myDir.create)==FALSE,dir.create(myDir.create),"Directory already exists")
#  #
#  # Save example data (assumes directory ./Data1_RAW/ exists)
#  myData <- data_raw_test2_AW_20130426_20130725
#    write.csv(myData,paste0("./",Selection.SUB[1],"/test2_AW_20130426_20130725.csv"))
#  myData <- data_raw_test2_AW_20130725_20131015
#    write.csv(myData,paste0("./",Selection.SUB[1],"/test2_AW_20130725_20131015.csv"))
#  myData <- data_raw_test2_AW_20140901_20140930
#    write.csv(myData,paste0("./",Selection.SUB[1],"/test2_AW_20140901_20140930.csv"))
#  myData <- data_raw_test4_AW_20160418_20160726
#    write.csv(myData,paste0("./",Selection.SUB[1],"/test4_AW_20160418_20160726.csv"))
#  myFile <- "config.TZ.Central.R"
#    file.copy(file.path(path.package("ContDataQC"),"extdata",myFile)
#              ,file.path(getwd(),Selection.SUB[1],myFile))

## ----Vignette_Build, eval=FALSE------------------------------------------
#  # generate Vignette
#  library(baytrends)
#  library(devtools)
#  devtools::build_vignettes()
#  
#  # create vignette folder and default file
#  #devtools::use_vignette("ContDataQC_Vignette")

## ----PKG_BUILD, eval=FALSE-----------------------------------------------
#  # Library Name
#  myLibrary <- "baytrends"
#  # Load Library
#  library(devtools)
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  # Create Package
#  # create(myLibrary)
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  #
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  # Document, Install, and Reload Library
#  ## Generate Documentation
#  setwd(paste0("./", myLibrary))
#  devtools::document()
#  ## Install New Package (locally)
#  setwd("..") # return to root directory first
#  devtools::install(myLibrary)
#  ## Reload library
#  library(myLibrary, character.only = TRUE)
#  # change wd back to package
#  setwd(paste0("./", myLibrary))
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ---- eval=FALSE---------------------------------------------------------
#  # Restart R within RStudio:  Ctrl + Shift + F10
#  library("baytrends")
#  help(package="baytrends")

## ---- eval=FALSE---------------------------------------------------------
#  help(package="baytrends")

## ---- eval=FALSE---------------------------------------------------------
#  browseVignettes("baytrends")

## ---- eval=FALSE---------------------------------------------------------
#  example(analysisOrganizeData)

## ---- eval=FALSE---------------------------------------------------------
#  # Check for errors (or press Cmd + Shift + E in RStudio)
#  #http://r-pkgs.had.co.nz/check.html
#  devtools::check()

## ---- eval=FALSE---------------------------------------------------------
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  # Upload to Github via GitHub Desktop utility
#  # 0. download from web via "clone or download" via "Open in Desktop" (GitHub Desktop) if not already in GitHub Desktop
#  # 1. Make changes in download/clone folder. (done above)
#  # 3. Open GH Desktop commit changes then sync.
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  # install from GitHub (via devtools)
#  devtools::install_github(paste0("leppott/",myLibrary))
#  #
#  
#  
#  
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  # remove installed packages (if needed for troubleshooting)
#  search() # find
#  #detach(3) # remove by number
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  
#  
#  # to build package
#  #https://thepoliticalmethodologist.com/2014/08/14/building-and-maintaining-r-packages-with-devtools-and-roxygen2/
#  # To build the package as a compressed file in your working directory, run build(current.code, path=getwd()).
#  
#  # to save internal data for examples
#  # example
#  #http://r-pkgs.had.co.nz/data.html#data-sysdata
#  # have to be at root directory (above package)
#  #devtools::use_data(NV.predictors,NV.bugs,pkg="MMIcalcNV",internal=TRUE,overwrite=TRUE)
#  ## verify with data()
#  
#  # To save RMD files
#  # http://stackoverflow.com/questions/30377213/how-to-include-rmarkdown-file-in-r-package
#  # /pkg/inst/rmd/
#  # system.file("rmd/file.Rmd", package="packagename")
#  #
#  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  #https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#  # Create Package
#  # create(myLibrary)
#  

## ----Stats_Gage, eval=FALSE----------------------------------------------
#  # Parameters
#  Selection.Operation <- c("GetGageData","QCRaw", "Aggregate", "SummaryStats")
#  Selection.Type      <- c("Air","Water","AW","Gage","AWG","AG","WG")
#  Selection.SUB <- c("Data1_RAW","Data2_QC","Data3_Aggregated","Data4_Stats")
#  myDir.BASE <- getwd()
#  #
#  # Summary Stats, File
#  ## Have to use "file" version.
#  ## Base version builds file name and expects "DATA" prefix
#  
#  # Gage File + Other File
#  myData.Operation <- "SummaryStats" #Selection.Operation[4]
#  myFile <- c("Data4Stats_01187300_Gage_20130101_20141231.csv"
#              ,"Data4Stats_test2_Aw_20130101_20141231.csv")
#  myDir.import <- file.path(".","data-raw")
#  myDir.export <- file.path(".","Data4_Stats")
#  
#  #Leave off myReport.format and get default (docx).
#  ContDataQC(myData.Operation
#             , fun.myDir.import=myDir.import
#             , fun.myDir.export=myDir.export
#             , fun.myFile=myFile)
#  
#  

