Erik.Leppo@tetratech.com
2018-07-06

# Test environments
* local Win 7, R 3.4.4
* win-builder (devel and release)

# R CMD check results
1 error  | 0 warnings | 1 note 

## Error

checking examples ... ERROR
Running examples in 'baytrends-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: qw.import
> ### Title: qw.import
> ### Aliases: qw.import
> 
> ### ** Examples
... 17 lines ...
> dataCensored.test<- qw.import(fn.import, qw.names, rounding)
[1] "Processing; 1/13 (secchi)."
[1] "Processing; 2/13 (chla)."
[1] "Processing; 3/13 (do)."
[1] "Processing; 4/13 (tn)."
[1] "Processing; 5/13 (tp)."
[1] "Processing; 6/13 (po4f)."
Error in `[.data.frame`(df.import, , paste0(i, "_", "lo")) : 
  undefined columns selected
Calls: qw.import -> [ -> [.data.frame
Execution halted

## Warnings
zero

## NOTE
checking R code for possible problems ... NOTE
detrended.flow: no visible binding for '<<-' assignment to 'figNum'
detrended.flow: no visible binding for global variable 'figNum'
Undefined global functions or variables:
  figNum

# Downstream dependencies
Run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 

There are currently no downstream dependencies for this package.