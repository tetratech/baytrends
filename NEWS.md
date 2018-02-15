NEWS
================
<Erik.Leppo@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2018-02-15 10:14:37

# baytrends 1.0.0.9002

  - Released - 2018-02-15
  - Add back censoring.R from smwrQW package as it is needed for qw
    objects. Was removed in v0.3.3.9016

# baytrends 1.0.0.9001

  - Released - 2018-02-05
  - Improve error trapp for when to allow baytrends to run an
    intervention model.
      - selectData.R
      - gamTest.R

# baytrends 1.0.0

  - Released - 2018-02-01
  - Set as release on GitHub.

# baytrends 0.3.3.9019

  - Released - 2018-02-01
  - Update vignettes. Remove setwd().

# baytrends 0.3.3.9018

  - Released - 2018-02-01
  - Updated Vignettes to run with data in vignettes folder.
  - Added function fillMissing from smwrBase for detrended flow
    vignette.

# baytrends 0.3.3.9017

  - Released - 2018-01-31
  - Fix errors in Namespace noted in Check.

# baytrends 0.3.3.9016

  - Released - 2018-01-31
  - Remove unneeded functions from smwrQW package.

# baytrends 0.3.3.9015

  - Released - 2018-01-31
  - Created vignettes:
      - QW object conversion
      - Detrended flow data
      - Detrended salinity data
  - Add and remove funtions from index with keyword internal.

# baytrends 0.3.3.9014

  - Released - 2018-01-12
  - Add more functions, as.x, to keywords internal to remove from help
    file list.
  - Add Erik W Leppo as author in DESCRIPTION.

# baytrends 0.3.3.9013

  - Released - 2018-01-11
  - Modified dataCensored to include timezone. Should fluctuate between
    EST and EDT.
      - Exported original data file with TZ included with smwrQW
        present. Then rebuilt dataCensored with smwrQW removed.

# baytrends 0.3.3.9012

  - Released - 2018-01-05
  - Fix files names. OneDrive on development computer introduced
    duplicate files. Some of older versions.
  - Hid all smwr\*.R files from help file index with keywords internal.

# baytrends 0.3.3.9011

  - Released - 2017-12-20
  - dataCensored
      - Clean up “data-raw” folder and GenerateData script.
      - Overwrite smwrQW dependant dataCensored.rda file with baytrends
        version.
      - Remove dataCensored CSV files from .
  - Move
  - Rename temporary R scripts from “x\_\*" to “smwrQW\_x\_\*“.
      - Known files that are needed already have smwrQW prefix. Keep new
        ones different.

# baytrends 0.3.3.9010

  - Released - 2017-12-19
  - Rebuild with minor edits for smwrQW references in files (Find in
    Files).
  - Added smwrQW\_is.na.R.
  - Edits to \_GenerateData\_dataCensored.R.
      - qw object creates but still fails when adding to data.frame.
  - Copy in all smwrQW function not already added. Test if builds.

# baytrends 0.3.3.9009

  - Released - 2017-12-06
  - Added function as.qw from smwrQW.
      - Modified include from qw-class.R to a1\_smwrQW\_qw-class.R.
      - Did not add as.lcens or as.mcens as they require other functions
        in smwrBase and smwrQW.
  - Added “data-raw” folder.
  - Created all parts of dataCensored object and saved to “data-raw”.
  - Added script to build dataCensored.
  - Renamed back to baytrends on GitHub and in development code.
  - a1\_smwrQW\_qw-class.R
      - Comment out “name”
  - as.qw.R
      - Comment out “See Also” (refers to function not ported from
        smwrQW).

# baytrends 0.3.3.9008

  - Released - 2017-12-05
  - Resave dataCensored.rda. Installed smwrQW so could open and resets
    “qw” class to first reference in namespace (baytrends). Then saved
    and removed smwr packages.
  - Add lscens class as well; a2\_smwrQW\_lcens-class.R.
  - Rename mscens from a2 to a3 to preserve collate order of smwrQW.

# baytrends 0.3.3.9007

  - Released - 2017-12-05
  - Rename new class objects to appear in R folder first. And will load
    first.
      - ‘smwrQW\_qw-class.R’ (prefix “a1\_”).
      - ‘smwrQW\_mcens-class.R’ (prefix “a2\_”).
  - Need to fix functions and examples that are looking for smwrQW.
      - analysisOrganizeData
      - selectData
      - gamDiff
      - gamTest

# baytrends 0.3.3.9006

  - Released - 2017-12-05
  - Removed swmr packages from R install.
  - Rebuild baytrends with smwrBase functions included.
      - baseDay
      - baseDay2decimal
  - Removed import from smwrQW from gamTest. Had import for smwrQW.
  - Added class objects qw and mcens from smwrQW.
  - Renamed all added R files with smwrBase\_ or smwrQW\_.

# baytrends 0.3.3.9005

  - Released - 2017-12-05
  - Remove all smwrX packages (Base, Graphs, QW, and Stats). Not just
    the oprhaned QW and Stats. Base and Graphs DEPEND on QW as well.

# baytrends 0.3.3.9004

  - Released - 2017-12-04
  - Modified NEWS to NEWS.rmd.

# baytrends 0.3.3.9003

  - Released - 2017-12-04
  - Remove “smwrQW” from Depends in DESCRIPTION.
  - Remove “smwrStats” from Suggests in DESCRIPTION.

# baytrends 0.3.3.9002

  - Released - 2017-12-04
  - Add Notebooks for Library Creation and Testing to .

# baytrends 0.3.3.9001

  - Released - 2017-12-04
  - Renamed to baytrends033
  - Test version to GitHub

# baytrends 0.3.3

  - Released - 11/24/2017
  - Updated gamDiff: flow/salinity model to calculate baseline/current
    mean differences based on average conditions
  - Updated gamPlotDisp: corrected full model display option

# baytrends 0.3.2

  - Released - 8/09/2017
  - Updated seasonal model to include averaging over season and
    flow\[salinity\]

# baytrends 0.3.1

  - Released - 8/6/2017
  - Added function loadExcel to facilitate reading user Excel files.
    Similar functionality to the existing function loadData.
  - Changed nomenclature from tidalStations to stationMasterList
  - Enhanced knot specification to allow 2 levels of knots (gamK1 and
    gamK2). Previously only could specify gamK\_CritSel. Specification
    of gamK1 and gamK2 now included as part of specifying gamModels
  - Enhanced to allow for including flow or salinity as an independent
    variable in gam models, including adding gam4 to default list of gam
    models evaluated. (**Does** not include updated for computing
    seasonally averaged/flow\[salinity\] averaged model in this
    version.)

# baytrends 0.2.7

  - Released - 7/20/2017
  - Enhanced baytrends to output p-values and estimates of change
    related to laboratory method changes (“interventions”) in a style
    that compares method “A” to “B”, “B” to “C”, “C” to “D”, etc. rather
    than “A” to “B”, “A” to “C”, “A” to “D”, etc.

# baytrends 0.2.6

  - Released - 4/25/2017
  - corrected usage of user specified station file

# baytrends 0.2.5

  - Released - 3/15/2017
  - updated loadData to allow for specifying strings indicating null
    values, see naChar
  - updated multiple functions to address ‘no visible binding for global
    variable’ notes
  - added @importFrom statements for graphics and stat functions

# baytrends 0.2.4

  - Released - 2/9/2017
  - updated seasonal average model algorithm to be independent of mn.doy
  - added algorithm to compute number of knots in s(cyear) term based on
    record length
  - added user ability to set ‘select’ term in mgcv::gam function to
    TRUE, FALSE, or algorithm selected
  - added user ability to set expectation maximization convergence
    threshold
  - added F-stat evaluation and ANOVA table modification

# baytrends 0.2.3

  - Released - 11/09/2016
  - Documentation updated
  - Added default settings to for gam formula for intervention testing
  - Added functionality to trim early data where the level of censoring
    exceeds desired levels
  - Added functionality to thin prediction data set to improve speed and
    reduce the size of the returned results
  - loadData: added functionality to convert date-like fields to POSIXct
    format (see argument convDates)
  - Add error trap for when all data associated with dependent variable
    are NA

# baytrends 0.2.2

  - Released - 10/17/2016
  - Updated graphics for censored data

# baytrends 0.2.1

  - Original release - 10/13/2016
