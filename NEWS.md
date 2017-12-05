NEWS
================
<Erik.Leppo@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
    #> Last Update: 2017-12-05 11:37:43

v0.3.3.9005
===========

-   2017-12-05
-   Remove all smwrX packages (Base, Graphs, QW, and Stats). Not just the oprhaned QW and Stats. Base and Graphs DEPEND on QW as well.

v0.3.3.9004
===========

-   2017-12-04
-   Modified NEWS to NEWS.rmd.

v0.3.3.9003
===========

-   2017-12-04
-   Remove "smwrQW" from Depends in DESCRIPTION.
-   Remove "smwrStats" from Suggests in DESCRIPTION.

v0.3.3.9002
===========

-   2017-12-04
-   Add Notebooks for Library Creation and Testing to .

v0.3.3.9001
===========

-   2017-12-04
-   Renamed to baytrends033
-   Test version to GitHub

baytrends 0.3.3
===============

-   Released - 11/24/2017
-   Updated gamDiff: flow/salinity model to calculate baseline/current mean differences based on average conditions
-   Updated gamPlotDisp: corrected full model display option

baytrends 0.3.2
===============

-   Released - 8/09/2017
-   Updated seasonal model to include averaging over season and flow\[salinity\]

baytrends 0.3.1
===============

-   Released - 8/6/2017
-   Added function loadExcel to facilitate reading user Excel files. Similar functionality to the existing function loadData.
-   Changed nomenclature from tidalStations to stationMasterList
-   Enhanced knot specification to allow 2 levels of knots (gamK1 and gamK2). Previously only could specify gamK\_CritSel. Specification of gamK1 and gamK2 now included as part of specifying gamModels
-   Enhanced to allow for including flow or salinity as an independent variable in gam models, including adding gam4 to default list of gam models evaluated. (**Does** not include updated for computing seasonally averaged/flow\[salinity\] averaged model in this version.)

baytrends 0.2.7
===============

-   Released - 7/20/2017
-   Enhanced baytrends to output p-values and estimates of change related to laboratory method changes (“interventions”) in a style that compares method “A” to “B”, “B” to “C”, “C” to “D”, etc. rather than “A” to “B”, “A” to “C”, “A” to “D”, etc.

baytrends 0.2.6
===============

-   Released - 4/25/2017
-   corrected usage of user specified station file

baytrends 0.2.5
===============

-   Released - 3/15/2017
-   updated loadData to allow for specifying strings indicating null values, see naChar
-   updated multiple functions to address 'no visible binding for global variable' notes
-   added @importFrom statements for graphics and stat functions

baytrends 0.2.4
===============

-   Released - 2/9/2017
-   updated seasonal average model algorithm to be independent of mn.doy
-   added algorithm to compute number of knots in s(cyear) term based on record length
-   added user ability to set 'select' term in mgcv::gam function to TRUE, FALSE, or algorithm selected
-   added user ability to set expectation maximization convergence threshold
-   added F-stat evaluation and ANOVA table modification

baytrends 0.2.3
===============

-   Released - 11/09/2016
-   Documentation updated
-   Added default settings to for gam formula for intervention testing
-   Added functionality to trim early data where the level of censoring exceeds desired levels
-   Added functionality to thin prediction data set to improve speed and reduce the size of the returned results
-   loadData: added functionality to convert date-like fields to POSIXct format (see argument convDates)
-   Add error trap for when all data associated with dependent variable are NA

baytrends 0.2.2
===============

-   Released - 10/17/2016
-   Updated graphics for censored data

baytrends 0.2.1
===============

-   Original release - 10/13/2016
