version 18
clear all
set type double
set linesize 120

* Identify inputs.
local PWD "`c(pwd)'"
local ADO "`PWD'/src/ado"
local VAR "`PWD'/in/varlist.xlsx"
local DTA "[ HSLS ]/analysis.dta"

* Identify outputs.
local LOG "`PWD'/src/02-run-analysis.log"
local TMP "[ HSLS ]/imputed.dta"
local OUT "`PWD'/out"
local TAB "`OUT'/table1.xlsx"

* Add project commands to the path.
adopath ++ "`ADO'"

* Start the log.
capture log close
log using "`LOG'", replace

********************************************************************************
*
* Project:  Foreclosures and endogenous coursetaking
* Purpose:  Run analyses
* Author:   Patrick Lavallee Delgado
* Created:  8 May 2024
*
* Notes:    Update path to HSLS directory on data room workstation.
*           Assuming linear probability model.
*
* To do:
*
********************************************************************************


* Read analysis file.
use "`DTA'", clear
describe
isid stu_id

* Collect variable lists from control file.
preserve
  import excel "`VAR'", sheet("variables") firstrow clear
  foreach i in Y X G Z {
    display _n(2) "variable list: `i'"
    listof varname if vargroup == "`i'", clean verbose
    local `i' = r(varname)
  }
restore
confirm variable `Y' `X' `G' `Z'

* Distinguish continuous and categorical regressors.
quietly ds `X', has(vallabel)
local Xi = r(varlist)
local Xc : list X - Xi

* Summarize outcomes and characteristics.
capture rm "`TAB'"
egen uniq = tag(fips)
crosstab `Y' `G' `X' i.region [aw = pw] using "`TAB'", sheet(students) statistics(N missing mean min q max)
crosstab `Z' if uniq using "`TAB'", sheet(counties) statistics(N missing mean min q max)

* Calculate correlations between endogenous and instrumental variables.
foreach z of local Z {
  pwcorr `G' `z' [aw = pw]
}

********************************************************************************
* Impute missing data.
********************************************************************************

capture confirm file "`TMP'"
if _rc {

  * Set up multiply imputed date.
  mi set flong
  mi register imputed `G' `X'
  mi register regular `Y' `Z'

  * Generate imputations.
  mi impute chained (regress) `Xc' `G' (ologit) `Xi' [pw = pw], add(20) rseed(1) double

  * Write to disk.
  quietly compress
  save "`TMP'", replace
}

********************************************************************************
* Run analyses.
********************************************************************************

* Reload data.
use "`TMP'", replace

* Recover state.
gen state = floor(fips / 1000)

* Consider each outcome.
tempvar sample
foreach y of local Y {

  display _n(2) _dup(80) "*" _n "`y': `: variable label `y''" _n _dup(80) "*"

  * Mark estimation sample.
  capture drop `sample'
  bysort state: egen `sample' = sd(`y')
  replace `sample' = `sample' > 0
  capture assert `sample'
  if _rc {
    display _n(2) "states excluded:"
    tabulate state `y' if !`sample'
  }

  * Collect joint tests.
  local ftest "F-test "

  * Estimate with potentially endogenous variables.
  tempname m1
  mi estimate: areg `y' `G' i.(`Xi') `Xc' if `sample' [pw = pw], absorb(state) vce(cluster nces)
  estimates store `m1'
  mi test `G'
  local p : display %7.6f `r(p)'
  local ftest "`ftest' & `p'"

  * Test whether instruments are part of the model.
  tempname m2
  mi estimate: areg `y' `G' `Z' i.(`Xi') `Xc' if `sample' [pw = pw], absorb(state) vce(cluster nces)
  estimates store `m2'
  mi test `Z'
  local p : display %7.6f `r(p)'
  local ftest "`ftest' & `p'"

  * Estimate with S2SLS.
  tempname m3
  miiv 2sls `y' i.(`Xi') `Xc' (`G' = `Z') if `sample' [pw = pw], absorb(state)
  estimates store `m3'
  test `G'
  local p : display %7.6f `r(p)'
  local ftest "`ftest' & `p'"

  /* * Estimate with GIV.
  tempname m4
  miiv giv `y' i.(`Xi') `Xc' (`G' = `Z') if `sample' [pw = pw], absorb(state)
  estimates store `m4'
  test `G'
  local p : display %7.6f `r(p)'
  local ftest "`ftest' & `p'" */

  * Make table.
  modeltab * using "`OUT'/`y'.tex", keep(`G' `Z') barebones addrows("`ftest'")
  estimates clear
}

* Close the log.
log close
archive "`LOG'", into("_archive")
