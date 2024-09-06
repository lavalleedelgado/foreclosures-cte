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
local LOG "`PWD'/src/02-impute-missing.log"
local OUT "[ HSLS ]/imputed.dta"

* Add project commands to the path.
adopath ++ "`ADO'"

* Start the log.
capture log close
log using "`LOG'", replace

********************************************************************************
*
* Project:  Mediated effects of foreclosures on coursetaking
* Purpose:  Impute missing data
* Author:   Patrick Lavallee Delgado
* Created:  2 July 2024
*
* Notes:    Update path to HSLS directory on data room workstation.
*           Imputes with linear regression where more appropriate methods fail.
*           CTE concentrator defined at three-credit threshold.
*
* To do:
*
********************************************************************************


********************************************************************************
* Load specs.
********************************************************************************

* Read control file.
import excel "`VAR'", sheet("variables") firstrow clear
isid varname

* Collect outcomes.
listof varname if vargroup == "Y", clean verbose
local Y = r(varname)

* Separate registration type and imputation method.
gen     miregtype = mimethod  if inlist(mimethod, "regular", "passive")
replace mimethod  = ""        if !mi(miregtype)
replace miregtype = "imputed" if !mi(mimethod)

* Collect registration lists.
local miregtypes regular passive imputed
quietly levelsof miregtype, local(specmiregtypes)
assert `: list miregtypes === specmiregtypes'
foreach i of local miregtypes {
  display _n(2) "register: `i'"
  listof varname if miregtype == "`i'", clean verbose
  local `i' = r(varname)
}

* Collect imputation queue.
local mimethods regress logit mlogit ologit intreg
quietly levelsof mimethod, local(specmimethods)
assert `: list mimethods === specmimethods'
foreach i of local mimethods {
  display _n(2) "imputation method: `i'"
  listof varname if mimethod == "`i'", clean verbose
  local `i' = r(varname)
}

* Write multiple imputation spec.
drop if mi(mimethod)
local spec
forvalues i = 1/`c(N)' {
  local mimethod  = mimethod[`i']
  local varname   = varname[`i']
  local mioption  = mioption[`i']
  local spec `spec' (`mimethod', `mioption') `varname'
}
local spec `spec' = `regular'

********************************************************************************
* Impute missing data.
********************************************************************************

* Read analysis file.
use "`DTA'", clear
isid stuid

* Summarize missingness in outcomes.
misstable summarize `Y', all

* Mark complete cases.
gen complete = 1
foreach var of local Y {
  replace complete = 0 if `var' == .
}
tabulate complete

* Free variables for interval regression.
* Note spec must include bounds and target variable must be uninitialized.
local imputed : list imputed - intreg
rename (`intreg') =_old

* Set multiple imputation design.
mi set flong
mi register imputed `imputed'
mi register passive `passive'
mi register regular `regular'

* Summarize missingness in registered variables.
mi misstable summarize `regular' `imputed' if complete, all

* Generate imputations.
mi impute chained `spec' if complete [pw = pw], add(20) rseed(1) double augment

* Update school-level covariates with average of imputations.
tempvar x y
ds `imputed', has(varlabel "School:*")
foreach var in `r(varlist)' {

  * Mark observations to fix and ensure constant within school.
  bysort _mi_id (_mi_m): gen `x' = mi(`var'[1])
  bysort nces: assert `x'[1] == `x'[_n]

  * Updated marked observations with dataset-level mean.
  bysort _mi_m: egen `y' = mean(`var') if `x'
  replace `var' = `y' if `x' & _mi_m > 0
  drop `x' `y'
}
mi update

* Update indicators imputed as continuous variables.
ds `imputed', has(vallabel yesno_vlab)
foreach var in `r(varlist)' {
  assert inlist(`var', 0, 1) if _mi_m == 0 & !mi(`var')
  replace `var' = `var' >= 0.5 if !mi(`var')
}
mi update

* Bottom code imputed credits.
ds `imputed', has(varlabel "Credits:*")
foreach cred in `r(varlist)' {
  assert `cred' >= 0 if _mi_m == 0
  replace `cred' = 0 if `cred' < 0
}

* Top and bottom code imputed GPA.
ds `imputed', has(varlabel "GPA:*")
foreach gpa in `r(varlist)' {

  * Fix imputations.
  assert inrange(`gpa', 0, 4) if _mi_m == 0 & !mi(`gpa')
  replace `gpa' = 0 if `gpa' < 0
  replace `gpa' = 4 if `gpa' > 4 & !mi(`gpa')

  * Reconcile with credits.
  local cred = ustrregexrf("`gpa'", "^gpa_", "cred_", 0)
  assert `gpa' == 0 if `cred' == 0 & _mi_m == 0
  replace `gpa' = 0 if `cred' == 0
}

* Update concentration indicators.
ds `passive', has(varlabel "Concentrator:*")
foreach conc in `r(varlist)' {
  local cred = ustrregexrf("`conc'", "^conc_", "cred_", 0)
  mi passive: replace `conc' = `cred' >= 3
}
mi update

* Attach variable labels for imputations from interval regression.
foreach var of local interval {
  label variable `var' "`: variable label `var'_old'"
}

* Write to disk.
quietly compress
save "`OUT'", replace

* Close the log.
log close
archive "`LOG'", into("_archive")
