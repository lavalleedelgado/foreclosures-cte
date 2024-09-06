version 18
clear all
set type double
set linesize 120

* Identify inputs.
local PWD "`c(pwd)'"
local ADO "`PWD'/src/ado"
local VAR "`PWD'/in/varlist.xlsx"
local IMP "[ HSLS ]/imputed.dta"

* Identify outputs.
local LOG "`PWD'/src/03-run-analysis.log"
local OUT "`PWD'/out"
local TAB "`OUT'/table1.xlsx"
local EST "`OUT'/table2.xlsx"

* Add project commands to the path.
adopath ++ "`ADO'"

* Start the log.
capture log close
log using "`LOG'", replace

********************************************************************************
*
* Project:  Mediated effects of foreclosures on coursetaking
* Purpose:  Run analyses
* Author:   Patrick Lavallee Delgado
* Created:  8 May 2024
*
* Notes:    Update path to HSLS directory on data room workstation.
*           Assumes linear probability model.
*
* To do:    Bootstrap standard errors of indirect effects.
*
********************************************************************************


********************************************************************************
* Load specs.
********************************************************************************

* Read control file.
import excel "`VAR'", sheet("variables") firstrow clear
isid varname

* Collect variable lists.
local vargroups Y X M W K
quietly levelsof vargroup, local(specvargroups)
assert `: list vargroups === specvargroups'
foreach i of local vargroups {
  display _n(2) "variable list: `i'"
  listof varname if vargroup == "`i'", clean verbose
  local `i' = r(varname)
}

********************************************************************************
* Load data.
********************************************************************************

* Read multiple imputations.
use "`IMP'", clear
mi describe

* Set survey and cross-sectional designs.
mi svyset psu [pw = pw], strata(strata) vce(linearized) singleunit(centered)

* Distinguish continuous and categorical covariates.
quietly ds `X', has(vallabel)
local Xi = r(varlist)
local Xc : list X - Xi

preserve

  * Restore original data.
  mi extract 0, clear
  isid stuid

  * Summarize outcomes and characteristics.
  capture rm "`TAB'"
  egen uniq = tag(fips)
  crosstab `Y' `M' `K' `X' i.region [aw = pw] using "`TAB'", sheet(students) statistics(N missing mean min q max)
  crosstab `W' if uniq using "`TAB'", sheet(counties) statistics(N missing mean min q max)

  * Calculate correlations between mediating and moderating variables.
  foreach w of local W {
    pwcorr `M' `w' [aw = pw]
  }

restore

********************************************************************************
* Write a command to implement seemingly unrelated estimation.
********************************************************************************

program define misuest, eclass properties(mi)

  * Parse arguments.
  syntax anything(name=queue everything), [*]

  * Run estimations in the queue.
  local estlist
  while "`queue'" != "" {

    * Parse next estimation.
    gettoken next queue : queue, bind
    assert ustrregexm("`next'", "^\s*\(([a-z0-9_]+)\s*:(.*)\)$", 1)
    local name = ustrregexs(1)
    local ecmd = ustrregexs(2)

    * Run.
    `ecmd'
    estimates store `name'
    local estlist `estlist' `name'
  }

  * Run seemingly unrelated estimation.
  suest `estlist', `options'
  estimates drop `estlist'

end

********************************************************************************
* Specify models.
********************************************************************************

* Consider each mediator.
local medlist
foreach m of local M {

  * Student and economic predictors of coursetaking.
  local model `m' `W' i.(`Xi') `Xc'
  local medlist `medlist' (`m': svy: regress `model')
}

* Consider each outcome.
local reslist
foreach y of local Y {

  local estlist
  local parlist
  local idxlist

  * Economic moderation with coursetaking mediation.
  local model `y' c.(`M')##c.(`W') i.(`Xi') `Xc'
  local estlist `estlist' (`y': svy: regress `model')

  * Economic and CTE cluster moderation with coursetaking mediation.
  local model `y' c.(`M')##c.(`W') c.(`M')##i.(`K') i.(`Xi') `Xc'
  local estlist `estlist' (`y'_k: svy: regress `model')

  * Specify parameters for indirect effects.
  foreach w of local W {
    foreach q in 05 25 50 75 95 {

      * Without CTE cluster moderation.
      foreach m of local M {
        local a _b[`m':`w']
        local b _b[`y':`m']
        local c 0.`q' * _b[`y':`m'#`w']
        local parlist `parlist' (`a' * (`b' + `c'))
        local idxlist `idxlist' `y'__`m':`w'_`q'
      }

      * With CTE cluster moderation.
      foreach k of local K {
        foreach m of local M {
          local a _b[`m':`w']
          local b _b[`y'_k:`m'] + _b[`y'_k:`m'#1.`k']
          local c 0.`q' * _b[`y'_k:`m'#`w']
          local parlist `parlist' (`a' * (`b' + `c'))
          local idxlist `idxlist' `y'__`m':`w'_`q'__`k'
        }
      }
    }
  }

  * Run estimation.
  mi estimate `parlist': misuest `medlist' `estlist'
  tempname res
  matrix `res' = r(table)'
  matrix rownames `res' = `idxlist'
  local reslist `reslist' `res'
}

********************************************************************************
* Make a table of direct and indirect effects.
********************************************************************************

* Load parameter estimates.
tempname res
matrix rowjoinbyname `res' = `reslist'
clear
svmat `res', name(col)

* Recover parameter labels.
local i 1
gen par = ""
quietly foreach par in `: rowfullnames `res'' {
  replace par = "`par'" in `i++'
}
assert !mi(par)

* Parse parameter labels.
replace par = ustrregexra(par, "(:|__)", " ", 0)
split par, parse(" ") gen(par) limit(4)
rename (par?) (y m w k)
assert !mi(y, m, w)

* Write to Excel.
drop par
order y m w k
export excel "`EST'", firstrow(variables) replace

* Close the log.
log close
archive "`LOG'", into("_archive")
archive "`TAB'", into("_archive")
archive "`EST'", into("_archive")
