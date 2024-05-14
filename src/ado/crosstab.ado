*! version 1.0, Patrick Lavallee Delgado, September 2023

capture program drop crosstab
program define crosstab, rclass

  ******************************************************************************
  *
  * crosstab
  *
  * To do:  Write documentation.
  *         Draw fewer borders.
  *         Export to Latex.
  *
  ******************************************************************************

  quietly {

  * Start timer.
  local start = now()

  * Parse arguments.
  syntax [varlist(default=none numeric fv)] using/ [if] [in] [fweight aweight], [ ///
    sheet(string) ///
    by(string) ///
    Statistics(namelist) ///
    VARnames NOVARnames ///
    TEST NOTEST ///
    HIghlight NOHIghlight ///
    STARSlevels(numlist max=3 >=0 <=1 sort) nostars ///
    OVERALLlabel(string) nooverall ///
    nolabel ///
    catt(integer 0) ///
  ]

  * Mark observations and variables.
  marksample touse, novarlist strok
  local masterlist `varlist'

  * Generate weight if specified.
  if !mi("`weight'") {
    tempvar weightvar
    gen `weightvar' `exp'
    local wgt `weight' = `weightvar'
  }

  * Check destination.
  local book `using'
  capture assert regexm("`book'", "\.xlsx?$")
  if _rc {
    display as error "destination file must be Excel workbook"
    error _rc
  }
  if mi("`sheet'") local sheet Sheet1
  capture assert strlen("`sheet'") <= 31
  if _rc {
    display as error "Excel worksheet name length cannot exceed 31 characters"
    error _rc
  }

  * Check statistics.
  local SUMMARY_STATISTICS N missing mean sum sum_w min max range sd variance cv semean skewness kurtosis p1 p5 p10 p25 median p50 p75 p90 p95 p99 iqr
  local clusters
  if mi("`statistics'") {
    local statistics N missing mean
  }
  else {
    local statistics : subinstr local statistics "q" "p25 p50 p75", all word
    local statistics : list uniq statistics
    foreach var in `: list statistics - SUMMARY_STATISTICS' {
      capture confirm variable `var'
      if _rc {
        display as error "cluster variable not found: `var'"
        error 111
      }
      local clusters `clusters' `var'
    }
  }

  * Set default variable names option.
  local varnames `varnames' `novarnames'
  if mi("`varnames'") local varnames novarnames
  opts_exclusive "`varnames'"

  * Set default test option.
  local test `test' `notest'
  if mi("`test'") local test notest
  opts_exclusive "`test'"

  * Set default highlight option.
  local highlight `highlight' `nohighlight'
  if mi("`highlight'") local highlight highlight
  opts_exclusive "`highlight'"

  * Set default significance levels option.
  if mi("`starslevels'") local starslevels 0.001 0.01 0.05

  * Set default overall options.
  if mi("`by'", "`overall'") local overall overall
  if mi("`overalllabel'") local overalllabel Overall

  * Parse subpopulations and expand interactions.
  local 0 `by'
  syntax [varlist(default=none fv)], [MIssing NOEMIssing emptyok order(numlist >0)]
  local by
  local byinteraction 0
  foreach var of local varlist {
    fvrevar `var', list
    local var = r(varlist)
    if `: word count `var'' > 2 {
      display as error "multiple interactions in subpopulations not supported"
      exit 9
    }
    local a : word 1 of `var'
    local b : word 2 of `var'
    if !mi("`b'") {
      local fmt : format `a'
      local vlab : value label `a'
      local lab_b : variable label `b'
      levelsof `a' if `touse', local(vals) `missing'
      foreach val of local vals {
        tempvar x
        clonevar `x' = `b' if `touse' & `a' == `val'
        if !mi("`vlab'") {
          local lab_a : label `vlab' `val'
        }
        else {
          local lab_a : display `fmt' `val'
        }
        label variable `x' "`lab_a' x `lab_b'"
        local by `by' `x'
      }
      local ++byinteraction
    }
    else {
      tempvar x
      clonevar `x' = `a'
      local by `by' `x'
    }
  }

  * Get current frame.
  pwf
  local pwf = r(currentframe)

  * Initialize results frame.
  tempname statsframe
  frame create `statsframe' long(idx) str80(varname) long(subpop) str32(stat) double(value)

  * Initialize means test frame.
  if "`test'" != "notest" {
    tempname testsframe
    frame create `testsframe' long(idx) double(p) str3(stars)
  }

  ******************************************************************************
  * Prepare variables.
  ******************************************************************************

  * Copy variables and observations considered to a new frame.
  tempname tmpframe
  fvrevar `masterlist' `clusters' `by' `weightvar', list
  frame put `r(varlist)' if `touse', into(`tmpframe')
  cwf `tmpframe'
  capture assert c(N)
  if _rc error 2000

  * Drop value labels if requestd.
  if "`label'" == "nolabel" {
    label values `masterlist' .
  }

  * Collect variables.
  local vars
  local labs
  local cats
  local chi2
  foreach var of local masterlist {

    * Expand factor variable list.
    fvexpand `var'
    local fvlist = r(varlist)
    local fvops = r(fvops)

    * Add missing factor variable notation to categorical variable.
    if "`fvops'" != "true" {

      * Per labeled values.
      capture elabel list (`var')
      if !_rc {
        local vals = r(values)
        levelsof `var', local(lvls)
        local var i(`: list vals & lvls').`var'
      }

      * Per distinct value threshold.
      else {
        capture assert !mod(`var', 1) & !inlist(`var', 0, 1) if !mi(`var'), null fast
        if !_rc {
          distinct `var'
          if `r(ndistinct)' <= `catt' {
            local var i.`var'
          }
        }
      }

      * Expand factor variable list again.
      fvexpand `var'
      local fvlist = r(varlist)
      local fvops = r(fvops)
    }

    * Register base variable.
    fvrevar `var', list
    local base = subinstr("`r(varlist)'", " ", "#", .)
    local vars `vars' `base'
    local labs `labs' `base'

    * Register values.
    if "`fvops'" == "true" {

      * Generate indicators on each value.
      local vals
      foreach fv of local fvlist {
        local fv = ustrregexra("`fv'", "(?<=[0-9])b(?=\.)", "")
        tempvar x
        gen `x' = `fv'
        local vals `vals' `x'
        local vars `vars' `x'
        local labs `labs' `fv'
      }

      * Generate consolidated categorical for chi-squared test.
      tempvar x
      egen `x' = group(`vals')
      local chi2 `chi2' `x'
      local cats `cats' `base'
    }
  }

  ******************************************************************************
  * Prepare subpopulations.
  ******************************************************************************

  * Generate overall subpopulation if requested.
  local tot = c(N)
  if "`overall'" != "nooverall" {
    tempvar overallvar
    tempname vlab
    gen `overallvar' = 1
    label define `vlab' 1 "`overalllabel'"
    label values `overallvar' `vlab'
    local by `overallvar' `by'
  }

  * Recode subpopulation values so that each is guaranteed unique.
  local i = 0
  local by : list uniq by
  local subpoplist `by'
  local vlablist
  foreach var of local by {

    * Encode strings.
    capture confirm numeric variable `var'
    if _rc {
      tempvar x
      tempname vlab
      encode `var', gen(`x') label(`vlab')
      label variable `x' "`: variable label `var''"
      local subpoplist : subinstr local subpoplist "`var'" "`x'", word
      local var `x'
    }

    * Make value labels from display format if not available.
    capture elabel list (`var')
    if _rc {
      capture assert `var' >= 0, fast
      if _rc {
        display as error "subpopulation variable `var' may not contain negative values"
        error 452
      }
      capture assert !mod(`var', 1) if !mi(`var'), fast
      if _rc {
        display as error "subpopulation variable `var' may not contain noninteger values"
        error 452
      }
      tempname vlab
      local fmt : format `var'
      levelsof `var', local(vals)
      foreach val of local vals {
        local lab : display `fmt' `val'
        label define `vlab' `val' "`lab'", add
      }
      label values `var' `vlab'
    }

    * Unshare value labels.
    tempname vlab
    local vlablist `vlablist' `vlab'
    elabel copy (`var') `vlab'
    elabel values `var' `vlab'

    * Prune empty values.
    if "`emptyok'" != "emptyok" {
      elabel list (`var')
      foreach val of numlist `r(values)' {
        count if `var' == `val'
        if !r(N) {
          label define `vlab' `val' "", modify
        }
      }
    }

    * Recode extended and system missing values.
    elabel list (`var')
    local max = r(max)
    if "`noemissing'" == "noemissing" {
      elabel adjust: recode `var' (miss = .)
    }
    else if `r(hasemiss)' > 0 {
      foreach val of numlist `r(values)' {
        if mi(`val') {
          elabel adjust: recode `var' (`val' = `++max')
        }
      }
    }
    if "`missing'" == "missing" {
      capture assert !mi(`var'), fast
      if _rc {
        local ++max
        replace `var' = `max' if `var' == .
        label define `vlab' `max' "Missing", add
      }
    }

    * Recode integer values.
    replace `var' = `var' + `i'
    elabel define (`var') (= # + `i') (= @), replace
    local i = `i' + `max' + 1
  }

  * Generate separate observations for each subpopulation.
  tempvar obs idx
  gen `obs' = _n
  expand `: word count `subpoplist''
  bysort `obs': gen `idx' = _n

  * Consolidate subpopulation variables into one.
  local i = 1
  tempvar by
  gen `by' = .
  foreach subpop of local subpoplist {
    replace `by' = `subpop' if `idx' == `i++'
  }
  drop if mi(`by')
  if "`overall'" != "nooverall" {
    assert (`by' == `overallvar') == (`idx' == 1)
  }

  * Label subpopulations.
  tempname by_vlab
  elabel define `by_vlab' = combine(`vlablist')
  elabel values `by' `by_vlab'
  elabel list `by_vlab'
  local by_vals = r(values)

  * Reorder subpopulations if requested.
  if !mi("`order'") {
    local by_vals2
    foreach i of numlist `order' {
      local val : word `i' of `by_vals'
      local by_vals2 `by_vals2' `val'
    }
    capture assert `: list by_vals2 === by_vals'
    if !_rc local by_vals `by_vals2'
  }

  ******************************************************************************
  * Crosstabulate.
  ******************************************************************************

  * Consider each variable.
  forvalues i = 1/`: word count `vars'' {

    * Get the variable, name, and categorical id.
    local var : word `i' of `vars'
    local lab : word `i' of `labs'
    local cat : list posof "`var'" in cats

    * Consider each subpopulation.
    foreach val of numlist `by_vals' {

      * Handle categorical header.
      if `cat' {
        local stat : word 1 of `statistics'
        frame post `statsframe' (`i') ("`lab'") (`val') ("`stat'") (.)
        continue
      }

      * Get the subpopulation size.
      count if `by' == `val'
      local N = r(N)

      * Calculate requested statistics.
      summarize `var' if `by' == `val' [`wgt'], detail
      foreach stat of local statistics {
        if "`stat'" == "missing" {
          local res = 1 - (r(N) / `N')
        }
        else if "`stat'" == "range" {
          local res = r(max) - r(min)
        }
        else if "`stat'" == "variance" {
          local res = r(Var)
        }
        else if "`stat'" == "cv" {
          local res = r(sd) / r(mean)
        }
        else if "`stat'" == "semean" {
          local res = r(sd) / sqrt(r(N))
        }
        else if "`stat'" == "median" {
          local res = r(p50)
        }
        else if "`stat'" == "iqr" {
          local res = r(p75) - r(p25)
        }
        else if `: list stat in SUMMARY_STATISTICS' {
          local res = r(`stat')
        }
        else if `: list stat in clusters' {
          continue
        }
        else {
          display as error "unknown statistic: `stat'"
          error 198
        }
        frame post `statsframe' (`i') ("`lab'") (`val') ("`stat'") (`res')
      }
      foreach cluster of local clusters {
        distinct `cluster' if `by' == `val' & !mi(`var')
        frame post `statsframe' (`i') ("`lab'") (`val') ("`cluster'") (r(ndistinct))
      }
    }

    * Run tests across subpopulations.
    if "`test'" != "notest" {

      * Do not test with overall subpopulation.
      local if
      if "`overall'" != "nooverall" local if if `by' > 1

      * Test independence of categories across subpopulations.
      if `cat' {
        local cat : word `cat' of `chi2'
        tabulate `cat' `by' `if' [`wgt'], chi2
        local p = r(p)
      }

      * Test equality of means across subpopulations.
      else {
        capture anova `var' `by' `if' [`wgt']
        if !_rc {
          local p = 1 - F(e(df_m), e(df_r), e(F))
        }
        else if _rc == 2000 {
          local p = .
        }
        else {
          error _rc
        }
      }

      * Set significance level.
      local sig
      foreach a of numlist `starslevels' {
        if `p' < `a' {
          local sig `sig'*
        }
      }

      * Report the p-value of the test.
      frame post `testsframe' (`i') (`p') ("`sig'")
    }
  }

  * Include subpopulation sample size and share.
  local sampsize : list posof "N" in statistics
  local sampprop : list posof "mean" in statistics
  foreach val of numlist `by_vals' {
    count if `by' == `val'
    local n = r(N)
    if `sampsize' {
      frame post `statsframe' (0) ("`by'") (`val') ("N") (`n')
    }
    if `sampprop' {
      local p = `n' / `tot'
      frame post `statsframe' (0) ("`by'") (`val') ("mean") (`p')
    }
  }

  ******************************************************************************
  * Make table.
  ******************************************************************************

  * Switch to results frame.
  cwf `statsframe'
  isid varname subpop stat

  * Transform to variable level.
  reshape wide value, i(varname subpop) j(stat) string
  rename value* *
  reshape wide `statistics', i(varname) j(subpop)
  isid idx

  * Label rows.
  tempname x
  split varname, parse(#) gen(`x')
  local K = r(k_new)
  gen varlabel = ""
  forvalues i = 1/`c(N)' {

    * Handle sample size.
    if idx[`i'] == 0 {
      replace varname   = ""            in `i'
      replace varlabel  = "Sample size" in `i'
      continue
    }

    * Handle variable.
    local varlabel
    forvalues k = 1/`K' {

      * Get variable and value.
      assert ustrregexm(`x'`k'[`i'], "^(([0-9]+)\.)?([a-z0-9_]+)$", 1)
      local var = ustrregexs(3)
      local val = ustrregexs(2)
      assert !mi("`var'") | (mi("`var'") & `k' > 1)

      * Get corresponding variable and value labels.
      frame `pwf' {
        if "`val'" == "" {
          local lab : variable label `var'
        }
        else {
          if !mi("`: value label `var''") {
            local lab : label (`var') `val'
          }
          else {
            local fmt : format `var'
            local lab : display `fmt' `val'
          }
        }
      }

      * Set row label.
      local varlabel = strtrim("`lab'")
      if `k' == 1 & "`val'" != "" {
        local varlabel "    `lab'"
      }
      else if `k' > 1 {
        local varlabel " x `lab'"
      }
      replace varlabel = varlabel + "`varlabel'" in `i'
    }
  }

  * Label columns.
  label variable varlabel "Variable"
  label variable varname  "Variable name"
  foreach val of numlist `by_vals' {
    frame `tmpframe': local lab : label `by_vlab' `val'
    foreach stat of local statistics {
      label variable `stat'`val' "`lab' (`stat')"
    }
  }

  * Include tests.
  if "`test'" != "notest" {
    frlink 1:1 idx, frame(`testsframe')
    frget p stars, from(`testsframe')
  }

  * Clean up.
  sort idx
  local varlist varlabel
  foreach val of numlist `by_vals' {
    foreach stat of local statistics {
      local varlist `varlist' `stat'`val'
    }
  }
  if "`varnames'" != "novarnames" {
    local varlist varname `varlist'
  }
  if "`test'" != "notest" {
    local varlist `varlist' p
    if "`stars'" != "nostars" local varlist `varlist' stars
  }
  keep `varlist'
  order `varlist'
  compress

  ******************************************************************************
  * Write to Excel workbook.
  ******************************************************************************

  * Set row number anchors per number of subpopulation variables.
  local r = cond(`byinteraction' | `: word count `subpoplist'' > 2, 4, 3)
  local h3 = `r'  - 1
  local h2 = `h3' - 1
  local h1 = `h2' - 1

  * Save to the workbook and initialize a handler for formatting.
  export excel "`book'", sheet("`sheet'", replace) firstrow(varlabels) cell(A`h3')
  putexcel set "`book'", sheet("`sheet'") open modify

  * Write column headers over variables, subpopulations, and statistics.
  foreach var of local subpoplist {
    local superlist
    frame `tmpframe': elabel list (`var')
    local subpop_vals = r(values)
    local subpop_labs = r(labels)
    forvalues i = 1/`r(k)' {
      local sublist
      local val : word `i' of `subpop_vals'
      local lab2 : word `i' of `subpop_labs'
      local lab3
      foreach stat of local statistics {
        local sublist `sublist' `stat'`val'
        local lab3 `lab3' `stat'
      }
      local superlist `superlist' `sublist'
      xlfmt header `h2' `sublist', start(A`r') label("`lab2'") merge hcenter bold
      xlfmt header `h3' `sublist', start(A`r') label("`lab3'") hcenter italic
      xlfmt cols `: word 1 of `sublist'', start(A`r') border(left) headerrows
    }
    if `h1' & "`var'" != "`overallvar'" {
      frame `tmpframe': local lab1 : variable label `var'
      xlfmt header `h1' `superlist', start(A`r') label("`lab1'") merge hcenter bold
    }
  }

  * Set column widths.
  ds, has(type str#)
  foreach col in `r(varlist)' {
    local width = regexr("`: type `col''", "^str", "")
    local width = max(16, min(`width', 80))
    xlfmt cols `col', colwidth(`width')
  }

  * Format numbers.
  ds, has(type byte int long)
  local integers = r(varlist)
  if !mi("`integers'") xlfmt cols `integers', start(A`r') nformat(0)
  ds, has(type float double)
  local decimals = r(varlist)
  if !mi("`decimals'") xlfmt cols `decimals', start(A`r') nformat(0.00)

  * Format cross-group test p-value.
  if "`test'" != "notest" {
    xlfmt header `h2' p, start(A`r') label(" ")
    xlfmt header `h3' p, start(A`r') label("p") hcenter italic
    xlfmt cols p, start(A`r') border(left) headerrows
    if "`stars'" != "nostars" {
      xlfmt header `h2' stars, start(A`r') label(" ")
      xlfmt header `h3' stars, start(A`r') label("stars") hcenter italic
    }
    if "`highlight'" != "nohighlight" {
      local n : word count `starslevels'
      local sig : word `n' of `starslevels'
      xlfmt cols p, start(A`r') nformat(0.000)
      xlfmt rows p < `sig', start(A`r') fpattern(solid, yellow)
    }
  }

  * Add borders around row headers.
  * xlfmt rows ustrregexm(varlabel, "^[^\s]"), start(A`r') border(top)
  * xlfmt rows ustrregexm(varlabel, "^[^\s]"), start(A`r') border(bottom)

  * Freeze row and column labels.
  local --r
  local c = cond("`varnames'" != "novarnames", 2, 1)
  putexcel sheetset, split(`r', `c')

  * Close the worksheet.
  putexcel save
  putexcel clear

  * Return calling frame.
  cwf `pwf'

  * Report total time elapsed.
  local timer = now() - `start'
  local timer : display %-tcHH:MM:SS `timer'
  return local timer = "`timer'"

  }

end
