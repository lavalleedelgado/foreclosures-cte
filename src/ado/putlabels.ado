*! version 1.0, Patrick Lavallee Delgado, September 2023

capture program drop putlabels
program define putlabels

  ******************************************************************************
  *
  * putlabels
  *
  * Set variable names, value labels, and formats from a control file given as
  * an Excel workbook. Optionally drop variables not specified.
  *
  ******************************************************************************

  quietly {

  * Parse arguments.
  syntax using/, [ ///
    variables(string) ///
    varname(string) ///
    varlabel(string) ///
    vallabel(string) ///
    format(string) ///
    series(string) ///
    values(string) NOVALUES ///
    value(string) ///
    label(string) ///
    DROP NODROP ///
    ORDER NOORDER ///
    keepif(string asis) ///
  ]

  * Set default sheet and column names.
  foreach x in variables varname varlabel vallabel format series values value label {
    if mi("``x''") local `x' `x'
  }

  * Enforce mutually exclusive options.
  foreach x in values drop order {
    local `x' ``x'' `no`x''
    opts_exclusive "``x''"
  }

  * Check control file is an Excel workbook.
  capture assert regexm("`using'", "\.xlsx?$")
  if _rc {
    display as error "control file must be Excel workbook"
    exit _rc
  }

  * Initialize do file.
  tempfile do
  tempname f
  file open `f' using "`do'", write

  * Work with control file in new frame.
  pwf
  local pwf = r(currentframe)
  tempname tmp
  mkf `tmp'
  cwf `tmp'

  ******************************************************************************
  * Make value label instructions.
  ******************************************************************************

  if "`novalues'" == "" {

    * Read value label list.
    import excel "`using'", sheet(`values') firstrow
    isid `vallabel' `value' `label'

    * Get list of value label definitions.
    levelsof `vallabel', local(defvlablist)

    * Set value labels.
    forvalues i = 1/`c(N)' {
      local x = `vallabel'[`i']
      local v = `value'[`i']
      local l = `label'[`i']
      file write `f' `"label define `x' `v' "`l'", add"' _n
    }
  }

  ******************************************************************************
  * Load variable list.
  ******************************************************************************

  * Read variable list.
  import excel "`using'", sheet(`variables') firstrow clear
  isid `varname'

  * Restrict variable list if requested.
  if "`keepif'" != "" {
    keep if `keepif'
  }

  * Check whether to set value labels and formats.
  foreach x in vallabel format series {
    capture {
      confirm variable ``x''
      count if !mi(``x'')
      assert r(N)
    }
    local set`x' = cond(_rc, 0, 1)
  }

  ******************************************************************************
  * Expand variable list for variable series.
  ******************************************************************************

  if `setseries' {

    * Ensure series is a string.
    capture confirm string variable `series'
    if _rc {
      display as error "`series' must be type string"
      error _rc
    }

    * Track original sort.
    tempvar sort
    gen `sort' = _n

    * Parse numlists in series specification.
    forvalues i = 1/`c(N)' {
      local x = `series'[`i']
      capture numlist "`x'"
      if !_rc {
        local x = r(numlist)
        replace `series' = "`x'" in `i'
      }
    }

    * Transform to series-element level.
    tempvar e j
    split `series', gen(`e')
    reshape long `e', i(`varname') j(`j')
    drop if `j' > 1 & mi(`e')

    * Update labels with series elements.
    foreach var of varlist `varname' `varlabel' {
      replace `var' = ustrregexra(`var', "@", `e') if !mi(`e')
    }
    isid `varname'

    * Restore original sort.
    sort `sort' `e'

  }

  ******************************************************************************
  * Make variable label instructions.
  ******************************************************************************

  * Track variables not in the data.
  tempvar ok
  gen `ok' = 1

  * Consider each variable.
  local ctrlvarlist
  local ctrlvlablist
  forvalues i = 1/`c(N)' {

    * Check whether the variable exists.
    local var = `varname'[`i']
    frame `pwf': capture confirm variable `var', exact
    if _rc {
      replace `ok' = 0 in `i'
      continue
    }
    local ctrlvarlist `ctrlvarlist' `var'

    * Set the variable label.
    local x = `varlabel'[`i']
    file write `f' `"label variable `var' "`x'""' _n

    * Set the value label.
    if `setvallabel' {
      frame `pwf': capture confirm numeric variable `var', exact
      if !_rc {
        local x = `vallabel'[`i']
        if "`x'" == "" {
          local x .
        }
        else {
          local ctrlvlablist `ctrlvlablist' `x'
        }
        file write `f' `"label value `var' `x'"' _n
      }
    }

    * Set the format.
    if `setformat' {
      local x = `format'[`i']
      file write `f' `"format `var' `x'"' _n
    }
  }

  * Report variables not in the data.
  capture assert `ok'
  if _rc {
    noisily {
      display "variables not in the data:"
      list `varname' `varlabel' if !`ok', ab(32) noobs
    }
  }

  ******************************************************************************
  * Label variables.
  ******************************************************************************

  * Write labels.
  cwf `pwf'
  file close `f'
  if "`defvlablist'" != "" label drop _all
  include "`do'"

  * Report variables not in the control file.
  ds `ctrlvarlist', not
  local missvarlist `r(varlist)'
  if "`missvarlist'" != "" {
    noisily display "variables not defined in control file:"
    foreach x of local missvarlist {
      noisily display _col(4) "`x'"
    }
    if "`drop'" == "drop" {
      noisily display "dropping these variables"
      drop `missvarlist'
    }
  }

  * Report value labels not in the control file.
  if "`defvlablist'" != "" {
    local ctrlvlablist : list uniq ctrlvlablist
    local missvlablist : list ctrlvlablist - defvlablist
    if "`missvlablist'" != "" {
      noisily display "value labels not defined in control file:"
      foreach x of local missvlablist {
        noisily display _col(4) "`x'"
      }
    }
  }

  * Reorder variables if requested.
  if "`order'" == "order" {
    order `ctrlvarlist'
  }

  }

end
