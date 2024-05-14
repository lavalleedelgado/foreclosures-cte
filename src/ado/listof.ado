*! version 1.0, Patrick Lavallee Delgado, August 2023

capture program drop listof
program define listof, rclass

  ******************************************************************************
  *
  * listof
  *
  * Read variables into macro lists. Unlike the -levelsof- command, this
  * implementation does not deduplicate and preserves element order.
  *
  * listof [varlist] [if] [in], [clean verbose]
  *
  * varlist: variables to list into macro lists of the same name
  * clean: whether to retokenize with minimal adornment
  * verbose: print control file to console
  *
  ******************************************************************************

  quietly {

  * Parse arguments.
  syntax [varlist] [if] [in], [clean Verbose]

  * Get current frame.
  pwf
  local pwf = r(currentframe)

  * Put selected variables and observations into a new frame.
  tempname tmp
  frame put `varlist' `if' `in', into(`tmp')
  cwf `tmp'

  * Print control file if requested.
  if "`verbose'" == "verbose" {
    noisily list, noobs ab(32)
  }

  * Return list item count.
  return scalar N = c(N)

  capture {

  * Load macro lists.
  foreach var of local varlist {
    local list
    forvalues i = 1/`c(N)' {
      local item = `var'[`i']
      local list "`list' `"`item'"'"
    }
    if "`clean'" == "clean" {
      local list : list clean list
      return local `var' `list'
    }
    else {
      return local `var' `"`list'"'
    }
  }

  }

  * Clean up.
  cwf `pwf'
  exit _rc

  }

end
