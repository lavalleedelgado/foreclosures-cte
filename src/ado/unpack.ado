*! version 1.0, Patrick Lavallee Delgado, August 2023

capture program drop unpack
program define unpack

  ******************************************************************************
  *
  * unpack
  *
  * Assign list elements to macro names.
  *
  ******************************************************************************

  * Parse arguments.
  gettoken namelist 0 : 0, parse(":")
  gettoken colon itemlist : 0
  assert "`colon'" == ":"

  * Validate arguments.
  local N : word count `namelist'
  capture assert `N' == `: word count `itemlist''
  if _rc {
    display as error "must have as many macro names as list elements"
    exit _rc
  }

  * Assign list elements to corresponding macro names.
  forvalues i = 1/`N' {
    local name : word `i' of `namelist'
    local item : word `i' of `itemlist'
    c_local `name' "`item'"
  }

end
