*! version 2.1, Patrick Lavallee Delgado, May 2023

capture program drop xlfmt
program define xlfmt

  ******************************************************************************
  * -xlfmt-
  *
  * Format Excel worksheet rows and columns using -putexcel- but by observation
  * index or variable name instead of Excel address. Use existing -putexcel-
  * settings or specify a destination workbook and sheet name.
  *
  * xlfmt {rows|cols} {numlist or exp|varlist} [using], format_options [sheet(sheetname) start(string) {rowheight|colwidth}(integer) headerrows]
  * xlfmt header rownumber varlist [using], labels(string) [sheet(sheetname) start(string) format_options]
  *
  * sheet: worksheet to use
  * start: Excel address where data start to align worksheet, default is A2
  * format_options: see -putexcel- documentation
  * rowheight: row height in point size
  * colwidth: column width in character count
  * headerrows: extend column formatting to header rows
  * labels: label to span columns, or list of labels for each column
  *
  ******************************************************************************

  * Parse subcommand.
  gettoken cmd 0 : 0
  capture assert inlist("`cmd'", "rows", "cols", "header")
  if _rc {
    display as error "subcommand must be rows, cols, or header"
    error 198
  }

  * Parse main arguments.
  syntax anything [using/], [sheet(string) start(string)] *

  * Initialize a worksheet handler or ensure one already exists.
  if !mi("`using'") {
    putexcel set "`using'", sheet("`sheet'") open modify
  }
  capture putexcel describe
  if _rc {
    display as error "no Excel worksheet specified"
    error 198
  }

  * Parse upper left Excel address where data start.
  local c_min = 1
  local r_min = 2
  capture assert ustrregexm(strtrim("`start'"), "^([A-Z]+)([0-9]+)$", 1)
  if !_rc {
    _xlcol c_min : `= ustrregexs(1)'
    local r_min = ustrregexs(2)
  }
  local c_max = c(k) + `c_min' - 1
  local r_max = c(N) + `r_min' - 1

  * Reconstruct command line arguments for subcommand.
  local 0 `anything', `options'

  quietly {

  ******************************************************************************
  * Format rows.
  ******************************************************************************

  if "`cmd'" == "rows" {

    * Parse arguments.
    syntax anything, [rowheight(integer -1) *]

    * Get row numbers.
    local rows
    capture numlist "`anything'"
    if !_rc {
      foreach row in `r(numlist)' {
        local row = `row' + `r_min' - 1
        local rows `rows' `row'
      }
    }
    else {
      forvalues row = 1/`c(N)' {
        capture assert `anything' in `row'
        if !_rc {
          local row = `row' + `r_min' - 1
          local rows `rows' `row'
        }
      }
      if mi("`rows'") exit
    }

    * List Excel addresses for requested rows in the column range.
    local addresses
    _xlcol c_min : `c_min'
    _xlcol c_max : `c_max'
    foreach r of numlist `rows' {
      local addresses `addresses' `c_min'`r':`c_max'`r'
    }

    * Set row height.
    if `rowheight' >= 0 {
      local rows : subinstr local rows " " ", ", all
      mata : xlfmt_row_height((`rows'), `rowheight')
    }

    * Set other row formats.
    if !mi("`options'") {
      putexcel `addresses', `options'
    }
  }

  ******************************************************************************
  * Format columns.
  ******************************************************************************

  if "`cmd'" == "cols" {

    * Parse arguments.
    syntax varlist, [colwidth(integer -1) headerrows *]

    * Reset minimum row to first row if requested.
    if "`headerrows'" == "headerrows" local r_min 1

    * Get column indices.
    local cols
    unab allvars : *
    foreach v of local varlist {
      local k : list posof "`v'" in allvars
      local col = `k' + `c_min' - 1
      local cols `cols' `col'
    }

    * List Excel addresses for requested columns in the row range.
    local addresses
    foreach col of local cols {
      _xlcol c : `col'
      local addresses `addresses' `c'`r_min':`c'`r_max'
    }

    * Set column width.
    if `colwidth' >= 0 {
      local cols : subinstr local cols " " ", ", all
      mata : xlfmt_col_width((`cols'), `colwidth')
    }

    * Set other column formats.
    if !mi("`options'") {
      putexcel `addresses', `options'
    }
  }

  ******************************************************************************
  * Write column header.
  ******************************************************************************

  if "`cmd'" == "header" {

    * Parse arguments.
    gettoken r 0 : 0
    capture {
      confirm integer number `r'
      assert inrange(`r', 1, `r_min' - 1)
    }
    if _rc {
      display as error "header spec must have row number in [1, `r_min')"
      error 198
    }
    syntax varlist, LABels(string) [merge *]

    * Get column indices.
    local cols
    local prev
    unab allvars : *
    foreach v of local varlist {
      local k : list posof "`v'" in allvars
      capture assert `k' == (`prev' + 1) | mi("`prev'")
      if _rc {
        display as error "header spec must have consecutive columns"
        error 198
      }
      local col = `k' + `c_min' - 1
      local cols `cols' `col'
      local prev `col'
    }

    * Get leftmost and rightmost column letters.
    local k : list sizeof varlist
    _xlcol left : `: word 1 of `cols''
    _xlcol right : `: word `k' of `cols''

    * Read column labels into a vector.
    local n : list sizeof labels
    local labs ""`: word 1 of `labels''""
    forvalues i = 2/`n' {
      local lab : word `i' of `labels'
      local labs "`labs', "`lab'""
    }

    * Ensure correspondence of columns to labels.
    if !mi("`merge'") {
      capture assert "`left'" != "`right'"
      if _rc local merge
    }
    if mi("`merge'") & `k' > 1 {
      capture assert `k' == `n'
      if _rc {
        display as error "need one label for each column"
        error 198
      }
    }

    * Handle one label for multiple columns.
    if !mi("`merge'") {
      capture putexcel `left'`r', unmerge
      putexcel `left'`r' = "`labels'"
      putexcel `left'`r':`right'`r', merge `options'
    }

    * Handle one label for each column.
    else {
      local c : word 1 of `cols'
      mata : xlfmt_put_string(`r', `c', (`labs'))
      if !mi("`options'") putexcel `left'`r':`right'`r', `options'
    }
  }

  * Save changes.
  if mi("`using'") {
    _putexcel_save_reinit
  }
  else {
    putexcel save
    putexcel clear
  }

  }

end


program define _xlcol

  ******************************************************************************
  * -_xlcol-
  *
  * Convert a variable name or index to an Excel column address, or an Excel
  * column address to an index, placing the result in the local macro lmacname.
  *
  * _xlcol lmacname : {varname|exp|letters}
  *
  ******************************************************************************

  * Parse arguments.
  gettoken lmacname 0 : 0, parse(" :")
  gettoken colon    0 : 0, parse(" :")
  if "`colon'" != ":" {
    display as error "syntax must follow: _xlcol lmacname : {varname|exp|letters}"
    exit 198
  }
  syntax anything

  * Convert variable name.
  capture unab varname : `anything', max(1)
  if !_rc {
    mata: idx = st_varindex(st_local("varname"))
    mata: st_local("col", numtobase26(idx))
    c_local `lmacname' `col'
    exit
  }

  * Convert Excel column address.
  capture assert regexm("`anything'", "^[a-zA-Z]+$")
  if !_rc {
    local idx 1
    local col A
    while "`col'" != "`anything'" {
      mata: st_local("col", numtobase26(`++idx'))
    }
    c_local `lmacname' `idx'
    exit
  }

  * Convert column index.
  local idx = `anything'
  capture assert `idx' > 0 & !mi(`idx')
  if !_rc {
    mata: st_local("col", numtobase26(`idx'))
    c_local `lmacname' `col'
    exit
  }

  * Arguments improperly specified otherwise.
  display as error "must provide variable name, positive integer, or letters"
  exit 198

end


program define _putexcel_save_reinit

  local sheet ${S_PUTEXCEL_SHEET_NAME}
  local using ${S_PUTEXCEL_FILE_NAME}
  local fmode ${S_PUTEXCEL_FILE_MODE}

  putexcel save
  putexcel set "`using'", sheet("`sheet'") open `fmode'

end


capture mata: mata drop xlfmt_init() xlfmt_row_height() xlfmt_col_width()
mata:

  pointer (class xl) scalar xlfmt_init() {

    pointer (class xl) scalar b
    b = findexternal("__putexcel_open_fhandle")

    if ((*b).query("filename") == "") {
      (*b).load_book(st_global("S_PUTEXCEL_FILE_NAME"))
    }

    if ((*b).query("sheetname") == "") {
      (*b).set_sheet(st_global("S_PUTEXCEL_SHEET_NAME"))
    }

    if (!(*b).query("mode")) {
      (*b).set_mode("open")
    }

    return(b)
  }

  void xlfmt_row_height(real vector rows, real scalar h) {

    pointer (class xl) scalar b
    b = xlfmt_init()

    for (i = 1; i <= length(rows); i++) {
      (*b).set_row_height(rows[i], rows[i], h)
    }
  }

  void xlfmt_col_width(real vector cols, real scalar w) {

    pointer (class xl) scalar b
    b = xlfmt_init()

    w = min((floor(w * 0.8), 255))

    for (i = 1; i <= length(cols); i++) {
      (*b).set_column_width(cols[i], cols[i], w)
    }
  }

  void xlfmt_put_string(real scalar row, real scalar col, string vector labs) {

    pointer (class xl) scalar b
    b = xlfmt_init()

    (*b).put_string(row, col, labs)
  }

end
