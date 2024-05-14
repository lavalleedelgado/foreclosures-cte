*! version 1.0, Patrick Lavallee Delgado, August 2023

capture program drop archive
program define archive

  ******************************************************************************
  *
  * archive
  *
  * Copy file to an archive and append a timestamp to the filename.
  *
  * archive filename, into(relpath) format(string)
  *
  * filename: file to copy
  * into: relative path from file to archive
  * format: timestamp format, default is %tCCCYYNNDD
  *
  ******************************************************************************

  * Parse arguments.
  syntax anything(name=path), into(string) [format(string)]
  local path = `path'
  confirm file "`path'"

  * Get timestamp.
  if "`format'" == "" local format %tCCCYYNNDD
  local dt : display `format' now()

  * Construct path to archive and filename of archived item.
  local re "^(.*)[\\/](.*)\.(.*)$"
  local d = regexreplace("`path'", "`re'", "\1/`into'")
  local f = regexreplace("`path'", "`re'", "\2-`dt'.\3")

  * Archive the file.
  capture mkdir "`d'"
  copy "`path'" "`d'/`f'", replace

end
