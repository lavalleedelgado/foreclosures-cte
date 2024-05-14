version 18
clear all
set type double
set linesize 120

* Identify inputs.
local PWD "`c(pwd)'"
local ADO "`PWD'/src/ado"
local DTA "`PWD'/in"
local VAR "`DTA'/varlist.xlsx"
local STU "[ HSLS ]/all_student_variables.dta"
local SCH "[ HSLS ]/all_school_variables.dta"
local CCD "`DTA'/ccd-2009.txt"
local PSS "`DTA'/pss-2009.txt"
local FIX "`DTA'/nces-fips-fix.xlsx"
local NSP "`DTA'/nsp.xlsx"

* Identify outputs.
local LOG "`PWD'/src/01-build-dataset.log"
local OUT "[ HSLS ]/analysis.dta'"

* Add project commands to the path.
adopath ++ "`ADO'"

* Start the log.
capture log close
log using "`LOG'", replace

********************************************************************************
*
* Project:  Foreclosures and endogenous coursetaking
* Purpose:  Build analytic file
* Author:   Patrick Lavallee Delgado
* Created:  7 May 2024
*
* Notes:    Update path to HSLS directory on data room workstation.
*
* To do:
*
********************************************************************************


********************************************************************************
* Load student characteristics from HSLS.
********************************************************************************

* Read HSLS student file.
use "`STU'", clear
rename *, lower
isid stu_id

* NCES id.
rename x1ncesid nces

* Sampling weight.
rename w4w1stup1 pw
assert pw >= 0 & !mi(pw)
drop if !pw

* Gender.
recode x1sex (1 = 0) (2 = 1), gen(female)
assert inlist(female, 0, 1)

* Race.
recode x1race (8 = 1) (4 5 = 2) (2 = 4) (1 6 7 = 5), gen(race)
assert inlist(race, 1, 2, 3, 4, 5)

* English language learner.
recode x3ellstatus (-8 = .), gen(ell)
assert inlist(ell, 0, 1, .)

* Special education status.
recode p1specialed (2 3 = 0) (-9 = .), gen(sped)
assert inlist(sped, 0, 1, .)

* Socioeconomic status.
rename x1ses ses
assert !mi(ses)
summarize ses

* Poverty status.
rename x1poverty pov
assert inlist(pov, 0, 1)

* Parents' educational attainment.
recode x1paredu (6 7 = 5), gen(edu)
assert inlist(edu, 1, 2, 3, 4, 5)

* Family arrangement.
recode x1parpattern (7 8 = 2) (2/6 9/11 = 3), gen(fam)
assert inlist(fam, 1, 2, 3)

* Older siblings.
recode p1oldersib (0 = 1) (1 = 2) (2 = 3) (3/9 = 4) (-9 = .), gen(sib)
assert inlist(sib, 1, 2, 3, 4, .)

* Credits.
local old x3tcredtot  x3tcredacad x3tcredcte  x3tcredstem x3tcredmat  x3tcredsci
local new cred        cred_acad   cred_cte    cred_stem   cred_math   cred_sci
recode `old' (-8 = .), gen(`new')
foreach var of varlist `new' {
  assert `var' >= 0
}

* Dual enrollment.
recode s4anydualcred (-9 -4 -1 = .), gen(dualenr)
assert inlist(dualenr, 0, 1, .)

* GPA.
local old x3tgpatot x3tgpaacad  x3tgpacte x3tgpastem  x3tgpamat x3tgpasci
local new gpa       gpa_acad    gpa_cte   gpa_stem    gpa_math  gpa_sci
recode `old' (-9 -8 -1 = .), gen(`new')
foreach var of varlist `new' {
  assert inrange(`var', 0, 4) if !mi(`var')
}

* Dropped out.
recode x4everdrop (-9 = .), gen(hs_dropout)
assert inlist(hs_dropout, 0, 1, .)

* Graduated high school.
recode x4hscompstat (3 = 0) (2 = 1), gen(hs_grad)
assert inlist(hs_grad, 0, 1)

* Graduated high school on time.
gen     hs_ontime = inrange(x3hscompdate, 200900, 201308) & hs_grad
replace hs_ontime = . if x3hscompdate == -9 & !hs_grad
tabulate x3hscompdate hs_ontime, mi
tabulate hs_grad hs_ontime, mi

* Applied to college.
recode x4evrappclg (-9 = .), gen(coll_app)
assert inlist(coll_app, 0, 1, .)

* Enrolled in college.
rename x4evratndclg coll_enr
assert inlist(coll_enr, 0, 1)

* Expectations for educational attainment.
local old s1eduexpect   x1paredexpct
local new base_exp_stu  base_exp_par
recode `old' (3 = 2) (4 5 = 3) (6 7 9 = 4) (8 10 = 5) (-8 -9 11 = .), gen(`new')
foreach var of varlist `new' {
  assert inlist(`var', 1, 2, 3, 4, 5, .)
}

* Math class in 8th grade.
recode s1m8 (2 = 1) (3 = 2) (4 = 3) (5/8 = 4) (9 = 5) (-9 -8 -7 = .), gen(base_math_g8)
assert inlist(base_math_g8, 1, 2, 3, 4, 5, .)

* Ability, self-efficacy, and attitudes.
local old x1txmtscor      x1mtheff        x1scieff      x1schooleng x1schoolbel
local new base_math_score base_math_seff  base_sci_seff base_engage base_belong
recode `old' (-9 -8 -7 = .), gen(`new')
summarize `new'

* Time spent in extracurricular activities.
recode s1hractivity (5 6 = 4) (-9 -8 = .), gen(base_extra)
assert inlist(base_extra, 1, 2, 3, 4, .)

* Set aside.
tempfile stu
save "`stu'"

********************************************************************************
* Load school characteristics from HSLS.
********************************************************************************

* Read HSLS school characteristics file.
use "`SCH'", clear
rename *, lower
isid sch_id

* NCES id.
rename x1ncesid nces
isid nces

* Private school.
recode x1control (1 = 0) (2 3 = 1), gen(private)
assert inlist(private, 0, 1)

* Locale.
rename x1locale locale
assert inlist(locale, 1, 2, 3, 4)

* Region.
rename x1region region
assert inlist(region, 1, 2, 3, 4)

* Demographics.
local old a1ell   a1specialed a1freelunch
local new pct_ell pct_sped    pct_frpl
recode `old' (-9 -8 = .), gen(`new')
foreach var of varlist `new' {
  quietly summarize `var'
  assert r(max) > 1
  replace `var' = `var' / 100
  assert inrange(`var', 0, 1) if !mi(`var')
}

* Climate.
recode x1schoolcli (-9 -8 = .), gen(climate)
summarize climate

* Under-resourced.
recode a1resources (1 2 = 0) (3 4 = 1) (-9 -8 = .), gen(ures)
assert inlist(ures, 0, 1, .)

* Teachers.
local old a1fttchrs a1ftmtchrs  a1ftstchrs
local new tch       tch_math    tch_sci
recode `old' (-9 -8 = .), gen(`new')
foreach var of varlist `new' {
  assert `var' >= 0
}

* Set aside.
tempfile sch
save "`sch'"

********************************************************************************
* Load school characteristics from CCD/PSS.
********************************************************************************

* Read CCD.
import delimited "`CCD'", clear
isid ncessch

* Cast school id to string.
tostring ncessch, format(%12.0f) gen(nces)
isid nces

* Numericize FIPS code.
destring conum09, gen(fips) ignore(MN)
drop if mi(fips)

* State and county.
rename (lstate09 coname09) (state county)

* Race.
foreach x in white black hisp asian {
  gen pct_`x' = `x'09 / toteth09
  assert inrange(pct_`x', 0, 1) if !mi(pct_`x')
}

* Set aside.
keep nces fips state county pct_*
tempfile ccd
save "`ccd'"

* Read PSS.
import delimited "`PSS'", clear
isid ppin
rename ppin nces

* Generate FIPS code.
gen fips = pstfip * 1000 + pcnty
assert !mi(fips)

* State and county.
rename (pstabb pcntnm) (state county)

* Race.
rename (p_white p_hisp p_black p_asian) (pct_white pct_hisp pct_black pct_asian)
foreach var of varlist pct_* {
  quietly summarize `var'
  assert r(max) > 1
  replace `var' = `var' / 100
  assert inrange(`var', 0, 1)
}

* Stack with CCD.
keep nces fips pct_* state county
append using "`ccd'"
isid nces

* Update FIPS codes in Alaska to facilitate merge onto NSP.
tablist fips county if state == "AK", sort(v)
assert !inlist(fips, 02201, 02232, 02280)
replace fips = 02201 if fips == 02198
replace fips = 02232 if inlist(fips, 02105, 02230)
replace fips = 02280 if inlist(fips, 02195, 02275)

* Drop schools in the Virgin Islands.
drop if state == "VI"

* Set aside.
keep nces fips pct_*
save "`ccd'", replace

* Read list of schools in HSLS missing from CCD/PSS.
import excel "`FIX'", firstrow clear
isid nces

* Stack with CCD/PSS.
keep nces fips
append using "`ccd'"
isid nces

* Set aside.
save "`ccd'", replace

********************************************************************************
* Load foreclosures.
********************************************************************************

* Read NSP file.
import excel "`NSP'", sheet("County") firstrow clear
isid countycode

* FIPS code.
destring countycode, gen(fips)
isid fips

* Rename variables to keep.
rename estimated_foreclosure_rate pct_foreclose
rename estimated_hicost_loan_rate pct_hicost
rename ofheo_price_change         pct_decline
rename bls_unemployment_rate      pct_unemp

********************************************************************************
* Save clean file.
********************************************************************************

* Merge datasets.
merge 1:m fips using "`ccd'", assert(1 3) keep(3) nogen
merge 1:1 nces using "`sch'", assert(1 3) keep(3) nogen
merge 1:m nces using "`stu'", assert(1 3) keep(3) nogen

* Attach variable and value labels.
quietly putlabels using "`VAR'", drop order

* Write to disk.
quietly compress
save "`OUT'", replace

* Close the log.
log close
archive "`LOG'", into("_archive")
