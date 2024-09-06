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
local CRS "D:/hsls/data/all_stu_course.dta"
local XWK "`DTA'/SCED_Version_2_030514.xlsx" "CTE and FCS Attributes" "A2"
local CCD "`DTA'/ccd-2009.txt"
local PSS "`DTA'/pss-%d.txt" "2005 2007 2009"
local NSP "`DTA'/nsp.xlsx"
local FIX "`DTA'/fips-split.xlsx"
local BEA "`DTA'/bea.csv"

* Identify outputs.
local LOG "`PWD'/src/01-build-dataset.log"
local OUT "[ HSLS ]/analysis.dta"

* Add project commands to the path.
adopath ++ "`ADO'"

* Start the log.
capture log close
log using "`LOG'", replace

********************************************************************************
*
* Project:  Mediated effects of foreclosures on coursetaking
* Purpose:  Build clean file
* Author:   Patrick Lavallee Delgado
* Created:  7 May 2024
*
* Notes:    Update path to HSLS directory on data room workstation.
*           CTE concentrator defined at three-credit threshold.
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

* Student, school, strata identifiers.
rename stu_id   stuid
rename x1ncesid nces
rename strat_id strata

* Sampling weight.
* Note accounts for student nonresponse in BY/F2, parent nonresponse in BY.
gen pw = w4w1stup1
assert pw >= 0 & !mi(pw)
drop if !pw

* Female.
recode x1sex (1 = 0) (2 = 1), gen(female)
assert inlist(female, 0, 1)
tablist female x1sex, sort(v) ab(32)

* Race.
recode x1race (8 = 1) (4 5 = 2) (2 = 4) (1 6 7 = 5), gen(race)
assert inlist(race, 1, 2, 3, 4, 5)
tablist race x1race, sort(v) sepby(race) ab(32)

* English language learner.
recode x3ellstatus (-8 = .), gen(ell)
assert inlist(ell, 0, 1, .)
tablist ell x3ellstatus, sort(v) ab(32)

* Special education status.
* Note combines school and parent responses.
recode x1iepflag p1specialed (2 3 = 0) (-9 = .), gen(sped1 sped2)
egen sped = rowmax(sped?)
assert inlist(sped, 0, 1, .)
tablist sped x1iepflag p1specialed, sort(v) sepby(sped) ab(32)

* Socioeconomic status.
rename x1ses ses
assert !mi(ses)
summarize ses

* Poverty status.
rename x1poverty pov
assert inlist(pov, 0, 1)

* Family income.
assert !mi(p1income)
recode p1income (-9 = .), gen(inc)
assert inc >= 0
summarize inc, detail
local p99 = r(p99)

* Imputed family income bounds.
label list `: value label x1famincome'
assert inrange(x1famincome, 1, 13)
decode x1famincome, gen(inc_bounds)
replace inc_bounds = regexreplace(inc_bounds, "less than or equal to", "<=")

* Non-missing family income lower bound.
gen     inc_lb = real(regexreplace(inc_bounds, "^.*>\s?\\$([0-9]+),(000).*$", "\1\2")) + 1
replace inc_lb = inc if !mi(inc)
assert mi(inc_lb) == (x1famincome == 1) if mi(inc)
replace inc_lb = 0 if mi(inc_lb)

* Non-missing family income upper bound.
gen     inc_ub = real(regexreplace(inc_bounds, "^.*<=\s?\\$([0-9]+),(000)$", "\1\2"))
replace inc_ub = inc if !mi(inc)
assert mi(inc_ub) == (x1famincome == 13) if mi(inc)
replace inc_ub = `p99' if mi(inc_ub)

* Check correspondence of family income variables.
assert inc_lb <= inc_ub & !mi(inc_lb, inc_ub)
assert inc_lb == inc_ub if !mi(inc)
table x1famincome, statistic(freq) statistic(min inc_lb) statistic(max inc_ub)

* Family owns home.
recode p1ownhome (2/3 = 0) (-9 = .), gen(own)
assert inlist(own, 0, 1, .)
tablist own p1ownhome, sort(v) sepby(own) ab(32)

* College savings.
* Recode legitimate skip to zero. These are cases where:
* 1. parents do not expect student to attend college, or
* 2. parents do not plan or have not thought about paying for college, or
* 3. parents have not begun to prepare for paying for college.
foreach var of varlist p1savedpay p1eduexpect p1helppay p1preppay {
  display _n(2) "`var': `: variable label `var''"
  label list `: value label `var''
}
assert (p1savedpay == -7) == (inlist(p1eduexpect, 1, 2) | inlist(p1helppay, 2, 3) | p1preppay == 4)
recode p1savedpay (-7 1 = 0) (2/9 = 1) (-9 = .), gen(svg)
assert inlist(svg, 0, 1, .)
tablist svg p1savedpay, sort(v) sepby(svg) ab(32)

* Parents' educational attainment.
recode x1paredu (6 7 = 5), gen(edu)
assert inlist(edu, 1, 2, 3, 4, 5)
tablist edu x1paredu, sort(v) sepby(edu) ab(32)

* Family arrangement.
recode x1parpattern (7 8 = 2) (2/6 9/11 = 3), gen(fam)
assert inlist(fam, 1, 2, 3)
tablist fam x1parpattern, sort(v) sepby(fam) ab(32)

* Older siblings.
recode p1oldersib (0 = 1) (1 = 2) (2 = 3) (3/9 = 4) (-9 = .), gen(sib)
assert inlist(sib, 1, 2, 3, 4, .)
tablist sib p1oldersib, sort(v) sepby(sib) ab(32)

* CTE center.
recode x3attendcte (-8 = .), gen(ctecenr)
assert inlist(ctecenr, 0, 1, .)
tablist ctecenr x3attendcte, sort(v) ab(32)

* Dual enrollment.
recode s4anydualcred (-9 -4 -1 = .), gen(dualenr)
assert inlist(dualenr, 0, 1, .)
tablist dualenr s4anydualcred, sort(v) sepby(dualenr) ab(32)

* Dropped out.
recode x4everdrop (-9 = .), gen(hs_dropout)
assert inlist(hs_dropout, 0, 1, .)
tablist hs_dropout x4everdrop, sort(v) ab(32)

* Graduated high school.
recode x4hscompstat (3 = 0) (2 = 1), gen(hs_grad)
assert inlist(hs_grad, 0, 1)
tablist hs_grad x4hscompstat, sort(v) ab(32)

* Graduated high school on time.
gen hs_grad_ontime = inrange(x4hscompdate, 200900, 201308)
assert inlist(hs_grad_ontime, 0, 1)
tabulate x4hscompdate hs_grad_ontime if hs_grad
tabulate hs_grad hs_grad_ontime

* Applied to college.
recode x4evrappclg (-9 = .), gen(coll_app)
assert inlist(coll_app, 0, 1, .)
tablist coll_app x4evrappclg, sort(v) ab(32)

* Enrolled in college.
rename x4evratndclg coll_enr
assert inlist(coll_enr, 0, 1)
assert coll_app == 1 if coll_enr

* First enrollment college level.
recode x4ps1level (-7 1 3 = 0) (2 = 1) (-9 = .), gen(coll_enr_2yr)
recode x4ps1level (-7 2 3 = 0) (-9 = .), gen(coll_enr_4yr)
foreach var of varlist coll_enr_?yr {
  assert inlist(`var', 0, 1, .)
  assert !`var' if !coll_enr
}
tablist coll_enr x4ps1level coll_enr_?yr, sort(v) ab(32)

* Expectations for educational attainment.
local old s1eduexpect   x1paredexpct
local new base_exp_stu  base_exp_par
recode `old' (3 = 2) (4 5 = 3) (6 7 9 = 4) (8 10 = 5) (-8 -9 11 = .), gen(`new')
forvalues i = 1/`: word count `new'' {
  local a : word `i' of `new'
  local b : word `i' of `old'
  assert inlist(`a', 1, 2, 3, 4, 5, .)
  tablist `a' `b', sort(v) sepby(`a') ab(32)
}

* Perceptions about studying and college.
local old s1payoff    s1getintoclg  s1afford          s1working
local new base_study  base_coll_enr base_coll_afford  base_coll_prefer
recode `old' (1/2 = 0) (3/4 = 1) (-8 -9 = .), gen(`new')
forvalues i = 1/`: word count `new'' {
  local a : word `i' of `new'
  local b : word `i' of `old'
  assert inlist(`a', 0, 1, .)
  tablist `a' `b', sort(v) sepby(`a') ab(32)
}

* Math class in 8th grade.
recode s1m8 (2 = 1) (3 = 2) (4 = 3) (5/8 = 4) (9 = 5) (-9 -8 -7 = .), gen(base_math_g8)
assert inlist(base_math_g8, 1, 2, 3, 4, 5, .)
tablist base_math_g8 s1m8, sort(v) sepby(base_math_g8) ab(32)

* Math and science ability and self-efficacy, and attitudes.
local old x1txmtscor      x1mtheff        x1scieff
local new base_math_score base_math_seff  base_sci_seff
recode `old' (-9 -8 -7 = .), gen(`new')
summarize `new'

* Sense of engagement and belonging.
local old x1schooleng x1schoolbel
local new base_engage base_belong
recode `old' (-9 -8 -7 = .), gen(`new')
summarize `new'

* Time spent in extracurricular activities.
recode s1hractivity (5 6 = 4) (-9 -8 = .), gen(base_extra)
assert inlist(base_extra, 1, 2, 3, 4, .)
tablist base_extra s1hractivity, sort(v) sepby(base_extra) ab(32)

* Set aside.
tempfile stu
save "`stu'"

********************************************************************************
* Load coursetaking within CTE clusters.
********************************************************************************

* Read SCED-cluster crosswalk.
unpack book sheet range : "`XWK'"
import excel "`book'", sheet("`sheet'") cellrange("`range'") firstrow clear
rename * (course sced ///
  cte_agnr  cte_trad1 cte_comm  cte_mgmt1 cte_edut  cte_mgmt2 cte_safe1 ///
  cte_hlth  cte_hosp  cte_hsvc  cte_it    cte_safe2 cte_trad2 cte_mgmt3 ///
  cte_stem  cte_trad3 cte_famc)
describe

* Course content identifier.
assert mi(sced) == mi(course)
drop if mi(sced)
destring sced, replace
isid sced

* Numericize indicators.
tempvar x
foreach var of varlist cte_* {
  assert `var' == "x" if !mi(`var')
  gen `x' = `var' == "x"
  drop `var'
  rename `x' `var'
}

* Ensure all courses on the file map to a cluster.
* Note manually map one course to Hospitality and Tourism.
egen `x' = rowtotal(cte_*)
summarize `x'
assert !`x' == (sced == 16995)
list course sced if !`x', string(80)
replace cte_hosp = 1 if sced == 16995
drop `x'

* Merge select clusters.
foreach stub in trad mgmt safe {
  egen cte_`stub' = rowmax(cte_`stub'?)
  drop cte_`stub'?
}

* Create "other" cluster.
rename (cte_edut cte_hosp cte_hsvc cte_safe cte_stem) =_other
rename cte_*_other cte_other_*
egen cte_other = rowmax(cte_other_*)

* Set aside.
tempfile xwk
save "`xwk'"

* Read HSLS student transcript file.
use "`CRS'", clear
rename *, lower

* Student identifier.
rename stu_id stuid

* Course content identifier.
destring t3ssced, gen(sced)
assert !mi(sced)
gen sced2 = floor(sced / 1000)
assert inrange(sced2, 1, 99)

* Credits.
rename t3scred cred
assert cred >= 0 & !mi(cred)

* Grade points.
* Note bunching at full letter grade, so recode to full grade point.
label list `: value label t3sgrd'
recode t3sgrd (1/3 = 4) (4/6 14 = 3) (7/9 = 2) (10/12 15 = 1) (13 = 0) (else = .), gen(gpa)
assert inlist(gpa, 4, 3, 2, 1, 0, .)
tablist gpa t3sgrd, sort(v) sepby(gpa) ab(32)

* Mark academic courses.
gen acad      = inrange(sced2, 1, 6)
gen acad_math = sced2 == 2
gen acad_sci  = sced2 == 3
tablist acad*, sort(v) ab(32)

* Mark CTE courses.
gen cte = inrange(sced2, 10, 21) | inrange(sced, 22151, 22153) | inrange(sced, 22201, 22249)
tablist acad cte, sort(v) ab(32)

* Merge onto CTE cluster assignments.
merge m:1 sced using "`xwk'", keep(1 3) nogen
recode cte_* (miss = 0)

* Ensure no overlap between academic and CTE courses.
egen `x' = rowmax(cte*)
assert acad != `x' if acad | `x'
drop `x'

* Mark STEM courses.
gen stem = inlist(sced2, 2, 3, 10, 21)
tablist acad cte stem, sort(v) ab(32)

* Transform to student-course-cluster level.
gen all = 1
keep t3scrse_seq stuid gpa cred all acad* cte* stem
rename (all acad* cte* stem) k_=
greshape long k, i(t3scrse_seq) j(cluster) string

* Keep courses assigned to clusters.
assert inlist(k, 0, 1)
keep if k

* Summarize coursetaking within clusters.
replace gpa = gpa * cred
collapse (sum) gpa cred, by(stuid cluster)
replace gpa = gpa / cred
assert inrange(gpa, 0, 4) if !mi(gpa)
gen conc = cred >= 3

* Transform to student level.
greshape wide gpa cred conc, i(stuid) j(cluster) string
recode gpa* cred* conc* (miss = 0)
rename *_all *

* Note fewer than 1% of students are concentrators in "other" clusters.
unab clusters : conc_cte_*
unab minor : conc_cte_other_*
local major : list clusters - minor
foreach var of local minor {
  quietly summarize `var'
  assert r(mean) < 0.01
}
foreach var of local major {
  quietly summarize `var'
  assert r(mean) >= 0.01
}

* Set aside.
tempfile clu
save "`clu'"

********************************************************************************
* Load school characteristics from HSLS.
********************************************************************************

* Read HSLS school characteristics file.
use "`SCH'", clear
rename *, lower
isid sch_id

* School identifier.
rename x1ncesid nces
isid nces

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
tablist ures a1resources, sort(v) sepby(ures) ab(32)

* Set aside.
tempfile sch
save "`sch'"

********************************************************************************
* Load school demographics from CCD/PSS.
********************************************************************************

* Read CCD.
import delimited "`CCD'", clear
isid ncessch

* School identifier.
tostring ncessch, format(%012.0f) gen(nces)
assert strlen(nces) == 12
isid nces

* FIPS code.
destring conum09, gen(fips) ignore(MN)
drop if mi(fips)

* State and county.
rename (lstate09 coname09) (state county)

* Replace one school id to enable merge onto HSLS.
local old 231477700788
local new 231476000788
assert state == "ME" if nces == "`old'"
assert nces != "`new'"
replace nces = "`new'" if nces == "`old'"

* Total students.
recode member09 (-2 = 0) (-1 -9 = .), gen(n_stu)
assert n_stu >= 0

* Demographics.
local old white09   black09   hisp09    asian09   totfrl09
local new pct_white pct_black pct_hisp  pct_asian pct_frpl
recode `old' (-2 = 0) (-1 -9 = .), gen(`new')
foreach var of varlist `new' {
  assert `var' >= 0
  replace `var' = `var' / n_stu
  assert inrange(`var', 0, 1) if !mi(`var')
}

* Set aside.
keep nces fips state county pct_*
tempfile ccd
save "`ccd'"

* Read PSS.
tempfile pss
unpack pat yearlist : "`PSS'"
foreach year of local yearlist {

  * Read data for this year.
  local using = regexreplace("`pat'", "%d", "`year'")
  import delimited "`using'", clear
  gen year = `year'

  * School identifier.
  isid ppin
  rename ppin nces

  * FIPS code.
  gen fips = pstfip * 1000 + pcnty
  assert !mi(fips)

  * State and county.
  rename (pstabb pcntnm) (state county)

  * Race.
  foreach race in white hisp black asian {
    rename p_`race' pct_`race'
    local var pct_`race'
    quietly summarize `var'
    assert r(max) > 1
    replace `var' = `var' / 100
    assert inrange(`var', 0, 1)
  }

  * Stack required variables with all other years.
  keep nces year fips state county pct_*
  capture confirm new file "`pss'"
  if _rc append using "`pss'"
  save "`pss'", replace
}

* Deduplicate to most recent year.
isid nces year
bysort nces (year): keep if _n == _N
tabulate year
isid nces
drop year

* Stack with CCD.
append using "`ccd'", gen(ccd)
isid nces

* Private school.
gen private = !ccd

* Update FIPS codes in Alaska to facilitate merge onto NSP/BEA.
tablist fips county if state == "AK", sort(v)
replace fips = 02201 if fips == 02198
replace fips = 02232 if inlist(fips, 02105, 02230)
replace fips = 02280 if inlist(fips, 02195, 02275)

* Drop schools in the Virgin Islands.
drop if state == "VI"

* Set aside.
keep nces fips private pct_*
save "`ccd'", replace

********************************************************************************
* Load county housing indicators from NSP.
********************************************************************************

* Read NSP file.
import excel "`NSP'", sheet("County") firstrow clear
isid countycode

* FIPS code.
destring countycode, gen(fips)
isid fips

* State and state label.
assert !mi(state, sta)
destring state, replace
elabel define state_vlab = levels(state sta)
label values state state_vlab

* County label.
assert mi(countyname) == ((fips == 51560) | (state == 72))
replace countyname = "Clifton Forge city" if fips == 51560
replace countyname = countyname + ", " + sta
elabel define fips_vlab = levels(fips countyname) if state != 72
label values fips fips_vlab

* Housing indicators.
rename estimated_foreclosure_rate pct_foreclose
rename estimated_hicost_loan_rate pct_hicost
rename ofheo_price_change         pct_decline
rename bls_unemployment_rate      pct_unemp

* Transform to county-measure level.
* Note temporarily change sign of decline and clean negative values.
keep fips pct_*
replace pct_decline = -pct_decline
reshape long pct, i(fips) j(var) string
replace pct = 0 if pct < 0

* Calculate percentage rank.
bysort var: egen rank = rank(pct), track
bysort var: egen rmax = max(rank)
replace rank = rank / rmax
assert inrange(rank, 0, 1)

* Transform to county level.
* Note restore sign of decline.
drop rmax
reshape wide pct rank, i(fips) j(var) string
replace pct_decline = -pct_decline

* Set aside.
tempfile nsp
save "`nsp'"

********************************************************************************
* Load county economic indicators from BEA.
********************************************************************************

* Read list of BEA county equivalents to split.
import excel "`FIX'", firstrow clear
rename geofips fips
isid fips

* Transform to county level.
split fips_split, gen(fips_fix) parse("|")
reshape long fips_fix, i(fips) j(j)
destring fips_fix, replace
drop if mi(fips_fix)
isid fips_fix

* Set aside.
tempfile fix
save "`fix'"

* Read BEA file.
import delimited "`BEA'", clear
rename (geofips timeperiod) (fips year)
describe
isid fips year

* Ensure expected years on file.
local yearlist 1999 2004 2007 2008 2009 2010 2011
quietly levelsof year, local(tmplist)
assert `: list yearlist == tmplist'

* Duplicate county equivalents.
joinby fips using "`fix'", unmatched(master)
replace fips = fips_fix if _merge == 3
drop _merge
isid fips year

* Transform to county-year-measure level.
keep fips year cap_*
reshape long cap, i(fips year) j(var) string

* Calculate per capita rank in each year.
bysort var year: egen rank = rank(cap), track
bysort var year: egen rmax = max(rank)
replace rank = rank / rmax
assert inrange(rank, 0, 1)

* Transform to county level.
drop cap
reshape wide rank, i(fips year) j(var) string
unab varlist : rank_*
reshape wide `varlist', i(fips) j(year)

* Set reference year to 2009.
local base 2009
rename *`base' *
confirm variable `varlist', exact

* Calculate per capita rank change.
foreach var of local varlist {
  foreach year of local yearlist {
    local d : display %02.0f `base' - `year'
    if `d' <= 0 continue
    gen `var'_d`d' = (`var'`year' - `var') / `var'`year'
  }
}

********************************************************************************
* Save clean file.
********************************************************************************

* Merge datasets.
* Note merge conflicts between BEA (master) and NSP fall away.
* Fill missing values in CCD with nonmissing values from HSLS.
merge 1:1 fips  using "`nsp'", gen(_m1)
merge 1:m fips  using "`ccd'", assert(1 3) keep(3) nogen
merge 1:1 nces  using "`sch'", update assert(1 3 4 5) keep(3 4 5) nogen nolabel
merge 1:m nces  using "`stu'", assert(1 3) keep(3) nogen
merge 1:1 stuid using "`clu'", keep(1 3) nogen
assert _m1 == 3

* Attach variable and value labels.
* Note drop any variable not on the control file.
quietly putlabels using "`VAR'", drop order

* Write to disk.
quietly compress
save "`OUT'", replace

* Close the log.
log close
archive "`LOG'", into("_archive")
