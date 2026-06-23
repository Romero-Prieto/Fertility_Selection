tempfile   lISt
tempfile   temp
tempfile   temp2
tempfile   tempBR
tempfile   tempIR


local      pATh         = "/Users/lshjr3/Documents/FertilitySelection"     /*Adjust path*/
local      DHS          = "AM72 AO81 BD81 BF81 BJ71 BU71 CD81 CI81 CM71 ET71 GA71 GH8C GM81 GN71 HT71 IA7E ID71 JO81 KE8C KH82 LB7A LS81 MD81 ML8A MR71 MV71 MW81 MZ81 NG8B NP82 PG71 PH82 PK71 RW81 SL7A SN8S TJ81 TL71 TZ82 UG7B ZA71 ZM81 ZW72"

clear
generate   survey       = ""
save      `lISt', replace                                                       /*Makes a list of DHS samples.*/
local      vARsIR       = "caseid v000 v001 v002 v003 v005 v006 v007 v008 v016 v011 v012 v021 v023 v024 v025 v018 v101 v102 v106 v501 v169* v190 v249 v313 b1_* b2_* b17_* v623 v312 v364 v613 v602" /*Variables from Individual Recode, i.e., women 15-49.*/
use       `vARsIR' in 1/1 using "`pATh'/Data/GHIR8CFL.DTA", clear                    /*Follows the same approach with the IR file.*/
drop       in 1
save      `tempIR', replace

local      vARsPR       = "hhid hvidx hv001 hv002 hv003 hv005 hv023 hv101 hv102 hv103 hv104 hv105 hv106 hv107 hv112 hv114 hv201 hv206 hv215" /*Variables from Household Member Recode, i.e., all family members plus household's characteristics.*/


foreach svy of local DHS {
	/* Women 15-49 */
	cls
	dis "`svy'"
	local      name           = substr("`svy'",1,2) + "IR" + substr("`svy'",3,4) + "FL.DTA"
	use        caseid v* b1_* b2_* b17_* using "`pATh'/Data/`name'", clear
	merge 1:1  caseid v002 v012 using `tempIR', nogenerate noreport nolabel keep(master) /*Brings the name of those variables not in the sample.*/
	keep      `vARsIR'                                                          /*Keeps just the variables of the list.*/
	drop if    v169a         == .                                               /*e.g., IA7E questions on mobile ownership are only for a sample.*/
	drop if    v005          == 0                                               /*e.g., MW81 (refugee's weights have a different variable) PK71*/
		
	generate   W              = v005/1000000                                    /*weights*/
	generate   DOB            = mdy(v011 - floor((v011 - 1)/12)*12,1,floor((v011 - 1)/12) + 1900) /*Day of birth is not available, and the first of the month is assumed.*/	
	sum        W 
	replace    W              = W/r(mean)
	generate   respondent     = v003
	generate   cluster        = v001
	generate   household      = v002
	generate   strata         = v023 
	generate   Region         = v024
	generate   UR             = v025
	generate   Education      = v106
	recode     Education   (0 = 1) (8 = 1) (. = 1)                              /*to make a group of unknown, primary or less than primary education.*/
	generate   Marital        = v501
	recode     Marital     (2 = 1) (3 = 0) (4 = 0) (5 = 0)                      /*to make two groups of individuals with or without a current partner.*/
	generate   age            = v012
	generate   ageG           = 5*floor(age/5)
	generate   mobile         = v169a
	generate   interview      = mdy(v006,v016,v007)
	generate   wealth_index   =	v190
	generate   menarche_age   = v249
	generate   any_usage      = min(v312,1)       + 1
	generate   modern_usage   = max(v313,2) - 2   + 1
	generate   no_use_no_inte = max(v364,3) - 3   + 1
	generate   another_child  = 2 - min(v602,2)   + 1
	generate   fecund         = 1 - min(v623,1)   + 1    
	generate   ideal_number   = v613
	
	/* Date of Birth */
	forvalues i = 1(1)20 {
		local      s         = substr("0" + "`i'",-2,.)
		generate   B_`i'     = mdy(b1_`s',b17_`s',b2_`s')
		}	
	
	local      vAr            = "caseid interview respondent UR Region DOB W cluster household strata Region UR Education Marital age ageG mobile wealth_index menarche_age any_usage modern_usage no_use_no_inte another_child fecund ideal_number B_*"
	save      `temp', replace
	
	/* Consecutive region numeration */
	contract   Region        
	generate   R              = _n
	keep       Region R
	save      `temp2', replace
	use       `temp', clear
	merge m:1  Region using `temp2', nogenerate noreport
	replace    Region         = R
	format     %tdDD/NN/CCYY interview DOB B_*
	
	keep      `vAr'
	save      `temp', replace

	/* All household members & household's characteristics */
	local      name           = substr("`svy'",1,2) + "PR" + substr("`svy'",3,4) + "FL.DTA"
	use       `vARsPR' using "`pATh'/Data/`name'", clear                             
	generate   cluster        = hv001
	generate   household      = hv002
	generate   respondent     = hvidx
	
	generate   Electricity    = hv206
	recode     Electricity (. = 0)
	generate   Roofing        = 0
	recode     Roofing     (0 = 1)    if hv215 == 31 | hv215 == 33 | hv215 == 34 | hv215 == 35 | hv215 == 36 /*good material excluding wood*/
	generate   Water          = 0
	recode     Water       (0 = 1)    if hv201 == 11 | hv201 == 12 | hv201 == 13 | hv201 == 14 | hv201 == 21 | hv201 == 31 | hv201 == 41 | hv201 == 51 | hv201 == 62 | hv201 == 71 
	generate   usual_resident = 1     if hv102 == 1
	bysort     hhid: egen   HH_size = sum(usual_resident)
	generate   householdS     = 1     if HH_si  < 5
	replace    householdS     = 2     if HH_si  < 9  & householdS == .
	recode     householdS  (. = 3)
	keep       cluster household respondent Electricity Roofing Water householdS
	save      `temp2', replace
	
	use       `temp.dta', clear
	merge m:1  cluster household respondent using `temp2', nogenerate keep(master match)
	generate   survey         = "`svy'"
	egen       GO             = cut(age), at(15,20,30,40,50) icodes
	replace    GO             = GO + 1
	replace    Electricity    = Electricity + 1
	save      `temp', replace
	
	/* Raking */	
	generate   Total          = 1
	tabulate   Total [aw = W], matcell(Total)
	local      rep_lISt       = "GO UR Region householdS Education Electricity fecund another_child no_use_no_inte"
	local      alpha          = 0.10
	local      rep_variable   = ""
	local      rep_totals     = "_cons = 10000"
	foreach var of local rep_lISt {
		local            rep_variable   = "`rep_variable'" + " i.`var'"
		tabulate        `var' [aw = W], matcell(`var')
		forvalues i = 1(1)`= rowsof(`var')' {
			local            number         = `var'[`i',1]/Total[1,1]*10000
			local            rep_totals     = "`rep_totals'" + " `i'.`var' = `number'"
			}
		}
	
	keep if    mobile        == 1
	svycal     rake `rep_variable' [pw = W], force generate(temp) totals(`rep_totals')
	xtile      Q_temp         = temp, nq(200)
	replace    Q_temp         = min(max(Q_temp/2,100*`alpha'/2),100*(1 - `alpha'/2))
	bysort     Q_temp: egen max = max(temp)
	bysort     Q_temp: egen min = min(temp)
	egen       LB             = min(max)
	egen       UB             = max(min)
	sum        temp
	generate   WR             = temp/r(mean)
	drop       Q_temp LB UB max min temp
	keep       cluster household respondent WR
	merge m:1  cluster household respondent using `temp', noreport nogenerate
	order      survey caseid cluster household respondent W WR DOB	
	sort       survey caseid cluster household respondent
	export     delimited using "`pATh'/Data/`svy'.csv", replace

	collapse  (mean) mobile [aw = W], by(survey)
	append     using `lISt'
	save      `lISt', replace	
	}

use       `lISt', clear
sort       survey
generate   DHS          = substr(survey,1,2)
merge m:1  DHS using "`pATh'/DHS_code.dta", nogenerate noreport keep(master match)
drop       DHS
order      country survey
export     delimited using "`pATh'/lISt.csv", replace
