/* -----------------------------------------------------------------------------
Shocks
Creation:     September 2023
Last edition: March 2024

This dofile:
    1. Processes IDEAM shocks data by municipality.
    2. Processes ELCA-reported shocks data.
    3. Generates shocks charts.
----------------------------------------------------------------------------- */

clear all
 global ELCA    "C:\Users\userecon10\Desktop\Bases ELCA\"
 global data    "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"
 global tables  "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\05_Tables"
 global figures "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\04_Plots"
cd "${tables}" 
global dpto   "15 23 25 68"



			* ================================== *
**#				    	  ELCA SHOCKS
			* ================================== *

* ------- *
**## 2010
* ------- *
use "${ELCA}\2010\Rural\Rchoques_hogar.dta", clear
keep consecutivo choque_1
rename choque_1 choque
foreach v of varlist choque {
	rename `v' `v'_10
}

duplicates report consecutivo
save "${data}\Rchoques2010.dta", replace


* ------- *
**## 2013
* ------- *
use "${ELCA}\2013\Rural\RChoques.dta", clear
drop if tuvo_choque==2

*Base by treatment
preserve
**Last shock date, by shock
egen ano = rowmax(ano_*)
gen  mes = .
forvalues i=1/12 {
	replace mes_`i'=1 if mes_`i'==13
	replace mes_`i'=6 if mes_`i'==14
	replace mes = mes_`i' if (ano_`i'==ano & mes_`i'!=.)
}
**Date of the most recent shock, by economic importance and by shock
global impeco "alta media baja"
local c = 1
foreach ie of global impeco {
	egen ano_`ie' = rowmax(ano_*) if (imp_econ == `c')
	gen  mes_`ie' = .
	forvalues i=1/12 {
		replace mes_`ie' = mes_`i' if (ano_`i'==ano & imp_econ==`c' & mes_`i'!=.)
	}

	local ++c
}
**Last shock date, by shock househols
bysort llave: egen year=max(ano)
gen mesaux = mes if year==ano
bysort llave: egen month=max(mesaux)
**Last shock date, by economic importance
local c = 1
foreach ie of global impeco {
	bysort llave: egen year_`ie' =max(ano)         if (imp_econ == `c')
	gen mesaux_`ie' = mes if year_`ie'==ano & imp_econ == `c'
	bysort llave: egen month_`ie'=max(mesaux_`ie') if (imp_econ == `c')
	local ++c
}
** Collapse
collapse (mean) year month year_alta month_alta year_media month_media year_baja month_baja, by(llave consecutivo)
rename llave llave_13
gen fecha_shock_13 = ym(year, month)
gen fecha_shock_alta_13  = ym(year_alta,  month_alta)
gen fecha_shock_media_13 = ym(year_media, month_media)
*gen fecha_shock_baja_13  = ym(year_baja,  month_baja)
format fecha_* %tm
keep consecutivo llave_13 fecha_shock*
duplicates report consecutivo llave_13
save "${data}\Fechaschoques2013.dta", replace

gen shock_imp = (fecha_shock_alta_13!=.)
tab shock_imp, m
keep if shock_imp==0
keep consecutivo llave_13
save "${data}\RC_Choques2013.dta", replace
restore

rename (mes* ano*) (mes*_ ano*_)
reshape wide tuvo_choque imp_econ hizo_princ mes* ano*, i(llave) j(choque)
forvalues i=1/17 {
	gen shock`i'    = (tuvo_choque`i'==1)
}
keep consecutivo llave hogar shock*
foreach v of varlist llave hogar shock* {
	rename `v' `v'_13
}
duplicates report consecutivo llave_13
save "${data}\Rchoques2013.dta", replace


* ------- *
**## 2016
* ------- *
use "${ELCA}\2016\Rural\RChoques.dta", clear
drop if tuvo_choque==2
drop llave hogar
rename (llave_n16 hogar_n16) (llave hogar)

*Base by treatment
preserve
**Last shock date, by shock
*** Any importance
gen     year = .
replace year = 2014 if (veces_2014!=. & veces_2014>0)
replace year = 2015 if (veces_2015!=. & veces_2015>0 & year==.) 
replace year = 2016 if (veces_2016!=. & veces_2016>0 & year==.) 
tab year, m
*** High importance
gen     year_alta = .
replace year_alta = 2014 if (veces_2014!=. & veces_2014>0 & imp_econ==1)
replace year_alta = 2015 if (veces_2015!=. & veces_2015>0 & imp_econ==1 & year_alta==.) 
replace year_alta = 2016 if (veces_2016!=. & veces_2016>0 & imp_econ==1 & year_alta==.) 
tab year_alta, m
*** Medium importance
gen     year_media = .
replace year_media = 2014 if (veces_2014!=. & veces_2014>0 & imp_econ==2)
replace year_media = 2015 if (veces_2015!=. & veces_2015>0 & imp_econ==2 & year_media==.) 
replace year_media = 2016 if (veces_2016!=. & veces_2016>0 & imp_econ==2 & year_media==.) 
tab year_media, m
*** Importance 
gen     year_baja = .
replace year_baja = 2014 if (veces_2014!=. & veces_2014>0 & imp_econ==2)
replace year_baja = 2015 if (veces_2015!=. & veces_2015>0 & imp_econ==2 & year_baja==.) 
replace year_baja = 2016 if (veces_2016!=. & veces_2016>0 & imp_econ==2 & year_baja==.) 
tab year_baja, m

** Collapse
collapse (min) year year_alta year_media year_baja, by(llave consecutivo)
rename llave llave_16
gen month = 1
gen fecha_shock_16 = ym(year, month)
gen fecha_shock_alta_16  = ym(year_alta,  month)
gen fecha_shock_media_16 = ym(year_media, month)
*gen fecha_shock_baja_16  = ym(year_baja,  month_baja)
format fecha_* %tm
keep consecutivo llave_16 fecha_shock*
duplicates report consecutivo llave_16
save "${data}\Fechaschoques2016.dta", replace
restore

reshape wide tuvo_choque imp_econ hizo_princ veces_201*, i(llave) j(choque)
forvalues i=1/19 {
	gen shock`i' = (tuvo_choque`i'==1)
	gen shockimp`i' = (tuvo_choque`i'==1 & imp_econ`i'==1)
}
keep consecutivo llave hogar shock*
foreach v of varlist llave hogar shock* {
	rename `v' `v'_16
}
duplicates report consecutivo llave_16
save "${data}\Rchoques2016.dta", replace


* ------------------ *
**## Joining rounds
* ------------------ *
*use "${data}\BaseUPA.dta", clear
 use "${data}\CovariablesSelectionDatabase.dta", clear
keep consecutivo llave_13 llave_16 region
merge m:1 consecutivo using "${data}\Rchoques2010.dta", keep(1 3) nogen
merge 1:1 llave_13    using "${data}\Rchoques2013.dta", keep(1 3) nogen
merge 1:1 llave_16    using "${data}\Rchoques2016.dta", keep(1 3) nogen
**Adjustments
foreach v of varlist shock* {
	replace `v'=0 if `v'==.
}

*All shocks
**Classification of shocks from 2016
egen gen_shock  = rowmax(shock1_16 shock2_16 shock3_16 shock4_16 shock5_16 shock6_16 shock7_16 shock8_16 shock9_16 shock10_16 shock11_16 shock12_16 shock13_16 shock14_16 shock15_16 shock16_16 shock17_16 shock18_16 shock19_16)
egen envi_shock = rowmax(shock13_16 shock14_16 shock16_16 shock17_16 shock18_16)
egen econ_shock = rowmax(shock5_16 shock6_16 shock7_16  shock10_16 shock11_16 shock12_16 )
egen soci_shock = rowmax(shock1_16 shock2_16 shock3_16 shock4_16 shock8_16)
egen inst_shock = rowmax(shock9_16 shock15_16 shock19_16)

**Number of shocks
egen num_shock  = rowtotal(shock1_16 shock2_16 shock3_16 shock4_16 shock5_16 shock6_16 shock7_16 shock8_16 shock9_16 shock10_16 shock11_16 shock12_16 shock13_16 shock14_16 shock15_16 shock16_16 shock17_16 shock18_16 shock19_16)
egen type_shock = rowtotal(envi_shock econ_shock soci_shock inst_shock)
tab num_shock type_shock, m

***Counting by shocks
gen num_shock_2shocks = (num_shock==2)
gen num_shock_2plus   = (num_shock>=3)
tab num_shock, gen(num_shock_)
tab num_shock num_shock_2shocks 
tab num_shock num_shock_2plus
***Counting by shock type
gen type_shock_2types = (type_shock==2)
gen type_shock_2plus  = (type_shock>=3)
tab type_shock, gen(type_shock_)
tab type_shock type_shock_2types 
tab type_shock type_shock_2plus

*Important shocks
**Classification of shocks from 2016
egen gen_shockimp  = rowmax(shockimp1_16 shockimp2_16 shockimp3_16 shockimp4_16 shockimp5_16 shockimp6_16 shockimp7_16 shockimp8_16 shockimp9_16 shockimp10_16 shockimp11_16 shockimp12_16 shockimp13_16 shockimp14_16 shockimp15_16 shockimp16_16 shockimp17_16 shockimp18_16 shockimp19_16)
egen envi_shockimp = rowmax(shockimp13_16 shockimp14_16 shockimp16_16 shockimp17_16 shockimp18_16)
egen econ_shockimp = rowmax(shockimp5_16 shockimp6_16 shockimp7_16  shockimp10_16 shockimp11_16 shockimp12_16 )
egen soci_shockimp = rowmax(shockimp1_16 shockimp2_16 shockimp3_16 shockimp4_16 shockimp8_16)
egen inst_shockimp = rowmax(shockimp9_16 shockimp15_16 shockimp19_16)

**Number of shock
egen num_shockimp  = rowtotal(shockimp1_16 shockimp2_16 shockimp3_16 shockimp4_16 shockimp5_16 shockimp6_16 shockimp7_16 shockimp8_16 shockimp9_16 shockimp10_16 shockimp11_16 shockimp12_16 shockimp13_16 shockimp14_16 shockimp15_16 shockimp16_16 shockimp17_16 shockimp18_16 shockimp19_16)
egen type_shockimp = rowtotal(envi_shockimp econ_shockimp soci_shockimp inst_shockimp)
tab num_shockimp type_shockimp, m

***Counting by shocks
gen num_shockimp_2shockimps = (num_shockimp==2)
gen num_shockimp_2plus   = (num_shockimp>=3)
tab num_shockimp, gen(num_shockimp_)
tab num_shockimp num_shockimp_2shockimps 
tab num_shockimp num_shockimp_2plus
***Counting by shock type
gen type_shockimp_2types = (type_shockimp==2)
gen type_shockimp_2plus  = (type_shockimp>=3)
tab type_shockimp, gen(type_shockimp_)
tab type_shockimp type_shockimp_2types 
tab type_shockimp type_shockimp_2plus

save "${data}\Base_choquesELCA.dta", replace
save "${data}\Base_choques.dta", replace




			* ======================================== *
**#			    SETTING UP THE DATASET FOR THE CHART
			* ======================================== *


use "${data}\Base_choques.dta", clear

*ELCA Shocks: (# UPAs involved in shock i / #UPAs in the region)
gen unos=1
collapse (sum) *_shock unos, by(region)
foreach v of varlist gen_shock envi_shock econ_shock soci_shock inst_shock {
	gen p_`v' = `v'/unos
}

*Base for graph in R
rename (gen_shock envi_shock econ_shock soci_shock inst_shock) (shock1 shock2 shock3 shock4 shock5)
rename (p_gen_shock p_envi_shock p_econ_shock p_soci_shock p_inst_shock) (shockd1 shockd2 shockd3 shockd4 shockd5)
reshape long shock shockd, i(region) j(variable)
label drop id

gen     Shock = "General Shock"       if variable==1
replace Shock = "Environmental Shock" if variable==2
replace Shock = "Economic Shock"      if variable==3
replace Shock = "Social Shock"        if variable==4
replace Shock = "Institutional Shock" if variable==5
rename region region2
gen     Region = "Atlantica-Media" if region2==6
replace Region = "Cundi-Boyacense" if region2==7
replace Region = "Eje Cafetero"    if region2==8
replace Region = "Centro-Oriente"  if region2==9

gen perc = shock/unos

save "${data}\Base_choques_ELCA_R.dta", replace

*Stats
collapse (sum) shock unos, by(variable Shock)
gen perc=(shock/unos)*100
