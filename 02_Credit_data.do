/* -----------------------------------------------------------------------------
Addressing Credit data
Author:		  Raquel
Creation:     November 2023
Last edition: December 2024

This dofile:
1. Processes the dates bases of the surveys to compare them with the dates of credit acquisition. This will be used for the treatment definition.
2. Organize the credits by keeping the relevant variables per household for each round.
2. Combine the data for the three rounds.
3. Create the treatment variables.
----------------------------------------------------------------------------- */

clear all
global ELCA "C:\Users\userecon10\Desktop\Bases ELCA\"
global data    "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"

global treat      "treat_resil13" /*Change main treatment: treat13 treat_resil13 treat_formal13 treat_agrop13*/ 
global agg         "agri pecu agrop"
global ronda       "10 13 16"
global cut_treat10 "72" /*60 for 5 years, 72 for 6 years, 120 for 10 years*/
global cut_treat13 "24"
global cut_year    "12"


			* ================================== *
**#				       1. SURVEY DATES
			* ================================== *

use    "${data}\BaseUPA_Jefes.dta", clear
keep   consecutivo llave_13 llave_16 hogar_13
rename (llave_13 llave_16) (llave llave_n16)

*Joining dates for 2010
merge  m:1 consecutivo using "${ELCA}\2010\FechasEncuesta_2010.dta", keep (3) nogen
gen    encuesta_2010 = ym(ano_encu, mes_encu)
format %tm encuesta_2010
drop   zona ano_encu mes_encu dia_encu

*Joining dates for 2013
merge  1:1 llave using "${ELCA}\2013\FechasEncuesta_2013.dta", keep(3) nogen
split  fecha_inicio, parse(/) destring
gen    encuesta_2013 = ym(fecha_inicio3, fecha_inicio2)
format %tm encuesta_2013
drop   zona fecha_inicio hora_inicio fecha_fin hora_fin fecha_inicio1 fecha_inicio2 fecha_inicio3

*Joining dates for 2016
merge   1:1 llave_n16 using "${ELCA}\2016\FechasEncuesta_2016.dta", keep(1 3) nogen
split   fecha_inicio, parse(/) destring
gen     encuesta_2016 = ym(fecha_inicio3, fecha_inicio2)
format  %tm encuesta_2016
drop    zona fecha_inicio hora_inicio fecha_fin hora_fin fecha_inicio1 fecha_inicio2 fecha_inicio3
rename  (llave llave_n16) (llave_13 llave_16)
sum     encuesta_2016
replace encuesta_2016 = round(r(mean)) if encuesta_2016==.

duplicates report consecutivo
duplicates report consecutivo llave_13
duplicates report consecutivo llave_16
save "${data}\BaseEncuestas.dta", replace




			* ========================= *
**#				    2. CREDIT DATA
			* ========================= *

* -------------- *
**## Data: 2010
* -------------- *
use    "${data}\BaseEncuestas.dta", clear
merge  m:1 consecutivo using "${ELCA}\2010\Rural\Rhogar.dta", keep(3) nogen
keep   consecutivo hogar_13 destino1_* fechai_mes_* fechai_ano_* meses_plazo_* con_quien_* vr_inicial_* vr_saldo_* cuota_*
rename (destino1_*) (destino_*)
save   "${data}\RCreditos2010_aux.dta", replace


* -------------- *
**## Data: 2013
* -------------- *
use    "${data}\BaseEncuestas.dta", clear
rename llave_13 llave
merge  1:1 llave using "${ELCA}\2013\Rural\Rhogar.dta", keep(3) nogen
rename llave    llave_13
keep   consecutivo llave_13 destino2013_* fechai_mes_* fechai_ano_* meses_plazo_* con_quien_* vr_inicial_* vr_saldo_* cuota_* cred_anombre_* cred_aldia_*
rename destino2013_* destino_*
local  i = 20
forvalues j=12(-1)1 {
	rename (destino_`j' fechai_mes_`j' fechai_ano_`j' meses_plazo_`j' con_quien_`j' vr_inicial_`j' vr_saldo_`j' cuota_`j' cred_anombre_`j' cred_aldia_`j') (destino_`i' fechai_mes_`i' fechai_ano_`i' meses_plazo_`i' con_quien_`i' vr_inicial_`i' vr_saldo_`i' cuota_`i' cred_anombre_`i' cred_aldia_`i')
	local i = `i'-1
}
save "${data}\RCreditos2013_aux.dta", replace


* -------------- *
**## Data: 2016
* -------------- *
use    "${data}\BaseEncuestas.dta", clear
rename llave_16  llave_n16
merge  1:1 llave_n16 using "${ELCA}\2016\Rural\Rhogar.dta", keep(3) nogen
rename llave_n16 llave_16 
keep   consecutivo llave_16 destino2016_* fechai_mes_* fechai_ano_* meses_plazo_* con_quien_* vr_inicial_* vr_saldo_* cuota_* cred_anombre_* cred_aldia_*
rename destino2016_* destino_*
local  i = 29
forvalues j=9(-1)1 {
	rename (destino_`j' fechai_mes_`j' fechai_ano_`j' meses_plazo_`j' con_quien_`j' vr_inicial_`j' vr_saldo_`j' cuota_`j' cred_anombre_`j' cred_aldia_`j') (destino_`i' fechai_mes_`i' fechai_ano_`i' meses_plazo_`i' con_quien_`i' vr_inicial_`i' vr_saldo_`i' cuota_`i' cred_anombre_`i' cred_aldia_`i')
	local i = `i'-1
}
save "${data}\RCreditos2016_aux.dta", replace




			* ============================= *
**#				  3. JOINING ROUNDS
			* ============================= *

*Joining rounds
use   "${data}\BaseEncuestas.dta", clear
merge m:1 consecutivo hogar_13 using "${data}\RCreditos2010_aux.dta", nogen keep(1 3)
merge 1:1 consecutivo llave_13 using "${data}\RCreditos2013_aux.dta", nogen keep(1 3)
merge 1:1 consecutivo llave_16 using "${data}\RCreditos2016_aux.dta", nogen keep(1 3)

*Reorganizing data
reshape long destino_ fechai_mes_ fechai_ano_ meses_plazo_ con_quien_ vr_inicial_ vr_saldo_ cuota_ cred_anombre_ cred_aldia_, i(consecutivo llave_13 llave_16) j(du_credito)
sort    consecutivo llave_13 du_credito
gen     round = 10 if du_credito<=8
replace round = 13 if du_credito>8 & du_credito<=20
replace round = 16 if du_credito>20

*Dates
gen    fecha_inicio = ym(fechai_ano_, fechai_mes_)
gen    fecha_fin    = fecha_inicio + meses_plazo_
format %tm fecha_inicio fecha_fin

*Converting to 2018 Colombian pesos
gen     year = fechai_ano_
gen     mes  = fechai_mes_
merge   m:1 year mes using "${data}\IPC_mensual.dta", keep (1 3) nogen
replace vr_inicial_ = vr_inicial_*IPC
replace cuota_      = cuota_*IPC
replace vr_saldo_   = vr_saldo_*IPC
drop    fechai_ano_ fechai_mes_ year mes

* Processing valid credits
**Deleting credits from 2010 that began after the survey (5)
drop if (fecha_inicio>encuesta_2010      & fecha_inicio!=. & round==10)
**Deleting credits from 2010 that were also reported in 2013 (121)
drop if (fecha_inicio<encuesta_2010      & fecha_inicio!=. & round==13)
**Deleting credits from 2013 that were also reported in 2016 (193)
drop if (fecha_inicio<encuesta_2013      & fecha_inicio!=. & round==16)
**Deleting credits from more than ${cut_treat10} years prior the 2013 survey
drop if (fecha_inicio<(encuesta_2013-${cut_treat10}) & fecha_inicio!=.)

*New variables
**Credit destination
***2010
/*1 Agriculture; 2 Cattle; 3 Livestock different from cattle; 4 Land acquisition; 5 Irrigation investment; 6 Investment in permanent and semipermanent structures; 7 Investment in soil conservation and water reserves; 8 Investment in fruit trees; 9 Investment in timber trees; 10 Investment in other commercial trees; 11 Free investment; 12 Purchase of machinnery and equipment; 13 Vehicle purchase; 14 Purchase of assets for the bussiness; 15 Household; 16 Health; 17 Education; 18 Recreation; 19 Consumption; 20 Payment of other debts; 21 Other*/
gen     destino_agrop_  = 0 if  destino_!=.
replace destino_agrop_  = 1 if (destino_<=10 & destino_!=.  & round==10)
gen     destino_otro_   = 0 if  destino_!=.
replace destino_otro_   = 1 if (destino_>10  & destino_!=.  & round==10)
gen     destino_formal_ = 0 if  destino_!=.
replace destino_formal_ = 1 if (destino_!=.  & destino_!=11 & destino_!=16 & destino_!=18 & destino_!=19 & destino_!=20 & destino_!=21 & round==10)
gen     destino_resil_  = 0 if  destino_!=.
replace destino_resil_  = 1 if (destino_!=. & destino_!=11 & destino_!=16 & destino_!=18 & destino_!=19 & destino_!=20 & destino_!=21 & round==10)
***2013
replace destino_agrop_  = 1 if  (destino_>=13 & destino_<=15 & destino_!=. & round==13)
replace destino_otro_   = 1 if ((destino_<13  | destino_>15) & destino_!=. & round==13)
replace destino_formal_ = 1 if  (destino_!=.  & destino_!=5  & destino_!=6 & destino_!=9 & destino_!=10 & destino_!=11 & destino_!=18 & round==13)
replace destino_resil_  = 1 if  (destino_!=.  & destino_!=5  & destino_!=6 & destino_!=9 & destino_!=10 & destino_!=11 & destino_!=18 & round==13)
***2016
replace destino_agrop_  = 1 if  (destino_>=13 & destino_<=15 & destino_!=. & round==16)
replace destino_otro_   = 1 if ((destino_<13  | destino_>15) & destino_!=. & round==16)
replace destino_formal_ = 1 if  (destino_!=.  & destino_!=5  & destino_!=6 & destino_!=9 & destino_!=10 & destino_!=11 & destino_!=18 & round==16)
replace destino_resil_  = 1 if  (destino_!=.  & destino_!=5  & destino_!=6 & destino_!=9 & destino_!=10 & destino_!=11 & destino_!=18 & round==16)
*Credit provider
gen     conquien_banco_   = 0 if (con_quien_!=.)
replace conquien_banco_   = 1 if (con_quien_==1)
gen     conquien_cajas_   = 0 if (con_quien_!=.)
replace conquien_cajas_   = 1 if (con_quien_==4)
gen     conquien_fondos_  = 0 if (con_quien_!=.)
replace conquien_fondos_  = 1 if (con_quien_==2)
gen     conquien_gremios_ = 0 if (con_quien_!=.)
replace conquien_gremios_ = 1 if (con_quien_==5)
gen     conquien_formal_  = 0 if (con_quien_!=.)
replace conquien_formal_  = 1 if (con_quien_!=6 & con_quien_!=7 & con_quien_!=8 & con_quien_!=9 & con_quien_!=10 & con_quien_!=13 & round==10 )
replace conquien_formal_  = 1 if (con_quien_!=6 & con_quien_!=7 & con_quien_!=8 & con_quien_!=9 & con_quien_!=12 & con_quien_!=13 & con_quien_!=14 & con_quien_!=15 & round!=10 )
**In whose name was the loan taken?
gen     anombre_jefe_     = 0 if (cred_anombre_!=.)
replace anombre_jefe_     = 1 if (cred_anombre_==1)
gen     anombre_conyuge_  = 0 if (cred_anombre_!=.)
replace anombre_conyuge_  = 1 if (cred_anombre_==2) 
gen     anombre_otro_     = 0 if (cred_anombre_!=.)
replace anombre_otro_     = 1 if (cred_anombre_>2 & cred_anombre_<100)
**Credit up to date?
gen     cred_aldia_si_    = 0 if (destino_!=. & round!=10)
replace cred_aldia_si_    = 1 if (cred_aldia_==1)
gen     cred_aldia_no_    = 0 if (destino_!=. & round!=10)
replace cred_aldia_no_    = 1 if (cred_aldia_==2)
gen     cred_aldia_noini_ = 0 if (destino_!=. & round!=10)
replace cred_aldia_noini_ = 1 if (cred_aldia_==3)
**Number of credits
gen credito_        = (destino_!=.)
*** Treatment with formal credits
gen     credito_formal_ = (con_quien_== 1 | con_quien_==2 | con_quien_==3 | con_quien_==4 | con_quien_==5 | con_quien_==9 | con_quien_==10 | con_quien_==11 | con_quien_==14)
replace credito_formal_ = 0 if (con_quien_==10 & round==10)
*** Treatment with resilience credits
gen     credito_resil_ = 0
replace credito_resil_ = 1 if (destino_!=. & destino_!=5 & destino_!=6 & destino_!=9 & destino_!=10 & destino_!=11 & destino_!=18 & round!=10)
replace credito_resil_ = 1 if (destino_!=. & destino_!=11 & destino_!=16 & destino_!=18 & destino_!=19 & destino_!=20 & destino_!=21 & round==10)
gen     credito_agrop_ = (destino_agrop_==1)

* Robustness check
capture drop _merge
merge   m:1 consecutivo llave_13 using "${data}\Fechaschoques2013.dta", nogen keep(1 3)
*merge m:1 consecutivo llave_16 using "${data}\Fechaschoques2016.dta", nogen keep(1 3)

save "${data}\Base_Creditos.dta", replace



			* ============================= *
**# 				  4. TREATMENT
			* ============================= *

use "${data}\Base_Creditos.dta", clear

*Organizando datos
collapse (sum) credito_ credito_formal_ credito_agrop_ credito_resil_ (max) credit_before_shock, by(consecutivo llave_13 llave_16 round)
replace  credito_        = 0 if credito_ ==.
replace  credito_formal_ = 0 if credito_formal_ ==.
replace  credito_agrop_  = 0 if credito_agrop_ ==.
replace  credito_resil_  = 0 if credito_resil_ ==.
reshape  wide credito_ credito_formal_ credito_agrop_ credito_resil_ credit_before_shock, i(consecutivo llave_13 llave_16) j(round)
drop     credit_before_shock10 credit_before_shock16
rename   credit_before_shock13 credit_before_shock
replace  credit_before_shock=0

*Treatment with all credits
gen     treat10 = (credito_10>0)
gen     treat13 = (credito_13>0 & treat10==0)
replace treat13 = . if (treat10==1 | credit_before_shock==1)
gen     treat16 = (credito_16>0 & treat10==0 & treat13==0)
replace treat16 = . if (treat13==1 | treat10==1 | credit_before_shock==1)

*Treatment with agricultural credits
gen     treat_agrop10 = (credito_agrop_10>0)
gen     treat_agrop13 = (credito_agrop_13>0 & treat_agrop10==0)
replace treat_agrop13 = . if (treat_agrop10==1 |  credit_before_shock==1)
gen     treat_agrop16 = (credito_agrop_16>0 & treat_agrop10==0 & treat_agrop13==0)
replace treat_agrop16 = . if (treat_agrop13==1 | treat_agrop10==1 | credit_before_shock==1)

*Treatment with formal credits
gen     treat_formal10 = (credito_formal_10>0)
gen     treat_formal13 = (credito_formal_13>0 & treat_formal10==0)
replace treat_formal13 = . if (treat_formal10==1 | credit_before_shock==1)
gen     treat_formal16 = (credito_formal_16>0 & treat_formal10==0 & treat_formal13==0)
replace treat_formal16 = . if (treat_formal13==1 | treat_formal10==1 | credit_before_shock==1)

*Treatment with resilience credits
gen     treat_resil10 = (credito_resil_10>0)
gen     treat_resil13 = (credito_resil_13>0 & treat_resil10==0)
replace treat_resil13 = . if (treat_resil10==1 | credit_before_shock==1)
gen     treat_resil16 = (credito_resil_16>0 & treat_resil10==0 & treat_resil13==0)
replace treat_resil16 = . if (treat_resil13==1 | treat_resil10==1 | credit_before_shock==1)

*Creating dummies and counting credits
foreach r of global ronda {
	gen d_credito_`r'   = (credito_`r'>0)
	gen num_credito_`r' = credito_`r' if d_credito_`r'==1
	gen d_credito_agrop_`r'   = (credito_agrop_`r'>0)
	gen num_credito_agrop_`r' = credito_agrop_`r' if d_credito_agrop_`r'==1
	gen d_credito_formal_`r'   = (credito_formal_`r'>0)
	gen num_credito_formal_`r' = credito_formal_`r' if d_credito_formal_`r'==1
	gen d_credito_resil_`r'   = (credito_resil_`r'>0)
	gen num_credito_resil_`r' = credito_resil_`r' if d_credito_resil_`r'==1
}

*Saving data
save "${data}\Base_Tratamiento.dta", replace


* ---------------------------------------- *
**## Saving data with treatment variables
* ---------------------------------------- *
drop    if ${treat} == .
replace treat13 = ${treat}
keep    llave_13 treat13
save    "${data}\BaseTreatmentIds.dta", replace
