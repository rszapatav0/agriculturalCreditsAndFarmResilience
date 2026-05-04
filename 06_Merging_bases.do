/* -----------------------------------------------------------------------------
Joining all important databases
Author:       Raquel
Creation:     March 2024
Last edition: January 2025
----------------------------------------------------------------------------- */

clear all
global ELCA "C:\Users\userecon10\Desktop\Bases ELCA\"
global data "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"

global agg    "agri pecu agrop"
global ronda  "10 13 16"



			* ================================== *
**#				       JOINING DATABASES
			* ================================== *

use "${data}\BaseUPA_Jefes.dta", clear

*Covariables ELCA 
merge 1:1 llave_ID_lb using "${data}\CovariablesELCA.dta",       nogen keep(3)
*Credits and treatment status
merge 1:1 llave_13    using "${data}\BaseTreatmentIds.dta",      nogen keep(3)
*Indicators
merge 1:1 llave_ID_lb using "${data}\indicadoresCompuestos.dta", nogen keep(3)

save "${data}\CovariablesSelectionDatabase.dta", replace



* Survey dates
rename llave_13 llave
keep llave
merge  1:1 llave using "${ELCA}\2013\FechasEncuesta_2013.dta", keep (1 3) nogen

split  fecha_inicio, parse(/) destring
gen    encuesta = dmy(fecha_inicio1, fecha_inicio2, fecha_inicio3)
format encuesta %d
drop   fecha_inicio hora_inicio fecha_fin hora_fin fecha_inicio* zona

sum encuesta
di "Min: " %d `r(min)'
di "Max: " %d `r(max)'
sum encuesta if encuesta>=dmy(01,08,2013)
