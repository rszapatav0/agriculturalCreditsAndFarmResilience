/* -----------------------------------------------------------------------------
Database with UPA IDs for sample selection
Author:       Raquel
Creation:     March 2024
Last edition: December 2024

This dofile:
Build a single database of UPA IDs for all three rounds.

We select UPAs under the following criteria:
	1. Same household head for the three rounds.
		Note: The result is the same as if we did it for the same household head only in 2013 an 2016.
	2. Households that had at least one plot in 2013, even if they did not have any in 2016. This is done in the 03_Merging_data dofile.
	3. That in 2016 the round they were located within the regions and departments considered, as some households may have moved. This is done in the 03_Merging_data dofile.
		Note: There are 14 household that select the right region but the department is not inside that region or is not on the initial sample.
	4. Household without credits for the 2010 round. This is done in the 02_Credit_data dofile.
----------------------------------------------------------------------------- */

clear all
global ELCA "C:\Users\userecon10\Desktop\Bases ELCA\"
global data "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"


			* =================================================================== *
**#				 1. PEOPLE AND HEADS OF HOUSEHOLDS (PERSONAS Y JEFES DE HOGAR)
			* =================================================================== *

/*Note: in 2013 and 2016 there are missing observations in the variable llave_ID_lb, which is the person identifier for all rounds. An attempt to correct this was made by combining other identification variables, but there was no correspondence between rounds. Therefore, observations with missing in the variable llave_ID_lb were eliminated.*/

* ------- *
**## 2010
* ------- *
use        "${ELCA}\2010\Rural\Rpersonas.dta", clear
keep       llave_ID_lb consecutivo parentesco sexo
duplicates report llave_ID_lb
foreach v of varlist parentesco sexo {
	rename `v' `v'_10
}
save       "${data}\Rpersonas2010_auxUPA.dta", replace


* ------- *
**## 2013
* ------- *
use        "${ELCA}\2013\Rural\Rpersonas.dta", clear
keep       llave llave_ID_lb consecutivo hogar parentesco sexo
duplicates report llave_ID_lb
drop       if     llave_ID_lb==.
duplicates report llave_ID_lb
foreach v of varlist llave hogar parentesco sexo {
	rename `v' `v'_13
}
save       "${data}\Rpersonas2013_auxUPA.dta", replace


* ------- *
**## 2016
* ------- *
use        "${ELCA}\2016\Rural\Rpersonas.dta", clear
drop       llave hogar
rename     (llave_n16 hogar_n16) (llave hogar)
keep       llave llave_ID_lb consecutivo hogar parentesco sexo
duplicates report llave_ID_lb
drop       if     llave_ID_lb==.
duplicates report llave_ID_lb
foreach v of varlist llave hogar parentesco sexo {
	rename `v' `v'_16
}
save       "${data}\Rpersonas2016_auxUPA.dta", replace


* ----------------------------------- *
**## People (Personas) 2010-2013-2016
* ----------------------------------- *
use   "${data}\Rpersonas2010_auxUPA.dta", clear
merge 1:1 llave_ID_lb using "${data}\Rpersonas2013_auxUPA.dta", gen(merge_13) keep(3)
merge 1:1 llave_ID_lb using "${data}\Rpersonas2016_auxUPA.dta", gen(merge_16) keep(3)
tab   merge_13 merge_16, m
save  "${data}\BaseUPA_Personas.dta", replace


* ----------------------------------------- *
**## Household heads (Jefes) 2010-2013-2016
* ----------------------------------------- *
use "${data}\BaseUPA_personas.dta", clear
duplicates report llave_ID_lb
keep       if parentesco_13==1 & parentesco_16==1
duplicates report llave_ID_lb
duplicates report llave_13
save       "${data}\BaseUPA_Jefes.dta", replace

*Test
gen test_sexo = (sexo_13 == sexo_16)
tab test_sexo /*Check*/
