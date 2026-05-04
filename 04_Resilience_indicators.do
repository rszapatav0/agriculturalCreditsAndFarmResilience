/* -----------------------------------------------------------------------------
Constructing resilience indicators with ELCA database.
Author: 	  Raquel
Creation:     July 2023
Last edition: January 2025

This dofile:
Uses the combined database to construct the indicators for each resilience capacity as defined in Slijper et al (2021), Stetter et al (2022), among others.
----------------------------------------------------------------------------- */


clear all
global ELCA "C:\Users\userecon10\Desktop\Bases ELCA\"
global data "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"

global agg    "agri pecu agrop"
global ronda  "10 13 16"
global ronda2 "13 16"
global shock  "0.3"


use  "${data}\Variables_Indicadores.dta", clear

			* ================================== *
**#				          ROBUSTNESS
			* ================================== *

*Organizing wealth index
foreach r of global ronda {
	sum riqueza_pca_`r'
	gen riqueza_std_`r' = ((riqueza_pca_`r' - r(min)) / (r(max) - r(min))) + 1 //Adding one to avoid cero.
}
sum riqueza_*


* ----------------------------------------------------- *
**## Resistance (Variation from Slijper et al., 2021)
* ----------------------------------------------------- *

*Auxiliar variables
foreach r of global ronda {
	**Benefit-Cost ratio
	gen  ROA_`r'    = (ing_agrop_`r' / riqueza_std_`r')
	**Min value
	egen ROAmin_`r' = min(ROA_`r')
	**Max value
	egen ROAmax_`r' = max(ROA_`r')
	**Standarizing
	gen  R_resist_`r' = (ROA_`r' - ROAmin_`r') / (ROAmax_`r'- ROAmin_`r')

}
sum ROA_*
sum R_resist_10 R_resist_13 R_resist_16 //Standarized ROA

*Indicator
** Rounds 10-13
gen     R_resist_v1 = (R_resist_13 - R_resist_10)/(R_resist_10)
replace R_resist_v1 = 0                                         if (R_resist_13>=R_resist_10)
** Rounds 13-16
gen     R_resist_v2 = (R_resist_16 - R_resist_13)/(R_resist_13)
replace R_resist_v2 = 0                                         if (R_resist_16>=R_resist_13)
sum R_resist*v*


* ----------------------------------------------------- *
**## Shock (Slijper et al., 2021)
* ----------------------------------------------------- *
*Indicator
** Rounds 10-13
*gen     R_shock_v1 = 0
*replace R_shock_v1 = 1 if (R_resist_v1 >= ${shock} | R_resist_v1 <= -${shock})
** Rounds 13-16
gen     R_shock_v2 = 0
replace R_shock_v2 = 1 if (R_resist_v2 >= ${shock} | R_resist_v2 <= -${shock})
sum R_shock*


* ----------------------------------------------------- *
**## (NO) Recovery rate (Slijper et al., 2021)
* ----------------------------------------------------- *
//In order to calculate the recovery rate as in Slijper et al. (2021), we need data for 2019. But, i) we don't have it, and ii) with three years period one could think that the change already includes the recovery.
//We use change the focus: we want to calculate the robustness on a farm considering not only the period before the shock (2013) and the period post-shock (2016), but also the previos period (2010) as a measure for agricultural income stability. We use the same formula in Slijper, but changing the logic: the change in the disturbance period (2013-2016) with respect to the change in the previos and allegedly more stable period (2010-2013). 
// * If the rate is >0, the change in the disturbance period is bigger (as expected.)
// * If the rate is <0, the change in the disturbance period is less (for more robust farms)
//There is a big problem with this indicator: there are some farms where ROA_10 and ROA_13 are both cero, so the result is missing.

/*Auxiliar variables
capture drop R_recovrt_16 R_recovrt_v2
gen     R_recovrt_16 = ((ROA_16-ROA_13)/(ROA_13-ROA_10)) if (ROA_16<ROA_13)

replace R_recovrt_16 = ((ROA_16-ROA_13)/(ROA_13-ROA_10)) if (ROA_16<ROA_13)
replace R_recovrt_16 = ((ROA_16-ROA_13)/(ROA_13-ROA_10)) if (ROA_16<ROA_13)

*Indicator
sum R_recovrt_16
gen R_recovrt_v2 = (R_recovrt_16 - r(min)) / (r(max)- r(min))
sum R_recovrt_v2
*/


* ----------------------------------------------------- *
**## Tendency
* ----------------------------------------------------- *
*Indicator
egen R_recovrt_v2 = rowmean(ROA_1*)




			* ================================== *
**#				          ADAPTATION
			* ================================== *

global areas "permanentes transitorios mixtos ganaderia pastos bosques otros_usos tierra_no_usada"

*Recalculating land size
foreach r of global ronda2 {
	gen tamanoAux_`r'=0
	foreach c of global areas {
		replace tamanoAux_`r' = tamanoAux_`r' + tamano_`c'_`r'
	}
}
sum tamanoAux*


* ----------------------------------------------------- *
**## Land diversity: SDI (variation from Slijper et al., 2021)
* ----------------------------------------------------- *

**Percentages
foreach r of global ronda2 {
	gen A_SDI_`r' = 0
	
	foreach c of global areas {
		***Percentage of use by area and round (for all plots)
		gen p_`c'_`r'   = tamano_`c'_`r' / tamanoAux_`r'
		***Shannon diversity index by area
		gen SDI_`c'_`r' = p_`c'_`r'*ln(p_`c'_`r')
		***Shannon diversity index
		replace A_SDI_`r' = A_SDI_`r' + SDI_`c'_`r' if (SDI_`c'_`r'!=.)
	}
	
	replace A_SDI_`r' = A_SDI_`r'*(-1)
	*replace A_SDI_`r' = .               if (tamano_`r'==0)
}
sum p_*
sum SDI_*
sum A_SDI_*,d

*Indicator
** Rounds 10-13
*gen A_SDI_v1 = abs(A_SDI_13 - A_SDI_10) if (A_SDI_10!=. & A_SDI_13!=.)
** Rounds 13-16
gen A_SDI_v2 = abs(A_SDI_16 - A_SDI_13) if (A_SDI_13!=. & A_SDI_16!=.)
sum A_SDI_v*


* ----------------------------------------------------- *
**## Land diversity: GSI (Stetter et al., 2022)
* ----------------------------------------------------- *
*Auxiliar variables
**Percentages
foreach r of global ronda2 {
	gen A_GSI_`r' = 0
	
	foreach c of global areas {
		***Gini-Simpson index by area
		gen GSI_`c'_`r' = (p_`c'_`r')^2
		***Gini-Simpson index
		replace A_GSI_`r' = A_GSI_`r' + GSI_`c'_`r' if (GSI_`c'_`r'!=.)
	}
	
	replace A_GSI_`r' = (1-A_GSI_`r')*100
}
sum GSI*
sum A_GSI_*, d

*Indicator
** Rounds 10-13
*gen A_GSI_v1 = abs(A_GSI_13 - A_GSI_10) if (A_GSI_10!=. & A_GSI_13!=.)
** Rounds 13-16
gen A_GSI_v2 = abs(A_GSI_16 - A_GSI_13) if (A_GSI_13!=. & A_GSI_16!=.)
sum A_GSI_v*


* ----------------------------------------------------- *
**## Irrigation (Variation from Slijper et al., 2021)
* ----------------------------------------------------- *

*Dummy for irrigation investment
gen A_irrigation_v1 = invd_1_13
gen A_irrigation_v2 = invd_1_16
tab1 A_irrigation*


* ----------------------------------------------------- *
**## Labour (Slijper et al., 2021)
* ----------------------------------------------------- *
/* Note: there is no data of labour in 2010. 
In 2013, only 78 households reported hiring labor, while in 2016 the number rose to 126.
* Laborers per month (number of months of labor used) 
/*NOTE: This metric is no longer used because very few households reported data*/
gen A_labour_13 = cuantos_obr_13*meses_obrero_13
gen A_labour_16 = cuantos_obr_16*meses_obrero_16
gen A_labour_v2 = (A_labour_16-A_labour_13)/A_labour_13 if (A_labour_13!=. & A_labour_16!=.)
sum A_labour*
This indicator has very few data points.
We'll use last year's labor costs (although this is a bit ambiguous).
*/

*Auxiliar variables
gen A_labourexp_13 = gastprom_manobra_13/tamano_13
replace A_labourexp_13=0 if tamano_13==0
gen A_labourexp_16 = gastprom_manobra_16/tamano_16
replace A_labourexp_16=0 if tamano_16==0

*Indicator
gen A_labourexp_v2 = abs(A_labourexp_16-A_labourexp_13) if (A_labourexp_13!=. & A_labourexp_16!=.)
*replace A_labourexp_v2=ln(A_labourexp_v2+1)
sum A_labourexp*
//Note: In order to decrease the magnitude of the indicator (because the units was colombian pesos), I introduced a logaritmic transformation.


* ----------------------------------------------------- *
**## Livestock Units (Variation from Slijper et al., 2021)
* ----------------------------------------------------- *
/*Note: Animals of similar size are used.*/
global activos "bueyes vacas cerdos caballos ovejas" /* avescorral  colmenas otros_anim*/
*Auxiliar variables
foreach r of global ronda2 {
	gen lu_`r' = 0
	
	foreach ac of global activos {
		replace lu_`r' = lu_`r' + n_`ac'_`r' if n_`ac'_`r'!=.
	}
	
	gen     A_luh_`r' = lu_`r'/ tamano_`r' if (tamano_`r'!=.)
	replace A_luh_`r'= 0 if tamano_`r'==0
}
sum A_luh*

*Indicator
*gen A_luh_v1 = abs(A_luh_13-A_luh_10) if (A_luh_10!=. & A_luh_13!=.)
gen A_luh_v2 = abs(A_luh_16-A_luh_13) if (A_luh_13!=. & A_luh_16!=.)
*replace A_luh_v2=ln(A_luh_v2+1)
//Note: In order to decrease the magnitude of the indicator (because the units was number of livestock units per hectare), I introduced a logaritmic transformation.
sum A_luh_v*


* ----------------------------------------------------- *
**## (NO) Feed ratio (Variation from Slijper et al., 2021)
* ----------------------------------------------------- *
*Auxiliar variables
*gen A_feedrt_13 = gastprom_alim_13 / A_luh_13 if A_luh_13!=.
*gen A_feedrt_16 = gastprom_alim_16 / A_luh_16 if A_luh_16!=.

*Indicator
*gen A_feedrt_v2 = abs(A_feedrt_16-A_feedrt_13) if (A_feedrt_13!=. & A_feedrt_16!=.)
*replace A_feedrt_v2=ln(A_feedrt_v2+1)
//Note: In order to decrease the magnitude of the indicator (because the units was colombian pesos), I introduced a logaritmic transformation.
*sum A_feedrt*


* ----------------------------------------------------- *
**## Addition of machinery and new technologies (Authors?)
* ----------------------------------------------------- *
*Auxiliar variables
replace invd_2_13 = 1 if invd_2_13 == 2
replace invd_2_16 = 1 if invd_2_16 == 2

*Dummy for structures investment
gen A_structures_v1 = invd_2_13
gen A_structures_v2 = invd_2_16
tab1 A_structures*


* ----------------------------------------------------- *
**## Farm investments (new)
* ----------------------------------------------------- *
*Farm investment for different pruposes
//Irrigation, infrastructure, conservation, fruit trees, timber trees, other commercial trees, housing, natural disasters, other. Up to three items may be listed per lot, and the value of the investments is added up.

//The proportion of investment in resilience relative to total investment. If no investments are made (zero denominator), the indicator is set to zero.
//Resil1: Irrigation, infrastructure
//Resil2: Irrigation, fruit trees, timber trees, other commercial trees, housing.
*Indicators
gen     A_invest_v1 = 0
replace A_invest_v1 = vr_inverResil2_13/vr_inverHecha_13 if vr_inverHecha_13!=0
gen     A_invest_v2 = 0
replace A_invest_v2 = vr_inverResil2_16/vr_inverHecha_16 if vr_inverHecha_16!=0
sum A_invest_*



		
			* ================================== *
**#				        TRANSFORMATION
			* ================================== *

* ----------------------------------------------------- *
**## Farm type (variation from Slijper et al., 2021)
* ----------------------------------------------------- *
* Indicators by category
*replace type_10 = 3 if (type_10==0 | type_10==.)
replace type_13 = 3 if (type_13==0 | type_13==.)
replace type_16 = 3 if (type_16==0 | type_16==.)
*gen T_farmtype_v1 = (type_10 != type_13)
gen T_farmtype_v2 = (type_13 != type_16)
sum T_farmtype*

* From where to where?
tab type_13, m
tab type_16, m
tab type_13 type_16, m


* ----------------------------------------------------- *
**## Land use (new)
* ----------------------------------------------------- *
//Change in the main land use

*Bigger area
foreach r of global ronda2{
	egen double area_max_`r' = rowmax(tamano_permanentes_`r' tamano_transitorios_`r' tamano_mixtos_`r' tamano_ganaderia_`r' tamano_pastos_`r' tamano_bosques_`r' tamano_otros_usos_`r' tamano_tierra_no_usada_`r')
	foreach c of global areas {
		gen area_max_`c'_`r' = (tamano_`c'_`r'==area_max_`r')
		replace area_max_`c'_`r' = 0 if tamano_`r'==0
	}
	egen area_count_`r' = rowtotal(area_max_*_`r')
}
tab1 area_count_1*

*Comparing years by crop
foreach c of global areas {
	gen area_maxv_`c' = (area_max_`c'_13!=area_max_`c'_16)
}
sum area_maxv*

*Indicator
egen T_landuse_v2 = rowmax(area_maxv_*)
tab  T_landuse_v2
tab  T_landuse_v2 if tamanoAux_16==0
//For those who doesn't have any land in 2016, T_landuse_v2==1. It is, they changed drastically their land use. Also, those who have more than one main use and changed at least one, También para los que tienen varios usos principales y al menos uno cambió.



			* ============================== *
**#			    SAVING INDICATORS DATABASE
			* ============================== *

*Keeping variables
keep consecutivo llave_* llave_ID_lb R_*v2 A_*v2 T_*v2 type_13
duplicates report llave_ID_lb
save "${data}\Indicadores.dta", replace

* ----------------------------------------------------- *
**## Normalizing indicators using the min-max procedure
* ----------------------------------------------------- *
**### Robustness
*Resistance
replace R_resist_v2 = R_resist_v2 + 1

*Tendency
sum R_recovrt_v2
replace R_recovrt_v2 = (R_recovrt_v2 - r(min))/(r(max) - r(min))


**### Adaptation
*SDI
sum A_SDI_v2
replace A_SDI_v2 = (A_SDI_v2 - r(min))/(r(max) - r(min))

*GSI
replace A_GSI_v2 =  A_GSI_v2/100

*Labour expenditure
sum A_labourexp_v2
replace A_labourexp_v2 = (A_labourexp_v2 - r(min))/(r(max) - r(min))

*Livestock units
sum A_luh_v2
replace A_luh_v2 = (A_luh_v2 - r(min))/(r(max) - r(min))

sum, d
save "${data}\IndicadoresStd.dta", replace



