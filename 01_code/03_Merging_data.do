/* -----------------------------------------------------------------------------
Merging the ELCA modules
Author:       Raquel
Creation:     February 2023
Last edition: February 2025

Organizing variables for:
	1. Causal Forest model covariables.
	2. Computation of resilience indicators.

This dofile cleans and combines every relevant module of the ELCA survey. The final result are two databases: One for the covariates and another for the variables used to construct the resilience indicators.
Both databases are constructed for the UPAs selected on the 01_UPAs dofile.
----------------------------------------------------------------------------- */

			* ================================== *
**#				 	   DEFINING GLOBALS
			* ================================== *

clear  all
global ELCA "C:\Users\userecon10\Desktop\Bases ELCA\"
global data "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"

global id        "consecutivo llave"
global agg       "agri pecu agrop"
global ronda     "10 13 16"
global class     "0.66666666666667"
global outl      "p99"
global areamax   "30" //Maximum area
global areamin   "0.1"
global fractions "10 100 1000 10000 100000 1000000"

   

			* =================================== *
**#				 1. PERSONS AND HOUSEHOLD HEAD
			* =================================== *

* ------------------------- *
**## 2010 - Household head
* ------------------------- *
*Calling database
use  "${ELCA}\2010\Rural\Rpersonas.dta", clear
keep if parentesco==1

*Organizing variables
**Education level
gen     educacion = 0 if (nivel_educ == 1) /*no education*/
replace educacion = 1 if (nivel_educ == 2 | nivel_educ == 3) /*preschool or elementary school*/
replace educacion = 2 if (nivel_educ == 4) /*middle and high school*/
replace educacion = 3 if (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*technical and technological*/
replace educacion = 4 if (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*university and graduate school*/

*Saving data
keep consecutivo educacion
rename (educacion) (educacion_10)
save "${data}\Rpersonas2010.dta", replace



* ----------------------- *
**## 2013 - Household head
* ----------------------- *
*Selecting variables
global personas13 "edad woman estadoCivil lee_escribe educacion main_job_agri tot_trabajos organizacion enfermedad fexpers"

*Calling database
use  "${ELCA}\2013\Rural\Rpersonas.dta", clear
keep if parentesco==1

*Joining dates of 2013 survey
merge  m:1 llave using "${ELCA}\2013\FechasEncuesta_2013.dta", keep (1 3) nogen
split  fecha_inicio, parse(/) destring
gen    encuesta = dmy(fecha_inicio1, fecha_inicio2, fecha_inicio3)
format encuesta %d
drop   fecha_inicio hora_inicio fecha_fin hora_fin fecha_inicio* zona

*Organizing variables
**Birthdate and age
gen    birthdate =  dmy(nac_dia, nac_mes, nac_ano)
format birthdate %d
capture drop edad
gen    edad      =  round((encuesta-birthdate)/365)
**Gender
gen woman = sexo - 1
**Civil status
gen     estadoCivil=estado_civil
replace estadoCivil=0 if estado_civil==5
***Civil status Dummy
gen estado_civil_union      = (estado_civil==1)
gen estado_civil_casado     = (estado_civil==2)
gen estado_civil_divorciado = (estado_civil==3)
gen estado_civil_viudo      = (estado_civil==4)
gen estado_civil_soltero    = (estado_civil==5)
**Main job agricultural
gen main_job_agri = (descrip_activ1==1)
**Number of jobs
replace tot_trabajos = 1 if actividad_ppal == 1 & tot_trabajos==. /*Trabajo por lo menos UNA hora en una actividad que le genero algun ingreso*/
replace tot_trabajos = 1 if actividad_ppal == 2 & tot_trabajos==. /*Trabajo como ayudante familiar sin que le pagaran por lo menos una hora*/
replace tot_trabajos = 1 if actividad_ppal == 3 & tot_trabajos==. /*No trabajo pero tenia un empleo o trabajo por el que recibe ingresos*/
replace tot_trabajos = 0 if actividad_ppal == 5 & tot_trabajos==. /*Es incapacitado(a) permanente para trabajar*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & estudia==1             /*Ninguna de las actividades principales anteriores. Estudia*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & actividad_principal==1 /*Ninguna de las actividades principales anteriores. Actividad principal son oficios del hogar*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & ahorra==3              /*Ninguna de las actividades principales anteriores. No recibe ingresos.*/
replace tot_trabajos = 0 if actividad_ppal == 6                                            /*Ninguna de las actividades principales anteriores.*/
replace tot_trabajos = 0 if tot_trabajos == .                                              /*No menciona número de trabajos.*/
tab tot_trabajos, m
**Organization
gen     organizacion =    (org_sindicato==1 | org_agremia==1)
replace organizacion =. if (org_sindicato==. & org_agremia==.)
**Reads-Writes
replace lee_escribe = 0 if (lee_escribe==2 | lee_escribe==8)
**Education level
gen     educacion = 0 if (nivel_educ == 1) /*ninguno*/
replace educacion = 1 if (nivel_educ == 2 | nivel_educ == 3) /*preescolar o primaria*/
replace educacion = 2 if (nivel_educ == 4) /*secundaria y media*/
replace educacion = 3 if (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*tecnica y tecnologica*/
replace educacion = 4 if (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*universitario y posgrado*/
***Education level - Dummies
gen educacion_ningun = (nivel_educ == 1) /*ninguno*/
gen educacion_prepri = (nivel_educ == 2 | nivel_educ == 3) /*preescolar o primaria*/
gen educacion_secmed = (nivel_educ == 4) /*secundaria y media*/
gen educacion_tecnio = (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*tecnica y tecnologica*/
gen educacion_unipos = (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*universitario y posgrado*/
gen educacion_nsnr   = (nivel_educ==. | nivel_educ==88) /*missing data. No sabe, no comenta*/
**Permanent condition
gen cond_permanente = (ceguera==1 | sordera==1 | mudez==1 | dif_moverse==1 | dif_banarse==1 | dif_calle==1 | dif_aprender==1)
tab cond_permanente,m
gen enfermedad      = (enf_corazon==1 | hipertenso==1 | hipertenso==1 | tuberculosis==1 | enfisema==1 | diabetes==1 | ulcera==1 | sida==1 | epilepsia==1 | cancer==1)
tab enfermedad, m
/*foreach v of varlist enf_corazon hipertenso hipertenso tuberculosis enfisema diabetes ulcera sida epilepsia cancer {
	qui replace `v'=0 if `v'==2 | `v'==3
	qui replace `v'=. if `v'==8
	sum `v'
}*/
replace enfermedad = 1 if cond_permanente == 1

*Saving data
keep llave_ID_lb ${id} ${personas13} 
rename (llave) (llave_13)
save "${data}\Rpersonas2013.dta", replace



* ------------------------- *
**## 2016 - Household head
* ------------------------- *
*Calling database
use  "${ELCA}\2016\Rural\Rpersonas.dta", clear
keep if parentesco==1

*Constructing variables
**Education level
gen     educacion = 0 if (nivel_educ == 1) /*ninguno*/
replace educacion = 1 if (nivel_educ == 2 | nivel_educ == 3) /*preescolar o primaria*/
replace educacion = 2 if (nivel_educ == 4) /*secundaria y media*/
replace educacion = 3 if (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*tecnica y tecnologica*/
replace educacion = 4 if (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*universitario y posgrado*/

*Saving data
keep llave_n16 educacion
rename (llave_n16 educacion) (llave_16 educacion_16)
save "${data}\Rpersonas2016.dta", replace



* ----------------------- *
**## 2013 - Spouse
* ----------------------- *
*Selecting variables
global conyuge13 "edad_cyg woman_cyg lee_escribe_cyg educacion_cyg main_job_agri_cyg tot_trabajos_cyg organizacion_cyg"

*Calling database
use  "${ELCA}\2013\Rural\Rpersonas.dta", clear
keep if parentesco==2

*Joining dates of 2013 survey
merge  m:1 llave using "${ELCA}\2013\FechasEncuesta_2013.dta", keep (1 3) nogen
split  fecha_inicio, parse(/) destring
gen    encuesta = dmy(fecha_inicio1, fecha_inicio2, fecha_inicio3)
format encuesta %d
drop   fecha_inicio hora_inicio fecha_fin hora_fin fecha_inicio* zona

*Constructing variables
**Birthdate and age
gen    birthdate =  dmy(nac_dia, nac_mes, nac_ano)
format birthdate %d
capture drop edad
gen    edad      =  round((encuesta-birthdate)/365)
**Gender
gen woman = sexo - 1
**Main job agricultural
gen main_job_agri = (descrip_activ1==1)
**Number of jobs
replace tot_trabajos = . if tot_trabajos   == 8
replace tot_trabajos = 1 if actividad_ppal == 1 & tot_trabajos==. /*Trabajo por lo menos UNA hora en una actividad que le genero algun ingreso*/
replace tot_trabajos = 1 if actividad_ppal == 2 & tot_trabajos==. /*Trabajo como ayudante familiar sin que le pagaran por lo menos una hora*/
replace tot_trabajos = 1 if actividad_ppal == 3 & tot_trabajos==. /*No trabajo pero tenia un empleo o trabajo por el que recibe ingresos*/
replace tot_trabajos = 0 if actividad_ppal == 5 & tot_trabajos==. /*Es incapacitado(a) permanente para trabajar*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & estudia==1             /*Ninguna de las actividades principales anteriores. Estudia*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & actividad_principal==1 /*Ninguna de las actividades principales anteriores. Actividad principal son oficios del hogar*/
replace tot_trabajos = 0 if actividad_ppal == 6 & tot_trabajos==. & ahorra==3              /*Ninguna de las actividades principales anteriores. No recibe ingresos.*/
replace tot_trabajos = 0 if actividad_ppal == 6                                            /*Ninguna de las actividades principales anteriores.*/
replace tot_trabajos = 0 if tot_trabajos == .                                            /*No menciona número de trabajos.*/
tab tot_trabajos, m
**Organization
gen     organizacion =    (org_sindicato==1 | org_agremia==1)
replace organizacion=. if (org_sindicato==. & org_agremia==.)
**Reads-Writes
replace lee_escribe = 0 if (lee_escribe==2 | lee_escribe==8)
**Education level
gen     educacion = 0 if (nivel_educ == 1) /*ninguno*/
replace educacion = 1 if (nivel_educ == 2 | nivel_educ == 3) /*preescolar o primaria*/
replace educacion = 2 if (nivel_educ == 4) /*secundaria y media*/
replace educacion = 3 if (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*tecnica y tecnologica*/
replace educacion = 4 if (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*universitario y posgrado*/
***Education level Dummies
gen educacion_ningun = (nivel_educ == 1) /*ninguno*/
gen educacion_prepri = (nivel_educ == 2 | nivel_educ == 3) /*preescolar o primaria*/
gen educacion_secmed = (nivel_educ == 4) /*secundaria y media*/
gen educacion_tecnio = (nivel_educ == 5  | nivel_educ == 6 | nivel_educ == 7 | nivel_educ == 8) /*tecnica y tecnologica*/
gen educacion_unipos = (nivel_educ == 9  | nivel_educ == 10 | nivel_educ == 11 | nivel_educ == 12)  /*universitario y posgrado*/
gen educacion_nsnr = (nivel_educ==. | nivel_educ==88) /*missing data. No sabe, no comenta*/

*Renaming variables
rename (edad woman lee_escribe educacion main_job_agri tot_trabajos organizacion) (edad_cyg woman_cyg lee_escribe_cyg educacion_cyg main_job_agri_cyg tot_trabajos_cyg organizacion_cyg)

*Saving data
keep ${id} ${conyuge13} 
rename (llave) (llave_13)
save "${data}\Rconyuge2013.dta", replace



			* ================== *
**#				 2. HOUSEHOLD
			* ================== *

**## Selecting variables
global ind_hogar10 "riqueza_pca"
global ind_hogar13 "riqueza_pca"
global ind_hogar16 "riqueza_pca"
global cov_hogar13 "t_personas sp_energia sp_acueducto sp_alcantarillado n_internet medio_transporte transporte_minutos ingmensual_* inganual_* empshare_* incshare_* gastmensual_all gastanual_all act_seghogar act_segcosechas riqueza_pca credito_rechazado mala_histcrediticia programas_ayudas fexhog region dpto mpio consecutivo_c "


* ------- *
**## 2010
* ------- *
use "${ELCA}\2010\Rural\Rhogar.dta", clear
keep consecutivo ${ind_hogar10}
foreach v of varlist $ind_hogar10 {
	rename `v' `v'_10
}
duplicates report consecutivo
save "${data}\Rhogar2010.dta", replace


* ------- *
**## 2013
* ------- *
*Calling database
use "${ELCA}\2013\Rural\Rhogar.dta", clear
rename fexhog_2013 fexhog

*Adjusting by inflation
gen year=2013
merge m:1 year using "${data}\IPC_anual.dta", keep (1 3) nogen
foreach v of varlist ing_trabnoagr ing_trabagr ing_pensiones ing_arriendos ing_intereses_div ing_ayudas ing_otros_nrem vr_gtos_mensuales {
	replace `v' = `v'*IPC
}

*Creating variables
**Aqueduct, energy, internet
replace sp_energia        = 0 if sp_energia==2
replace sp_acueducto      = 0 if sp_acueducto==2
replace sp_alcantarillado = 0 if sp_alcantarillado==2
replace n_internet        = 0 if n_internet==2
**Transportation in the county seat
gen     medio_transporte = 0 if (transp_cabec == 3)
replace medio_transporte = 1 if (transp_cabec == 2)
replace medio_transporte = 2 if (transp_cabec == 1)
**Time in municipal seat
gen transporte_minutos = (hor_cabec*60) + min_cabec
**Income proportion
gen empshare_agri   = ing_trabagr   / (ing_trabnoagr + ing_trabagr)/*Employment only*/
gen empshare_noagri = ing_trabnoagr / (ing_trabnoagr + ing_trabagr)
gen incshare_agri   = ing_trabagr   / (ing_trabnoagr + ing_trabagr + ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem) /*Other income*/
gen incshare_noagri = ing_trabnoagr / (ing_trabnoagr + ing_trabagr + ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem)
gen incshare_otro   = (ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem) / (ing_trabnoagr + ing_trabagr + ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem)
foreach v of varlist empshare_* incshare_* {
	replace `v'=0 if `v'==. /*Proportions with missing values are replaced with zero (this occurs when both the numerator and denominator are zero).*/
}
**Monthly income
gen ingmensual_otro   = (ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem)
gen ingmensual_agri   = ing_trabagr
gen ingmensual_noagri = ing_trabnoagr
gen ingmensual_all    = ingmensual_otro + ingmensual_agri + ingmensual_noagri
**Monthly expenses
gen gastmensual_all   = vr_gtos_mensuales
**Annual income
gen inganual_otro   = (ing_pensiones + ing_arriendos + ing_intereses_div + ing_ayudas + ing_otros_nrem)*12
gen inganual_agri   = ing_trabagr*12
gen inganual_noagri = ing_trabnoagr*12
gen inganual_all    = ingmensual_all*12
**Annual expenses
gen gastanual_all   = vr_gtos_mensuales*12
***Comparing income and expenses
gen ing_gast = ingmensual_all - gastmensual_all
sum ing_gast
//About 32% of households have income that exceeds their expenses. However, it is important to note that this figure does not include income from the sale of agricultural products, so households are actually earning more than this.
drop ing_gast
**Crop insurance
gen     act_seghogar    = (seguro_hogar==1)
replace act_segcosechas = 0 if (act_segcosechas==2 | seguro_hogar==2)
**Credit history
gen credito_rechazado   = (credito_si==2)
gen mala_histcrediticia = (rechazo_credito == 3)
**Programs or assistance
gen programas_ayudas = (familias_accion==1 | prg_adultomayor==1 | sena==1 | red_juntos==1 | icbf==1 | ayu_desastres_nat==1 | ayu_desplazados==1 | tit_baldios==1 | prg_tierras==1 | leydevictimas==1 | agro_ingresos==1 | oport_rural==1 | alianz_prod==1 | guardabosques==1 | otro_prg_rural==1)
foreach v of varlist familias_accion prg_adultomayor sena red_juntos icbf ayu_desastres_nat ayu_desplazados tit_baldios prg_tierras leydevictimas agro_ingresos oport_rural alianz_prod guardabosques otro_prg_rural{
	qui replace `v'=0 if `v'==2
	sum `v'
}

*Renaming variables for indicators
foreach v of varlist llave $ind_hogar13 {
	rename `v' `v'_13
}
gen riqueza_pca = riqueza_pca_13
drop transp_cabec transp_salud transp_escue

*Saving the database
keep consecutivo llave_13 riqueza_pca_13 ${ind_hogar13} ${cov_hogar13} 
duplicates report consecutivo llave_13
save "${data}\Rhogar2013.dta", replace


* ------- *
**## 2016
* ------- *
use "${ELCA}\2016\Rural\Rhogar.dta", clear
drop    llave
rename  (llave_n16) (llave)
keep consecutivo llave hogar ${ind_hogar16}
foreach v of varlist llave hogar $ind_hogar16 {
	rename `v' `v'_16
}
duplicates report consecutivo llave_16
save "${data}\Rhogar2016.dta", replace




			* ======================== *
**#				  3. HOME ASSETS
			* ======================== *

**## Selecting variables
global ind_activos "n_bueyes n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim"
global cov_activos "n_bueyes n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim"


* ------- *
**## 2010
* ------- *
use "${ELCA}\2010\Rural\Ractivos_hogar.dta", clear
rename  (abejas n_abejas n_oanim) (colmenas n_colmenas n_otros_anim)
foreach v of varlist n_* {
	replace `v'=0 if `v'==.
}
replace n_caballos = n_caballos + n_caballos_carga
keep    consecutivo ${ind_activos}
//Note: There are numbers such as 99 or 9999 (No information provided) that are outliers. In these cases, the data indicates that they do have animals but does not specify how many, likely because they do not know. This will be corrected later when the modules are combined, by calculating the average number of animals per hectare and imputing it based on the area (or livestock area) of these households. Everything will be left as 9999.
//Note 2: It was not necessary to fix this. None of the households in the sample had this problem.
replace n_cerdos=9999 if n_cerdos==99
foreach v of varlist $ind_activos {
	rename `v' `v'_10
}
duplicates report consecutivo
save "${data}\Ractivos2010.dta", replace


* ------- *
**## 2013
* ------- *
use "${ELCA}\2013\Rural\Ractivos_hogar.dta", clear
drop    n_otro_cual
foreach v of varlist n_* {
	replace `v'=0 if `v'==.
}
replace n_caballos = n_caballos + n_caballos_carga
keep     consecutivo llave ${ind_activos}
foreach v of varlist $ind_activos {
	gen `v'_13 = `v'
}
rename llave llave_13
duplicates report consecutivo llave_13
save "${data}\Ractivos2013.dta", replace


* ------- *
**## 2016
* ------- *
use "${ELCA}\2016\Rural\Ractivos_hogar.dta", clear
drop   llave hogar n_otro_cual
rename (llave_n16 hogar_n16) (llave hogar)
foreach v of varlist n_* {
	replace `v'=0 if `v'==.
}
replace n_caballos = n_caballos + n_caballos_carga
keep     consecutivo llave hogar ${ind_activos}
foreach v of varlist llave hogar $ind_activos {
	rename `v' `v'_16
}
duplicates report consecutivo llave_16
save "${data}\Ractivos2016.dta", replace




			* ======================== *
**#				  	 4. LANDS
			* ======================== *

**## Selecting variables
global ind_tierras10 "tamano* fuentes_agua*"
global ind_tierras13 "tamano* fuentes_agua* vr_inver* invd_*"
global ind_tierras16 "tamano* fuentes_agua* vr_inver* invd_*"
global cov_tierras13 "propietario asocio tipoTenencia_* totpred_* dadasPerdidasVendidas tamano tamano_* p_* class_tamano fuentes_agua* vr_inver* invd_*"
global imp_tierras16 "asocio propietario totpred_fincas dadasPerdidasVendidas tipoTenencia_*"
global areas "permanentes transitorios mixtos ganaderia pastos bosques otros_usos tierra_no_usada"

* ------- *
**## 2010
* ------- *
use "${ELCA}\2010\Rural\RTierras.dta", clear
keep if orden_predio!=.
drop otros_usos_cual

* Organizing sizes
recast  double tamano
replace tamano =      0.64*tamano if (unidad_medida==2 | unidad_medida==3) /*Fanegadas o cuadras*/
replace tamano = 0.0000835*tamano if  unidad_medida==5                     /*Varas*/
replace tamano =    0.0001*tamano if  unidad_medida==6                     /*Metro cuadrado*/
replace tamano = .                if (unidad_medida==7 | unidad_medida==.) /*Otro*/
gen     double  prueba_tamano  = 0
foreach v of global areas {
	replace `v' = 0                  if `v'==.
	gen     double tamano_`v' = `v'
	replace tamano_`v' = 0.64*`v'      if (`v'_um==2 | `v'_um==3) /*Fanegadas o cuadras*/
	replace tamano_`v' = 0.0000304*`v' if  `v'_um==4              /*Pies cuadrados*/
	replace tamano_`v' = 0.0000835*`v' if  `v'_um==5              /*Varas*/
	replace tamano_`v' = 0.0001 *`v'   if  `v'_um==6              /*Metro cuadrado*/
	replace tamano_`v' = .             if (`v'_um==7 | `v'_um==.) /*Otro*/
	replace prueba_tamano = prueba_tamano + tamano_`v' if tamano_`v'!=.
}
compare prueba_tamano tamano
gen double diferencia_tamano = tamano - prueba_tamano
sum diferencia_tamano if diferencia_tamano!=0, d

**No indication of land use (89)
count if prueba_tamano==0

**If the difference is less than 1 hectare -> the farm's size will be accepted
gen double prueba_tamano2 = 0
foreach c of global areas {
	gen double proportion_`c' = tamano_`c' / prueba_tamano
	replace tamano_`c' = proportion_`c'*tamano if abs(diferencia_tamano)<=1
	replace prueba_tamano2 = prueba_tamano2 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano2 tamano
gen double diferencia_tamano2 = tamano - prueba_tamano2
sum diferencia_tamano2 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
replace diferencia_tamano2=0 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
sum diferencia_tamano2 if diferencia_tamano2!=0, d

**Total area = Ratio * (10 × e#)
//Some have the same area but calculate it by multiplying the sum of the fractions by multiples of 10. In this case, the calculation will be based on the size of the property if it is less than ${areamax}, and then on the ratio if it is less than ${areamax}.
foreach f of global fractions {
	gen fraction`f' = (((prueba_tamano2*`f')-0.01)<tamano & ((prueba_tamano2*`f')+0.01)>tamano)
}
tab1 fraction1*
***Prioritizing size
gen double prueba_tamano3 = 0
foreach c of global areas {
	foreach f of global fractions {
		replace tamano_`c'=proportion_`c'*tamano if tamano<=${areamax} & tamano>${areamin} & diferencia_tamano2!=0 & fraction`f'==1
	}
	replace prueba_tamano3 = prueba_tamano3 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano3 tamano
gen double diferencia_tamano3 = tamano - prueba_tamano3
sum diferencia_tamano3 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d
***Prioritizing proportions
foreach f of global fractions {
	replace tamano = prueba_tamano3 if prueba_tamano3<=${areamax} &  prueba_tamano3>${areamin} & diferencia_tamano3!=0 & fraction`f'==1
}
compare prueba_tamano3 tamano
drop diferencia_tamano3
gen double diferencia_tamano3 = tamano - prueba_tamano3
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d

**Total farm area less than or equal to ${areamax} ha
//The proportions will be recalculated based on the farm's size if the farm is less than or equal to ${areamax} ha.
sum tamano diferencia_tamano3 if diferencia_tamano3!=0
sum tamano diferencia_tamano3 if diferencia_tamano3!=0 & tamano<=${areamax}
gen double prueba_tamano4 = 0
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano3!=0 & tamano<=${areamax} & tamano>${areamin}
	replace prueba_tamano4 = prueba_tamano4 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano4 tamano
gen double diferencia_tamano4 = tamano - prueba_tamano4
sum diferencia_tamano4 if diferencia_tamano4!=0 & tamano<=${areamax} & tamano>${areamin}
replace diferencia_tamano4=0 if diferencia_tamano4!=0 & tamano<=${areamax}

**Sum of areas less than or equal to ${areamax} ha
//The size will be recalculated based on the areas, provided that their sum is less than or equal to ${areamax} ha.
sum diferencia_tamano4 if diferencia_tamano4!=0
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0 & prueba_tamano4<=${areamax} & prueba_tamano4>${areamin}
replace tamano = prueba_tamano4 if prueba_tamano4<=${areamax} & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
compare prueba_tamano4 tamano
drop diferencia_tamano4
gen double diferencia_tamano4 = tamano - prueba_tamano4
replace diferencia_tamano4=0 if abs(diferencia_tamano4)<1 & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0, d

**Replacing with the smaller of the total area and the sum of the proportions
//For properties with inconsistencies between the total area and the sum of the proportions, the smaller value will be retained. The property size will be prioritized first; if it is an outlier, the proportions will be prioritized instead. Note that this approach still generates outliers, but they are the "least dramatic" ones.
*** Size less than the sum of the proportions
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano4!=0 & tamano<prueba_tamano4 & tamano>${areamin}
}
*** The sum of the fractions is less than the whole
replace tamano = prueba_tamano4 if diferencia_tamano4!=0 & tamano>prueba_tamano4 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0,d

**Evaluating
sum tamano, d

*Water sources
replace fuentes_agua_ext = 1 if fuentes_agua_ext == 2
gen     fuentes_agua = 1 if (fuentes_agua_ext==1 | fuentes_agua_pro==1)
replace fuentes_agua = 0 if (fuentes_agua_ninguna==3 & fuentes_agua!=1)
tab fuentes_agua, m

*Collapse by farm
collapse (sum) tamano tamano_* (max) fuentes_agua*, by(consecutivo)

*Organizing the database
foreach v of varlist $ind_tierras10 {
	rename `v' `v'_10
}
duplicates report consecutivo
save "${data}\Rtierras2010.dta", replace



* ------- *
**## 2013
* ------- *
*Calling database
use "${ELCA}\2013\Rural\RTierras.dta", clear

*Plots
bysort consecutivo: egen totpred_fincas   =max(orden_predio2013)
bysort consecutivo: egen totpred_dadas    =max(orden_dadas2013)
bysort consecutivo: egen totpred_vendidas =max(orden_vendidas2013)
bysort consecutivo: egen totpred_perdidas =max(orden_perdidas)
replace totpred_dadas=0 if totpred_dadas==.
replace totpred_vendidas=0 if totpred_vendidas==.
replace totpred_perdidas=0 if totpred_perdidas==.
gen dadasPerdidasVendidas = totpred_dadas + totpred_vendidas + totpred_perdidas
keep if orden_predio2013!=.

*Owner and partnership
replace propietario = 0 if propietario==2
replace propietario = 0 if propietario==. /*Farms that no longer have plots*/
replace asocio      = 0 if asocio     ==2

* Organizing sizes
recast  double tamano
replace tamano =   0.64*tamano if unidad_medida==2 /*Fanegadas o cuadras*/
replace tamano = 0.0001*tamano if unidad_medida==3 /*Metro cuadrado*/
gen     double  prueba_tamano  = 0
foreach v of global areas {
	replace `v' = 0                 if `v'==.
	gen     double tamano_`v' = `v'
	replace tamano_`v' =   0.64*`v' if `v'_um==2 /*Fanegadas o cuadras*/
	replace tamano_`v' = 0.0001*`v' if `v'_um==3 /*Metro cuadrado*/
	replace prueba_tamano = prueba_tamano + tamano_`v' if tamano_`v'!=.
}
compare prueba_tamano tamano
gen double diferencia_tamano = tamano - prueba_tamano
sum diferencia_tamano if diferencia_tamano!=0, d

**No indication of land use (110)
count if prueba_tamano==0

**If the difference is less than 1 hectare -> the farm's size will be accepted
gen double prueba_tamano2 = 0
foreach c of global areas {
	gen double proportion_`c' = tamano_`c' / prueba_tamano
	replace tamano_`c' = proportion_`c'*tamano if abs(diferencia_tamano)<=1
	replace prueba_tamano2 = prueba_tamano2 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano2 tamano
gen double diferencia_tamano2 = tamano - prueba_tamano2
sum diferencia_tamano2 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
replace diferencia_tamano2=0 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
sum diferencia_tamano2 if diferencia_tamano2!=0, d

**Total area = Ratio * (10 × e#)
//Some have the same area but calculate it by multiplying the sum of the fractions by multiples of 10. In this case, the system will use the plot size if it is less than ${areamax}, and then the ratio if it is less than ${areamax}.
foreach f of global fractions {
	gen fraction`f' = (((prueba_tamano2*`f')-0.01)<tamano & ((prueba_tamano2*`f')+0.01)>tamano)
}
tab1 fraction1*
***Prioritizing size
gen double prueba_tamano3 = 0
foreach c of global areas {
	foreach f of global fractions {
		replace tamano_`c'=proportion_`c'*tamano if tamano<=${areamax} & tamano>${areamin} & diferencia_tamano2!=0 & fraction`f'==1
	}
	replace prueba_tamano3 = prueba_tamano3 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano3 tamano
gen double diferencia_tamano3 = tamano - prueba_tamano3
sum diferencia_tamano3 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d
***Prioritizing proportions
foreach f of global fractions {
	replace tamano = prueba_tamano3 if prueba_tamano3<=${areamax} &  prueba_tamano3>${areamin} & diferencia_tamano3!=0 & fraction`f'==1
}
compare prueba_tamano3 tamano
drop diferencia_tamano3
gen double diferencia_tamano3 = tamano - prueba_tamano3
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d

**Total farm area less than or equal to ${areamax} ha
//The proportions will be recalculated based on the farm's size if the farm is less than or equal to ${areamax} ha.
sum tamano diferencia_tamano3 if diferencia_tamano3!=0
sum tamano diferencia_tamano3 if diferencia_tamano3!=0 & tamano<=${areamax}
gen double prueba_tamano4 = 0
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano3!=0 & tamano<=${areamax} & tamano>${areamin}
	replace prueba_tamano4 = prueba_tamano4 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano4 tamano
gen double diferencia_tamano4 = tamano - prueba_tamano4
sum diferencia_tamano4 if diferencia_tamano4!=0 & tamano<=${areamax} & tamano>${areamin}
replace diferencia_tamano4=0 if diferencia_tamano4!=0 & tamano<=${areamax}

**Sum of areas less than or equal to ${areamax} ha
//The size will be recalculated based on the areas, provided that their sum is less than or equal to ${areamax} ha.
sum diferencia_tamano4 if diferencia_tamano4!=0
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0 & prueba_tamano4<=${areamax} & prueba_tamano4>${areamin}
replace tamano = prueba_tamano4 if prueba_tamano4<=${areamax} & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
compare prueba_tamano4 tamano
drop diferencia_tamano4
gen double diferencia_tamano4 = tamano - prueba_tamano4
replace diferencia_tamano4=0 if abs(diferencia_tamano4)<1 & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0, d

**Replacing with the smaller of the total area and the sum of the proportions
//For properties with inconsistencies between the total area and the sum of the proportions, the smaller value will be retained. The property size will be prioritized first; if it is an outlier, the proportions will be prioritized instead. Note that this approach still generates outliers, but they are the "least dramatic" ones.
*** Size less than the sum of the proportions
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano4!=0 & tamano<prueba_tamano4 & tamano>${areamin}
}
*** The sum of the fractions is less than the whole
replace tamano = prueba_tamano4 if diferencia_tamano4!=0 & tamano>prueba_tamano4 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0,d

**Evaluating
sum tamano, d

*Tenancy
forvalues i=1/12 {
	gen tipoTenencia_`i' = 0
	replace tipoTenencia_`i'=`i' if tipo_tenencia==`i'
}
**Replacing properties not owned by the estate with "missing"
foreach v of varlist propietario asocio tipoTenencia_* tamano_* {
	replace `v'=. if orden_predio2013==.
}

*Water sources
replace fuentes_agua_pro = 0 if fuentes_agua_pro == 2
replace fuentes_agua_ext = 0 if fuentes_agua_ext == 2

*Dummy Investments
local i = 1
foreach v of varlist inv_riego inv_estructuras inv_conservacion inv_frutales inv_maderables inv_otros_ciales inv_vivienda inv_desnat inv_otra {
	gen     invd_`i' = 0
	replace invd_`i' = `i' if `v'==1
	local i = `i'+1
}

*Investments made
sum vr_inver*, d
gen double vr_inverHecha=0
gen double vr_inverResil1=0
gen double vr_inverResil2=0
forvalues i=1/3 {
	replace vr_inverHecha  = vr_inverHecha  + vr_inver_`i' if (vr_inver_`i'!=.)
	replace vr_inverResil1 = vr_inverResil1 + vr_inver_`i' if (inversion_`i'==1 | inversion_`i'==2)
	replace vr_inverResil2 = vr_inverResil2 + vr_inver_`i' if (inversion_`i'==1 | inversion_`i'==2 | inversion_`i'==4 | inversion_`i'==5 | inversion_`i'==6)
}
sum vr_inverHecha vr_inverResil*
compare vr_inverHecha vr_inverResil2

*Collapse by farm
collapse (mean) asocio propietario totpred_fincas dadasPerdidasVendidas fuentes_agua* /*
*/  (sum) tamano* vr_inverHecha vr_inverResil* (max) tipoTenencia_* invd_*, by(llave)

*Partner and owner
replace asocio      = 1 if asocio>0 & asocio!=.
replace propietario = 1 if propietario>0 & propietario!=.

** Generating outlier dummies and removing auxiliary variables
egen double auxdiferente_tamano = rowtotal(tamano_*)
gen diferente_tamano_13=(auxdiferente_tamano!=tamano)
tab diferente_tamano_13
sum tamano if diferente_tamano_13==1
drop auxdiferente_tamano

*Tenancy
tab1 tipoTenencia_*
egen pruebatipoTenencia = rowtotal(tipoTenencia_*)
tab pruebatipoTenencia, m
replace tipoTenencia_1 = 1 if tipoTenencia_2!=0  & tipoTenencia_2!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_3!=0  & tipoTenencia_3!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_4!=0  & tipoTenencia_4!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_5!=0  & tipoTenencia_5!=. //Posesion o herencia
replace tipoTenencia_6 = 1 if tipoTenencia_6==6 					 //Arriendo
replace tipoTenencia_7 = 1 if tipoTenencia_7==7 					 //Aparceria a otros
replace tipoTenencia_7 = 1 if tipoTenencia_9!=0  & tipoTenencia_1!=. //Empeno a otros
replace tipoTenencia_7 = 1 if tipoTenencia_10!=0 & tipoTenencia_1!=. //Anticresis a otros
replace tipoTenencia_7 = 1 if tipoTenencia_11!=0 & tipoTenencia_1!=. //Comodato a otros
replace tipoTenencia_7 = 1 if tipoTenencia_12!=0 & tipoTenencia_1!=. //Compania a otros
replace tipoTenencia_8 = 1 if tipoTenencia_8==8 					 //Usufructo
drop pruebatipoTenencia tipoTenencia_2 tipoTenencia_3 tipoTenencia_4 tipoTenencia_5 tipoTenencia_9 tipoTenencia_10 tipoTenencia_11 tipoTenencia_12
tab1 tipoTenencia_*

*Investments
tab1 invd_*
egen pruebainvd = rowtotal(invd_*)
tab pruebainvd, m
replace invd_3 = 1 if invd_4==1 //Frutales a conservacion
replace invd_3 = 1 if invd_5==1 //Maderables a conservacion
replace invd_3 = 1 if invd_6==1 //Otros_ciales a conservacion
drop    pruebainvd invd_4 invd_5 invd_6
tab1 invd_*

*Size classification
gen     class_tamano = 0 if (tamano<=3)
replace class_tamano = 1 if (tamano>3  & tamano<=10)
replace class_tamano = 2 if (tamano>10 & tamano<=20)
replace class_tamano = 3 if (tamano>20 & tamano<=200)
replace class_tamano = 4 if (tamano>200)

*Water sources
gen     fuentes_agua = (fuentes_agua_ext==1 | fuentes_agua_pro==1)
replace fuentes_agua = . if(fuentes_agua_ext==. & fuentes_agua_pro==.)
tab fuentes_agua, m

*Investment made in 2018 pesos
gen year=2013
merge m:1 year using "${data}\IPC_anual.dta", keep (1 3) nogen
replace vr_inverHecha  = vr_inverHecha*IPC
replace vr_inverResil1 = vr_inverResil1*IPC
replace vr_inverResil2 = vr_inverResil2*IPC
drop year IPC

*Saving database
foreach v of varlist $ind_tierras13 {
	gen `v'_13 = `v'
}
rename llave llave_13
duplicates report llave_13
save "${data}\Rtierras2013.dta", replace


* ------- *
**## 2016
* ------- *
use "${ELCA}\2016\Rural\RTierras.dta", clear
drop llave hogar
rename (llave_n16 hogar_n16) (llave hogar)
*Plots
bysort consecutivo: egen totpred_fincas   =max(orden_predio2016)
bysort consecutivo: egen totpred_dadas    =max(orden_dadas2016)
bysort consecutivo: egen totpred_vendidas =max(orden_vendidas2016)
bysort consecutivo: egen totpred_perdidas =max(orden_perdidas)
replace totpred_dadas=0 if totpred_dadas==.
replace totpred_vendidas=0 if totpred_vendidas==.
replace totpred_perdidas=0 if totpred_perdidas==.
gen dadasPerdidasVendidas = totpred_dadas + totpred_vendidas + totpred_perdidas
keep if orden_predio2016!=.

*Owner and partnership
replace propietario = 0 if propietario==2
replace propietario = 0 if propietario==. /*Farms that no longer have plots*/
replace asocio      = 0 if asocio     ==2

* Organizing sizes
recast  double tamano
replace tamano =   0.64*tamano if unidad_medida==2 /*Fanegadas or blocks*/
replace tamano = 0.0001*tamano if unidad_medida==3 /*Square meter*/
gen     double  prueba_tamano  = 0
foreach v of global areas {
    replace `v' = 0                 if `v'==.
    gen     double tamano_`v' = `v'
    replace tamano_`v' =   0.64*`v' if `v'_um==2 /*Fanegadas or blocks*/
    replace tamano_`v' = 0.0001*`v' if `v'_um==3 /*Square meter*/
    replace prueba_tamano = prueba_tamano + tamano_`v' if tamano_`v'!=.
}
compare prueba_tamano tamano
gen double diferencia_tamano = tamano - prueba_tamano
sum diferencia_tamano if diferencia_tamano!=0, d

**They do not indicate use of the areas (110)
count if prueba_tamano==0

**Difference less than 1 Ha -> the farm size will be trusted
gen double prueba_tamano2 = 0
foreach c of global areas {
    gen double proportion_`c' = tamano_`c' / prueba_tamano
    replace tamano_`c' = proportion_`c'*tamano if abs(diferencia_tamano)<=1
    replace prueba_tamano2 = prueba_tamano2 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano2 tamano
gen double diferencia_tamano2 = tamano - prueba_tamano2
sum diferencia_tamano2 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
replace diferencia_tamano2=0 if abs(diferencia_tamano2)<1 & diferencia_tamano2!=0
sum diferencia_tamano2 if diferencia_tamano2!=0, d

**Total area = Proportion*(10xe#)
//Some have the same area but multiply the sum of fractions by multiples of 10. In this case the farm size will be trusted if it is less than ${areamax}, and then the proportion if it is less than ${areamax}.
foreach f of global fractions {
    gen fraction`f' = (((prueba_tamano2*`f')-0.01)<tamano & ((prueba_tamano2*`f')+0.01)>tamano)
}
tab1 fraction1*
***Prioritizing size
gen double prueba_tamano3 = 0
foreach c of global areas {
    foreach f of global fractions {
        replace tamano_`c'=proportion_`c'*tamano if tamano<=${areamax} & tamano>${areamin} & diferencia_tamano2!=0 & fraction`f'==1
    }
    replace prueba_tamano3 = prueba_tamano3 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano3 tamano
gen double diferencia_tamano3 = tamano - prueba_tamano3
sum diferencia_tamano3 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d
***Prioritizing proportions
foreach f of global fractions {
    replace tamano = prueba_tamano3 if prueba_tamano3<=${areamax} &  prueba_tamano3>${areamin} & diferencia_tamano3!=0 & fraction`f'==1
}
compare prueba_tamano3 tamano
drop diferencia_tamano3
gen double diferencia_tamano3 = tamano - prueba_tamano3
replace diferencia_tamano3=0 if abs(diferencia_tamano3)<1 & diferencia_tamano3!=0
sum diferencia_tamano3 if diferencia_tamano3!=0, d

**Total farm area less than or equal to ${areamax} ha
//The proportions will be recalculated based on the farm's size if the farm is less than or equal to ${areamax} ha.
sum tamano diferencia_tamano3 if diferencia_tamano3!=0
sum tamano diferencia_tamano3 if diferencia_tamano3!=0 & tamano<=${areamax}
gen double prueba_tamano4 = 0
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano3!=0 & tamano<=${areamax} & tamano>${areamin}
	replace prueba_tamano4 = prueba_tamano4 + tamano_`c' if tamano_`c'!=.
}
compare prueba_tamano4 tamano
gen double diferencia_tamano4 = tamano - prueba_tamano4
sum diferencia_tamano4 if diferencia_tamano4!=0 & tamano<=${areamax} & tamano>${areamin}
replace diferencia_tamano4=0 if diferencia_tamano4!=0 & tamano<=${areamax}

**Sum of areas less than or equal to ${areamax} ha
//The size will be recalculated based on the areas, provided that their sum is less than or equal to ${areamax} ha.
sum diferencia_tamano4 if diferencia_tamano4!=0
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0 & prueba_tamano4<=${areamax} & prueba_tamano4>${areamin}
replace tamano = prueba_tamano4 if prueba_tamano4<=${areamax} & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
compare prueba_tamano4 tamano
drop diferencia_tamano4
gen double diferencia_tamano4 = tamano - prueba_tamano4
replace diferencia_tamano4=0 if abs(diferencia_tamano4)<1 & diferencia_tamano4!=0 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0, d

**Replacing with the smaller of the total area and the sum of the proportions
//For properties with inconsistencies between the total area and the sum of the proportions, the smaller value will be retained. The property size will be prioritized first; if it is an outlier, the proportions will be prioritized instead. Note that this approach still generates outliers, but they are the "least dramatic" ones.
*** Size less than the sum of the proportions
foreach c of global areas {
	replace tamano_`c' = proportion_`c'*tamano if diferencia_tamano4!=0 & tamano<prueba_tamano4 & tamano>${areamin}
}
*** The sum of the fractions is less than the whole
replace tamano = prueba_tamano4 if diferencia_tamano4!=0 & tamano>prueba_tamano4 & prueba_tamano4>${areamin}
sum tamano prueba_tamano4 diferencia_tamano4 if diferencia_tamano4!=0,d

**Evaluating
sum tamano, d

*Tenancy
forvalues i=1/12 {
	gen tipoTenencia_`i' = 0
	replace tipoTenencia_`i'=`i' if tipo_tenencia==`i'
}

*Water sources
replace fuentes_agua_pro = 0 if fuentes_agua_pro == 2
replace fuentes_agua_ext = 0 if fuentes_agua_ext == 2

*Dummy Investments
local i = 1
foreach v of varlist inv_riego inv_estructuras inv_conservacion inv_frutales inv_maderables inv_otros_ciales inv_vivienda inv_desnat inv_otra {
	gen     invd_`i' = 0
	replace invd_`i' = `i' if `v'==1
	local i = `i'+1
}

*Investments made
sum vr_inver*
gen double vr_inverHecha=0
gen double vr_inverResil1=0
gen double vr_inverResil2=0
forvalues i=1/3 {
	replace vr_inverHecha  = vr_inverHecha  + vr_inver_`i' if (vr_inver_`i'!=.)
	replace vr_inverResil1 = vr_inverResil1 + vr_inver_`i' if (inversion_`i'==1 | inversion_`i'==2)
	replace vr_inverResil2 = vr_inverResil2 + vr_inver_`i' if (inversion_`i'==1 | inversion_`i'==2 | inversion_`i'==4 | inversion_`i'==5 | inversion_`i'==6 | inversion_`i'==7)
}
sum vr_inverHecha vr_inverResil*
compare vr_inverHecha vr_inverResil2

*Collapse by farm
collapse (mean) asocio propietario totpred_fincas dadasPerdidasVendidas fuentes_agua* /*
*/  (sum) tamano* vr_inverHecha vr_inverResil* (max) tipoTenencia_* invd_*, by(llave)

*Partner and owner
replace asocio      = 1 if asocio>0 & asocio!=.
replace propietario = 1 if propietario>0 & propietario!=.

** Generating outlier dummies and removing auxiliary variables
egen double auxdiferente_tamano = rowtotal(tamano_*)
gen diferente_tamano_16=(auxdiferente_tamano!=tamano)
tab diferente_tamano_16
sum tamano if diferente_tamano_16==1
drop auxdiferente_tamano

*Tenancy
tab1 tipoTenencia_*
egen pruebatipoTenencia = rowtotal(tipoTenencia_*)
tab pruebatipoTenencia, m
replace tipoTenencia_1 = 1 if tipoTenencia_2!=0  & tipoTenencia_2!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_3!=0  & tipoTenencia_3!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_4!=0  & tipoTenencia_4!=. //Posesion o herencia
replace tipoTenencia_1 = 1 if tipoTenencia_5!=0  & tipoTenencia_5!=. //Posesion o herencia
replace tipoTenencia_6 = 1 if tipoTenencia_6==6 					 //Arriendo
replace tipoTenencia_7 = 1 if tipoTenencia_7==7 					 //Aparceria a otros
replace tipoTenencia_7 = 1 if tipoTenencia_9!=0  & tipoTenencia_1!=. //Empeno a otros
replace tipoTenencia_7 = 1 if tipoTenencia_10!=0 & tipoTenencia_1!=. //Anticresis a otros
replace tipoTenencia_7 = 1 if tipoTenencia_11!=0 & tipoTenencia_1!=. //Comodato a otros
replace tipoTenencia_7 = 1 if tipoTenencia_12!=0 & tipoTenencia_1!=. //Compania a otros
replace tipoTenencia_8 = 1 if tipoTenencia_8==8 					 //Usufructo
drop pruebatipoTenencia tipoTenencia_2 tipoTenencia_3 tipoTenencia_4 tipoTenencia_5 tipoTenencia_9 tipoTenencia_10 tipoTenencia_11 tipoTenencia_12
tab1 tipoTenencia_*

* Investments
tab1 invd_*
egen pruebainvd = rowtotal(invd_*)
tab pruebainvd, m
replace invd_3 = 1 if invd_4==1 //Frutales a conservacion
replace invd_3 = 1 if invd_5==1 //Maderables a conservacion
replace invd_3 = 1 if invd_6==1 //Otros_ciales a conservacion
drop    pruebainvd invd_4 invd_5 invd_6
tab1 invd_*

* Water sources
gen     fuentes_agua = (fuentes_agua_ext==1 | fuentes_agua_pro==1)
replace fuentes_agua = . if(fuentes_agua_ext==. & fuentes_agua_pro==.)
tab fuentes_agua, m

* Investment made in 2018 pesos
gen year=2016
merge m:1 year using "${data}\IPC_anual.dta", keep (1 3) nogen
replace vr_inverHecha  = vr_inverHecha*IPC
replace vr_inverResil1 = vr_inverResil1*IPC
replace vr_inverResil2 = vr_inverResil2*IPC
drop year IPC

* Organizing the database
foreach v of varlist llave $ind_tierras16 $imp_tierras16 {
	rename `v' `v'_16
}
keep *_16
duplicates report llave_16
save "${data}\Rtierras2016.dta", replace




			* ======================== *
**#				   5. PRODUCTION
			* ======================== *

**## Selecting variables
global ind_produccion10 "ing_* gastprom_*"
global ind_produccion13 "ing_* gastprom_*"
global ind_produccion16 "ing_* gastprom_*"
global cov_produccion13 "ing_* gastprom_* sitioVenta_*"


* ------- *
**## 2010
* ------- *
use "${ELCA}\2010\Rural\RProduccion.dta", clear

* Production data *
** Sales variables
gen produc_agri  = (t_prod_agri>0)
gen produc_pecu  = (t_prod_pec>0)
gen produc_agrop = (produc_agri==1 | produc_pecu==1)
gen venta_produc = (orden_agrop!=. & produc_agri==1 & vr_ingtotagr>0) | (orden_pecu!=. & produc_pecu==1 & vr_ingtotpec>0)
gen venta_agri   = (orden_agrop!=. & produc_agri==1 & vr_ingtotagr!=. & vr_ingtotagr>0)
gen venta_pecu   = (orden_pecu!=.  & produc_pecu==1 & vr_ingtotpec!=. & vr_ingtotpec>0)
gen venta_agrop  = (venta_agri==1 | venta_pecu==1)

** Organizing values
label drop vr_transp
foreach v of varlist vr_* {
	replace `v'=. if (`v'==98 | `v'==99)
	recast double `v'
}

** Adjusting for inflation
gen   year = 2010
merge m:1  year using "${data}\IPC_anual.dta", keep (1 3) nogen
foreach v of varlist vr_transp vr_ingtotagr vr_insinsecticidas vr_insotros vr_ingtotpec vr_gastopec {
	replace `v' = `v'*IPC
}

** Agricultural (crop) income
/* ing_agri = number of crops harvested in the last 12 months * the amount received from the last sale.
Notes: 
+ Assumption: all crops harvested in the last 12 months were sold
+ Assumption: all crops harvested in the last 12 months were sold for the same value
+ The harvest frequency variable is not used because there may be multiple plantations.
*/
replace n_cosechas=1 if n_cosechas==98 | n_cosechas==99
gen double ing_agri   = n_cosechas*vr_ingtotagr if (orden_agrop!=. & orden_agrop>0 & n_cosechas!=. & n_cosechas>0  & vr_ingtotagr!=. & vr_ingtotagr>0 & venta_agri==1)
tab n_cosechas venta_produc if orden_agrop!=., m
sum vr_ingtotagr if venta_agri==1, d
sum ing_agri, d

** Livestock income
/* ing_pecu = the value received over the past 12 months
Notes: 
+ Assumption: sales figures were recorded for every period over the past 12 months
+ Assumption: the same sales figure was recorded for every period over the past 12 months
*/
gen double ing_pecu   = vr_ingtotpec if (orden_pecu!=. & orden_pecu>0 & vr_ingtotpec!=. & vr_ingtotpec>0 & venta_pecu==1)
sum vr_ingtotpec if venta_pecu==1, d
sum ing_pecu

** Annualized expenses
capture drop gast*
gen double gastprom_agri = 0 if (orden_agrop!=.)
gen double gastprom_pecu = 0 if (orden_pecu !=.)
replace gastprom_pecu = vr_gastopec if (orden_pecu!=. & vr_gastopec!=.)
foreach v of varlist vr_transp vr_insinsecticidas vr_insotros {
	gen     gast`v' = n_cosechas*`v'                if (orden_agrop!=. & n_cosechas!=. & `v'!=.)
	replace gastprom_agri = gastprom_agri + gast`v' if (orden_agrop!=. & gast`v'!=.)
	
	gen     aux`v'  =            `v' if (orden_pecu!=.  & `v'!=.)
	replace gastprom_pecu = gastprom_pecu + aux`v'  if (orden_pecu!=.  & gast`v'!=.)
}
sum ing_* gastprom_*

* Base per UPA *
collapse (sum) ing_* gastprom* , by (consecutivo)
gen double ing_agrop = ing_agri + ing_pecu
gen double gastprom_agrop = gastprom_agri + gastprom_pecu
foreach v of varlist ing_* gast* {
	rename `v' `v'_10
}

** Evaluating
sum ing_* gastprom_*
compare ing_agri gastprom_agri
compare ing_pecu gastprom_pecu
compare ing_agrop gastprom_agrop

*Farm classification
**Percentaje
gen p_agri_10 =  ing_agri_10 / ing_agrop_10
gen p_pecu_10 =  ing_pecu_10 / ing_agrop_10
**Clasification indicator
gen     type_10 = 1 if p_agri_10>${class} & p_agri_10!=.
replace type_10 = 2 if p_pecu_10>${class} & p_pecu_10!=.
replace type_10 = 3 if (p_agri_10<=${class} & p_agri_10!=. & p_pecu_10<=${class} & p_pecu_10!=.)
drop p_agri_1* p_pecu_1*

*Saving data
duplicates report consecutivo
save "${data}\Rproduccion2010.dta", replace


* ------- *
**## 2013
* ------- *
*Calling database
use "${ELCA}\2013\Rural\RProduccion.dta", clear

*Adjusting by inflation
gen year=2013
merge m:1 year using "${data}\IPC_anual.dta", keep (1 3) nogen
foreach v of varlist vr_recibe_venta prom_* gast_* {
	replace `v' = `v'*IPC
}

*Generating variables
label drop m7_410
** Production variables
gen     produc_agrop = (produc_agri==1 | produc_pecu==1)
replace produc_agri  = 0 if produc_agri==2 
replace produc_pecu  = 0 if produc_pecu==2 
** Sales variables
replace venta_produc = 0 if venta_produc==2 
gen     venta_agri   = (orden_agrop!=. & orden_agrop>0 & vr_recibe_venta!=. & vr_recibe_venta>0)
gen     venta_pecu   = (orden_pecu!=.  & orden_pecu>0  & vr_recibe_venta!=. & vr_recibe_venta>0)
gen     venta_agrop  = (venta_agri==1 | venta_pecu==1)
** Agricultural (crop) Income
/* ing_agri = number of harvests collected in the last 12 months * the amount received from the last sale.
Notes: 
+ Assumption: all harvests collected in the last 12 months were sold
+ Assumption: all harvests collected in the last 12 months were sold for the same amount
+ ELCA_Manual_Recolección2013 page 115: "Some products are not harvested in a single crop but are picked as they bear fruit [...]; for these products, the option `not harvested in crops' must be selected, and the following questions will apply to the `last month' rather than the `last harvest'." This is reflected in the replacement.
+ The harvest harvesting frequency variable is not used because there may be multiple plantations.
*/
gen double ing_agri = n_cosechas*vr_recibe_venta if (orden_agrop!=. & orden_agrop>0 & n_cosechas!=. & n_cosechas>0  & vr_recibe_venta!=. & vr_recibe_venta>0)
replace ing_agri    =            vr_recibe_venta if (orden_agrop!=. & orden_agrop>0 & n_cosechas==0                 & vr_recibe_venta!=. & vr_recibe_venta>0)
** Livestock income
/* ing_pecu = frequency with which proceeds from sales are received * the amount received from the last sale.
Notes: 
+ Assumption: the sale amount was recorded for all periods over the last 12 months
+ Assumption: the same sale amount was recorded for all periods over the last 12 months
*/
gen double ing_pecu =   1*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==1) /*anual*/
replace ing_pecu =   2*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==2) /*semestral*/
replace ing_pecu =   4*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==3) /*trimestral*/
replace ing_pecu =   6*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==4) /*bimensual*/
replace ing_pecu =  12*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==5) /*mensual*/
replace ing_pecu =  52*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==6) /*semanal*/
replace ing_pecu = 365*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. & vr_recibe_venta>0 & period_venta==7) /*diaria*/
**Annualized expenses
global gastos "semilla maqui fertz insec cria alim vacu drog vitam asitec manobra transp otrosg"
gen double gastprom_agri = 0
gen double gastprom_pecu = 0
foreach v of global gastos {
	gen double gastprom_`v' = 0
	replace    gastprom_`v' = 52*gast_`v' if gast_`v'!=. & perio_`v'==1 /*semana*/
	replace    gastprom_`v' = 12*gast_`v' if gast_`v'!=. & perio_`v'==2 /*mes*/
	replace    gastprom_`v' =  2*gast_`v' if gast_`v'!=. & perio_`v'==3 /*semestre*/
	replace    gastprom_`v' =  1*gast_`v' if gast_`v'!=. & perio_`v'==4 /*año*/
	replace    gastprom_`v' = 0           if gast_`v'!=. & perio_`v'==5 /*no gastó*/
	replace gastprom_agri = gastprom_agri + gastprom_`v' if orden_agrop==0 & gast_`v'!=.
	replace gastprom_pecu = gastprom_pecu + gastprom_`v' if orden_pecu ==0 & gast_`v'!=.
}
sum gastprom_*

*Retail location
forvalues i=1/6 {
	gen     sitioVenta_`i' = 0
	replace sitioVenta_`i' = 1 if sitio_venta==`i'
}

*Database by UPA
collapse (sum) ing_* gastprom_* (max) sitioVenta_*, by (consecutivo llave hogar)

*Database by UPA
gen double ing_agrop      = ing_agri + ing_pecu
gen double gastprom_agrop = gastprom_agri + gastprom_pecu
foreach v of varlist $ind_produccion13 {
	gen `v'_13 = `v'
}

** Evaluating
sum ing_agri gastprom_agri ing_pecu gastprom_pecu ing_agrop gastprom_agrop
compare ing_agri gastprom_agri
compare ing_pecu gastprom_pecu
compare ing_agrop gastprom_agrop

*Farm classification
**Percentaje
gen     p_agri_13 =  ing_agri_13 / ing_agrop_13
replace p_agri_13 = 0 if ing_agrop_13==0
gen     p_pecu_13 =  ing_pecu_13 / ing_agrop_13
replace p_pecu_13 = 0 if ing_agrop_13==0
**Clasification indicator
gen     type_13 = 1 if p_agri_13>${class} & p_agri_13!=.
replace type_13 = 2 if p_pecu_13>${class} & p_pecu_13!=.
replace type_13 = 3 if (p_agri_13<=${class} & p_agri_13!=. & p_pecu_13<=${class} & p_pecu_13!=.)
replace type_13 = 0 if type_13==.
**Class in 2013
gen class_10 = type_13
gen class_13 = type_13
gen class_16 = type_13
drop p_agri_1* p_pecu_1*

*Saving database
rename llave llave_13
duplicates report llave_13
save "${data}\Rproduccion2013.dta", replace


* ------- *
**## 2016
* ------- *
use "${ELCA}\2016\Rural\RProduccion.dta", clear
drop llave 
rename (llave_n16) (llave)

* Production data *
** Sales variables
gen     produc_agrop = (produc_agri==1 | produc_pecu==1)
replace produc_agri  = 0 if produc_agri==2 
replace produc_pecu  = 0 if produc_pecu==2 
replace venta_produc = 0 if venta_produc==2 
gen     venta_agri   = (orden_agrop!=. & orden_agrop>0 & vr_recibe_venta!=. & vr_recibe_venta>0)
gen     venta_pecu   = (orden_pecu!=.  & orden_pecu>0  & vr_recibe_venta!=. & vr_recibe_venta>0)
gen     venta_agrop   = (venta_agri==1 | venta_pecu==1)

** Adjusting by inflation
gen   year = 2016
merge m:1  year using "${data}\IPC_anual.dta", keep (1 3) nogen
foreach v of varlist vr_recibe_venta gast_* prom_* {
	replace `v' = `v'*IPC
}

** Agricultural (crop) Income
/* ing_agri = number of harvests collected in the last 12 months * the amount received from the last sale.
Notes: 
+ Assumption: all harvests collected in the last 12 months were sold
+ Assumption: all harvests collected in the last 12 months were sold for the same amount
+ ELCA_Manual_Recolección2013 page 115: "Some products are not harvested in a single crop but are picked as they bear fruit [...]; for these products, the option `not harvested in crops' must be selected, and the following questions will apply to the `last month' rather than the `last harvest'." This is reflected in the replacement.
+ The harvest harvesting frequency variable is not used because there may be multiple plantations.
*/
gen double ing_agri   = n_cosechas*vr_recibe_venta if (orden_agrop!=. & orden_agrop>0 & n_cosechas!=. &  n_cosechas>0  & vr_recibe_venta!=. & vr_recibe_venta>0)
replace    ing_agri   =            vr_recibe_venta if (orden_agrop!=. & orden_agrop>0 & n_cosechas==0 & vr_recibe_venta!=. & vr_recibe_venta>0)
tab n_cosechas periodicidad
tab venta_produc n_cosechas if orden_agrop!=., m
sum vr_recibe_venta if orden_agrop!=., d
sum ing_agri

** Livestock income
/* ing_pecu = frequency with which proceeds from sales are received * the amount received from the last sale.
Notes: 
+ Assumption: the sale amount was recorded for all periods over the last 12 months
+ Assumption: the same sale amount was recorded for all periods over the last 12 months
*/
gen double ing_pecu =   1*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==1) /*anual*/
replace ing_pecu =   2*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==2) /*semestral*/
replace ing_pecu =   4*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==3) /*trimestral*/
replace ing_pecu =   6*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==4) /*bimensual*/
replace ing_pecu =  12*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==5) /*mensual*/
replace ing_pecu =  52*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==6) /*semanal*/
replace ing_pecu = 365*vr_recibe_venta if (orden_pecu!=. & orden_pecu>0 & vr_recibe_venta!=. &vr_recibe_venta>0 & period_venta==7) /*diaria*/
tab venta_produc period_venta if orden_pecu!=., m
sum vr_recibe_venta if orden_pecu!=., d
sum ing_pecu

** Annualized expenses
global gastos "semilla maqui fertz insec cria alim vacu drog vitam asitec manobra transp otrosg"
gen double gastprom_agri = 0
gen double gastprom_pecu = 0
foreach v of global gastos {
	gen double gastprom_`v' = 0
	replace    gastprom_`v' = 52*gast_`v' if gast_`v'!=. & perio_`v'==1 /*semana*/
	replace    gastprom_`v' = 12*gast_`v' if gast_`v'!=. & perio_`v'==2 /*mes*/
	replace    gastprom_`v' =  2*gast_`v' if gast_`v'!=. & perio_`v'==3 /*semestre*/
	replace    gastprom_`v' =  1*gast_`v' if gast_`v'!=. & perio_`v'==4 /*año*/
	replace    gastprom_`v' = 0           if gast_`v'!=. & perio_`v'==5 /*no gastó*/
	replace gastprom_agri = gastprom_agri + gastprom_`v' if orden_agrop==0 & gast_`v'!=.
	replace gastprom_pecu = gastprom_pecu + gastprom_`v' if orden_pecu ==0 & gast_`v'!=.
}
sum gastprom_*


*Retail location
forvalues i=1/6 {
	gen     sitioVenta_`i' = 0
	replace sitioVenta_`i' = 1 if sitio_venta==`i'
}

* Base by UPA *
collapse (sum) ing_* gastprom_* (max) sitioVenta_*, by (consecutivo llave hogar)

*Base by UPA
gen double ing_agrop      = ing_agri + ing_pecu
gen double gastprom_agrop = gastprom_agri + gastprom_pecu
foreach v of varlist llave $ind_produccion16 {
	rename `v' `v'_16
}
keep *_16

** Evaluating
sum ing_agri_16 gastprom_agri_16 ing_pecu_16 gastprom_pecu_16 ing_agrop_16 gastprom_agrop_16
compare ing_agri_16 gastprom_agri_16
compare ing_pecu_16 gastprom_pecu_16
compare ing_agrop_16 gastprom_agrop_16

*Farm classification
**Percentaje
gen     p_agri_16 =  ing_agri_16 / ing_agrop_16
replace p_agri_16 = 0 if ing_agrop_16==0
gen     p_pecu_16 =  ing_pecu_16 / ing_agrop_16
replace p_pecu_16 = 0 if ing_agrop_16==0
**Clasification indicator
gen     type_16 = 1 if p_agri_16>${class} & p_agri_16!=.
replace type_16 = 2 if p_pecu_16>${class} & p_pecu_16!=.
replace type_16 = 3 if (p_agri_16<=${class} & p_agri_16!=. & p_pecu_16<=${class} & p_pecu_16!=.)
drop p_agri_1* p_pecu_1*

*Saving data
duplicates report llave_16
save "${data}\Rproduccion2016.dta", replace



			* ======================== *
**#				   6. COMMUNITY
			* ======================== *

**## Selecting variables
global comunidad13 "pp_faltacap acceso alquila_maquinaria seguridad solidaridad"

*Calling database
use "${ELCA}\2013\Rural\Rcomunidades.dta", clear
drop acceso_otro_cual

*Generating variables
**Lack of training
replace pp_faltacap = 0 if pp_faltacap==2
**Access to town center
rename  acceso acceso_aux
gen     acceso = 0 if (acceso_aux == 6) /*trail or dirt road*/
replace acceso = 1 if (acceso_aux == 5 | acceso_aux == 4) /*unpaved*/
replace acceso = 2 if (acceso_aux == 3 | acceso_aux == 2) /*paved*/
**Rents machinery
replace alquila_maquinaria = 0 if alquila_maquinaria==2
**Security
rename seguridad seguridad_aux
gen     seguridad = 0 if (seguridad_aux == 4) /*Very unsafe*/
replace seguridad = 1 if (seguridad_aux == 3) /*Unsafe*/
replace seguridad = 2 if (seguridad_aux == 2) /*Relatively safe*/
replace seguridad = 3 if (seguridad_aux == 1) /*Very safe*/
**Solidarity
rename solidaridad solidaridad_aux
gen     solidaridad = 0 if (solidaridad_aux == 3) /*Do not help each other*/
replace solidaridad = 1 if (solidaridad_aux == 2) /*Help each other a little*/
replace solidaridad = 2 if (solidaridad_aux == 1) /*Help each other a lot*/

*Saving dataset
keep consecutivo_c ${comunidad13} 
save "${data}\Rcomunidad2013.dta", replace





			* ================================== *
**#				         7. TERRIDATA
			* ================================== *

*Organizing data
use "${data}\terriDataProcessed.dta", clear
encode indicador, generate(id)
reshape wide dimension subcategoria indicador valor unidad, i(mpio year dpto namedpto namempio) j(id)

*Selecting indicators
gen aux2 = valor2 if year==2013
bysort mpio: egen valor2m = max(aux2)
rename (valor2m valor13 valor14) (td_pibAgrop td_ocupados td_tasaAfect)
keep if year==2012
replace td_tasaAfect = log(td_tasaAfect +1)
keep mpio namempio td_*
duplicates report mpio
save "${data}\terriDataMerge.dta", replace




			* ============================= *
**#				   8. COMBINING MODULES
			* ============================= *

* ------------------ *
**## Base UPA household heads
* ------------------ * 
/*NOTE: Households with the same head of household are retained for the 2013 and 2016 rounds and are combined with households with valid data*/
use "${data}\BaseUPA_Jefes.dta", clear
merge 1:1 llave_13 using "${data}\BaseTreatmentIds.dta", keep(3) keepusing(llave_13) nogen


* ------------------ *
**## Household
* ------------------ * 
merge m:1 consecutivo using "${data}\Rhogar2010.dta", nogen keep(3) //m:1 -> households that split up
merge 1:1 llave_13 using "${data}\Rhogar2013.dta", nogen keep(3)
merge 1:1 llave_16 using "${data}\Rhogar2016.dta", nogen keep(3)


* ------------------ *
**## Lands
* ------------------ * 
/*NOTE: Households with at least one plot of land are retained (even if they have no production). These households are later removed during the inter-round imputations*/
*merge m:1 consecutivo          using "${data}\Rtierras2010.dta",    nogen keep(1 3)
merge 1:1 llave_13 using "${data}\Rtierras2013.dta", gen(tierras13_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist asocio propietario totpred_fincas dadasPerdidasVendidas fuentes_agua* tamano* vr_inver* tipoTenencia_* invd_* class_tamano {
	replace `v'=0 if `v'==. & tierras13_merge==1
}
merge 1:1 llave_16 using "${data}\Rtierras2016.dta", gen(tierras16_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist asocio_16 propietario_16 totpred_fincas_16 dadasPerdidasVendidas_16 fuentes_agua*16 tamano*16 vr_inver*_16 tipoTenencia*_16 invd*_16 {
	replace `v'=0 if `v'==. & tierras16_merge==1
}


* ------------------ *
**## Production
* ------------------ *
merge 1:1 llave_13 using "${data}\Rproduccion2013.dta", gen(produc13_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist ing_* gastprom_* sitioVenta_* {
	replace `v'=0 if `v'==. & produc13_merge==1
}
foreach v of varlist type_13 class_1* {
	replace `v'=3 if `v'==. & produc13_merge==1
}
merge m:1 consecutivo using "${data}\Rproduccion2010.dta", gen(produc10_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist ing_*10 gastprom_*10 {
	replace `v'=0 if `v'==. & produc10_merge==1
}
replace type_10=3 if type_10==. & produc10_merge==1
merge 1:1 llave_16 using "${data}\Rproduccion2016.dta", gen(produc16_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist ing_*16 gastprom_*16 {
	replace `v'=0 if `v'==. & produc16_merge==1
}
replace type_16=3 if type_16==. & produc16_merge==1


* ---------------------------- *
**## Head of household and spouse
* ---------------------------- *
**Head of household
merge 1:1 llave_13 using "${data}\Rpersonas2013.dta", nogen keep(3)

**Imputing education
***2016
merge 1:1 llave_16 using "${data}\Rpersonas2016.dta", nogen keep(1 3)
replace educacion = educacion_16 if educacion==.
drop educacion_16
***2010
merge m:1 consecutivo using "${data}\Rpersonas2010.dta", nogen keep(1 3)
replace educacion = educacion_10 if educacion==.
drop educacion_10
drop if educacion==. /*Removing observations without education data*/

** Spouse
merge 1:1 llave_13 using "${data}\Rconyuge2013.dta", gen(conyuge_merge) keep(1 3)
gen conyuge = (conyuge_merge==3)
*Missing values are not replaced because they are not being used anyway.


* ------------------ *
**## Assets
* ------------------ *
merge 1:1 llave_13 using "${data}\Ractivos2013.dta", gen(activos13_merge) keep(1 3)
foreach v of varlist n_bueyes* n_vacas* n_cerdos* n_avescorral* n_caballos* n_ovejas* n_colmenas* n_otros_anim* {
	replace `v'=0 if `v'==. & activos13_merge==1
}
*merge m:1 consecutivo using "${data}\Ractivos2010.dta", nogen keep(3)
merge 1:1 llave_16 using "${data}\Ractivos2016.dta", gen(activos16_merge) keep(1 3)
* Correcting missing values based on observations that did not match
foreach v of varlist n_*_16 {
	replace `v'=0 if `v'==. & activos16_merge==1
}
* Fixing "No data"
//This was going to be fixed, but the few observations marked as "No data" do not appear to be part of the selected sample.


* ------------------ *
**## Community
* ------------------ *
merge m:1 consecutivo_c using "${data}\Rcomunidad2013.dta", gen(comunidad13_merge) keep(1 3)


* ------------------ *
**## TerriData
* ------------------ *
merge m:1 mpio using "${data}\terriDataMerge.dta", keep(1 3)



			* ============================= *
**#				 9. ORGANIZING DATABASE
			* ============================= *

* -------------------------------- *
**## Imputing from other rounds
* -------------------------------- *

* Households where size_13==0 and production_13>0
//47 households to which the 2013 land values will be imputed because they have agricultural expenses
sum gastprom_agrop_13 if tamano_13==0 & gastprom_agrop_13!=0
global imp2_tierras16 "asocio propietario totpred_fincas dadasPerdidasVendidas tipoTenencia_1 tipoTenencia_6 tipoTenencia_7 tipoTenencia_8" //First, the global variable containing only imputation variables, which are removed after replacement.
foreach v of global imp2_tierras16 {
	replace `v' = `v'_16 if tamano_13==0 & gastprom_agrop_13!=0
	drop    `v'_16
}
global varrep "fuentes_agua_pro fuentes_agua_ext fuentes_agua tamano_permanentes tamano_transitorios tamano_mixtos tamano_ganaderia tamano_pastos tamano_bosques tamano_otros_usos tamano_tierra_no_usada vr_inverHecha vr_inverResil1 vr_inverResil2 invd_1 invd_2 invd_3 invd_7 invd_8 invd_9 tamano" //This should match the global variable ${ind_tierras13}, but it must also be the last one.
foreach v of global varrep {
	replace `v'    = `v'_16 if tamano_13==0 & gastprom_agrop_13!=0
	replace `v'_13 = `v'_16 if tamano_13==0 & gastprom_agrop_13!=0
}
sum tamano_13 tamano_16 gastprom_agrop_13 gastprom_agrop_16 if tamano_13==0 & gastprom_agrop_13!=0 //41 with no size in any year
sum tamano_13 tamano_16 gastprom_agrop_13 gastprom_agrop_16 if tamano_13==0 & gastprom_agrop_13==0 //168 with no size or expenses in 2013
drop if tamano_13==0 //209 entries are removed because they have no data for any year, or because their production volume is insufficient to be included in the 2016 figures.

* Households where size_16==0 and production_16>0
//33 households to which the 2016 land values will be imputed because they have agricultural expenses
sum ing_agrop_16 if tamano_16==0 & gastprom_agrop_16!=0
global varrep "fuentes_agua_pro fuentes_agua_ext fuentes_agua tamano_permanentes tamano_transitorios tamano_mixtos tamano_ganaderia tamano_pastos tamano_bosques tamano_otros_usos tamano_tierra_no_usada vr_inverHecha vr_inverResil1 vr_inverResil2 invd_1 invd_2 invd_3 invd_7 invd_8 invd_9 tamano" //This should match the global variable ${ind_tierras16}, but it must be the last one.
foreach v of global varrep {
	replace `v'_16 = `v'_13 if tamano_16==0 & gastprom_agrop_16!=0
}

* Problems with water sources
global fuentes "fuentes_agua_pro fuentes_agua_ext fuentes_agua"
foreach v of global fuentes {
	replace `v'    = `v'_16 if `v'==.
	replace `v'_13 = `v'_16 if `v'_13==.
}
drop if fuentes_agua==.


* -------------------------------- *
**## Assesing farm size
* -------------------------------- *

* Differences between total area and the sum of proportions.
// For farms where differences exist in one year but not in the other, we use the figures from the year with consistent data. Although farm size may have varied from one survey round to the next, we rely on the farms with consistent measurements, as this is more reliable than a sum of proportions that differs from the total size.
tab diferente_tamano_13 diferente_tamano_16, m
**Comparing the differences (in proportion) from 2013 with the "no differences" from 2016 
compare tamano_13 tamano_16 if diferente_tamano_13==1 & diferente_tamano_16==0
sum tamano_13 tamano_16 if diferente_tamano_13==1 & diferente_tamano_16==0 & tamano_13!=tamano_16, d
replace tamano    = tamano_16 if diferente_tamano_13==1 & diferente_tamano_16==0 & tamano!=tamano_16
replace tamano_13 = tamano_16 if diferente_tamano_13==1 & diferente_tamano_16==0 & tamano_13!=tamano_16
foreach c of global areas {
	replace tamano_`c'    = tamano_`c'_16 if diferente_tamano_13==1 & diferente_tamano_16==0
	replace tamano_`c'_13 = tamano_`c'_16 if diferente_tamano_13==1 & diferente_tamano_16==0
}
**Comparing the differences (in proportion) from 2016 with the "no differences" from 2013
compare tamano_13 tamano_16 if diferente_tamano_13==0 & diferente_tamano_16==1
sum tamano_13 tamano_16 if diferente_tamano_13==0 & diferente_tamano_16==1, d
replace tamano_16 = tamano_13 if diferente_tamano_13==0 & diferente_tamano_16==1
foreach c of global areas {
	replace tamano_`c'_16    = tamano_`c'_13 if diferente_tamano_13==0 & diferente_tamano_16==1
}
**Verifying
replace diferente_tamano_13=0 if diferente_tamano_13==1 & diferente_tamano_16==0
replace diferente_tamano_16=0 if diferente_tamano_13==0 & diferente_tamano_16==1
tab diferente_tamano_13 diferente_tamano_16, m
sum tamano tamano_1* if diferente_tamano_13==0 & diferente_tamano_16==0
sum tamano tamano_1* if diferente_tamano_13==1 & diferente_tamano_16==1

* Outliers in one year but not in another
// If, in one round, a property has an outlier value that is consistent (total area = sum of proportions), but in another round it has a normal value that is consistent, the value from the round with the normal value will be used.
tab diferente_tamano_13 diferente_tamano_16, m
gen outlier_tamano_13 = (tamano_13 > ${areamax})
gen outlier_tamano_16 = (tamano_16 > ${areamax})
replace outlier_tamano_16=. if tamano_16==0
tab outlier_tamano_13 outlier_tamano_16
tab outlier_tamano_13 outlier_tamano_16 if diferente_tamano_13==0 & tamano_16!=0
**Outlier 2013, no outlier 2016
sum tamano_1* if outlier_tamano_13==1 & outlier_tamano_16==0 & tamano_13!=tamano_16
compare tamano_13 tamano_16 if outlier_tamano_13==1 & outlier_tamano_16==0 //Diferencia mínima de 11 ha.
replace tamano    = tamano_16 if outlier_tamano_13==1 & outlier_tamano_16==0 & tamano_13!=tamano_16
replace tamano_13 = tamano_16 if outlier_tamano_13==1 & outlier_tamano_16==0 & tamano_13!=tamano_16
foreach c of global areas {
	replace tamano_`c'    = tamano_`c'_16 if outlier_tamano_13==1 & outlier_tamano_16==0 & tamano_13!=tamano_16
	replace tamano_`c'_13 = tamano_`c'_16 if outlier_tamano_13==1 & outlier_tamano_16==0 & tamano_13!=tamano_16
}
**Outlier 2016, no outlier 2013
sum tamano_1* if outlier_tamano_13==0 & outlier_tamano_16==1 & tamano_13!=tamano_16
compare tamano_13 tamano_16 if outlier_tamano_13==0 & outlier_tamano_16==1 //Diferencia mínima de 17.5 ha.
replace tamano_16 = tamano_13 if outlier_tamano_13==0 & outlier_tamano_16==1 & tamano_13!=tamano_16
foreach c of global areas {
	replace tamano_`c'_16    = tamano_`c'_13 if outlier_tamano_13==0 & outlier_tamano_16==1 & tamano_13!=tamano_16
}
**Verifying
replace outlier_tamano_13=0 if outlier_tamano_13==1 & outlier_tamano_16==0
replace outlier_tamano_16=0 if outlier_tamano_13==0 & outlier_tamano_16==1
tab outlier_tamano_13 outlier_tamano_16, m  //51 outliers en ambas rondas, 31 solo en 2013 que no existe en 2016.
sum tamano tamano_1* if outlier_tamano_13==0 & outlier_tamano_16==0
sum tamano tamano_1* if outlier_tamano_13==1 & outlier_tamano_16==1
tab diferente_tamano_13 outlier_tamano_13, m
sum tamano tamano_1* if diferente_tamano_13==0 & outlier_tamano_13==1
replace outlier_tamano_13 = 0 if diferente_tamano_13==0 & outlier_tamano_13==1 //8 cuya área es máximo 55.5
sum tamano tamano_1* if diferente_tamano_13==1 & outlier_tamano_13==1,d //66

* Replacing with the smaller of the total area and the proportions
//For properties with inconsistencies between the total area and the sum of the proportions, the smaller value will be retained. The property size will be prioritized first; if it is an outlier, the proportions will be prioritized instead. Note that this approach still generates outliers, but they are the "least dramatic" ones.
gen prueba_tamano_13 = 0
gen prueba_tamano_16 = 0
foreach c of global areas {
	replace prueba_tamano_13 = prueba_tamano_13 + tamano_`c'_13
	replace prueba_tamano_16 = prueba_tamano_16 + tamano_`c'_16
}
foreach c of global areas {
	gen proporciones_`c'_13 = tamano_`c'_13 / prueba_tamano_13
	gen proporciones_`c'_16 = tamano_`c'_16 / prueba_tamano_16
}
** Size less than the sum of the proportions
foreach c of global areas {
	replace tamano_`c'_13 = proporciones_`c'_13*tamano_13 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_13<prueba_tamano_13
	replace tamano_`c'_16 = proporciones_`c'_13*tamano_13 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_16<prueba_tamano_16
}
** The sum of the fractions is less than the size
replace tamano_13 = prueba_tamano_13 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_13>prueba_tamano_13
replace tamano_16 = prueba_tamano_16 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_16>prueba_tamano_16
sum tamano_1* if diferente_tamano_13==1 & outlier_tamano_13==1,d //66
sum tamano_13 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_13==prueba_tamano_13,d //63
sum tamano_16 if diferente_tamano_13==1 & outlier_tamano_13==1 & tamano_16==prueba_tamano_16,d //54
drop prueba_tamano_1* proporciones_* outlier_* diferente_*
sum tamano, d

*Generating proportions
foreach v of global areas {
	gen p_`v'   = tamano_`v' / tamano
}

*Generating farm size
capture drop class_tamano
gen     class_tamano = 0 if (tamano<=3)
replace class_tamano = 1 if (tamano>3  & tamano<=10)
replace class_tamano = 2 if (tamano>10 & tamano<=20)
replace class_tamano = 3 if (tamano>20 & tamano<=200)
replace class_tamano = 4 if (tamano>200)
tab class_tamano, m


* -------------------------------- *
**## Outliers treatment
* -------------------------------- *

* Identifying outliers
capture drop outlier*
sum ing_agrop_1* gastprom_agrop_1*
sum ing_agrop_13 if ing_agrop_13>50000000
sum ing_agrop_16 if ing_agrop_16>50000000
sum gastprom_agrop_13 if gastprom_agrop_13>50000000
sum gastprom_agrop_16 if gastprom_agrop_16>50000000

**size
sum tamano_13, d
*sum tamano_13 if tamano_13>r(p99)
gen outlier_tamano_13 = (tamano_13>r(${outl}))
sum tamano_16, d
*sum tamano_16 if tamano_16>r(p99)
gen outlier_tamano_16 = (tamano_16>r(${outl}))
tab outlier_tamano_13 outlier_tamano_16

**Agricultural income
sum ing_agrop_13, d
*sum ing_agrop_13 if ing_agrop_13>r(p99)
gen outlier_ing_13 = (ing_agrop_13>r(${outl}))
sum ing_agrop_16, d
*sum ing_agrop_16 if ing_agrop_16>r(p99)
gen outlier_ing_16 = (ing_agrop_16>r(${outl}))
tab outlier_ing_13 outlier_ing_16 // 45 outliers

**Other incomes
sum ingmensual_* //Máximo son 5.7 millones. Se ven equilibrados

**Agricultural expenses
sum gastprom_agrop_13, d
*sum gastprom_agrop_13 if gastprom_agrop_13>r(p99)
gen outlier_gast_13 = (gastprom_agrop_13>r(${outl}))
sum gastprom_agrop_16, d
*sum gastprom_agrop_16 if gastprom_agrop_16>r(p99)
gen outlier_gast_16 = (gastprom_agrop_16>r(${outl}))
tab outlier_gast_13 outlier_gast_16

**Other expenses
sum gastmensual_all //Máximo 4.7 millones. Se ven equilibrados

* Dropping outliers
*drop if outlier_tamano_13==1 | outlier_tamano_16==1 | outlier_tamano_16==1 | outlier_ing_16==1 //50
drop if outlier_tamano_13==1 | outlier_tamano_16==1 | outlier_ing_13==1 | outlier_ing_16==1 | outlier_gast_13==1 | outlier_gast_16==1 //93

* Excluding departments outside the sample
keep if dpto == 15 | dpto == 23 | dpto == 25 | dpto == 63 | dpto == 66 | dpto == 68 | dpto == 70 | dpto == 73



			* ============================= *
**#				   10. SAVING DATABASES
			* ============================= *

* -------------------------------- *
**## Base Indicators
* -------------------------------- *
drop *merge*
preserve
keep consecutivo llave_ID_lb *_10 *_13 *_16
save "${data}\Variables_Indicadores.dta", replace
count
restore


* -------------------------------- *
**## Base Covariables
* -------------------------------- *
rename (llave_13 llave_16 type_13) (llave_13a llave_16a type)
drop *_10 *_13 *_16
rename (llave_13a llave_16a) (llave_13 llave_16)

* Variables treatment: Logaritmic transformation
*global variables "transporte_minutos ingmensual_otro ingmensual_agri ingmensual_noagri ingmensual_all gastmensual_all inganual_otro inganual_agri inganual_noagri inganual_all gastanual_all tamano tamano_permanentes tamano_transitorios tamano_mixtos tamano_ganaderia tamano_pastos tamano_bosques tamano_otros_usos tamano_tierra_no_usada tamano_13 tamano_permanentes_13 tamano_transitorios_13 tamano_mixtos_13 tamano_ganaderia_13 tamano_pastos_13 tamano_bosques_13 tamano_otros_usos_13 tamano_tierra_no_usada_13 tamano_16 tamano_permanentes_16 tamano_transitorios_16 tamano_mixtos_16 tamano_ganaderia_16 tamano_pastos_16 tamano_bosques_16 tamano_otros_usos_16 tamano_tierra_no_usada_16 vr_inverHecha vr_inverHecha_13 vr_inverHecha_16 ing_agri ing_pecu ing_agrop ing_agri_13 ing_pecu_13 ing_agrop_13 ing_agri_16 ing_pecu_16 ing_agrop_16 gastprom_agri gastprom_pecu gastprom_asitec gastprom_manobra gastprom_transp gastprom_agrop gastprom_agri_13 gastprom_pecu_13 gastprom_asitec_13 gastprom_manobra_13 gastprom_transp_13 gastprom_agrop_13 gastprom_agri_16 gastprom_pecu_16 gastprom_asitec_16 gastprom_manobra_16 gastprom_transp_16 gastprom_agrop_16 n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim n_vacas_13 n_cerdos_13 n_avescorral_13 n_caballos_13 n_ovejas_13 n_colmenas_13 n_otros_anim_13 n_vacas_16 n_cerdos_16 n_avescorral_16 n_caballos_16 n_ovejas_16 n_colmenas_16 n_otros_anim_16"
*global variables "transporte_minutos ingmensual_otro ingmensual_agri ingmensual_noagri ingmensual_all gastmensual_all inganual_otro inganual_agri inganual_noagri inganual_all gastanual_all tamano tamano_permanentes tamano_transitorios tamano_mixtos tamano_ganaderia tamano_pastos tamano_bosques tamano_otros_usos tamano_tierra_no_usada vr_inverHecha vr_inverResil1 vr_inverResil2 ing_agri ing_pecu ing_agrop gastprom_agri gastprom_pecu gastprom_asitec gastprom_manobra gastprom_transp gastprom_agrop n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim"
global variables "transporte_minutos ingmensual_otro ingmensual_agri ingmensual_noagri ingmensual_all gastmensual_all inganual_otro inganual_agri inganual_noagri inganual_all gastanual_all vr_inverHecha vr_inverResil1 vr_inverResil2 ing_agri ing_pecu ing_agrop gastprom_agri gastprom_pecu gastprom_asitec gastprom_manobra gastprom_transp gastprom_agrop"

foreach v of global variables {
	gen l_`v' = log(`v'+1)
	*hist `v', freq
}
sum l_*


* Departments and municipalities
global rgion "AM CB EC CO"
global dptos "15 23 25 63 66 68 70 73"
global mpios "15001 15109  15131  15176  15632  15676  15776  15808  23162  23182  23189  23570  23660  23670  23686  25743  25745  25779  25781  25815  25817  63130  63190  63212  63272  63302  63401  63470  63594  63690  66001  66088  66170  66318  66400  66440  66682  66687  68020  68572  70215  70670  73001  73217  73483  73504  73585"
local region = 6
foreach r of global rgion {
	gen region_`r'=(region==`region')
	local ++region
}
/*foreach d of global dptos {
	gen dpto_`d'=(dpto==`d')
} 
foreach m of global mpios {
	gen mpio_`m'=(mpio==`m')
}*/

* Saving data
save "${data}\CovariablesELCA.dta", replace
