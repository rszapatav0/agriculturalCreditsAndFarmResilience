/* -----------------------------------------------------------------------------
Descriptive statistics using the final dataset
Author:       Raquel
Creation:     January 2025
Last edition: January 2025
----------------------------------------------------------------------------- */

clear all
global ELCA   "C:\Users\userecon10\Desktop\Bases ELCA\"
global data   "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"
global tables "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\05_Tables"


 			* ================================== *
**#				 		 1. CREDITS
			* ================================== *

use "${data}\Base_Creditos.dta", clear
merge m:1 llave_13 using "${data}\CovariablesELCA.dta", nogen keep(3) keepusing(llave_13)

foreach v of varlist conquien_banco conquien_cajas_ conquien_fondos_ conquien_gremios_ conquien_formal_ anombre_jefe_ anombre_conyuge_ anombre_otro_ cred_aldia_si_ cred_aldia_no_ cred_aldia_noini_ vr_inicial_ cuota_ vr_saldo_ {
	gen     `v'agrop_  = `v' if destino_agrop_==1
	replace `v'agrop_  = .   if (destino_agrop_==0 | destino_agrop_==.)	
	gen     `v'formal_ = `v' if credito_formal_==1
	replace `v'formal_ = .   if (credito_formal_==0 | credito_formal_==.)
	gen     `v'resil_ = `v' if credito_resil_==1
	replace `v'resil_ = .   if (credito_resil_==0 | credito_resil_==.)
}
drop destino_
collapse (max) credito_* destino_* conquien_* anombre_* cred_aldia_* ///
(sum) numcreditos_=credito_ numcreditos_agrop_=credito_agrop_ numcreditos_formal_=credito_formal_ numcreditos_resil_=credito_resil_ vr_inicial_* cuota_* vr_saldo_*, by(consecutivo llave_13 llave_16 round)

foreach v of varlist destino_* conquien_* anombre_* cred_aldia_* vr_inicial_* cuota_* vr_saldo_* numcreditos_* {
	replace `v' = . if (credito_==0)
}
foreach v of varlist numcreditos_agrop_ vr_inicial_agrop_ cuota_agrop_ vr_saldo_agrop_ {
	replace `v'  = .   if (destino_agrop_==0 | destino_agrop_==.)		
}
foreach v of varlist numcreditos_formal_ vr_inicial_formal_ cuota_formal_ vr_saldo_formal_ {
	replace `v'  = .   if (destino_formal_==0 | destino_formal_==.)		
}
foreach v of varlist numcreditos_resil_ vr_inicial_resil_ cuota_resil_ vr_saldo_resil_ {
	replace `v'  = .   if (destino_resil_==0 | destino_resil_==.)		
}
reshape wide credito_* destino_* numcreditos_* conquien_* anombre_* cred_aldia_* vr_inicial_* cuota_* vr_saldo_*, i(consecutivo llave_13 llave_16) j(round)
drop llave_13 llave_16


**### 2010
** All credits **
estpost sum credito_10 destino_agrop_10 destino_formal_10 destino_resil_10 destino_otro_10 conquien_banco_10 conquien_cajas_10 conquien_fondos_10 conquien_gremios_10 conquien_formal_10 anombre_jefe_10 anombre_conyuge_10 anombre_otro_10 cred_aldia_si_10 cred_aldia_no_10 cred_aldia_noini_10 numcreditos_10 vr_inicial_10 cuota_10 vr_saldo_10
matrix  general_10_m = e(mean)
svmat   general_10_m
matrix  general_10_c = e(count)
svmat   general_10_c
matrix  general_10_i = e(min)
svmat   general_10_i
matrix  general_10_a = e(max)
svmat   general_10_a
matrix  general_10_s = e(sd)
svmat   general_10_s
** Only agricultural credits **
estpost sum credito_agrop_10 destino_agrop_10 destino_formal_10 destino_resil_10 destino_otro_10 conquien_banco_agrop_10 conquien_cajas_agrop_10 conquien_fondos_agrop_10 conquien_gremios_agrop_10 conquien_formal_agrop_10 anombre_jefe_agrop_10 anombre_conyuge_agrop_10 anombre_otro_agrop_10 cred_aldia_si_agrop_10 cred_aldia_no_agrop_10 cred_aldia_noini_agrop_10 numcreditos_agrop_10 vr_inicial_agrop_10 cuota_agrop_10 vr_saldo_agrop_10
matrix  soloagrop_10_m = e(mean)
svmat   soloagrop_10_m
matrix  soloagrop_10_c = e(count)
svmat   soloagrop_10_c
matrix  soloagrop_10_i = e(min)
svmat   soloagrop_10_i
matrix  soloagrop_10_a = e(max)
svmat   soloagrop_10_a
matrix  soloagrop_10_s = e(sd)
svmat   soloagrop_10_s
** Only formal credits **
estpost sum credito_formal_10 destino_agrop_10 destino_formal_10 destino_resil_10 destino_otro_10 conquien_banco_formal_10 conquien_cajas_formal_10 conquien_fondos_formal_10 conquien_gremios_formal_10 conquien_formal_formal_10 anombre_jefe_formal_10 anombre_conyuge_formal_10 anombre_otro_formal_10 cred_aldia_si_formal_10 cred_aldia_no_formal_10 cred_aldia_noini_formal_10 numcreditos_formal_10 vr_inicial_formal_10 cuota_formal_10 vr_saldo_formal_10
matrix  soloformal_10_m = e(mean)
svmat   soloformal_10_m
matrix  soloformal_10_c = e(count)
svmat   soloformal_10_c
matrix  soloformal_10_i = e(min)
svmat   soloformal_10_i
matrix  soloformal_10_a = e(max)
svmat   soloformal_10_a
matrix  soloformal_10_s = e(sd)
svmat   soloformal_10_s
** Only resilience credits **
estpost sum credito_resil_10 destino_agrop_10 destino_formal_10 destino_resil_10 destino_otro_10 conquien_banco_resil_10 conquien_cajas_resil_10 conquien_fondos_resil_10 conquien_gremios_resil_10 conquien_formal_resil_10 anombre_jefe_resil_10 anombre_conyuge_resil_10 anombre_otro_resil_10 cred_aldia_si_resil_10 cred_aldia_no_resil_10 cred_aldia_noini_resil_10 numcreditos_resil_10 vr_inicial_resil_10 cuota_resil_10 vr_saldo_resil_10
matrix  soloresil_10_m = e(mean)
svmat   soloresil_10_m
matrix  soloresil_10_c = e(count)
svmat   soloresil_10_c
matrix  soloresil_10_i = e(min)
svmat   soloresil_10_i
matrix  soloresil_10_a = e(max)
svmat   soloresil_10_a
matrix  soloresil_10_s = e(sd)
svmat   soloresil_10_s
** Joining matrixes **
matrix  Creditos10 = [general_10_c \ general_10_m \ general_10_s \ general_10_i \ general_10_a \ soloagrop_10_c \ soloagrop_10_m \ soloagrop_10_s \ soloagrop_10_i \ soloagrop_10_a \ soloformal_10_c \ soloformal_10_m \ soloformal_10_s \ soloformal_10_i \ soloformal_10_a \ soloresil_10_c \ soloresil_10_m \ soloresil_10_s \ soloresil_10_i \ soloresil_10_a]
matrix  rownames Creditos10 = Gen_count Gen_mean Gen_sd Gen_min Gen_max  Agrop_count Agrop_mean Agrop_sd Agrop_min Agrop_max  Formal_count Formal_mean Formal_sd Formal_min Formal_max  Resil_count Resil_mean Resil_sd Resil_min Resil_max
matrix  Creditos_2010 = Creditos10'
matlist Creditos_2010
putexcel set "${tables}\Creditos", sheet(CreditosUPA_10) modify
putexcel A2=matrix(Creditos_2010), names


**### 2013
** All credits **
estpost sum credito_13 destino_agrop_13 destino_formal_13 destino_resil_13 destino_otro_13 conquien_banco_13 conquien_cajas_13 conquien_fondos_13 conquien_gremios_13 conquien_formal_13 anombre_jefe_13 anombre_conyuge_13 anombre_otro_13 cred_aldia_si_13 cred_aldia_no_13 cred_aldia_noini_13 numcreditos_13 vr_inicial_13 cuota_13 vr_saldo_13
matrix  general_13_m = e(mean)
svmat   general_13_m
matrix  general_13_c = e(count)
svmat   general_13_c
matrix  general_13_i = e(min)
svmat   general_13_i
matrix  general_13_a = e(max)
svmat   general_13_a
matrix  general_13_s = e(sd)
svmat   general_13_s
** Only agricultural credits **
estpost sum credito_agrop_13 destino_agrop_13 destino_formal_13 destino_resil_13 destino_otro_13 conquien_banco_agrop_13 conquien_cajas_agrop_13 conquien_fondos_agrop_13 conquien_gremios_agrop_13 conquien_formal_agrop_13 anombre_jefe_agrop_13 anombre_conyuge_agrop_13 anombre_otro_agrop_13 cred_aldia_si_agrop_13 cred_aldia_no_agrop_13 cred_aldia_noini_agrop_13 numcreditos_agrop_13 vr_inicial_agrop_13 cuota_agrop_13 vr_saldo_agrop_13
matrix  soloagrop_13_m = e(mean)
svmat   soloagrop_13_m
matrix  soloagrop_13_c = e(count)
svmat   soloagrop_13_c
matrix  soloagrop_13_i = e(min)
svmat   soloagrop_13_i
matrix  soloagrop_13_a = e(max)
svmat   soloagrop_13_a
matrix  soloagrop_13_s = e(sd)
svmat   soloagrop_13_s
** Only formal credits **
estpost sum credito_formal_13 destino_agrop_13 destino_formal_13 destino_resil_13 destino_otro_13 conquien_banco_formal_13 conquien_cajas_formal_13 conquien_fondos_formal_13 conquien_gremios_formal_13 conquien_formal_formal_13 anombre_jefe_formal_13 anombre_conyuge_formal_13 anombre_otro_formal_13 cred_aldia_si_formal_13 cred_aldia_no_formal_13 cred_aldia_noini_formal_13 numcreditos_formal_13 vr_inicial_formal_13 cuota_formal_13 vr_saldo_formal_13
matrix  soloformal_13_m = e(mean)
svmat   soloformal_13_m
matrix  soloformal_13_c = e(count)
svmat   soloformal_13_c
matrix  soloformal_13_i = e(min)
svmat   soloformal_13_i
matrix  soloformal_13_a = e(max)
svmat   soloformal_13_a
matrix  soloformal_13_s = e(sd)
svmat   soloformal_13_s
** Only resilience credits **
estpost sum credito_resil_13 destino_agrop_13 destino_formal_13 destino_resil_13 destino_otro_13 conquien_banco_resil_13 conquien_cajas_resil_13 conquien_fondos_resil_13 conquien_gremios_resil_13 conquien_formal_resil_13 anombre_jefe_resil_13 anombre_conyuge_resil_13 anombre_otro_resil_13 cred_aldia_si_resil_13 cred_aldia_no_resil_13 cred_aldia_noini_resil_13 numcreditos_resil_13 vr_inicial_resil_13 cuota_resil_13 vr_saldo_resil_13
matrix  soloresil_13_m = e(mean)
svmat   soloresil_13_m
matrix  soloresil_13_c = e(count)
svmat   soloresil_13_c
matrix  soloresil_13_i = e(min)
svmat   soloresil_13_i
matrix  soloresil_13_a = e(max)
svmat   soloresil_13_a
matrix  soloresil_13_s = e(sd)
svmat   soloresil_13_s
** Joining matrixes **
matrix  Creditos13 = [general_13_c \ general_13_m \ general_13_s \ general_13_i \ general_13_a \ soloagrop_13_c \ soloagrop_13_m \ soloagrop_13_s \ soloagrop_13_i \ soloagrop_13_a \ soloformal_13_c \ soloformal_13_m \ soloformal_13_s \ soloformal_13_i \ soloformal_13_a \ soloresil_13_c \ soloresil_13_m \ soloresil_13_s \ soloresil_13_i \ soloresil_13_a]
matrix  rownames Creditos13 = Gen_count Gen_mean Gen_sd Gen_min Gen_max  Agrop_count Agrop_mean Agrop_sd Agrop_min Agrop_max  Formal_count Formal_mean Formal_sd Formal_min Formal_max  Resil_count Resil_mean Resil_sd Resil_min Resil_max
matrix  Creditos_2013 = Creditos13'
matlist Creditos_2013
putexcel set "${tables}\Creditos", sheet(CreditosUPA_13) modify
putexcel A2=matrix(Creditos_2013), names


**### 2016
** All credits **
estpost sum credito_16 destino_agrop_16 destino_formal_16 destino_resil_16 destino_otro_16 conquien_banco_16 conquien_cajas_16 conquien_fondos_16 conquien_gremios_16 conquien_formal_16 anombre_jefe_16 anombre_conyuge_16 anombre_otro_16 cred_aldia_si_16 cred_aldia_no_16 cred_aldia_noini_16 numcreditos_16 vr_inicial_16 cuota_16 vr_saldo_16
matrix  general_16_m = e(mean)
svmat   general_16_m
matrix  general_16_c = e(count)
svmat   general_16_c
matrix  general_16_i = e(min)
svmat   general_16_i
matrix  general_16_a = e(max)
svmat   general_16_a
matrix  general_16_s = e(sd)
svmat   general_16_s
** Only agricultural credits **
estpost sum credito_agrop_16 destino_agrop_16 destino_formal_16 destino_resil_16 destino_otro_16 conquien_banco_agrop_16 conquien_cajas_agrop_16 conquien_fondos_agrop_16 conquien_gremios_agrop_16 conquien_formal_agrop_16 anombre_jefe_agrop_16 anombre_conyuge_agrop_16 anombre_otro_agrop_16 cred_aldia_si_agrop_16 cred_aldia_no_agrop_16 cred_aldia_noini_agrop_16 numcreditos_agrop_16 vr_inicial_agrop_16 cuota_agrop_16 vr_saldo_agrop_16
matrix  soloagrop_16_m = e(mean)
svmat   soloagrop_16_m
matrix  soloagrop_16_c = e(count)
svmat   soloagrop_16_c
matrix  soloagrop_16_i = e(min)
svmat   soloagrop_16_i
matrix  soloagrop_16_a = e(max)
svmat   soloagrop_16_a
matrix  soloagrop_16_s = e(sd)
svmat   soloagrop_16_s
** Only formal credits **
estpost sum credito_formal_16 destino_agrop_16 destino_formal_16 destino_resil_16 destino_otro_16 conquien_banco_formal_16 conquien_cajas_formal_16 conquien_fondos_formal_16 conquien_gremios_formal_16 conquien_formal_formal_16 anombre_jefe_formal_16 anombre_conyuge_formal_16 anombre_otro_formal_16 cred_aldia_si_formal_16 cred_aldia_no_formal_16 cred_aldia_noini_formal_16 numcreditos_formal_16 vr_inicial_formal_16 cuota_formal_16 vr_saldo_formal_16
matrix  soloformal_16_m = e(mean)
svmat   soloformal_16_m
matrix  soloformal_16_c = e(count)
svmat   soloformal_16_c
matrix  soloformal_16_i = e(min)
svmat   soloformal_16_i
matrix  soloformal_16_a = e(max)
svmat   soloformal_16_a
matrix  soloformal_16_s = e(sd)
svmat   soloformal_16_s
** Only resilience credits **
estpost sum credito_resil_16 destino_agrop_16 destino_formal_16 destino_resil_16 destino_otro_16 conquien_banco_resil_16 conquien_cajas_resil_16 conquien_fondos_resil_16 conquien_gremios_resil_16 conquien_formal_resil_16 anombre_jefe_resil_16 anombre_conyuge_resil_16 anombre_otro_resil_16 cred_aldia_si_resil_16 cred_aldia_no_resil_16 cred_aldia_noini_resil_16 numcreditos_resil_16 vr_inicial_resil_16 cuota_resil_16 vr_saldo_resil_16
matrix  soloresil_16_m = e(mean)
svmat   soloresil_16_m
matrix  soloresil_16_c = e(count)
svmat   soloresil_16_c
matrix  soloresil_16_i = e(min)
svmat   soloresil_16_i
matrix  soloresil_16_a = e(max)
svmat   soloresil_16_a
matrix  soloresil_16_s = e(sd)
svmat   soloresil_16_s
** Joining matrixes **
matrix  Creditos16 = [general_16_c \ general_16_m \ general_16_s \ general_16_i \ general_16_a \ soloagrop_16_c \ soloagrop_16_m \ soloagrop_16_s \ soloagrop_16_i \ soloagrop_16_a \ soloformal_16_c \ soloformal_16_m \ soloformal_16_s \ soloformal_16_i \ soloformal_16_a \ soloresil_16_c \ soloresil_16_m \ soloresil_16_s \ soloresil_16_i \ soloresil_16_a]
matrix  rownames Creditos16 = Gen_count Gen_mean Gen_sd Gen_min Gen_max  Agrop_count Agrop_mean Agrop_sd Agrop_min Agrop_max  Formal_count Formal_mean Formal_sd Formal_min Formal_max  Resil_count Resil_mean Resil_sd Resil_min Resil_max
matrix  Creditos_2016 = Creditos16'
matlist Creditos_2016
putexcel set "${tables}\Creditos", sheet(CreditosUPA_16) modify
putexcel A2=matrix(Creditos_2016), names



 			* ================================== *
**#				 	  2. TREATMENT STATUS
			* ================================== *

use "${data}\Base_Tratamiento.dta", clear
merge 1:1 llave_13 using "${data}\CovariablesELCA.dta", nogen keep(3) keepusing(llave_13)

* Treated summary *
estpost sum treat10 treat13 treat16 treat_agrop10 treat_agrop13 treat_agrop16 treat_formal10 treat_formal13 treat_formal16 treat_resil10 treat_resil13 treat_resil16 d_credito_10 d_credito_13 d_credito_16 d_credito_agrop_10 d_credito_agrop_13 d_credito_agrop_16 d_credito_formal_10 d_credito_formal_13 d_credito_formal_16 d_credito_resil_10 d_credito_resil_13 d_credito_resil_16 num_credito_10 num_credito_13 num_credito_16 num_credito_agrop_10 num_credito_agrop_13 num_credito_agrop_16 num_credito_formal_10 num_credito_formal_13 num_credito_formal_16 num_credito_resil_10 num_credito_resil_13 num_credito_resil_16
matrix  general_m = e(mean)
svmat   general_m
matrix  general_c = e(count)
svmat   general_c
** Joining matrixes **
matrix  Tratamiento = [general_m \ general_c]
matrix  rownames Tratamiento = Percentage Count
matrix  colnames Tratamiento = Treat_10 Treat_13 Treat_16 TreatAgrop_10 TreatAgrop_13 TreatAgrop_16 TreatFormal_10 TreatFormal_13 TreatFormal_16 TreatResil_10 TreatResil_13 TreatResil_16 Dummy_Credito_10 Dummy_Credito_13 Dummy_Credito_16 Dummy_CreditoAgrop_10 Dummy_CreditoAgrop_13 Dummy_CreditoAgrop_16 Dummy_CreditoFormal_10 Dummy_CreditoFormal_13 Dummy_CreditoFormal_16 Dummy_CreditoResil_10 Dummy_CreditoResil_13 Dummy_CreditoResil_16 #Creditos_10 #Creditos_13 #Creditos_16 #CreditosAgrop_10 #CreditosAgrop_13 #CreditosAgrop_16 #CreditosFormal_10 #CreditosFormal_13 #CreditosFormal_16 #CreditosResil_10 #CreditosResil_13 #CreditosResil_16
matrix  TratamientoT = Tratamiento'
matlist TratamientoT
putexcel set "${tables}\Creditos", sheet(Tratamiento) modify
putexcel A2=matrix(TratamientoT), names



			* ================================== *
**#						3. COVARIABLES
			* ================================== *

* ------------------- *
**## General
* ------------------- *
use "${data}\CovariablesELCA.dta", replace

**### People
global personas13 "edad woman estadoCivil lee_escribe educacion main_job_agri tot_trabajos organizacion enfermedad"

*Descriptive statistics
estpost sum ${personas13}
matrix  personas_c = e(count)
svmat   personas_c
matrix  personas_m = e(mean)
svmat   personas_m
matrix  personas_s = e(sd)
svmat   personas_s
matrix  personas_i = e(min)
svmat   personas_i
matrix  personas_a = e(max)
svmat   personas_a

*Matrix
matrix  personas = [personas_c \ personas_m \ personas_s \ personas_i \ personas_a]
matrix  rownames personas = count mean sd min max 
*matrix  colnames personas = Edad Mujer Estado_Civil LeeyEscribe Educacion TrabajoPrincipalAgropecuario TotalTrabajos Organizacion Enfermedad 
matrix  personasT = personas'
matlist personasT
putexcel set "${tables}\Covariables", sheet(personas) modify
putexcel A2=matrix(personasT), names


**### Spouse
*global conyuge13 "edad_cyg woman_cyg lee_escribe_cyg educacion_cyg main_job_agri_cyg tot_trabajos_cyg organizacion_cyg"
global conyuge13 "conyuge edad_cyg woman_cyg lee_escribe_cyg educacion_cyg main_job_agri_cyg tot_trabajos_cyg organizacion_cyg"

*Descriptive statistics
estpost sum ${conyuge13}
matrix  conyuge_c = e(count)
svmat   conyuge_c
matrix  conyuge_m = e(mean)
svmat   conyuge_m
matrix  conyuge_s = e(sd)
svmat   conyuge_s
matrix  conyuge_i = e(min)
svmat   conyuge_i
matrix  conyuge_a = e(max)
svmat   conyuge_a

*Matrix
matrix  conyuge = [conyuge_c \ conyuge_m \ conyuge_s \ conyuge_i \ conyuge_a]
matrix  rownames conyuge = count mean sd min max 
*matrix  colnames conyuge = Conyuge Edad Mujer LeeyEscribe Educacion TrabajoPrincipalAgropecuario TotalTrabajos Organizacion
matrix  conyugeT = conyuge'
matlist conyugeT
putexcel set "${tables}\Covariables", sheet(conyuge) modify
putexcel A2=matrix(conyugeT), names

 
**### Household
global cov_hogar13 "t_personas sp_energia sp_acueducto sp_alcantarillado n_internet medio_transporte transporte_minutos ingmensual_* inganual_* empshare_* incshare_* gastmensual_all gastanual_all act_seghogar act_segcosechas riqueza_pca credito_rechazado mala_histcrediticia programas_ayudas fexhog region dpto mpio consecutivo_c "

*Descriptive statistics
estpost sum ${cov_hogar13} region_AM region_CB region_EC region_CO
matrix  hogares_c = e(count)
svmat   hogares_c
matrix  hogares_m = e(mean)
svmat   hogares_m
matrix  hogares_s = e(sd)
svmat   hogares_s
matrix  hogares_i = e(min)
svmat   hogares_i
matrix  hogares_a = e(max)
svmat   hogares_a

*Matrix
matrix  hogares = [hogares_c \ hogares_m \ hogares_s \ hogares_i \ hogares_a]
matrix  rownames hogares = count mean sd min max 
*matrix  colnames hogares = TotalPersonas SP_Energia SP_Acueducto SP_Alcantarillado SP_Internet MedioTransporteCabecera Transporte_Minutos IncMensual_Otros IncMensual_Agropecuario IncMensual_NoAgropecuario IncMensual_Total IncAnual_Otros IncAnual_Agropecuario IncAnual_NoAgropecuario IncAnual_Total Emp_Agropecuario Emp_NoAgropecuario IncShare_Agropecuario IncShare_NoAgropecuario IncShare_Otros GastosMensuales GastosAnuales SegurosHogar SegurosCosechas RiquezaPCA Credito_Rechazado MalaHistoriaCrediticia ProgramasoAyudas FEX_Hogar Region Departamento Municipio Comunidad RegionAM RegionCB RegionEC RegionCO
matrix  hogaresT = hogares'
matlist hogaresT
putexcel set "${tables}\Covariables", sheet(hogares) modify
putexcel A2=matrix(hogaresT), names


**### Assets
global cov_activos "n_bueyes n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim"

*Descriptive statistics
estpost sum ${cov_activos}
matrix  activos_c = e(count)
svmat   activos_c
matrix  activos_m = e(mean)
svmat   activos_m
matrix  activos_s = e(sd)
svmat   activos_s
matrix  activos_i = e(min)
svmat   activos_i
matrix  activos_a = e(max)
svmat   activos_a

*Matrix
matrix  activos = [activos_c \ activos_m \ activos_s \ activos_i \ activos_a]
matrix  rownames activos = count mean sd min max  
*matrix  colnames activos = Bueyes Vacas Cerdos AvesCorral Caballos Ovejas Colmenas OtrosAnimales
matrix  activosT = activos'
matlist activosT
putexcel set "${tables}\Covariables", sheet(activos) modify
putexcel A2=matrix(activosT), names


**### Lands
global cov_tierras13 "propietario asocio tipoTenencia_* totpred_* dadasPerdidasVendidas tamano tamano_* p_* class_tamano fuentes_agua* vr_inverHecha invd_*"

*Descriptive statistics
estpost sum ${cov_tierras13}
matrix  tierras_c = e(count)
svmat   tierras_c
matrix  tierras_m = e(mean)
svmat   tierras_m
matrix  tierras_s = e(sd)
svmat   tierras_s
matrix  tierras_i = e(min)
svmat   tierras_i
matrix  tierras_a = e(max)
svmat   tierras_a

*Matrix
matrix  tierras = [tierras_c \ tierras_m \ tierras_s \ tierras_i \ tierras_a]
matrix  rownames tierras = count mean sd min max 
*matrix  colnames tierras = Propietario Asocio Tenencia_Posesion Tenencia_Herencia Tenencia_Arriendo Tenencia_Aparceria Tenencia_Usufructo Tenencia_Empeno Tenencia_Comodato Tenencia_Compania Tot_PrediosUtilizados Tot_PrediosOtros Ha_UPA Ha_CultivosPermanentes Ha_CultivosTransitorios Ha_CultivosMixtos Ha_Ganaderia Ha_Pastos Ha_Bosques Ha_OtrosUsos Ha_TierraNoUsada Share_CultivosPermanentes Share_CultivosTransitorios Share_CultivosMixtos Share_Ganaderia Share_Pastos Share_Bosques Share_OtrosUsos Share_TierraNoUsada Clasificacion_Tamano Fuentes_Agua_Pro Fuentes_Agua_Ext Fuentes_Agua Vr_InversionHecha Inv_Riego Inv_Estructuras Inv_Conservacion Inv_Frutales Inv_Maderables Inv_OtrosCiales Inv_Vivienda Inv_DesNat Inv_Otra
matrix  tierrasT = tierras'
matlist tierrasT
putexcel set "${tables}\Covariables", sheet(tierras) modify
putexcel A2=matrix(tierrasT), names


**### Production
global cov_produccion13 "ing_* gastprom_* sitioVenta_*"

*Descriptive statistics
estpost sum ${cov_produccion13}
matrix  produccion_c = e(count)
svmat   produccion_c
matrix  produccion_m = e(mean)
svmat   produccion_m
matrix  produccion_s = e(sd)
svmat   produccion_s
matrix  produccion_i = e(min)
svmat   produccion_i
matrix  produccion_a = e(max)
svmat   produccion_a

*Matrix
matrix  produccion = [produccion_c \ produccion_m \ produccion_s \ produccion_i \ produccion_a]
matrix  rownames produccion = count mean sd min max 
*matrix  colnames produccion = Ingreso_Agricola Ingreso_Pecuario Ingreso_Agropecuario Gasto_Agricola Gasto_Pecuario Gasto_Semilla Gasto_Maquinaria Gasto_Fertilizantes Gasto_Insecticida Gasto_Cria GastoAlimentacion Gasto_Vacunas Gasto_Drogas Gasto_Vitaminas Gasto_AsiTec Gasto_ManoObra Gasto_Transp Gasto_OtrosGastos Gasto_Agropecuario SitVen_Finca SitVen_Vereda SitVen_OtraVereda SitVen_Cabecera SitVen_Otracabecera SitVen_Otro
matrix  produccionT = produccion'
matlist produccionT
putexcel set "${tables}\Covariables", sheet(produccion) modify
putexcel A2=matrix(produccionT), names


**### Community
global comunidad13 "pp_faltacap acceso alquila_maquinaria seguridad solidaridad"

*Descriptive statistics
estpost sum ${comunidad13} td_pibAgrop td_ocupados td_tasaAfect
matrix  comunidad_c = e(count)
svmat   comunidad_c
matrix  comunidad_m = e(mean)
svmat   comunidad_m
matrix  comunidad_s = e(sd)
svmat   comunidad_s
matrix  comunidad_i = e(min)
svmat   comunidad_i
matrix  comunidad_a = e(max)
svmat   comunidad_a

*Matrix
matrix  comunidad = [comunidad_c \ comunidad_m \ comunidad_s \ comunidad_i \ comunidad_a]
matrix  rownames comunidad = count mean sd min max 
*matrix  colnames comunidad = Problema_FaltaCapacitacion Acceso Vereda_AlquilaMaquinaria Seguridad Solidaridad td_pibAgrop td_ocupados td_tasaAfect
matrix  comunidadT = comunidad'
matlist comunidadT
putexcel set "${tables}\Covariables", sheet(comunidad) modify
putexcel A2=matrix(comunidadT), names


* ---------------------- *
**## By farm type
* ---------------------- *
/*
use "${data}\CovariablesELCA.dta", replace

forvalues clase=1(1)3 {
	preserve
	replace type=3 if type==0
	keep if type == `clase'

**### Personas
	*global personas13 "edad woman estado_civil_* lee_escribe educacion main_job_agri tot_trabajos organizacion enfermedad fexpers"

	*Descriptive statistics
	estpost sum ${personas13}
	matrix  personas_c = e(count)
	svmat   personas_c
	matrix  personas_m = e(mean)
	svmat   personas_m
	matrix  personas_s = e(sd)
	svmat   personas_s
	matrix  personas_i = e(min)
	svmat   personas_i
	matrix  personas_a = e(max)
	svmat   personas_a

	*Matriz
	matrix  personas = [personas_c \ personas_m \ personas_s \ personas_i \ personas_a]
	matrix  rownames personas = count mean sd min max 
	matrix  colnames personas = Edad Mujer EC_UnionLibre EC_Casado EC_Divorciado EC_Viudo EC_Soltero LeeyEscribe Educacion TrabajoPrincipalAgropecuario TotalTrabajos Organizacion Enfermedad FEX_Personas
	matrix  personasT = personas'
	matlist personasT
	putexcel set "${tables}\Covariables_class`clase'", sheet(personas) modify
	putexcel A2=matrix(personasT), names

	
**### Cónyuges
	*global conyuge13 "conyuge edad_cyg woman_cyg lee_escribe_cyg educacion_cyg main_job_agri_cyg tot_trabajos_cyg organizacion_cyg"

	*Descriptive statistics
	estpost sum ${conyuge13}
	matrix  conyuge_c = e(count)
	svmat   conyuge_c
	matrix  conyuge_m = e(mean)
	svmat   conyuge_m
	matrix  conyuge_s = e(sd)
	svmat   conyuge_s
	matrix  conyuge_i = e(min)
	svmat   conyuge_i
	matrix  conyuge_a = e(max)
	svmat   conyuge_a

	*Matriz
	matrix  conyuge = [conyuge_c \ conyuge_m \ conyuge_s \ conyuge_i \ conyuge_a]
	matrix  rownames conyuge = count mean sd min max 
	matrix  colnames conyuge = Conyuge Edad Mujer LeeyEscribe Educacion TrabajoPrincipalAgropecuario TotalTrabajos Organizacion
	matrix  conyugeT = conyuge'
	matlist conyugeT
	putexcel set "${tables}\Covariables_class`clase'", sheet(conyuge) modify
	putexcel A2=matrix(conyugeT), names

	
**### Hogar
	*global cov_hogar13 "t_personas sp_energia sp_acueducto sp_alcantarillado n_internet medio_transporte transporte_minutos ingmensual_* inganual_* empshare_* incshare_* gastmensual_all gastanual_all act_seghogar act_segcosechas riqueza_pca credito_rechazado mala_histcrediticia programas_ayudas fexhog dpto mpio consecutivo_c "

	*Descriptive statistics
	estpost sum ${cov_hogar13}
	matrix  hogares_c = e(count)
	svmat   hogares_c
	matrix  hogares_m = e(mean)
	svmat   hogares_m
	matrix  hogares_s = e(sd)
	svmat   hogares_s
	matrix  hogares_i = e(min)
	svmat   hogares_i
	matrix  hogares_a = e(max)
	svmat   hogares_a

	*Matriz
	matrix  hogares = [hogares_c \ hogares_m \ hogares_s \ hogares_i \ hogares_a]
	matrix  rownames hogares = count mean sd min max 
	matrix  colnames hogares = TotalPersonas SP_Energia SP_Acueducto SP_Alcantarillado SP_Internet MedioTransporteCabecera Transporte_Minutos IncMensual_Otros IncMensual_Agropecuario IncMensual_NoAgropecuario IncMensual_Total IncAnual_Otros IncAnual_Agropecuario IncAnual_NoAgropecuario IncAnual_Total Emp_Agropecuario Emp_NoAgropecuario IncShare_Agropecuario IncShare_NoAgropecuario IncShare_Otros GastosMensuales GastosAnuales SegurosHogar SegurosCosechas RiquezaPCA Credito_Rechazado MalaHistoriaCrediticia ProgramasoAyudas FEX_Hogar Departamento Municipio Comunidad
	matrix  hogaresT = hogares'
	matlist hogaresT
	putexcel set "${tables}\Covariables_class`clase'", sheet(hogares) modify
	putexcel A2=matrix(hogaresT), names


**### Activos
	*global cov_activos "n_bueyes n_vacas n_cerdos n_avescorral n_caballos n_ovejas n_colmenas n_otros_anim"

	*Descriptive statistics
	estpost sum ${cov_activos}
	matrix  activos_c = e(count)
	svmat   activos_c
	matrix  activos_m = e(mean)
	svmat   activos_m
	matrix  activos_s = e(sd)
	svmat   activos_s
	matrix  activos_i = e(min)
	svmat   activos_i
	matrix  activos_a = e(max)
	svmat   activos_a

	*Matriz
	matrix  activos = [activos_c \ activos_m \ activos_s \ activos_i \ activos_a]
	matrix  rownames activos = count mean sd min max  
	matrix  colnames activos = Bueyes Vacas Cerdos AvesCorral Caballos Ovejas Colmenas OtrosAnimales
	matrix  activosT = activos'
	matlist activosT
	putexcel set "${tables}\Covariables_class`clase'", sheet(activos) modify
	putexcel A2=matrix(activosT), names


**### Tierras
	*global cov_tierras13 "propietario asocio tipoTenencia_* totpred_* dadasPerdidasVendidas tamano tamano_* p_* class_tamano fuentes_agua* vr_inverHecha invd_*"

	*Descriptive statistics
	estpost sum ${cov_tierras13}
	matrix  tierras_c = e(count)
	svmat   tierras_c
	matrix  tierras_m = e(mean)
	svmat   tierras_m
	matrix  tierras_s = e(sd)
	svmat   tierras_s
	matrix  tierras_i = e(min)
	svmat   tierras_i
	matrix  tierras_a = e(max)
	svmat   tierras_a

	*Matriz
	matrix  tierras = [tierras_c \ tierras_m \ tierras_s \ tierras_i \ tierras_a]
	matrix  rownames tierras = count mean sd min max 
	matrix  colnames tierras = Propietario Asocio Tenencia_Posesion Tenencia_Herencia Tenencia_Arriendo Tenencia_Aparceria Tenencia_Usufructo Tenencia_Empeno Tenencia_Comodato Tenencia_Compania Tot_PrediosUtilizados Tot_PrediosOtros Ha_UPA Ha_CultivosPermanentes Ha_CultivosTransitorios Ha_CultivosMixtos Ha_Ganaderia Ha_Pastos Ha_Bosques Ha_OtrosUsos Ha_TierraNoUsada Share_CultivosPermanentes Share_CultivosTransitorios Share_CultivosMixtos Share_Ganaderia Share_Pastos Share_Bosques Share_OtrosUsos Share_TierraNoUsada Clasificacion_Tamano Fuentes_Agua_Pro Fuentes_Agua_Ext Fuentes_Agua Vr_InversionHecha Inv_Riego Inv_Estructuras Inv_Conservacion Inv_Frutales Inv_Maderables Inv_OtrosCiales Inv_Vivienda Inv_DesNat Inv_Otra
	matrix  tierrasT = tierras'
	matlist tierrasT
	putexcel set "${tables}\Covariables_class`clase'", sheet(tierras) modify
	putexcel A2=matrix(tierrasT), names


**### Producción
	*global cov_produccion13 "ing_* gastprom_* sitioVenta_*"

	*Descriptive statistics
	estpost sum ${cov_produccion13}
	matrix  produccion_c = e(count)
	svmat   produccion_c
	matrix  produccion_m = e(mean)
	svmat   produccion_m
	matrix  produccion_s = e(sd)
	svmat   produccion_s
	matrix  produccion_i = e(min)
	svmat   produccion_i
	matrix  produccion_a = e(max)
	svmat   produccion_a

	*Matriz
	matrix  produccion = [produccion_c \ produccion_m \ produccion_s \ produccion_i \ produccion_a]
	matrix  rownames produccion = count mean sd min max 
	matrix  colnames produccion = Ingreso_Agricola Ingreso_Pecuario Ingreso_Agropecuario Gasto_Agricola Gasto_Pecuario  Gasto_AsiTec Gasto_ManoObra  Gasto_Transp Gasto_Agropecuario SitVen_Finca SitVen_Vereda SitVen_OtraVereda SitVen_Cabecera SitVen_Otracabecera SitVen_OtroGasto_Agropecuario Gasto_ManoObra
	matrix  produccionT = produccion'
	matlist produccionT
	putexcel set "${tables}\Covariables_class`clase'", sheet(produccion) modify
	putexcel A2=matrix(produccionT), names


**### Comunidad
	*global comunidad13 "pp_faltacap acceso alquila_maquinaria seguridad solidaridad"

	*Descriptive statistics
	estpost sum ${comunidad13}
	matrix  comunidad_c = e(count)
	svmat   comunidad_c
	matrix  comunidad_m = e(mean)
	svmat   comunidad_m
	matrix  comunidad_s = e(sd)
	svmat   comunidad_s
	matrix  comunidad_i = e(min)
	svmat   comunidad_i
	matrix  comunidad_a = e(max)
	svmat   comunidad_a

	*Matriz
	matrix  comunidad = [comunidad_c \ comunidad_m \ comunidad_s \ comunidad_i \ comunidad_a]
	matrix  rownames comunidad = count mean sd min max 
	matrix  colnames comunidad = Problema_FaltaCapacitacion Acceso Vereda_AlquilaMaquinaria Seguridad Solidaridad
	matrix  comunidadT = comunidad'
	matlist comunidadT
	putexcel set "${tables}\Covariables_class`clase'", sheet(comunidad) modify
	putexcel A2=matrix(comunidadT), names

	restore
}
*/



			* ================================== *
**#					4. SIMPLE INDICATORS
			* ================================== *

* ------------------ *
**## General
* ------------------ *
use "${data}\Indicadores.dta", clear

**### Robustness
** General **
 estpost sum R_*_v2
matrix  robustness_co = e(count)
svmat   robustness_co
matrix  robustness_m  = e(mean)
svmat   robustness_m
matrix  robustness_sd = e(sd)
svmat   robustness_sd
matrix  robustness_i = e(min)
svmat   robustness_i
matrix  robustness_a = e(max)
svmat   robustness_a
** Joining matrixes **
matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
matrix  rownames robustness = count mean sd min max 
matrix  robustness= robustness'
matlist robustness
putexcel set "${tables}\Indicadores", sheet(Indicators_gen) modify
putexcel A2=matrix(robustness), names


**### Adaptation
** General **
 estpost sum A_*_v2
matrix  adaptation_co = e(count)
svmat   adaptation_co
matrix  adaptation_m  = e(mean)
svmat   adaptation_m
matrix  adaptation_sd = e(sd)
svmat   adaptation_sd
matrix  adaptation_i = e(min)
svmat   adaptation_i
matrix  adaptation_a = e(max)
svmat   adaptation_a
** Joining matrixes **
matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
matrix  rownames adaptation = count mean sd min max 
matrix  adaptation= adaptation'
matlist adaptation
putexcel set "${tables}\Indicadores", sheet(Indicators_gen) modify
putexcel H2=matrix(adaptation), names


**### Transformation
** General **
 estpost sum T_*_v2
matrix  transformation_co = e(count)
svmat   transformation_co
matrix  transformation_m  = e(mean)
svmat   transformation_m
matrix  transformation_sd = e(sd)
svmat   transformation_sd
matrix  transformation_i = e(min)
svmat   transformation_i
matrix  transformation_a = e(max)
svmat   transformation_a
** Joining matrixes **
matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
matrix  rownames transformation = count mean sd min max 
matrix  transformation = transformation'
matlist transformation
putexcel set "${tables}\Indicadores", sheet(Indicators_gen) modify
putexcel O2=matrix(transformation), names


* ------------------ *
**## By treatment
* ------------------ *
use "${data}\Indicadores.dta", clear
merge 1:1 llave_13 using "${data}\BaseTreatmentIds.dta", keep(3)

forvalues i=0/1 {
	preserve
	keep if treat13==`i'

	**### Robustness
	** General **
	 estpost sum R_*
	matrix  robustness_co = e(count)
	svmat   robustness_co
	matrix  robustness_m  = e(mean)
	svmat   robustness_m
	matrix  robustness_sd = e(sd)
	svmat   robustness_sd
	matrix  robustness_i = e(min)
	svmat   robustness_i
	matrix  robustness_a = e(max)
	svmat   robustness_a
	** Joining matrixes **
	matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
	matrix  rownames robustness = count mean sd min max 
	matrix  robustness= robustness'
	matlist robustness
	putexcel set "${tables}\Indicadores", sheet(Indicators_gen_treat`i') modify
	putexcel A2=matrix(robustness), names


	**### Adaptation
	** General **
	 estpost sum A_*
	matrix  adaptation_co = e(count)
	svmat   adaptation_co
	matrix  adaptation_m  = e(mean)
	svmat   adaptation_m
	matrix  adaptation_sd = e(sd)
	svmat   adaptation_sd
	matrix  adaptation_i = e(min)
	svmat   adaptation_i
	matrix  adaptation_a = e(max)
	svmat   adaptation_a
	** Joining matrixes **
	matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
	matrix  rownames adaptation = count mean sd min max 
	matrix  adaptation= adaptation'
	matlist adaptation
	putexcel set "${tables}\Indicadores", sheet(Indicators_gen_treat`i') modify
	putexcel H2=matrix(adaptation), names


	**### Transformation
	** General **
	 estpost sum T_*
	matrix  transformation_co = e(count)
	svmat   transformation_co
	matrix  transformation_m  = e(mean)
	svmat   transformation_m
	matrix  transformation_sd = e(sd)
	svmat   transformation_sd
	matrix  transformation_i = e(min)
	svmat   transformation_i
	matrix  transformation_a = e(max)
	svmat   transformation_a
	** Joining matrixes **
	matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
	matrix  rownames transformation = count mean sd min max 
	matrix  transformation = transformation'
	matlist transformation
	putexcel set "${tables}\Indicadores", sheet(Indicators_gen_treat`i') modify
	putexcel O2=matrix(transformation), names

	restore
}


* ---------------------- *
**## By farm type
* ---------------------- *
use "${data}\Indicadores.dta", clear
forvalues clase=1(1)3 {
	preserve
	replace type = 3 if type==0
	keep if type == `clase'

	**### Robustness
	** General **
	 estpost sum R_*
	matrix  robustness_co = e(count)
	svmat   robustness_co
	matrix  robustness_m  = e(mean)
	svmat   robustness_m
	matrix  robustness_sd = e(sd)
	svmat   robustness_sd
	matrix  robustness_i = e(min)
	svmat   robustness_i
	matrix  robustness_a = e(max)
	svmat   robustness_a
	** Joining matrixes **
	matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
	matrix  rownames robustness = count mean sd min max 
	matrix  robustness= robustness'
	matlist robustness
	putexcel set "${tables}\Indicadores", sheet(Indicators_type`clase') modify
	putexcel A2=matrix(robustness), names
		

	**### Adaptation
	** General **
	 estpost sum A_*
	matrix  adaptation_co = e(count)
	svmat   adaptation_co
	matrix  adaptation_m  = e(mean)
	svmat   adaptation_m
	matrix  adaptation_sd = e(sd)
	svmat   adaptation_sd
	matrix  adaptation_i = e(min)
	svmat   adaptation_i
	matrix  adaptation_a = e(max)
	svmat   adaptation_a
	** Joining matrixes **
	matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
	matrix  rownames adaptation = count mean sd min max 
	matrix  adaptation= adaptation'
	matlist adaptation
	putexcel set "${tables}\Indicadores", sheet(Indicators_type`clase') modify
	putexcel H2=matrix(adaptation), names


	**### Transformation
	** General **
	 estpost sum T_*
	matrix  transformation_co = e(count)
	svmat   transformation_co
	matrix  transformation_m  = e(mean)
	svmat   transformation_m
	matrix  transformation_sd = e(sd)
	svmat   transformation_sd
	matrix  transformation_i = e(min)
	svmat   transformation_i
	matrix  transformation_a = e(max)
	svmat   transformation_a
	** Joining matrixes **
	matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
	matrix  rownames transformation = count mean sd min max 
	matrix  transformation = transformation'
	matlist transformation
	putexcel set "${tables}\Indicadores", sheet(Indicators_type`clase') modify
	putexcel O2=matrix(transformation), names

	restore
}


* ---------------------- *
**## By region
* ---------------------- *
/*
use "${data}\Indicadores.dta", clear
merge 1:1 llave_ID_lb using "${data}\BaseUPA.dta", nogen keepusing(region)
drop *v1
foreach v of varlist R_* A_* T_* {
	gen `v'_AM = `v' if region==6
	gen `v'_CB = `v' if region==7
	gen `v'_EC = `v' if region==8
	gen `v'_CO = `v' if region==9
}
drop *v2


**### Robustness
** General **
*estpost sum R_*_v1 R_*_v2
 estpost sum R_*
matrix  robustness_co = e(count)
svmat   robustness_co
matrix  robustness_m  = e(mean)
svmat   robustness_m
matrix  robustness_sd = e(sd)
svmat   robustness_sd
matrix  robustness_i = e(min)
svmat   robustness_i
matrix  robustness_a = e(max)
svmat   robustness_a
** Joining matrixes **
matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
matrix  rownames robustness = count mean sd min max 
matrix  robustness= robustness'
matlist robustness
putexcel set Indicadores, sheet(Indicators_region) modify
putexcel A2=matrix(robustness), names
	

**### Adaptation
** General **
*estpost sum A_*_v1 A_*_v2
 estpost sum A_*
matrix  adaptation_co = e(count)
svmat   adaptation_co
matrix  adaptation_m  = e(mean)
svmat   adaptation_m
matrix  adaptation_sd = e(sd)
svmat   adaptation_sd
matrix  adaptation_i = e(min)
svmat   adaptation_i
matrix  adaptation_a = e(max)
svmat   adaptation_a
** Joining matrixes **
matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
matrix  rownames adaptation = count mean sd min max 
matrix  adaptation= adaptation'
matlist adaptation
putexcel set Indicadores, sheet(Indicators_region) modify
putexcel H2=matrix(adaptation), names


**### Transformation
** General **
*estpost sum T_*_v1 T_*_v2
 estpost sum T_*
matrix  transformation_co = e(count)
svmat   transformation_co
matrix  transformation_m  = e(mean)
svmat   transformation_m
matrix  transformation_sd = e(sd)
svmat   transformation_sd
matrix  transformation_i = e(min)
svmat   transformation_i
matrix  transformation_a = e(max)
svmat   transformation_a
** Joining matrixes **
matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
matrix  rownames transformation = count mean sd min max 
matrix  transformation = transformation'
matlist transformation
putexcel set Indicadores, sheet(Indicators_region) modify
putexcel O2=matrix(transformation), names
*/



			* ================================== *
**#				  5. COMPOSITE INDICATORS
			* ================================== *

* ------------------ *
**## General
* ------------------ *
use "${data}\indicadoresCompuestos.dta", clear
keep llave_ID_lb llave_13 llave_16 *ci*

**### Robustness
** General **
 estpost sum R_*
matrix  robustness_co = e(count)
svmat   robustness_co
matrix  robustness_m  = e(mean)
svmat   robustness_m
matrix  robustness_sd = e(sd)
svmat   robustness_sd
matrix  robustness_i = e(min)
svmat   robustness_i
matrix  robustness_a = e(max)
svmat   robustness_a
** Joining matrixes **
matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
matrix  rownames robustness = count mean sd min max 
matrix  robustness= robustness'
matlist robustness
putexcel set "${tables}\Indicadores", sheet(CompositeIndicators) modify
putexcel A2=matrix(robustness), names


**### Adaptation
** General **
 estpost sum A_*
matrix  adaptation_co = e(count)
svmat   adaptation_co
matrix  adaptation_m  = e(mean)
svmat   adaptation_m
matrix  adaptation_sd = e(sd)
svmat   adaptation_sd
matrix  adaptation_i = e(min)
svmat   adaptation_i
matrix  adaptation_a = e(max)
svmat   adaptation_a
** Joining matrixes **
matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
matrix  rownames adaptation = count mean sd min max 
matrix  adaptation= adaptation'
matlist adaptation
putexcel set "${tables}\Indicadores", sheet(CompositeIndicators) modify
putexcel H2=matrix(adaptation), names


**### Transformation
** General **
 estpost sum T_*
matrix  transformation_co = e(count)
svmat   transformation_co
matrix  transformation_m  = e(mean)
svmat   transformation_m
matrix  transformation_sd = e(sd)
svmat   transformation_sd
matrix  transformation_i = e(min)
svmat   transformation_i
matrix  transformation_a = e(max)
svmat   transformation_a
** Joining matrixes **
matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
matrix  rownames transformation = count mean sd min max 
matrix  transformation = transformation'
matlist transformation
putexcel set "${tables}\Indicadores", sheet(CompositeIndicators) modify
putexcel O2=matrix(transformation), names


* ------------------ *
**## By treatment
* ------------------ *
use "${data}\indicadoresCompuestos.dta", clear
keep llave_ID_lb llave_13 llave_16 *ci*
merge 1:1 llave_13 using "${data}\BaseTreatmentIds.dta", keep(3)

forvalues i=0/1 {
	preserve
	keep if treat13==`i'

	**### Robustness
	** General **
	 estpost sum R_*
	matrix  robustness_co = e(count)
	svmat   robustness_co
	matrix  robustness_m  = e(mean)
	svmat   robustness_m
	matrix  robustness_sd = e(sd)
	svmat   robustness_sd
	matrix  robustness_i = e(min)
	svmat   robustness_i
	matrix  robustness_a = e(max)
	svmat   robustness_a
	** Joining matrixes **
	matrix  robustness = [robustness_co \ robustness_m \ robustness_sd \ robustness_i \ robustness_a]
	matrix  rownames robustness = count mean sd min max 
	matrix  robustness= robustness'
	matlist robustness
	putexcel set "${tables}\Indicadores", sheet(CompositeIndicators_treat`i') modify
	putexcel A2=matrix(robustness), names


	**### Adaptation
	** General **
	 estpost sum A_*
	matrix  adaptation_co = e(count)
	svmat   adaptation_co
	matrix  adaptation_m  = e(mean)
	svmat   adaptation_m
	matrix  adaptation_sd = e(sd)
	svmat   adaptation_sd
	matrix  adaptation_i = e(min)
	svmat   adaptation_i
	matrix  adaptation_a = e(max)
	svmat   adaptation_a
	** Joining matrixes **
	matrix  adaptation = [adaptation_co \ adaptation_m \ adaptation_sd \ adaptation_i\ adaptation_a]
	matrix  rownames adaptation = count mean sd min max 
	matrix  adaptation= adaptation'
	matlist adaptation
	putexcel set "${tables}\Indicadores", sheet(CompositeIndicators_treat`i') modify
	putexcel H2=matrix(adaptation), names


	**### Transformation
	** General **
	 estpost sum T_*
	matrix  transformation_co = e(count)
	svmat   transformation_co
	matrix  transformation_m  = e(mean)
	svmat   transformation_m
	matrix  transformation_sd = e(sd)
	svmat   transformation_sd
	matrix  transformation_i = e(min)
	svmat   transformation_i
	matrix  transformation_a = e(max)
	svmat   transformation_a
	** Joining matrixes **
	matrix  transformation = [transformation_co \ transformation_m \ transformation_sd \ transformation_i \ transformation_a]
	matrix  rownames transformation = count mean sd min max 
	matrix  transformation = transformation'
	matlist transformation
	putexcel set "${tables}\Indicadores", sheet(CompositeIndicators_treat`i') modify
	putexcel O2=matrix(transformation), names

	restore
}