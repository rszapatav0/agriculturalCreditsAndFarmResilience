/* -----------------------------------------------------------------------------
Procesing Consumer Price Index
Author:       Raquel
Creation:     August 2023
Last edition: August 2023

This dofile organizes the CPI to merge with other databases.
----------------------------------------------------------------------------- */

clear all
global data "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data"


			* ======================== *
**#				  		 CPI
			* ======================== *

* Database *
 import excel "C:\Users\userecon10\Desktop\Raquel Sofia Zapata\00. Processed data\IPC.xlsx", sheet("Datos") firstrow clear
drop in 829/831

* Dates **
split Fecha, p(/)
rename   (Fecha1 Fecha2 Fecha3) (year mes dia)
destring year mes dia, replace
gen     period  = ym(year, mes)
gen     periodd = mdy(mes, dia, year)
format  period  %tm
format  periodd %td
drop    Fecha
*keep    if year>=2000
replace IPC = IPC/100

* CPI by day *
preserve
drop period
save  "${data}\IPC_diario.dta", replace
restore

* CPI by month *
preserve
collapse (mean) IPC year mes, by (period)
save  "${data}\IPC_mensual.dta", replace
restore

* CPI by month *
preserve
collapse (mean) IPC, by (year)
save  "${data}\IPC_anual.dta", replace
restore

