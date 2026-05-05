# ---------------------------------------------------------------------------- #
#   Covariables selection
#   Author:       Raquel
#   Creation:     January 2025
#   Last edition: February 2025

# This script selects the covariables for the causal forest.
# ---------------------------------------------------------------------------- #


# ========================================================================= #
# Packages    -------------------------------------------------------------
# ========================================================================= #

library(corrplot); library(dplyr); library(GGally); library(ggcorrplot); 
library(ggplot2); library(haven); library(kableExtra); library(knitr)



# ========================================================================= #
# Processing    -----------------------------------------------------------
# ========================================================================= #

 setwd("C:/Users/userecon10/Desktop/Raquel Sofia Zapata/")
#setwd("C:/Users/rszap/OneDrive - Universidad Nacional de Colombia/Maestría/Climate_resilience")

rm(list=ls())



# ========================================================================= #
# Reading database     ----------------------------------------------------
# ========================================================================= #

 data_all <- read_dta("./00. Processed data/CovariablesSelectionDatabase.dta")
#data_all <- read_dta("./01_Data_cleaning/ELCA/CovariablesSelectionDatabase.dta")

## Base variables a utilizar ----------------------------------------------
DF <- as.data.frame(data_all) %>% 
  select(
    #' HOUSEHOLD
    region, t_personas, sp_energia, sp_acueducto, sp_alcantarillado, n_internet,
    medio_transporte, l_transporte_minutos, #transporte_minutos, 
    empshare_agri, empshare_noagri,
    incshare_agri, incshare_noagri, incshare_otro,
    #ingmensual_otro, ingmensual_agri, ingmensual_noagri, ingmensual_all,
    #inganual_otro, inganual_agri, inganual_noagri, inganual_all,
    #gastmensual_all, gastanual_all,
    l_ingmensual_otro, l_ingmensual_agri, l_ingmensual_noagri, l_ingmensual_all,
    l_inganual_otro, l_inganual_agri, l_inganual_noagri, l_inganual_all,
    l_gastmensual_all,l_gastanual_all,
    act_segcosechas,  act_seghogar,
    credito_rechazado, mala_histcrediticia, programas_ayudas, riqueza_pca,
    
    #' PEOPLE (HOUSEHOLD HEAD) / PERSONAS (JEFE)
    lee_escribe, edad, woman,
    educacion, tot_trabajos, main_job_agri,
    estadoCivil, conyuge,
    organizacion, enfermedad, 
    
    #' ASSETS
    n_bueyes, n_vacas, n_cerdos, n_avescorral, n_caballos, n_ovejas, n_colmenas, n_otros_anim,

    #' LANDS
    class_tamano, tamano, 
    asocio, propietario, totpred_fincas, dadasPerdidasVendidas,
    fuentes_agua_pro, fuentes_agua_ext, fuentes_agua,
    tipoTenencia_1, tipoTenencia_6, tipoTenencia_8, tipoTenencia_7,
    tamano_permanentes, tamano_transitorios, tamano_mixtos, tamano_ganaderia,
    tamano_pastos, tamano_bosques, tamano_otros_usos, tamano_tierra_no_usada,
    p_permanentes, p_transitorios, p_mixtos, p_ganaderia, p_pastos, p_bosques,
    p_otros_usos, p_tierra_no_usada,
    
    #' PRODUCTION
    #ing_agri, ing_pecu, ing_agrop,
    #gastprom_agri, gastprom_pecu, gastprom_agrop,
    #gastprom_asitec, gastprom_manobra, gastprom_transp,
    #gastprom_semilla, gastprom_maqui, gastprom_fertz, gastprom_insec, gastprom_cria, gastprom_alim, gastprom_vacu, gastprom_drog,
    #gastprom_vitam, gastprom_otrosg,
    l_ing_agri, l_ing_pecu, l_ing_agrop,
    l_gastprom_agri, l_gastprom_pecu, l_gastprom_agrop,
    l_gastprom_asitec, l_gastprom_manobra, l_gastprom_transp,
    
    #' LANDS 2.0
    #vr_inverHecha, vr_inverResil1, vr_inverResil2
    l_vr_inverHecha, l_vr_inverResil1, l_vr_inverResil2,
    invd_1, invd_2, invd_3, invd_7, invd_8, invd_9,
    
    #' PRODUCTION 2.0
    sitioVenta_1, sitioVenta_2, sitioVenta_3, sitioVenta_4, sitioVenta_5,
    
    #' COMMUNITY
    #alquila_maquinaria, pp_faltacap, acceso, seguridad, solidaridad,
    td_pibAgrop, td_ocupados, td_tasaAfect,
    
    #' INDICATORS
    R_ci_1, R_ci_2, R_ci_3,
    A_ci_1, A_ci_2, A_ci_3, A_ci_4,
    T_ci_1,
    
    #' GENERAL
    #dpto, mpio, consecutivo_c,
    fexhog, llave_ID_lb,
    treat13
) %>% 
  na.omit()

## Covariables adjustments ------------------------------------------------
#Number of indicators
number_indicadores <- 8
#Number of auxiliar variables
number_auxvar      <- 3
#Number of covariables
number_variables    <- length(DF) - number_indicadores - number_auxvar

## Descriptive statistics -------------------------------------------------
X_corr <- DF[, 1:number_variables]

summ_stats <- fBasics::basicStats(X_corr)
summ_stats <- as.data.frame(t(summ_stats))
summ_stats <- summ_stats %>% 
  select("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum") %>%
  rename('No. Obs.'='nobs', 'St. Dev.'='Stdev', 'Lower quartile'='1. Quartile', 'Upper quartile'='3. Quartile')

### Printing in HTML
summ_stats_table <- kable(summ_stats, 'html', digits=3)
kable_styling(summ_stats_table,
              bootstrap_options=c("striped", "hover", "condensed", "responsive"),
              full_width=FALSE) %>%
  save_kable("./03_Analysis/covariablesSelection/1_DescriptiveStatistics.html")


## Correlations by modules ------------------------------------------------

### Hogar -----------------------------------------------------------------
X_corr_hogar <- X_corr %>% select(
  region, t_personas, sp_energia, sp_acueducto, sp_alcantarillado, n_internet,
  medio_transporte, l_transporte_minutos, #transporte_minutos, 
  empshare_agri, empshare_noagri,
  incshare_agri, incshare_noagri, incshare_otro,
  #ingmensual_otro, ingmensual_agri, ingmensual_noagri, ingmensual_all,
  #inganual_otro, inganual_agri, inganual_noagri, inganual_all,
  #gastmensual_all, gastanual_all,
  l_ingmensual_otro, l_ingmensual_agri, l_ingmensual_noagri, l_ingmensual_all,
  l_inganual_otro, l_inganual_agri, l_inganual_noagri, l_inganual_all,
  l_gastmensual_all,l_gastanual_all,
  act_segcosechas,  act_seghogar,
  credito_rechazado, mala_histcrediticia, programas_ayudas, riqueza_pca,
)

#Pairwise
pairwise_hogar <- psych::corr.test(X_corr_hogar,X_corr_hogar)$p
png("./03_Analysis/covariablesSelection/pairwise_hogar.png", width = 1800, height = 1200)
corrplot(cor(X_corr_hogar),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_hogar,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotHogar.png", width = 4200, height = 2800)
ggpairs(X_corr_hogar)
dev.off()

# Deleting variables
DF <- DF %>% select(-c(
  empshare_agri, empshare_noagri, #Ya esta la proporcion en ingresos
  l_ingmensual_otro, l_ingmensual_agri, l_ingmensual_noagri, l_ingmensual_all, #Se van a conservar los valores anuales
  l_inganual_otro, l_inganual_agri, l_inganual_noagri, #Se va a conservar solo el total, para lo otro quedan las proporciones
  l_gastmensual_all, #Se van a conservar los valores anuales
  mala_histcrediticia, #Ya esta comprendida en credito_rechazado
  riqueza_pca #Es una combinacion de las otras variables
))


### People ----------------------------------------------------------------
X_corr_personas <- X_corr %>% select(
  lee_escribe, edad, woman,
  educacion, tot_trabajos, main_job_agri,
  estadoCivil, conyuge,
  organizacion, enfermedad
)

#Pairwise
pairwise_personas <- psych::corr.test(X_corr_personas,X_corr_personas)$p
png("./03_Analysis/covariablesSelection/pairwise_personas.png", width = 1800, height = 1200)
corrplot(cor(X_corr_personas),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_personas,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotPersonas.png", width = 4200, height = 2800)
ggpairs(X_corr_personas)
dev.off()

# Deleting variables
DF <- DF %>% select(-c(
  lee_escribe, #More detailed at the educational level
  conyuge #More details on marital status
))


### Assets -----------------------------------------------------------------
X_corr_activos <- X_corr %>% select(
  n_bueyes, n_vacas, n_cerdos, n_avescorral, n_caballos, n_ovejas, n_colmenas, n_otros_anim,
)

#Pairwise
pairwise_activos <- psych::corr.test(X_corr_activos,X_corr_activos)$p
png("./03_Analysis/covariablesSelection/pairwise_activos.png", width = 1800, height = 1200)
corrplot(cor(X_corr_activos),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_activos,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotActivos.png", width = 4200, height = 2800)
ggpairs(X_corr_activos)
dev.off()

# Deleting variables
DF <- DF %>% select(-c(
  n_bueyes, #Few have them
  n_otros_anim #Does not provide accurate information
))


### Lands -----------------------------------------------------------------
X_corr_tierras <- X_corr %>% select(
  class_tamano, tamano, 
  asocio, propietario, totpred_fincas, dadasPerdidasVendidas,
  fuentes_agua_pro, fuentes_agua_ext, fuentes_agua,
  tipoTenencia_1, tipoTenencia_6, tipoTenencia_7, tipoTenencia_8,
  tamano_permanentes, tamano_transitorios, tamano_mixtos, tamano_ganaderia,
  tamano_pastos, tamano_bosques, tamano_otros_usos, tamano_tierra_no_usada,
  p_permanentes, p_transitorios, p_mixtos, p_ganaderia, p_pastos, p_bosques,
  p_otros_usos, p_tierra_no_usada,
  l_vr_inverHecha, l_vr_inverResil1, l_vr_inverResil2,
  invd_1, invd_2, invd_3, invd_7, invd_8, invd_9,
)

#Pairwise
pairwise_tierras <- psych::corr.test(X_corr_tierras,X_corr_tierras)$p
png("./03_Analysis/covariablesSelection/pairwise_tierras.png", width = 1800, height = 1200)
corrplot(cor(X_corr_tierras),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_tierras,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotTierras.png", width = 4200, height = 2800)
ggpairs(X_corr_tierras)
dev.off()

# Deleting variables
DF <- DF %>% select(-c(
  totpred_fincas, #Does not provide information
  propietario, #It's already in tipo_tenencia_1
  fuentes_agua_pro, fuentes_agua_ext, #A single measure for water
  class_tamano, #It's on the farm the size
  tamano_permanentes, tamano_transitorios, tamano_mixtos, tamano_ganaderia,
  tamano_pastos, tamano_bosques, tamano_otros_usos, tamano_tierra_no_usada,
  l_vr_inverHecha, l_vr_inverResil1, l_vr_inverResil2, #They are related to investments
))


### Production ------------------------------------------------------------
X_corr_produccion <- X_corr %>% select(
  l_ing_agri, l_ing_pecu, l_ing_agrop,
  l_gastprom_agri, l_gastprom_pecu, l_gastprom_agrop,
  l_gastprom_asitec, l_gastprom_manobra, l_gastprom_transp, 
  sitioVenta_1, sitioVenta_2, sitioVenta_3, sitioVenta_4, sitioVenta_5
)

#Pairwise
pairwise_produccion <- psych::corr.test(X_corr_produccion,X_corr_produccion)$p
png("./03_Analysis/covariablesSelection/pairwise_produccion.png", width = 1800, height = 1200)
corrplot(cor(X_corr_produccion),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_produccion,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotProduccion.png", width = 4200, height = 2800)
ggpairs(X_corr_produccion)
dev.off()

# Deleting variables
DF <- DF %>% select(-c(
  l_ing_agrop, l_gastprom_agrop, #They're already in agriculture + livestock
))


### Community -------------------------------------------------------------
X_corr_comunidad <- X_corr %>% select(
  td_pibAgrop, td_ocupados, td_tasaAfect
)

#Pairwise
pairwise_comunidad <- psych::corr.test(X_corr_comunidad,X_corr_comunidad)$p
png("./03_Analysis/covariablesSelection/pairwise_comunidad.png", width = 1800, height = 1200)
corrplot(cor(X_corr_comunidad),
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         p.mat=pairwise_comunidad,sig.level=0.05,number.font=8,insig="blank")
dev.off()

#Pairplot
png("./03_Analysis/covariablesSelection/pairplotComunidad.png", width = 4200, height = 2800)
ggpairs(X_corr_comunidad)
dev.off()

# Deleting variables
# DF <- DF %>% select(-c(
#   
# ))


## Organizing variable types -----------------------------------------
colnames(DF) <- make.names(colnames(DF), unique = TRUE)
sapply(DF, class)

# Variables type "double" to "numeric"
DF$tot_trabajos      <- as.numeric(DF$tot_trabajos)
DF$act_segcosechas   <- as.numeric(DF$act_segcosechas)
DF$n_internet        <- as.numeric(DF$n_internet)
DF$sp_alcantarillado <- as.numeric(DF$sp_alcantarillado)
DF$sp_acueducto      <- as.numeric(DF$sp_acueducto)
DF$sp_energia        <- as.numeric(DF$sp_energia)
DF$region            <- as.numeric(DF$region)
#DF$dpto              <- as.numeric(DF$dpto)
#DF$mpio              <- as.numeric(DF$mpio)



# ========================================================================= #
# Saving data -------------------------------------------------------------
# ========================================================================= #

#write_dta(DF, "./01_Data_cleaning/ELCA/CausalForest_database.dta")
 write_dta(DF, "./00. Processed data/CausalForest_database.dta")
