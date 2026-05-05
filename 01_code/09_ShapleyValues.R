# ---------------------------------------------------------------------------- #
#   Shapley Values
#   Author:       Raquel
#   Creation:     January 2025
#   Last edition: February 2025

# This script graphs the Shapley Values
# ---------------------------------------------------------------------------- #

library(dplyr); library(ggplot2); library(grf); library(haven); library(hrbrthemes)
library(iml); library(parallel)


rm(list=ls())

# Working directory
 setwd("C:/Users/userecon10/Desktop/Raquel Sofia Zapata/")
#setwd("C:/Users/rszap/OneDrive - Universidad Nacional de Colombia/Maestría/Climate_resilience")

#indicadores <- c("R_ci_1","R_ci_2","R_ci_3","A_ci_1","A_ci_2","A_ci_3","A_ci_4")
#indicadores <- c("A_ci_6") #R_ci_1 R_ci_2 R_ci_3 A_ci_1 A_ci_2 A_ci_3 A_ci_4 
#indicadores <- c("R_ci_3","A_ci_4")
#indicadores <- c("R_ci_3")
indicadores <- c("A_ci_4")
 
# Numerical variables
# "t_personas","l_transporte_minutos","incshare_agri","incshare_noagri","incshare_otro",
# "l_inganual_all","l_gastanual_all","edad","tot_trabajos","n_vacas","n_cerdos","n_avescorral",
# "n_caballos","n_ovejas","n_colmenas","tamano","dadasPerdidasVendidas","p_permanentes",
# "p_transitorios","p_mixtos","p_ganaderia","p_pastos","p_bosques","p_otros_usos","p_tierra_no_usada",
# "l_ing_agri","l_ing_pecu","l_gastprom_agri","l_gastprom_pecu","l_gastprom_asitec","l_gastprom_manobra",
# "l_gastprom_transp","td_pibAgrop","td_ocupados","td_tasaAfect"
 shapley_numerical <- c(
 "t_personas","incshare_agri","incshare_noagri","incshare_otro","edad",
 "tamano","td_pibAgrop","td_ocupados","td_tasaAfect")

# Categorical variables
# "region","sp_energia","sp_acueducto","sp_alcantarillado","n_internet","medio_transporte",
# "act_segcosechas","act_seghogar","credito_rechazado","programas_ayudas","woman","educacion",
# "main_job_agri","estadoCivil","organizacion","enfermedad","asocio","fuentes_agua",
# "tipoTenencia_1","tipoTenencia_4","tipoTenencia_6","tipoTenencia_7","tipoTenencia_8",
# "invd_1","invd_2","invd_3","invd_7","invd_8","invd_9","sitioVenta_1","sitioVenta_2","sitioVenta_3",
# "sitioVenta_4","sitioVenta_5"
 shapley_factor <- c(
 "region","sp_energia","sp_acueducto","sp_alcantarillado","n_internet","medio_transporte",
 "act_segcosechas","credito_rechazado","woman","educacion","tot_trabajos",
 "main_job_agri","estadoCivil","organizacion","asocio","fuentes_agua","tipoTenencia_1")

# Todas las variables
shapley_variables <- c(shapley_numerical,shapley_factor)



# ========================================================================= #
# Calling data ------------------------------------------------------------
# ========================================================================= #

# Auxiliar numbers
number_indicadores <- 8
number_auxvar      <- 3

# Number of cores for use
detectCores()
options(mc.cores = detectCores() - 2)

# Assigning colors
onecolor  <- "#2c7a27"
twocolors <- c("#ACACAC", "#2c7a27")

# Original data
 DF <- read_dta("./00. Processed data/CausalForest_database.dta")
#DF <- read_dta("./01_Data_cleaning/ELCA/CausalForest_database.dta")

# Observations, variables and covariables
number_observations <- length(DF[[1]])
number_variables    <- length(DF)-number_auxvar
number_covariables  <- length(DF)-number_indicadores-number_auxvar

# Training and test sets
taining.fraction <- 1 #0.8
df_train <- DF

# X_train
X_raw_train <- as.data.frame(df_train)
X_raw_train <- X_raw_train[1:number_covariables]

# Number of shapley observations
number_shapley_ob1 <- 5 #number_observations*0.05
number_shapley_ob2 <- number_observations*1



# ========================================================================= #
# Loop for resilience indicators ------------------------------------------
# ========================================================================= #

for (ind in indicadores) {
  #ind <- "R_ci_3"
  print(ind)
  
  # Iteration indicator
  Y <- as.vector(df_train[[ind]])
  
  # Causal forest RDS
  cf <- readRDS(paste0("./05_Tables/CF/", ind, "/3_CausalForestRDS.rds"))

  
  #' ----------------------------------------------------------------------
  ## Average variable contributions ---------------------------------------
  #' ----------------------------------------------------------------------
  #' An alternative for explaining individual predictions is a method from coalitional 
  #' game theory named Shapley value. Assume that for one data point, the feature values 
  #' play a game together, in which they get the prediction as a payout. The Shapley 
  #' value tells us how to fairly distribute the payout among the feature values.
  # print("Average variable contributions")
  # 
  # # Creating predictor object 
  # predictor_all <- Predictor$new(model = cf, data = X_raw_train, y = Y)
  # 
  # # Selecting sample
  # ## Option 1: all observations
  # #X_sample1 <- X_raw_train
  # ## Option 2: first n rows
  # #X_sample1 <- X_raw_train[1:number_shapley_ob1, ]
  # ## Option 3: random sample
  # sample_indices <- sample(1:nrow(X_raw_train), number_shapley_ob1)
  # X_sample1 <- X_raw_train[sample_indices,]
  # 
  # # Calculating Shapley values
  # shapley_all <- Shapley$new(predictor_all, x.interest = X_sample1)
  # 
  # # Saving data
  # saveRDS(shapley_all, paste0("./05_Tables/CF/", ind, "/8_ShapleyAll.rds"))
  # 
  # # Graph
  # #shapley_all <- readRDS(paste0("./05_Tables/CF/", ind, "/8_ShapleyAll.rds"))
  # png(paste0("./04_Plots/CF/", ind, "/8_ShapleyGeneral.png"), width = 1200, height = 800)
  # p <- plot(shapley_all)
  # print(p)
  # dev.off()
  
  
  #' ----------------------------------------------------------------------
  ## Individual variable contributions ------------------------------------
  #' ----------------------------------------------------------------------
  print("Individual variable contributions")
  
  # Creating predictor object 
  predictor_indiv <- Predictor$new(model = cf, data = X_raw_train, y = Y)
  
  # Selecting sample
  ## Option 1: all observations
  #X_sample2 <- X_raw_train
  ## Option 3: random sample
  sample_indices <- sample(1:nrow(X_raw_train), number_shapley_ob2)
  X_sample2 <- X_raw_train[sample_indices, ]
  
  # Shapley data object
  shapley_data <- list()
  
  
  ### Loop for observations -----------------------------------------------
  for (i in 1:number_shapley_ob2) {
    #i <- 2
    print(i)

    # Calculating shapley values for observation i
    shapley_indiv <- Shapley$new(predictor_indiv, x.interest = X_sample2[i,])$results


    #### Loop for variables -----------------------
    for (var in shapley_variables) {
      #var <- "t_personas"
      # Shapley values for variable var and observation i
      shapley_indiv_var <- shapley_indiv[shapley_indiv$feature == var, ] %>%
        select(c(phi, feature.value))
      # Adding to shapley data object
      shapley_data[[var]] <- rbind(shapley_data[[var]], shapley_indiv_var)
    }
  }

  # Saving shapley data object with variables of interest
  saveRDS(shapley_data, paste0("./05_Tables/CF/", ind, "/8_ShapleyData.rds"))
  
  
  ### Graphs ------------------------------------------------------------
  shapley_data <- readRDS(paste0("./05_Tables/CF/", ind, "/8_ShapleyData.rds"))
  
  #### Numerical variables -----------------------
  for (var in shapley_numerical) {
    # Adding column for variable value
    shapley_var <- shapley_data[[var]] %>% 
      mutate(value = as.numeric(sub(".*=", "", feature.value)))
    # Graph
    png(paste0("./04_Plots/CF/", ind, "/8_Shapley_", var, ".png"), width = 1200, height = 800)
    p <- shapley_var %>% 
      ggplot(aes(x = value, y = phi)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", color = onecolor, se = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
      labs(
        title = "",
        x = "",
        y = "Shapley Value"
      ) +
      theme_ipsum(axis_title_size = 20, axis_text_size = 20) +
      theme(legend.position = "none")
    print(p)
    dev.off()
  }
  
  #### Categorical variables -----------------------
  for (var in shapley_factor) {
    # Adding column for variable value
    shapley_var <- shapley_data[[var]] %>% 
      mutate(value = as.numeric(sub(".*=", "", feature.value)))
    # Graph
    png(paste0("./04_Plots/CF/", ind, "/8_Shapley_", var, ".png"), width = 1200, height = 800)
    p <- shapley_var %>% 
      ggplot(aes(x = factor(value), y = phi)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
      labs(
        title = "",
        x = "",
        y = "Shapley Value"
      ) +
      theme_ipsum(axis_title_size = 20, axis_text_size = 20) +
      theme(legend.position = "none")
    print(p)
    dev.off()
  }
}

