# ---------------------------------------------------------------------------- #
#   Causal Forest
#   Author:       Raquel
#   Creation:     March 2024
#   Last edition: February 2025

# This script grows the causal forests.
# ---------------------------------------------------------------------------- #


# ========================================================================= #
# Preprocesing ------------------------------------------------------------
# ========================================================================= #

rm(list=ls())
set.seed(1)

# Working directory
 setwd("C:/Users/userecon10/Desktop/Raquel Sofia Zapata/")
#setwd("C:/Users/rszap/OneDrive - Universidad Nacional de Colombia/Maestría/Climate_resilience")

# Random forest parameters
rf_trees    = 6000 #1200
rf_tn_reps  = 150 #30
rf_tn_trees = 600 #120
rf_tn_draws = 3000 #600

# Causal forest parameters
cf_trees    = 50000 #1200
cf_tn_reps  = 150 #30
cf_tn_trees = 600 #120
cf_tn_draws = 3000 #600

# Auxiliar numbers
number_indicadores <- 8
number_auxvar      <- 3

# Vector with names
#indicadores <- c("R_ci_1", "R_ci_2", "R_ci_3","A_ci_1","A_ci_2","A_ci_3","A_ci_4")
#indicadores <- c("A_ci_6") #R_ci_1 R_ci_2 R_ci_3 A_ci_1 A_ci_2 A_ci_3 A_ci_4 
indicadores <- c("R_ci_3","A_ci_4")


# Assigning colors
onecolor  <- "#2c7a27"
twocolors <- c("#ACACAC", "#2c7a27")


# Packages
library(aod); library(car); library(corrplot); library(DiagrammeR); library(DiagrammeRsvg)
library(dplyr); library(fBasics); library(GGally); library(ggplot2); library(grf)
library(haven); library(Hmisc); library(hrbrthemes); library(iml); library(kableExtra)
library(knitr); library(lmtest); library(parallel); library(psych); library(purrr);
library(sandwich); library(tibble); library(webshot2)
if (packageVersion("grf")<'0.10.2') {
  warning("This script requires grf 0.10.2 of higher")
}

# Number of cores for use
detectCores()
options(mc.cores = detectCores() - 2)



# ========================================================================= #
# Reading database --------------------------------------------------------
# ========================================================================= #

 DF <- read_dta("./00. Processed data/CausalForest_database.dta")
#DF <- read_dta("./01_Data_cleaning/ELCA/CausalForest_database.dta")


## Preparing variables -------------------------------------------------
#Observations
number_observations <- length(DF[[1]])
#Number of variables and covariables
number_variables    <- length(DF)-number_auxvar
number_covariables  <- length(DF)-number_indicadores-number_auxvar

#Covariables names
X_names <- c(
  ##HOUSEHOLD
  "Microregion", "People in the household", "Utilities: Electricity service", "Utilities: Aqueduct", "Utilities: Sewerage", "Utilities: Internet",
  "Transportation vehicle to municipal seat", "Log transport time (minutes) to municipal seat",
  "Agricultural employment income share",  "Non-Agricultural employment income share", "Other income share",
  "Log anual household income", "Log anual household expenditures",
  "Agricultural insurance", "Home insurance",
  "Rejected credit", "Programs or aids",

  ##HEAD OF HOUSEHOLD
  "Age", "Gender (women = 1)", "Education level",
  "Number of jobs", "Main job agricultural", "Civil status",
  "Union membership", "Permanent Condition or disease",

  ##LIVESTOCK UNITS
  "Number of cows", "Number of pigs", "Number of poultry", "Number of horses","Number of sheeps", "Number of hives",

  ##FARM CHARACTERISTICS
  "Farm size", "Land in association", "Given/lost/sold lands", "Water sources",
  "Land tenure: Possession or inheritance", "Land tenure: Lease", "Land tenure: Sharecropping",
  "Land tenure: Other",

  ##LAND USE
  "Area participation: Permanent crops", "Area participation: Transitional crops", "Area participation: Mixed crops",
  "Area participation: Livestock", "Area participation: Pasture", "Area participation: Forests", "Area participation: Other uses",
  "Area participation: Unused land",

  ##MATERIALS AND CAPITAL
  "Log anual crop income", "Log anual livestock income", "Log anual expenditure in crops", "Log anual expenditure in livestock",
  "Log anual expenditure in technical assistance", "Log anual expenditure in labor", "Log anual expenditure transport",
  "Investment: Irrigation", "Investment: Structure", "Investment: Conservation and planting",
  "Investment: Housing", "Investment: Natural disaster", "Investment: Other",

  ##INSTITUTIONAL FACTORS
  "Sales location: Farm", "Sales location: Community", "Sales location: Other community",
  "Sales location: Municipal center", "Sales location: Another municipality",
  "Municipal GDP agricultural activities", "Municipal Employed/Total population", "Municipal rate of people affected by environmental events",
  #"Region: Atlantica-Media", "Region: Cundi-Boyacense", "Region: Eje-Cafetero", "Region: Centro-Oriental",
  #"Access", "Rental of agricultural machinery", "Safety", "Solidarity",
  #"dpto_15", "dpto_23", "dpto_25", "dpto_63", "dpto_66", "dpto_68", "dpto_70", "dpto_73",
  #"mpio_15001", "mpio_15109", "mpio_15131", "mpio_15176", "mpio_15632", "mpio_15676", "mpio_15776", "mpio_15808", "mpio_23162", "mpio_23182", "mpio_23189", "mpio_23570", "mpio_23660", "mpio_23670", "mpio_23686", "mpio_25743", "mpio_25745", "mpio_25779", "mpio_25781", "mpio_25815", "mpio_25817", "mpio_63130", "mpio_63190", "mpio_63212", "mpio_63272", "mpio_63302", "mpio_63401", "mpio_63470", "mpio_63594", "mpio_63690", "mpio_66001", "mpio_66088", "mpio_66170", "mpio_66318", "mpio_66400", "mpio_66440", "mpio_66682", "mpio_66687", "mpio_68020", "mpio_68572", "mpio_70215", "mpio_70670", "mpio_73001", "mpio_73217", "mpio_73483", "mpio_73504", "mpio_73585"
  #"consecutivo_c",

  #INDICATORS
  "Robustness1", "Robustness2", "Robustness3",
  "Adaptation1", "Adaptation2", "Adaptation3", "Adaptation4",
  "Transformation"
)
#X_names <- names(DF[1:number_variables])

number_covariables_nombres  <- length(DF)-number_indicadores-number_auxvar
covariate_names <- X_names[1:number_covariables_nombres]


## Descriptive statistics -------------------------------------------------
X_corr <- DF[, 1:number_covariables]
colnames(X_corr) <- X_names[1:number_covariables]

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
  save_kable("./05_Tables/CF/1_DescriptiveStatistics.html")


## Correlations -----------------------------------------------------------
# Pairwise
pairwise_values <- psych::corr.test(X_corr,X_corr)$p
png("./04_Plots/CF/1a_CorrelationMap.png", width = 1800, height = 1200)
corrplot(cor(X_corr), p.mat=pairwise_values,
         type="upper",tl.col="black",order="hclust",tl.cex=1,addgrid.col="black",
         sig.level=0.05,number.font=8,insig="blank")
dev.off()

# Pairplot
png("./04_Plots/CF/1b_CorrelationMap_pairplot.png", width = 4200, height = 2800)
ggpairs(X_corr)
dev.off()


## Training and test sets -------------------------------------------------
taining.fraction <- 1 #0.8
#df_train <- sample_frac(DF, replace=F, size=taining.fraction)
#df_test  <- anti_join(DF, df_train, by="llave_ID_lb")
 df_train <- DF

# Weights
 sample_weights = NULL
#sample_weights = df_train$fexhog



# ========================================================================= #
# Propensity score e(x) ---------------------------------------------------
# ========================================================================= #

## Preparing names --------------------------------------------------------
# Treatment variable
W <- as.vector(df_train$treat13)

# Covariables
X_raw_train <- as.data.frame(df_train)
X_raw_train <- X_raw_train[1:number_covariables]


## Random forest for e(x) --------------------------------------------------
W_forest = regression_forest(
  X_raw_train, W,
  num.trees = rf_trees, sample.weights = sample_weights,
  tune.num.reps = rf_tn_reps, tune.num.trees = rf_tn_trees, tune.num.draws = rf_tn_draws,
  #tune.parameters = c("mtry", "min.node.size", "alpha", "imbalance.penalty", "sample.fraction"),
  #honesty.prune.leaves = F,
  #seed = 321612552,
  #honesty.fraction = 0.7)
  tune.parameters = "all")
# W_forest <- readRDS(paste0("./05_Tables/CF/2_PropensityScoreRDS.rds"))
# W_hat <- predict(W_forest)$predictions

# Saving forest
saveRDS(W_forest, "./05_Tables/CF/2_PropensityScoreRDS.rds")



## Propensity score results -----------------------------------------------
#' Overlap assumption: The propensity score must be away from 0 and 1.

# Table of statistics
summ_stats_ps <- fBasics::basicStats(W_hat)
summ_stats_ps <- as.data.frame(t(summ_stats_ps))
summ_stats_ps <- summ_stats_ps %>% 
  select("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum") %>%
  rename('No. Obs.'='nobs', 'St. Dev.'='Stdev', 'Lower quartile'='1. Quartile', 'Upper quartile'='3. Quartile')
## Printing in HTML
summ_stats_ps_table <- kable(summ_stats_ps, 'html', digits=3)
kable_styling(summ_stats_ps_table,
              bootstrap_options=c("striped", "hover", "condensed", "responsive"),
              full_width=FALSE) %>%
  save_kable("./05_Tables/CF/2_PropensityScore_DescriptiveStatistics.html")

# Propensity score histogram
## Labeling treatment
W_labels <- ifelse(W==0, "Control", "Treatment")
## Data
g2_data <- data.frame(
  Type  = W_labels,
  value = W_hat
)
## Graph
png("./04_Plots/CF/2_PropensityScore_Histogram.png", width = 1200, height = 800)
g2_data %>%
  ggplot(aes(x=value, fill=Type)) +
  geom_histogram(color="#FFFFFF", alpha = 0.4, position = 'identity') +
  scale_fill_manual(values=twocolors) +
  xlab("Propensity score") + ylab("Count") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20) +
  theme(legend.position = 'bottom', legend.text = element_text(size=25)) +
  labs(fill="")
dev.off()

# Confidence interval
## Data
W_hat_se <- sqrt(predict(W_forest, estimate.variance = TRUE)$variance.estimates)
W_hat_se_up <- W_hat + 1.96*W_hat_se
W_hat_se_lo <- W_hat - 1.96*W_hat_se
g3_data <- as.data.frame(cbind(W_hat, W_hat_se_up, W_hat_se_lo))
g3_data <- g3_data[order(W_hat),]
g3_data$id <- seq(1:length(W_hat))
## Graph
png("./04_Plots/CF/2_PropensityScore_CI.png", width = 1200, height = 800)
ggplot(g3_data, aes(x=id)) +
  geom_line(aes(y=W_hat_se_up), color="#ACACAC", size=0.5, linetype=2) +
  geom_line(aes(y=W_hat_se_lo), color="#ACACAC", size=0.5, linetype=2) +
  geom_line(aes(y=W_hat), color="black", size=1) +
  geom_line(aes(y=0), color=onecolor, size=1, linetype=2) +
  geom_line(aes(y=1), color=onecolor, size=1, linetype=2) +
  xlab("Observation (ordered by propensity score)") + 
  ylab("Propensity Score") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20)
dev.off()

# Variable importance graph
varimp_pf      <- variable_importance(forest=W_forest)
selectvar_pf   <- which(varimp_pf > mean(varimp_pf))
fill_lenght_pf <- length(selectvar_pf)
## Data
g4_data <- data.frame(
  name =X_names[c(selectvar_pf)],
  value=varimp_pf[c(selectvar_pf)]
)
## Graph
png("./04_Plots/CF/2_PropensityScore_varimp.png", width = 1200, height = 800)
ggplot(g4_data, aes(x=reorder(name, value),value)) +
  geom_bar(fill=onecolor, alpha=0.5, stat='identity', colour=onecolor, size=0.1) +
  xlab("") + ylab("Feature importance") +
  coord_flip() +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20)
dev.off()

# Variable importance table
var_imp_w      <- c(variable_importance(forest=W_forest))
names(var_imp_w) <- X_names[1:number_covariables]
sorted_var_imp_w <- sort(var_imp_w, decreasing = TRUE)
## Table
as.data.frame(sorted_var_imp_w, row.names = names(sorted_var_imp_w)) %>%
  kable("html", digits = 4, row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
  save_kable("./05_Tables/CF/2_PropensityScore_varimp.html")



# ========================================================================= #
# Loop for resilience indicators ------------------------------------------
# ========================================================================= #

for (ind in indicadores) {
  #ind <- "R_ci_3"
  #ind <- "A_ci_4"
  print(ind)
  
  # Iteration indicator
  Y <- as.vector(df_train[[ind]])
  
  #' ----------------------------------------------------------------------
  ## Random forest for m(x) -----------------------------------------------
  #' ----------------------------------------------------------------------
  print("Random forest for m(x)")
  
  Y_forest = regression_forest(
    X_raw_train, Y,
    num.trees = rf_trees, sample.weights = sample_weights,
    tune.num.reps = rf_tn_reps, tune.num.trees = rf_tn_trees, tune.num.draws = rf_tn_draws,
    #tune.parameters = c("mtry", "min.node.size", "alpha", "imbalance.penalty", "sample.fraction"),
    #honesty.prune.leaves = F,
    #seed = 321612552,
    #honesty.fraction = 0.7)
    tune.parameters = "all")
  # Y_forest <- readRDS(paste0("./05_Tables/CF/", ind, "/3_RandomForestRDS.rds"))
  # Y_hat <- predict(Y_forest)$predictions
  
  # Saving forest
  saveRDS(Y_forest, paste0("./05_Tables/CF/", ind, "/3_RandomForestRDS.rds"))
  
  
  
  #' ----------------------------------------------------------------------
  ## Causal Forest --------------------------------------------------------
  #' ----------------------------------------------------------------------
  print("Causal Forest")
  
  ### Step 1: Fit the forest ----------------------------------------------
  # Forest using response functions
  cf = causal_forest(
    X_raw_train, Y, W,
    Y.hat = Y_hat, W.hat = W_hat,
    num.trees = cf_trees, sample.weights = sample_weights,
    tune.num.reps = cf_tn_reps, tune.num.trees = cf_tn_trees, tune.num.draws = cf_tn_draws,
    #tune.parameters = c("mtry", "min.node.size", "alpha", "imbalance.penalty", "sample.fraction"),
    #sample.fraction = 0.5,
    #seed = 1147615082,
    #honesty.prune.leaves = F,
    #seed = 1391778055,
    #honesty.fraction = 0.7)
    tune.parameters = "all")
  # cf <- readRDS(paste0("./05_Tables/CF/", ind, "/3_CausalForestRDS.rds"))
  cf
  #View(cf)
  
  # Forest with only the important variables
  #Variable importance
  varimp       = variable_importance(forest=cf_raw)
  selected_idx = which(varimp > mean(varimp))
  X_train <- X_raw_train[selected_idx]
  X_raw_test  <- X_raw_test[selected_idx]
  cf = causal_forest(X_train, Y, W,
                     sample.weights = sample_weights,
                     tune.parameters = "all")
  cf = cf_raw
  
  # Saving forest
  saveRDS(cf, paste0("./05_Tables/CF/", ind, "/3_CausalForestRDS.rds"))
  
  #Example Tree
  tree_plot = plot(tree <- get_tree(cf, 4))
  cat(DiagrammeRsvg::export_svg(tree_plot), file=paste0("./04_Plots/CF/", ind, "/3_CausalForest_Treeplot.svg"))
  
  
  ### Step 2(a): predict (training set, out of bag) -----------------------
  #' Columns 'predictions' and 'variance.estimates' contains estimates of the CATE 
  #' and its variance for each observation.
  #' The column 'debiased.error' contains estimates of the error on the CATE predictions.
  #' debiased means that the error is only due to sample variability in the data, and 
  #' the variability due to randomness in the construction of the random forest has 
  #' been removed. It is, 'debiased.error' represents the error we should expect if 
  #' we grew an infinite forest.
  
  #Predicciones 
  oob_pred <- predict(cf, estimate.variance = TRUE)
  kable_styling(
    kable(oob_pred, "html", digits=4),
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/4_Predictions_oob_table.html"))
  oob_tauhat_cf    <- oob_pred$predictions
  oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)
  
  
  ### ATE for Causal Forest -----------------------------------------------
  ATE = average_treatment_effect(cf)
  kable_styling(
    kable(ATE, "html", digits=4),
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/5_ATE_table.html"))
  
  
  
  #' ----------------------------------------------------------------------
  ## Assessing heterogeneity  ---------------------------------------------
  #' ----------------------------------------------------------------------
  print("Assessing heterogeneity")
  
  ### Distribution of predictions -------------------------------------------
  #' The histogram of out-of-bag CATE estimates for the dataset.
  #' Histogram is not a definitive way to assess heterogeneity.
  #' If the histogram is concentrated at a point, then the forests were not able to 
  #' detect any heterogeneity, but it may be that we are simply underpowered.If the 
  #' histogram is spread out, it may be that the forests are simply overfitting and
  #' producing very noisy estimates.
  
  # Histogram OOB
  id <- seq(1:length(oob_tauhat_cf))
  oob_tauhat_cf_hist <- as.data.frame(cbind(oob_tauhat_cf, id))
  png(paste0("./04_Plots/CF/", ind, "/4_Predictions_oob_Histogram.png"), 
      width = 1200, height = 800)
  p <- oob_tauhat_cf_hist %>%
    ggplot(aes(x=oob_tauhat_cf)) +
    geom_histogram(fill=onecolor, color="#FFFFFF", size=0, alpha=0.5) +
    xlab("Treatment effect") + ylab("") +
    theme_ipsum(axis_title_size = 20, axis_text_size = 20)
  print(p)
  dev.off()
  
  # Confidence interval OOB
  ## Data
  oob_tauhat_cf_se_up <- oob_tauhat_cf + 1.96*oob_tauhat_cf_se
  oob_tauhat_cf_se_lo <- oob_tauhat_cf - 1.96*oob_tauhat_cf_se
  g7_data <- as.data.frame(cbind(oob_tauhat_cf, oob_tauhat_cf_se_up, oob_tauhat_cf_se_lo))
  g7_data <- g7_data[order(oob_tauhat_cf),]
  g7_data$id <- seq(1:length(oob_tauhat_cf))
  ## Graph
  png(paste0("./04_Plots/CF/", ind, "/4_Predictions_oob_CI.png"),
      width = 1200, height = 800)
  p <- ggplot(g7_data, aes(x=id)) +
    geom_line(aes(y=oob_tauhat_cf_se_up), color="#ACACAC", size=0.5, linetype=2) +
    geom_line(aes(y=oob_tauhat_cf_se_lo), color="#ACACAC", size=0.5, linetype=2) +
    geom_line(aes(y=oob_tauhat_cf), color="black", size=1) +
    geom_line(aes(y=0), color=onecolor, size=1, linetype=2) +
    xlab("Observation (ordered by estimated treatment effect)") + 
    ylab("Estimated Treatment Effect") +
    theme_ipsum(axis_title_size = 20, axis_text_size = 20)
  print(p)
  dev.off()
  
  # Significance
  Significant_values <- ifelse(oob_tauhat_cf_se_up*oob_tauhat_cf_se_lo>0, 1, 0)
  significance_print <- paste0(
    "Significant observations: ", sum(Significant_values),
    ". Percentaje: ", (sum(Significant_values)/number_observations)*100, "%" )
  writeLines(significance_print, con=paste0("./05_Tables/CF/", ind, "/5_SignificanceValues.html"))
  
  
  ### Variable importance ---------------------------------------------------
  #' Variable importance measures how often a variable is used in a tree split.
  #' If two covariates are highly correlated, the trees might split on one covariate 
  #' but not the other. However, if one was removed, the trees might split on the one
  #' remaining, and the leaf definitions might be unchanged.
  
  # Table
  var_imp        <- c(variable_importance(forest=cf))
  names(var_imp) <- X_names[1:number_covariables]
  sorted_var_imp <- sort(var_imp, decreasing = TRUE)
  as.data.frame(sorted_var_imp, row.names = names(sorted_var_imp)) %>%
    kable("html", digits = 4, row.names = TRUE) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/3_CausalForest_varimp.html"))
  
  # Bar graph
  varimp_of      <- variable_importance(forest=cf)
  selectvar_of   <- which(varimp_of > mean(varimp_of))
  fill_lenght_of <- length(selectvar_of)
  ## Data
  g9_data <- data.frame(
    name =X_names[c(selectvar_of)],
    value=varimp_of[c(selectvar_of)]
  )
  ## Graph
  png(paste0("./04_Plots/CF/", ind, "/3_CausalForest_varimp.png"), 
      width = 1200, height = 800)
  p <- ggplot(g9_data, aes(x=reorder(name, value),value)) +
    geom_bar(fill=onecolor, alpha=0.5, stat='identity', colour=onecolor, size=0.1) +
    xlab("") + ylab("Feature importance") +
    coord_flip() +
    theme_ipsum(axis_title_size = 20, axis_text_size = 20)
  print(p)
  dev.off()
  
  
  ### Best linear predictor ------------------------------------------------
  #' Test calibration on the forest. Computes the best linear fit of the target 
  #' estimand using the forest prediction as well as the mean forest prediction as 
  #' the sole two regressors.
  #' mean.forest.prediction = A coefficient of 1 suggests that the mean forest 
  #'    prediction is correct.
  #' differential.forest.prediction =
  #'    - Coefficient: If 1 suggests that the heterogeneity estimates from the forest 
  #'    are well calibrated.
  #'    - P-value: Act as an omnibust test for the presence of heterogeneity. If < alpha 
  #'    the we can reject the null of no heterogeneity.
  test_calibration <- test_calibration(cf)
  test_calibration <- as.data.frame(test_calibration)
  kable_styling(kable(test_calibration[1:2,], "html", digits=4),
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/5_ATE_TestCalibration.html"))
  
  
  
  ### Heterogeneity by ntiles ------------------------------------------------
  print("- Heterogeneity by ntiles")

  #### Creating subpopulations -----------------------------------------------
  #' Based on predicted treatment effect strength.
  num_tiles <- 2 #ntiles = CATE is above / below the median
  df_train$cate  <- oob_tauhat_cf
  df_train$ntile <- factor(ntile(oob_tauhat_cf, n=num_tiles))
  percent_ntiles <- table(df_train$ntile)
  kable_styling(kable(percent_ntiles, "html", digits=1),
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/7_Subgroups_ntiles_sample.html"))
  
  
  # Across subgroups ---------------------------------------------------
  # 1. Sample ATE (for RCTs)
  #' The average difference between "raw" outcomes for treated and control groups.
  #' The estimator is simply a difference of the average outcome for treated and 
  #' control observations within the subgroup.
  ols_sample_ate          <- lm("Y ~ ntile + ntile:W", data=df_train)
  estimated_sample_ate    <- coef(
    summary(ols_sample_ate))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
  hypothesis_sample_ate   <- paste0("ntile1:W = ", paste0("ntile", seq(2, num_tiles), ":W"))
  ftest_pvalue_sample_ate <- linearHypothesis(ols_sample_ate, hypothesis_sample_ate)[2, "Pr(>F)"]
  
  # 2. ATE - AIPW (for observational studies)
  #' Constructing and average doubly robust scores for the treatment effect, where
  #' \hat\tau(Xi) and e(Xi) are out-of-bag estimates.
  estimated_aipw_ate <- lapply(
    seq(num_tiles), function(w) {
      ate <- average_treatment_effect(cf, subset = df_train$ntile == w)
    })
  estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))
  
  # Testing for equality using Wald test
  #' Null hypothesis: The average CATE is the same across n-tiles.
  waldtest_pvalue_aipw_ate <- wald.test(
    Sigma = diag(estimated_aipw_ate$std.err^2),
    b = estimated_aipw_ate$estimate,
    Terms = 1:num_tiles)$result$chi2[3]
  
  # Tables and graphs
  ## Round the estimates and standard errors before displaying them
  estimated_sample_ate_rounded <- round(signif(estimated_sample_ate, digits = 6), 6)
  estimated_aipw_ate_rounded   <- round(signif(estimated_aipw_ate,   digits = 6), 6)
  ## Format Table: Parenthesis, row/column names
  sample_ate_w_se <- c(rbind(estimated_sample_ate_rounded[, "Estimate"], paste0("(", estimated_sample_ate_rounded[, "Std. Error"],")")))
  aipw_ate_w_se   <- c(rbind(estimated_aipw_ate_rounded[, "estimate"], paste0("(", estimated_aipw_ate_rounded[, "std.err"],")")))
  table <- cbind("Sample ATE" = sample_ate_w_se, "AIPW ATE" = aipw_ate_w_se)
  table <- rbind(table, round(signif(c(ftest_pvalue_sample_ate, waldtest_pvalue_aipw_ate), digits = 5), 4))
  left_column <- rep('', nrow(table))
  left_column[seq(1, nrow(table),2)] <-
    cell_spec(c(paste0("ntile", seq(num_tiles)), "P-value"),
              format = "html", escape = FALSE, color = "black", bold = TRUE)
  table <- cbind(" ", left_column, table)
  ## Output table
  table %>%
    kable("html", escape = FALSE, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
    footnote(general = "Average treatment effects per subgroup defined by out-of-bag CATE.<br>
           P-value is testing <i>H<sub>0</sub: ATE is constant across ntiles</i>.<br>
           Sample ATE uses an F-test and AIPW uses a Wald Test;<br>
           See the code above for mor details.",
             escape = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/7_Subgroups_ntiles_SampleAndAIPW.html"))
  ## Transform to data tables with relevant columns
  estimated_sample_ate <- as.data.frame(estimated_sample_ate)
  estimated_sample_ate$Method <- "Sample ATE"
  estimated_sample_ate$Ntile <- as.numeric(sub(".*([0-9]+).*", "\\1", rownames(estimated_sample_ate)))
  estimated_aipw_ate <- as.data.frame(estimated_aipw_ate)
  estimated_aipw_ate$Method <- "AIPW ATE"
  estimated_aipw_ate$Ntile <- as.numeric(rownames(estimated_aipw_ate))
  ## Unify column names and combine
  colnames(estimated_sample_ate) <- c("Estimate", "SE", "Method", "Ntile")
  colnames(estimated_aipw_ate)   <- c("Estimate", "SE", "Method", "Ntile")
  combined_ate_estimates <- rbind(estimated_sample_ate, estimated_aipw_ate)
  ## Plot
  #' Note that the average estimates of the treatment effect that is obtained by 
  #' averaging doubly-robust scores may not be monotonic. That is, the average estimate 
  #' for group 4 may end up being smaller than the one for I3. Asymptotically, these 
  #' differences should disappear, but this is a common occurrence in small samples.
  png(paste0("./04_Plots/CF/", ind, "/7_Subgroups_ntiles_SampleAndAIPW.png"),
      width = 1200, height = 800)
  p <- ggplot(combined_ate_estimates) +
    geom_pointrange(
      aes(x=Ntile, y=Estimate, ymax=Estimate+1.96*SE, ymin=Estimate-1.96*SE, colour=Method),
      size=0.7, position = position_dodge(width = .5)) +
    geom_errorbar(
      aes(x=Ntile, ymax=Estimate+1.96*SE, ymin=Estimate-1.96*SE, color=Method),
      size=0.6, position = position_dodge(width = .5)) +
    scale_colour_manual(values = twocolors) +
    scale_fill_manual(values=c("#FFFFFF")) +
    theme_minimal() +
    labs(x="N-tile", y="ATE Estimate")
  print(p)
  dev.off()
  
  # Is ATE different between all two pairs of n-tiles?
  ## Matrices
  p_values_tile_by_tile    <- matrix(nrow=num_tiles, ncol = num_tiles)
  differences_tile_by_tile <- matrix(nrow=num_tiles, ncol = num_tiles)
  stderror_tile_by_tyle    <- matrix(nrow=num_tiles, ncol = num_tiles)
  hypotheses_grid          <- combn(1:num_tiles, 2)
  ## Function
  invisible(apply(hypotheses_grid, 2, function(x) {
    .diff <- with(estimated_aipw_ate, Estimate[Ntile==x[2]] - Estimate[Ntile==x[1]])
    .se   <- with(estimated_aipw_ate, sqrt(SE[Ntile==x[2]]^2 + SE[Ntile==x[1]]^2))
    differences_tile_by_tile[x[2],x[1]] <<- .diff
    stderror_tile_by_tyle[x[2],x[1]]    <<- .se
    p_values_tile_by_tile[x[2],x[1]]    <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
  }))
  ##Display p-values under mean difference values in HTML
  diffs <- matrix(nrow=num_tiles, ncol = num_tiles)
  invisible(apply(hypotheses_grid, 2, function(x) {
    d <- differences_tile_by_tile[x[2],x[1]]
    s <- stderror_tile_by_tyle[x[2],x[1]]
    p <- p_values_tile_by_tile[x[2],x[1]]
    top <- cell_spec(
      round(d, 3), "html",
      background = case_when(
        is.na(p) || (p>0.05) ~ "white",
        p>0.01               ~ "gray",
        TRUE                 ~ "black"),
      color = ifelse(is.na(p), "white", ifelse(p<0.1, "white", "gray")))
    value <- ifelse(is.na(p), "", paste0(top, " <br> ", "(", round(s,3), ")"))
    diffs[x[2], x[1]] <<- value
  }))
  diffs <- as.data.frame(diffs) %>% mutate_all(as.character)
  rownames(diffs) <- paste0("tile", 1:num_tiles)
  colnames(diffs) <- paste0("tile", 1:num_tiles)
  ## Title of the table
  caption <- "Pairwise n-tile differences:<br>
  AIPW ATE differences between tile i and tile j"
  ## Styling color and background
  color <- function(x) ifelse(is.na(x), "white", "gray")
  # diffs %>%
  #   rownames_to_column() %>%
  #   mutate_all(function(x) cell_spec(x, "html", escape=FALSE, color=color(x))) %>%
  #   kable(format = "html", caption = caption, escape = FALSE) %>%
  #   kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = FALSE) %>%
  #   footnote(
  #   general = 'Standard errors in parenthesis. Significance (not adjusted for multiple testing):
  #   <ul>
  #   <li>No background color: p >=0.05
  #   <li><span style="color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gray;">Gray</span> background: p< 0.05
  #   <li><span style="color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gray;">Black</span> background: p< 0.01
  #   </ul>
  #   ', escape=FALSE) %>%
  #   save_kable(paste0("./05_Tables/CF/", ind, "/7_Subgroups_ntiles_AIPW.html"))
  
  
  #### Across covariates --------------------------------------------------
  # Regress each covariate on ntile assignment to means p
  covariate_names <- colnames(X_raw_train)
  #covariate_names <- X_raw_train
  cov_means <- lapply(covariate_names, function(covariate) {
    lm(paste0(covariate, '~ 0 + ntile'), data = df_train)
  })
  
  # Extract the man and standard deviation of each covariate per ntile
  cov_table <- lapply(cov_means, function(cov_mean) {
    as.data.frame(t(coef(summary(cov_mean))[, c("Estimate", "Std. Error")]))
  })
  
  # Prepararion to color the chart
  temp_standarized <- sapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    .mean <- mean(df_train[[covariate_name]], na.rm = TRUE)
    .sd   <- sd(df_train[[covariate_name]], na.rm = TRUE)
    m     <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
    .standardized <- (m["Estimate",] - .mean) / .sd
  })
  
  color_scale <- max(abs(c(max(temp_standarized, na.rm = TRUE), min(temp_standarized, na.rm = TRUE))))
  color_scale <- color_scale * c(-1, 1)
  
  # Little trick to display the standard errors
  table <- lapply(seq_along(covariate_names), function(j) {
    covariate_name <- covariate_names[j]
    .mean <- mean(df_train[[covariate_name]], na.rm = TRUE)
    .sd   <- sd(df_train[[covariate_name]], na.rm = TRUE)
    m     <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
    .standardized <- (m["Estimate",] - .mean) / .sd
    m["Estimate",] <- (
      cell_spec(m["Estimate",],
                color = "white",
                background = spec_color(.standardized, end = 0.9, begin = 0.1, scale_from = color_scale)
    ))
    m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
    m
  })
  table <- do.call(rbind,table)
  
  # Covariate names
  covnames <- rep("", nrow(table))
  covnames[seq(1, length(covnames), 2)] <-
    cell_spec(covariate_names, format = "html", escape = F, color = "black", bold = T)
  table <- cbind(covariates=covnames, table)
  
  # Title of table
  caption <- paste0("Average covariate values in each n-tile")
  table %>% 
    kable(format = "html", digits = 2, caption = caption, escape = FALSE, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = FALSE) %>%
    footnote(paste0("Colors are assigned according to where the subgroup's mean value lands on the standarized empirical distribution of it's variable: (x - mean(x))/sd(x) 
                  <br>Standardized distribution is colored from a scale of +/-", round(color_scale[2], 3)), escape=FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/7_Subgroups_ntiles_CovariateValues.html"))
  
  covariate_means_per_ntile <- df_train %>% group_by(ntile) %>% summarise_at(vars(covariate_names), mean)
  covariate_means <- df_train %>% summarise_at(vars(covariate_names), mean)
  ntile_weights <- table(df_train$ntile) / dim(df_train)[1]
  deviations <- t(covariate_means_per_ntile[,2:ncol(covariate_means_per_ntile)]) %>%
    lapply(function(x){x-t(covariate_means)}) %>%
    bind_cols()
  
  covariate_means_weighted_var <- (ntile_weights * deviations^2) %>% colSums()
  covariate_var <- df_train %>% summarise_at(vars(covariate_names), var)
  cov_variation <- covariate_means_weighted_var / covariate_var
  
  sorted_cov_variation <- cov_variation
  table <- t(as.data.frame(sorted_cov_variation))
  table <- sort(table[,1], decreasing = TRUE)
  
  kable_styling(kable(table, "html", digits=3, row.names=TRUE,
                      caption = "Covariate variation across n-tiles"),
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/7_Subgroups_ntiles_CovariateVariation.html"))

  
  
  ### Heterogeneity by subgroups around zero ---------------------------------
  print("- Heterogeneity by subgroups around zero")
  
  #### Creating subpopulations -----------------------------------------------
  #' Based on the treatment effect result (around zero)
  num_tiles <- 2 #ntiles = CATE is above / below the median
  df_train$cate  <- oob_tauhat_cf
  df_train$ntile <- factor(ifelse(oob_tauhat_cf<0, 1, 2))
  percent_ntiles <- table(df_train$ntile)
  kable_styling(kable(percent_ntiles, "html", digits=1),
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE) %>%
    save_kable(paste0("./05_Tables/CF/", ind, "/6_Subgroups_cero_sample.html"))
  
  # Checking if there are observations for both subgroups
  if (all(percent_ntiles > 1) & length(percent_ntiles)==num_tiles) {
    
    #### Across subgroups --------------------------------------------------
    # 1. Sample ATE (for RCTs)
    #' The average difference between "raw" outcomes for treated and control groups.
    #' The estimator is simply a difference of the average outcome for treated and 
    #' control observations within the subgroup.
    ols_sample_ate <- lm("Y ~ ntile + ntile:W", data=df_train)
    estimated_sample_ate <- coef(summary(ols_sample_ate))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
    hypothesis_sample_ate <- paste0("ntile1:W = ", paste0("ntile", seq(2, num_tiles), ":W"))
    ftest_pvalue_sample_ate <- linearHypothesis(ols_sample_ate, hypothesis_sample_ate)[2, "Pr(>F)"]
    
    # 2. ATE - AIPW (for observational studies)
    #' Constructing and average doubly robust scores for the treatment effect, where
    #' \hat\tau(Xi) and e(Xi) are out-of-bag estimates.
    estimated_aipw_ate <- lapply(
      seq(num_tiles), function(w) {
        ate <- average_treatment_effect(cf, subset = df_train$ntile == w)
      })
    estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))
    
    ##Testing for equality using Wald test
    #' Null hypothesis: The average CATE is the same across n-tiles.
    waldtest_pvalue_aipw_ate <- wald.test(
      Sigma = diag(estimated_aipw_ate$std.err^2),
      b = estimated_aipw_ate$estimate,
      Terms = 1:num_tiles)$result$chi2[3]
    
    # Table and graphs
    ## Round the estimates and standard errors before displaying them
    estimated_sample_ate_rounded <- round(signif(estimated_sample_ate, digits = 6), 6)
    estimated_aipw_ate_rounded   <- round(signif(estimated_aipw_ate,   digits = 6), 6)
    ## Format Table: Parenthesis, row/column names
    sample_ate_w_se <- c(rbind(estimated_sample_ate_rounded[, "Estimate"], paste0("(", estimated_sample_ate_rounded[, "Std. Error"],")")))
    aipw_ate_w_se   <- c(rbind(estimated_aipw_ate_rounded[, "estimate"], paste0("(", estimated_aipw_ate_rounded[, "std.err"],")")))
    table <- cbind("Sample ATE" = sample_ate_w_se, "AIPW ATE" = aipw_ate_w_se)
    table <- rbind(table, round(signif(c(ftest_pvalue_sample_ate, waldtest_pvalue_aipw_ate), digits = 5), 4))
    left_column <- rep('', nrow(table))
    left_column[seq(1, nrow(table),2)] <-
      cell_spec(
        c(paste0("ntile", seq(num_tiles)), "P-value"),
        format = "html", escape = FALSE, color = "black", bold = TRUE)
    table <- cbind(" ", left_column, table)
    ## Output table
    table %>%
      kable("html", escape = FALSE, row.names = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
      footnote(general = "Average treatment effects per subgroup defined by out-of-bag CATE.<br>
           P-value is testing <i>H<sub>0</sub: ATE is constant across ntiles</i>.<br>
           Sample ATE uses an F-test and AIPW uses a Wald Test;<br>
           See the code above for mor details.",
               escape = FALSE) %>%
      save_kable(paste0("./05_Tables/CF/", ind, "/6_Subgroups_cero_SampleAndAIPW.html"))
    ## Transform to data tables with relevant columns
    estimated_sample_ate <- as.data.frame(estimated_sample_ate)
    estimated_sample_ate$Method <- "Sample ATE"
    estimated_sample_ate$Ntile <- as.numeric(sub(".*([0-9]+).*", "\\1", rownames(estimated_sample_ate)))
    estimated_aipw_ate <- as.data.frame(estimated_aipw_ate)
    estimated_aipw_ate$Method <- "AIPW ATE"
    estimated_aipw_ate$Ntile <- as.numeric(rownames(estimated_aipw_ate))
    ##Unify column names and combine
    colnames(estimated_sample_ate) <- c("Estimate", "SE", "Method", "Ntile")
    colnames(estimated_aipw_ate)   <- c("Estimate", "SE", "Method", "Ntile")
    combined_ate_estimates <- rbind(estimated_sample_ate, estimated_aipw_ate)
    ## Plot
    #' Note that the average estimates of the treatment effect that is obtained by
    #' averaging doubly-robust scores may not be monotonic. That is, the average estimate
    #' for group 4 may end up being smaller than the one for I3. Asymptotically, these 
    #' differences should disappear, but this is a common occurrence in small samples.
    png(paste0("./04_Plots/CF/", ind, "/6_Subgroups_cero_SampleAndAIPW.png"),
        width = 1200, height = 800)
    p <- ggplot(combined_ate_estimates) +
      geom_pointrange(
        aes(x=Ntile, y=Estimate, ymax=Estimate+1.96*SE, ymin=Estimate-1.96*SE, colour=Method),
        size=0.5, position = position_dodge(width = .5)) +
      geom_errorbar(
        aes(x=Ntile, ymax=Estimate+1.96*SE, ymin=Estimate-1.96*SE, color=Method),
        size=0.4, position = position_dodge(width = .5)) +
      scale_colour_manual(values = twocolors) +
      scale_fill_manual(values=c("#FFFFFF")) +
      theme_minimal() +
      labs(x="N-tile", y="ATE Estimate")
    print(p)
    dev.off()
    
    # Is ATE different between all two pairs of n-tiles?
    ## Matrices
    p_values_tile_by_tile    <- matrix(nrow=num_tiles, ncol = num_tiles)
    differences_tile_by_tile <- matrix(nrow=num_tiles, ncol = num_tiles)
    stderror_tile_by_tyle    <- matrix(nrow=num_tiles, ncol = num_tiles)
    hypotheses_grid          <- combn(1:num_tiles, 2)
    ## Function
    invisible(apply(hypotheses_grid, 2, function(x) {
      .diff <- with(estimated_aipw_ate, Estimate[Ntile==x[2]] - Estimate[Ntile==x[1]])
      .se   <- with(estimated_aipw_ate, sqrt(SE[Ntile==x[2]]^2 + SE[Ntile==x[1]]^2))
      differences_tile_by_tile[x[2],x[1]] <<- .diff
      stderror_tile_by_tyle[x[2],x[1]]    <<- .se
      p_values_tile_by_tile[x[2],x[1]]    <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
    }))
    ## Display p-values under mean difference values in HTML
    diffs <- matrix(nrow=num_tiles, ncol = num_tiles)
    invisible(apply(hypotheses_grid, 2, function(x) {
      d <- differences_tile_by_tile[x[2],x[1]]
      s <- stderror_tile_by_tyle[x[2],x[1]]
      p <- p_values_tile_by_tile[x[2],x[1]]
      top <- cell_spec(
        round(d, 3), "html",
        background = case_when(
          is.na(p) || (p>0.05) ~ "white",
          p>0.01               ~ "gray",
          TRUE                 ~ "black"),
        color = ifelse(is.na(p), "white", ifelse(p<0.1, "white", "gray")))
      value <- ifelse(is.na(p), "", paste0(top, " <br> ", "(", round(s,3), ")"))
      diffs[x[2], x[1]] <<- value
    }))
    diffs <- as.data.frame(diffs) %>% mutate_all(as.character)
    rownames(diffs) <- paste0("tile", 1:num_tiles)
    colnames(diffs) <- paste0("tile", 1:num_tiles)
    ## Title of the table
    caption <- "Pairwise n-tile differences:<br>
    AIPW ATE differences between tile i and tile j"
    ## Styling color and background
    color <- function(x) ifelse(is.na(x), "white", "gray")
    # diffs %>%
    #   rownames_to_column() %>%
    #   mutate_all(function(x) cell_spec(x, "html", escape=FALSE, color=color(x))) %>%
    #   kable(format = "html", caption = caption, escape = FALSE) %>%
    #   kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = FALSE) %>%
    #   footnote(
    #     general = 'Standard errors in parenthesis. Significance (not adjusted for multiple testing):
    # <ul>
    # <li>No background color: p >=0.05
    # <li><span style="color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gray;">Gray</span> background: p< 0.05
    # <li><span style="color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gray;">Black</span> background: p< 0.01
    # </ul>
    # ', escape=FALSE) %>%
    #   save_kable(paste0("./05_Tables/CF/", ind, "/6_Subgroups_cero_AIPW.html"))
    
    
    #### Across covariates -------------------------------------------------
    # Regress each covariate on ntile assignment to means p
    covariate_names <- colnames(X_raw_train)
    cov_means <- lapply(covariate_names, function(covariate) {
      lm(paste0(covariate, '~ 0 + ntile'), data = df_train)
    })
    
    # Extract the man and standard deviation of each covariate per ntile
    cov_table <- lapply(cov_means, function(cov_mean) {
      as.data.frame(t(coef(summary(cov_mean))[, c("Estimate", "Std. Error")]))
    })
    
    # Prepararion to color the chart
    temp_standarized <- sapply(seq_along(covariate_names), function(j) {
      covariate_name <- covariate_names[j]
      .mean <- mean(df_train[[covariate_name]], na.rm = TRUE)
      .sd <- sd(df_train[[covariate_name]], na.rm = TRUE)
      m   <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
      .standardized <- (m["Estimate",] - .mean) / .sd
    })
    
    color_scale <- max(abs(c(max(temp_standarized, na.rm = TRUE), min(temp_standarized, na.rm = TRUE))))
    color_scale <- color_scale * c(-1, 1)
    
    # Little trick to display the standard errors
    table <- lapply(seq_along(covariate_names), function(j) {
      covariate_name <- covariate_names[j]
      .mean <- mean(df_train[[covariate_name]], na.rm = TRUE)
      .sd   <- sd(df_train[[covariate_name]], na.rm = TRUE)
      m     <- as.matrix(round(signif(cov_table[[j]], digits=4), 3))
      .standardized <- (m["Estimate",] - .mean) / .sd
      m["Estimate",] <- (
        cell_spec(m["Estimate",],
                  color = "white",
                  background = spec_color(.standardized,
                                          end = 0.9,
                                          begin = 0.1,
                                          scale_from = color_scale)
        ))
      m["Std. Error",] <- paste0("(", m["Std. Error",], ")")
      m
    })
    table <- do.call(rbind,table)
    
    # Covariate names
    covnames <- rep("", nrow(table))
    covnames[seq(1, length(covnames), 2)] <-
      cell_spec(covariate_names, format = "html", escape = F, color = "black", bold = T)
    table <- cbind(covariates=covnames, table)
    
    # Title of table
    caption <- paste0("Average covariate values in each n-tile")
    table %>% 
      kable(format = "html", digits = 2, caption = caption, escape = FALSE, row.names = FALSE) %>%
      kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = FALSE) %>%
      footnote(paste0("Colors are assigned according to where the subgroup's mean value lands on the standarized empirical distribution of it's variable: (x - mean(x))/sd(x) 
                  <br>Standardized distribution is colored from a scale of +/-", round(color_scale[2], 3)), escape=FALSE) %>%
      save_kable(paste0("./05_Tables/CF/", ind, "/6_Subgroups_cero_CovariateValues.html"))
    
    covariate_means_per_ntile <- df_train %>% group_by(ntile) %>% summarise_at(vars(covariate_names), mean)
    covariate_means <- df_train %>% summarise_at(vars(covariate_names), mean)
    ntile_weights <- table(df_train$ntile) / dim(df_train)[1]
    deviations <- t(covariate_means_per_ntile[,2:ncol(covariate_means_per_ntile)]) %>%
      lapply(function(x){x-t(covariate_means)}) %>%
      bind_cols()
    covariate_means_weighted_var <- (ntile_weights * deviations^2) %>% colSums()
    covariate_var <- df_train %>% summarise_at(vars(covariate_names), var)
    cov_variation <- covariate_means_weighted_var / covariate_var
    
    sorted_cov_variation <- cov_variation
    table <- t(as.data.frame(sorted_cov_variation))
    table <- sort(table[,1], decreasing = TRUE)
    
    kable_styling(kable(table, "html", digits=3, row.names=TRUE,
                        caption = "Covariate variation across n-tiles"),
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE) %>%
      save_kable(paste0("./05_Tables/CF/", ind, "/6_Subgroups_cero_CovariateVariation.html"))
    
  }
  
  
  
  #' ----------------------------------------------------------------------
  ## Variables importance (Shapley values) --------------------------------
  #' ----------------------------------------------------------------------
  #print("Variables importance (Shapley values)")
  
  #' An alternative for explaining individual predictions is a method from coalitional 
  #' game theory named Shapley value. Assume that for one data point, the feature values 
  #' play a game together, in which they get the prediction as a payout. The Shapley 
  #' value tells us how to fairly distribute the payout among the feature values.
  
  
  ### Average variable contributions --------------------------------------
  #print("Average variable contributions")
  
  # Creating predictor object 
  #predictor <- Predictor$new(model = cf, data = X_raw_train, y = Y)
  
  # Calculating Shapley values
  ## Option 1: all observations
  #X_sample <- X_raw_train
  #shapley_all <- Shapley$new(predictor, x.interest = X_sample)
  
  ## Option 2: first n rows
  #X_sample <- X_raw_train[1:number_shapley_obs, ]
  #shapley_all <- Shapley$new(predictor, x.interest = X_sample)
  
  ## Option 3: random sample
  #sample_indices <- sample(1:nrow(X_raw_train), number_shapley_obs)
  #X_sample <- X_raw_train[sample_indices, ]
  #shapley_all <- Shapley$new(predictor, x.interest = X_sample)
  
  # Saving data
  #saveRDS(shapley_all, paste0("./05_Tables/CF/", ind, "/8_ShapleyAll.rds"))
  
  
  ### Individual variable contributions --------------------------------------
  #print("Individual variable contributions")
  
  # Calculating Shapley values
  #shapley_indiv <- map_dfr(1:nrow(X_sample), function(i) {
  #  shap <- Shapley$new(predictor, x.interest = X_sample[i, ])
  #  shap$results
  #})
  
  # Row id
  #shapley_indiv <- shapley_indiv %>%
  #  mutate(observation = rep(1:nrow(X_sample), each = ncol(X_sample)))
  
  # Saving data
  #saveRDS(shapley_indiv, paste0("./05_Tables/CF/", ind, "/8_ShapleyIndiv.rds"))
  
}


# ========================================================================= #
# Shapley values (external script) ------------------------------------------
# ========================================================================= #

#source("./01. Data cleanning/09_ShapleyValues.R")
