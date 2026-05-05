# ---------------------------------------------------------------------------- #
#   Constructing composite indicators
#   Author:       Raquel
#   Creation:     October 2023
#   Last edition: February 2025

# This script:
# 1. Uses the PCA method to assign indicators weighs for robustness and adaptation capacities.
# 2. Constructs composite indicators as fractional responses to variables for the robustness and adaptation capacity.
# 3. Construct the composite indicator(s) for transformation as a dummy variable.
# ---------------------------------------------------------------------------- #

# Calling packages
library(chemometrics); library(knitr); library(haven); library(dplyr); library(ggcorrplot)
library(psych); library("FactoMineR"); library("factoextra")

#WD
 setwd("C:/Users/userecon10/Desktop/Raquel Sofia Zapata/")
#setwd("C:/Users/rszap/OneDrive - Universidad Nacional de Colombia/Maestría/Climate_resilience")

 
# ==================== #
# DATABASE              ----------------------------------------------------
# ==================== #

rm(list=ls())

# Dataframe
#indicators_database <- read_dta("./01_Data_cleaning/ELCA/IndicadoresStd.dta")
 indicators_database <- read_dta("./00. Processed data/IndicadoresStd.dta")

# Let's have a quick overview of the data
str(indicators_database)
names(indicators_database)


# ========================================================================= #
# GENERAL INDICATORS    ----------------------------------------------------
# ========================================================================= #

##==================== #
## ADAPTATION           ----------------------------------------------------
##==================== #

#* ------------------------------------------- *#
### Option 1: only numerical variables and SDI -----------------------------

# 0. Selecting indicators
adaptation <- as.data.frame(indicators_database) %>% 
  select(A_SDI_v2 , A_labourexp_v2, A_luh_v2, A_invest_v2)
#stripchart(adaptation, vertical=T)

# 1. Center the data and scale the data
#' This is already done from the previous dofile.
adaptation_scaled <- adaptation

# 2. Checking some attributes
## 2.1. Correlation matrix
adaptation_corrmatrix <- cor(adaptation_scaled)
adaptation_corrmatrix
ggcorrplot(adaptation_corrmatrix)
## 2.2. KMO measure
KMO(adaptation_corrmatrix)
#' Above 0.90: Marvelous  | 0.80 to 0.90: Meritorious           | 0.70 to 0.80: Average or middling
#' 0.60 to 0.70: Medicore | 0.50 to 0.60: Terrible or miserable | Below 0.50: Unacceptable
## 2.3. Bartlett's test
cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)
#' H0: the variance is the same for all product lines.
#' A p-value<0.05 indicates that the correlation matrix is not an identity 
#' matrix, and that the variables in the dataset are correlated. 
#' This means that factor analysis can be performed.

# 3. Data reduction
pca_adaptation <- prcomp(adaptation_scaled)
summarypca <- summary(pca_adaptation)
summarypca
## 3.1. Plots
plot(pca_adaptation$x)  #Graph of the first two component scores.
plot(pca_adaptation)    #scree plot: amount of variance explained by each component
barplot(pca_adaptation$rotation[,1]) #Loadong plot: 1 component
barplot(pca_adaptation$rotation[,2]) #Loadong plot: 2 component
barplot(pca_adaptation$rotation[,3]) #Loadong plot: 3 component
barplot(pca_adaptation$rotation[,4]) #Loadong plot: 4 component
## 3.2. Objects
adaptation_loadings <- pca_adaptation$rotation #loadings
adaptation_scores   <- pca_adaptation$x        #scores
adaptation_sdev     <- pca_adaptation$sdev     #standard deviation of the PC
adaptation_eigenval <- adaptation_sdev^2       #eigenvalues

# 4. For the composite indicators
adaptation_loadings
adaptation_loading1_1 <- adaptation_loadings[1,1]
adaptation_loading1_2 <- adaptation_loadings[2,1]
adaptation_loading1_3 <- adaptation_loadings[3,1]
adaptation_loading1_4 <- adaptation_loadings[4,1]

# 5. Matching with original database
indicators_database$A_ci_1 <- 
  adaptation_loading1_1*indicators_database$A_SDI_v2 + 
  adaptation_loading1_2*indicators_database$A_labourexp_v2 + 
  adaptation_loading1_3*indicators_database$A_luh_v2 +
  adaptation_loading1_4*indicators_database$A_invest_v2
summary(indicators_database$A_ci_1)

# 6. Rescaling for fractional response
indicators_database$A_ci_1 <- 
  (indicators_database$A_ci_1 - min(indicators_database$A_ci_1, na.rm = TRUE)) / 
  (max(indicators_database$A_ci_1, na.rm = TRUE) - min(indicators_database$A_ci_1, na.rm = TRUE))
summary(indicators_database$A_ci_1)

# 7. Saving results
# 7.1. KMO
KMO_print <- paste0(
  "Overall MSA = ",      KMO(adaptation_corrmatrix)[["MSA"]], "<br>",
  "<br>", "MSA var1 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[1]],
  "<br>", "MSA var2 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[2]],
  "<br>", "MSA var3 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[3]],
  "<br>", "MSA var4 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[4]])
# 7.2. Bartlett
bartlett_print <- paste0(
  "Chi square = ",      cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["chisq"]],
  "<br>", "P value = ", cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["p.value"]],
  "<br>", "df = ",      cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["df"]])
# 7.3. PCA summary
summary_print <- paste0(
  "Stdev PC1 = ",         summarypca[["importance"]][1,1], " / PropVar PC1 = ", summarypca[["importance"]][2,1],
  "<br>", "Stdev PC2 = ", summarypca[["importance"]][1,2], " / PropVar PC2 = ", summarypca[["importance"]][2,2],
  "<br>", "Stdev PC3 = ", summarypca[["importance"]][1,3], " / PropVar PC3 = ", summarypca[["importance"]][2,3],
  "<br>", "Stdev PC4 = ", summarypca[["importance"]][1,4], " / PropVar PC4 = ", summarypca[["importance"]][2,4])
# 7.4. Loadings
loadings_print <- paste0(
  "Loading_1 = ",         adaptation_loading1_1,
  "<br>", "Loading_2 = ", adaptation_loading1_2,
  "<br>", "Loading_3 = ", adaptation_loading1_3,
  "<br>", "Loading_4 = ", adaptation_loading1_4)
# 7.5. html
# writeLines(
#   paste0(KMO_print, "<br>", "<br>", bartlett_print, "<br>", "<br>", summary_print, "<br>", "<br>", loadings_print), 
#   con=paste0("./05_Tables/CF/", "A_ci_1", "/0_compositeIndicator.html"))


#* ------------------------------------------- *#
### Option 2: only numerical variables and GSI -----------------------------

# 0. Selecting indicators
adaptation <- as.data.frame(indicators_database) %>% 
  select(A_GSI_v2 , A_labourexp_v2, A_luh_v2, A_invest_v2)
stripchart(adaptation, vertical=T)

# 1. Center the data and scale the data
adaptation_scaled <- adaptation

# 2. Checking some attributes
## 2.1. Correlation matrix
adaptation_corrmatrix <- cor(adaptation_scaled)
adaptation_corrmatrix
ggcorrplot(adaptation_corrmatrix)
## 2.2. KMO measure
KMO(adaptation_corrmatrix)
## 2.3. Bartlett's test
cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=FALSE)

# 3. Data reduction
pca_adaptation <- prcomp(adaptation_scaled)
summarypca <- summary(pca_adaptation)
summarypca
## 3.2. Objects
adaptation_loadings <- pca_adaptation$rotation #loadings

# 4. For the composite indicators
adaptation_loadings
adaptation_loading2_1 <- adaptation_loadings[1,1]
adaptation_loading2_2 <- adaptation_loadings[2,1]
adaptation_loading2_3 <- adaptation_loadings[3,1]
adaptation_loading2_4 <- adaptation_loadings[4,1]

# 5. Matching with original database
indicators_database$A_ci_2 <- 
  adaptation_loading2_1*indicators_database$A_GSI_v2 + 
  adaptation_loading2_2*indicators_database$A_labourexp_v2 + 
  adaptation_loading2_3*indicators_database$A_luh_v2 +
  adaptation_loading2_4*indicators_database$A_invest_v2
summary(indicators_database$A_ci_2)

# 6. Rescaling for fractional response
indicators_database$A_ci_2 <- 
  (indicators_database$A_ci_2 - min(indicators_database$A_ci_2, na.rm = TRUE)) / 
  (max(indicators_database$A_ci_2, na.rm = TRUE) - min(indicators_database$A_ci_2, na.rm = TRUE))
summary(indicators_database$A_ci_2)

# 7. Saving results
# 7.1. KMO
KMO_print <- paste0(
  "Overall MSA = ",      KMO(adaptation_corrmatrix)[["MSA"]], "<br>",
  "<br>", "MSA var1 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[1]],
  "<br>", "MSA var2 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[2]],
  "<br>", "MSA var3 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[3]],
  "<br>", "MSA var4 = ", KMO(adaptation_corrmatrix)[["MSAi"]][[4]])
# 7.2. Bartlett
bartlett_print <- paste0(
  "Chi square = ",      cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["chisq"]],
  "<br>", "P value = ", cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["p.value"]],
  "<br>", "df = ",      cortest.bartlett(adaptation_corrmatrix, n=nrow(adaptation), diag=TRUE)[["df"]])
# 7.3. PCA summary
summary_print <- paste0(
  "Stdev PC1 = ",         summarypca[["importance"]][1,1], " / PropVar PC1 = ", summarypca[["importance"]][2,1],
  "<br>", "Stdev PC2 = ", summarypca[["importance"]][1,2], " / PropVar PC2 = ", summarypca[["importance"]][2,2],
  "<br>", "Stdev PC3 = ", summarypca[["importance"]][1,3], " / PropVar PC3 = ", summarypca[["importance"]][2,3],
  "<br>", "Stdev PC4 = ", summarypca[["importance"]][1,4], " / PropVar PC4 = ", summarypca[["importance"]][2,4])
# 7.4. Loadings
loadings_print <- paste0(
  "Loading_1 = ",         adaptation_loading2_1,
  "<br>", "Loading_2 = ", adaptation_loading2_2,
  "<br>", "Loading_3 = ", adaptation_loading2_3,
  "<br>", "Loading_4 = ", adaptation_loading2_4)
# 7.5. html
# writeLines(
#   paste0(KMO_print, "<br>", "<br>", bartlett_print, "<br>", "<br>", summary_print, "<br>", "<br>", loadings_print), 
#   con=paste0("./05_Tables/CF/", "A_ci_2", "/0_compositeIndicator.html"))




#* ------------------------------------------- *#
### Option 3: FAMD with SDI ----------------------------------------------

# 0. Selecting indicators
adaptation <- as.data.frame(indicators_database) %>% 
  select(A_SDI_v2 , A_labourexp_v2, A_invest_v2, A_irrigation_v2, A_structures_v2)
stripchart(adaptation, vertical=T)

# 1. Organizing data
adaptation$A_irrigation_v2 <- as.factor(adaptation$A_irrigation_v2)
adaptation$A_structures_v2 <- as.factor(adaptation$A_structures_v2)

# 2. FAMD
adaptation_famd <- FAMD(adaptation, graph = TRUE)
print(adaptation_famd)

# 3. Attributes
head(get_eigenvalue(adaptation_famd)) #Eigenvalue and variance
fviz_screeplot(adaptation_famd) #Screeplot
adaptation_famd_vars <- get_famd_var(adaptation_famd) #Variables results
adaptation_famd_vars$coord #Coordenates
adaptation_famd_vars$cos2
adaptation_contributions <- adaptation_famd_vars$contrib #Var contributions
fviz_famd_var(adaptation_famd, repel = TRUE) #Var graph
fviz_contrib(adaptation_famd, "var", axes = 1) #Contributions first dimension

# 3. For the composite indicators
adaptation_contributions
adaptation_contributions3_1 <- adaptation_contributions[1,1]
adaptation_contributions3_2 <- adaptation_contributions[2,1]
adaptation_contributions3_3 <- adaptation_contributions[3,1]
adaptation_contributions3_4 <- adaptation_contributions[4,1]
adaptation_contributions3_5 <- adaptation_contributions[5,1]

# 4. Matching with original database
indicators_database$A_ci_3 <- 
  adaptation_contributions3_1*indicators_database$A_SDI_v2 + 
  adaptation_contributions3_2*indicators_database$A_labourexp_v2 + 
  adaptation_contributions3_3*indicators_database$A_invest_v2 + 
  adaptation_contributions3_4*indicators_database$A_irrigation_v2 +
  adaptation_contributions3_5*indicators_database$A_structures_v2
summary(indicators_database$A_ci_3)

# 5. Rescaling for fractional response
indicators_database$A_ci_3 <- 
  (indicators_database$A_ci_3 - min(indicators_database$A_ci_3, na.rm = TRUE)) / 
  (max(indicators_database$A_ci_3, na.rm = TRUE) - min(indicators_database$A_ci_3, na.rm = TRUE))
summary(indicators_database$A_ci_3)

# 6. Saving results
# 6.1. Variance percentage
varpen <- get_eigenvalue(adaptation_famd)
varpen_print <- paste0(
  "Dim1 = ",         varpen[1,2],
  "<br>", "Dim2 = ", varpen[2,2],
  "<br>", "Dim3 = ", varpen[3,2],
  "<br>", "Dim4 = ", varpen[4,2],
  "<br>", "Dim5 = ", varpen[5,2])
# 6.2. Contributions
contributions_print <- paste0(
  "Contributions_1 = ",         adaptation_contributions3_1,
  "<br>", "Contributions_2 = ", adaptation_contributions3_2,
  "<br>", "Contributions_3 = ", adaptation_contributions3_3,
  "<br>", "Contributions_4 = ", adaptation_contributions3_4,
  "<br>", "Contributions_5 = ", adaptation_contributions3_5)
# 6.3. html
# writeLines(
#   paste0(varpen_print, "<br>", "<br>", contributions_print), 
#   con=paste0("./05_Tables/CF/", "A_ci_3", "/0_compositeIndicator.html"))

  
#* ------------------------------------------- *#
### Option 4: FAMD with GSI ----------------------------------------------

# 0. Selecting indicators
adaptation <- as.data.frame(indicators_database) %>% 
  select(A_GSI_v2 , A_labourexp_v2, A_invest_v2, A_irrigation_v2, A_structures_v2)
stripchart(adaptation, vertical=T)

# 1. Organizing data
adaptation$A_irrigation_v2 <- as.factor(adaptation$A_irrigation_v2)
adaptation$A_structures_v2 <- as.factor(adaptation$A_structures_v2)

# 2. FAMD
adaptation_famd <- FAMD(adaptation, graph = TRUE)

# 3. Attributes
fviz_screeplot(adaptation_famd) #Screeplot
fviz_contrib(adaptation_famd, "var", axes = 1) #Contributions first dimension
adaptation_famd_vars <- get_famd_var(adaptation_famd) #Variables results
adaptation_contributions <- adaptation_famd_vars$contrib #Var contributions


# 3. For the composite indicators
adaptation_contributions
adaptation_contributions4_1 <- adaptation_contributions[1,1]
adaptation_contributions4_2 <- adaptation_contributions[2,1]
adaptation_contributions4_3 <- adaptation_contributions[3,1]
adaptation_contributions4_4 <- adaptation_contributions[4,1]
adaptation_contributions4_5 <- adaptation_contributions[5,1]

# 4. Matching with original database
indicators_database$A_ci_4 <- 
  adaptation_contributions4_1*indicators_database$A_GSI_v2 + 
  adaptation_contributions4_2*indicators_database$A_labourexp_v2 + 
  adaptation_contributions4_3*indicators_database$A_invest_v2 + 
  adaptation_contributions4_4*indicators_database$A_irrigation_v2 +
  adaptation_contributions4_5*indicators_database$A_structures_v2
summary(indicators_database$A_ci_4)

# 5. Rescaling for fractional response
indicators_database$A_ci_4 <- 
  (indicators_database$A_ci_4 - min(indicators_database$A_ci_4, na.rm = TRUE)) / 
  (max(indicators_database$A_ci_4, na.rm = TRUE) - min(indicators_database$A_ci_4, na.rm = TRUE))
summary(indicators_database$A_ci_4)

# 6. Saving results
# 6.1. Variance percentage
varpen <- get_eigenvalue(adaptation_famd)
varpen_print <- paste0(
  "Dim1 = ",         varpen[1,2],
  "<br>", "Dim2 = ", varpen[2,2],
  "<br>", "Dim3 = ", varpen[3,2],
  "<br>", "Dim4 = ", varpen[4,2],
  "<br>", "Dim5 = ", varpen[5,2])
# 6.2. Contributions
contributions_print <- paste0(
  "Contributions_1 = ",         adaptation_contributions4_1,
  "<br>", "Contributions_2 = ", adaptation_contributions4_2,
  "<br>", "Contributions_3 = ", adaptation_contributions4_3,
  "<br>", "Contributions_4 = ", adaptation_contributions4_4,
  "<br>", "Contributions_5 = ", adaptation_contributions4_5)
# 6.3. Eigenvalues
vareng_print <- paste0(
  "Eigenvalue dim 1 = ",         adaptation_famd$eig[1,1],
  "<br>", "Eigenvalue dim 2 = ", adaptation_famd$eig[2,1],
  "<br>", "Eigenvalue dim 3 = ", adaptation_famd$eig[3,1],
  "<br>", "Eigenvalue dim 4 = ", adaptation_famd$eig[4,1],
  "<br>", "Eigenvalue dim 5 = ", adaptation_famd$eig[5,1])
# 6.4. Loadings
loadings_print <- paste0(
  "Loadings dim 1 = ",         adaptation_famd[["var"]][["coord"]][1,1],
  "<br>", "Loadings dim 2 = ", adaptation_famd[["var"]][["coord"]][2,1],
  "<br>", "Loadings dim 3 = ", adaptation_famd[["var"]][["coord"]][3,1],
  "<br>", "Loadings dim 4 = ", adaptation_famd[["var"]][["coord"]][4,1],
  "<br>", "Loadings dim 5 = ", adaptation_famd[["var"]][["coord"]][5,1])
# 6.5. html
writeLines(
  paste0(varpen_print, "<br>", "<br>", vareng_print, "<br>", "<br>", contributions_print, "<br>", "<br>", loadings_print), 
  con=paste0("./05_Tables/CF/", "A_ci_4", "/0_compositeIndicator.html"))



##==================== #
## ROBUSTNESS           ----------------------------------------------------
##==================== #

#* ------------------------------------------- *#
### Option 1: resistance and recovery rate -------------------------------

# 0. Selecting indicators
robustness <- as.data.frame(indicators_database) %>% 
  select(R_resist_v2, R_recovrt_v2)
stripchart(robustness, vertical=T)

# 1. Center and scale the data
robustness_scaled <- robustness

# 2. Checking some attributes
## 2.1. Correlation matrix
robustness_corrmatrix <- cor(robustness_scaled)
robustness_corrmatrix
ggcorrplot(robustness_corrmatrix)
## 2.2. KMO measure
KMO(robustness_corrmatrix)
## 2.3. Bartlett's test
cortest.bartlett(robustness_corrmatrix, n=nrow(robustness), diag=FALSE)

# 3. Data reduction
pca_robustness <- prcomp(robustness_scaled)
summarypca <- summary(pca_robustness)
summarypca
## 3.2. Objects
robustness_loadings <- pca_robustness$rotation #loadings

# 4. For the composite indicators
robustness_loadings
robustness_loading1_1 <- robustness_loadings[1,1]
robustness_loading1_2 <- robustness_loadings[2,1]

# 5. Matching with original database
indicators_database$R_ci_1 <- 
  robustness_loading1_1*indicators_database$R_resist_v2 + 
  robustness_loading1_2*indicators_database$R_recovrt_v2
summary(indicators_database$R_ci_1)

# 6. Rescaling for fractional response
indicators_database$R_ci_1 <- 
  (indicators_database$R_ci_1 - min(indicators_database$R_ci_1, na.rm = TRUE)) / 
  (max(indicators_database$R_ci_1, na.rm = TRUE) - min(indicators_database$R_ci_1, na.rm = TRUE))
summary(indicators_database$R_ci_1)

# 7. Saving results
# 7.1. KMO
KMO_print <- paste0(
  "Overall MSA = ",      KMO(robustness_corrmatrix)[["MSA"]], "<br>",
  "<br>", "MSA var1 = ", KMO(robustness_corrmatrix)[["MSAi"]][[1]],
  "<br>", "MSA var2 = ", KMO(robustness_corrmatrix)[["MSAi"]][[2]])
# 7.2. Bartlett
bartlett_print <- paste0(
  "Chi square = ",      cortest.bartlett(robustness_corrmatrix, n=nrow(robustness), diag=TRUE)[["chisq"]],
  "<br>", "P value = ", cortest.bartlett(robustness_corrmatrix, n=nrow(robustness), diag=TRUE)[["p.value"]],
  "<br>", "df = ",      cortest.bartlett(robustness_corrmatrix, n=nrow(robustness), diag=TRUE)[["df"]])
# 7.3. PCA summary
summary_print <- paste0(
  "Stdev PC1 = ",         summarypca[["importance"]][1,1], " / PropVar PC1 = ", summarypca[["importance"]][2,1],
  "<br>", "Stdev PC2 = ", summarypca[["importance"]][1,2], " / PropVar PC2 = ", summarypca[["importance"]][2,2])
# 7.4. Loadings
loadings_print <- paste0(
  "Loading_1 = ",         robustness_loading1_1,
  "<br>", "Loading_2 = ", robustness_loading1_2)
# 7.5. html
# writeLines(
#   paste0(KMO_print, "<br>", "<br>", bartlett_print, "<br>", "<br>", summary_print, "<br>", "<br>", loadings_print), 
#   con=paste0("./05_Tables/CF/", "R_ci_1", "/0_compositeIndicator.html"))


#* ------------------------------------------- *#
### Option 2: FAMD resistance & shocks ------------------------------------

# 0. Selecting indicators
robustness <- as.data.frame(indicators_database) %>% 
  select(R_resist_v2, R_shock_v2)
stripchart(robustness, vertical=T)

# 1. Organizing data
robustness$R_shock_v2 <- as.factor(robustness$R_shock_v2)

# 2. FAMD
robustness_famd <- FAMD(robustness, graph = TRUE)

# 3. Attributes
fviz_screeplot(robustness_famd) #Screeplot
fviz_contrib(robustness_famd, "var", axes = 1) #Contributions first dimension
robustness_famd_vars <- get_famd_var(robustness_famd) #Variables results
robustness_contributions <- robustness_famd_vars$contrib #Var contributions

# 3. For the composite indicators
robustness_contributions
robustness_contributions2_1 <- robustness_contributions[1,1]
robustness_contributions2_2 <- robustness_contributions[2,1]

# 4. Matching with original database
indicators_database$R_ci_2 <- 
  robustness_contributions2_1*indicators_database$R_resist_v2 + 
  robustness_contributions2_2*indicators_database$R_shock_v2
summary(indicators_database$R_ci_2)

# 5. Rescaling for fractional response
indicators_database$R_ci_2 <- 
  (indicators_database$R_ci_2 - min(indicators_database$R_ci_2, na.rm = TRUE)) / 
  (max(indicators_database$R_ci_2, na.rm = TRUE) - min(indicators_database$R_ci_2, na.rm = TRUE))
summary(indicators_database$R_ci_2)

# 6. Saving results
# 6.1. Variance percentage
varpen <- get_eigenvalue(robustness_famd)
varpen_print <- paste0(
  "Dim1 = ",         varpen[1,2],
  "<br>", "Dim2 = ", varpen[2,2])
# 6.2. Contributions
contributions_print <- paste0(
  "Contributions_1 = ",         robustness_contributions2_1,
  "<br>", "Contributions_2 = ", robustness_contributions2_2)
# 6.3. html
# writeLines(
#   paste0(varpen_print, "<br>", "<br>", contributions_print), 
#   con=paste0("./05_Tables/CF/", "R_ci_2", "/0_compositeIndicator.html"))


#* ------------------------------------------- *#
### Option 3: FAMD three variables ------------------------------------

# 0. Selecting indicators
robustness <- as.data.frame(indicators_database) %>% 
  select(R_resist_v2, R_shock_v2, R_recovrt_v2)
stripchart(robustness, vertical=T)

# 1. Organizing data
robustness$R_shock_v2 <- as.factor(robustness$R_shock_v2)

# 2. FAMD
robustness_famd <- FAMD(robustness, graph = TRUE)

# 3. Attributes
fviz_screeplot(robustness_famd) #Screeplot
fviz_contrib(robustness_famd, "var", axes = 1) #Contributions first dimension
robustness_famd_vars <- get_famd_var(robustness_famd) #Variables results
robustness_contributions <- robustness_famd_vars$contrib #Var contributions

# 3. For the composite indicators
robustness_contributions
robustness_contributions3_1 <- robustness_contributions[1,1]
robustness_contributions3_2 <- robustness_contributions[2,1]
robustness_contributions3_3 <- robustness_contributions[3,1]

# 4. Matching with original database
indicators_database$R_ci_3 <- 
  robustness_contributions3_1*indicators_database$R_resist_v2 + 
  robustness_contributions3_2*indicators_database$R_shock_v2 +
  robustness_contributions3_2*indicators_database$R_recovrt_v2
summary(indicators_database$R_ci_3)

# 5. Rescaling for fractional response
indicators_database$R_ci_3 <- 
  (indicators_database$R_ci_3 - min(indicators_database$R_ci_3, na.rm = TRUE)) / 
  (max(indicators_database$R_ci_3, na.rm = TRUE) - min(indicators_database$R_ci_3, na.rm = TRUE))
summary(indicators_database$R_ci_3)

# 6. Saving results
# 6.1. Variance percentage
varpen <- get_eigenvalue(robustness_famd)
varpen_print <- paste0(
  "Dim1 = ",         varpen[1,2],
  "<br>", "Dim2 = ", varpen[2,2],
  "<br>", "Dim3 = ", varpen[3,2])
# 6.2. Contributions
contributions_print <- paste0(
  "Contributions_1 = ",         robustness_contributions3_1,
  "<br>", "Contributions_2 = ", robustness_contributions3_2,
  "<br>", "Contributions_3 = ", robustness_contributions3_3)
# 6.3. eigenvalues
vareng_print <- paste0(
  "Eigenvalue dim 1 = ",         robustness_famd$eig[1,1],
  "<br>", "Eigenvalue dim 2 = ", robustness_famd$eig[2,1],
  "<br>", "Eigenvalue dim 3 = ", robustness_famd$eig[3,1])
# 6.4. Loadings
loadings_print <- paste0(
  "Loadings dim 1 = ",         robustness_famd[["var"]][["coord"]][1,1],
  "<br>", "Loadings dim 2 = ", robustness_famd[["var"]][["coord"]][2,1],
  "<br>", "Loadings dim 3 = ", robustness_famd[["var"]][["coord"]][3,1])
# 6.5. html
writeLines(
  paste0(varpen_print, "<br>", "<br>", vareng_print, "<br>", "<br>", contributions_print, "<br>", "<br>", loadings_print), 
  con=paste0("./05_Tables/CF/", "R_ci_3", "/0_compositeIndicator.html"))



##===================== #
## TRANSFORMATION        ----------------------------------------------------
##===================== #

#* ------------------------------------------- *#
### Option 1: Slijper approach ---------------------------------------------

indicators_database$T_ci_1 <-
  ifelse((indicators_database$T_farmtype_v2==1 | indicators_database$T_landuse_v2==1), 1, 0)
table(indicators_database$T_ci_1)




# ========================================================================= #
# SAVING RESULTS  ----------------------------------------------------
# ========================================================================= #

#write_dta(indicators_database, "./01_Data_cleaning/ELCA/indicadoresCompuestos.dta")
 write_dta(indicators_database, "./00. Processed data/indicadoresCompuestos.dta")



# ==================== #
# LIST OF INDICATORS    ----------------------------------------------------
# ==================== #

# ROBUSTNESS
# R_resist_v2 R_shock_v2 R_recovrt
# R_resist_agri_v2 R_shock_agri_v2 R_recovrt_agri
# R_resist_pecu_v2 R_shock_pecu_v2 R_recovrt_pecu
# R_resist_agrop_v2 R_shock_agrop_v2 R_recovrt_agrop

# ADAPTATION
# A_SDI_v2 A_GSI_v2 A_labourexp_v2 A_luh_v2 A_ganh_v2 A_feedrt_v2  
# A_SDI_agri_v2 A_GSI_agri_v2 A_labourexp_agri_v2 A_luh_agri_v2 A_ganh_agri_v2 A_feedrt_agri_v2
# A_SDI_pecu_v2 A_GSI_pecu_v2 A_labourexp_pecu_v2 A_luh_pecu_v2 A_ganh_pecu_v2 A_feedrt_pecu_v2 
# A_SDI_agrop_v2 A_GSI_agrop_v2 A_labourexp_agrop_v2 A_luh_agrop_v2 A_ganh_agrop_v2 A_feedrt_agrop_v2 

# TRANSFORMATION
# T_farmtype_v2
# T_farmtype_agri_v2
# T_farmtype_pecu_v2
# T_farmtype_agrop_v2
