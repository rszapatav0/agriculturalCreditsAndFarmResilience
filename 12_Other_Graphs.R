# ---------------------------------------------------------------------------- #
#   Other graphs
#   Creation:     March 2024
#   Last edition: March 2024

# This script generate other graphs for keeping the same format.
# ---------------------------------------------------------------------------- #


# ========================================================================= #
# PACKAGES    -------------------------------------------------------------
# ========================================================================= #

if (!require("haven"))    install.packages("haven");    library(haven)
if (!require("ggplot2"))       install.packages("ggplot2");       library(ggplot2)
if (!require("hrbrthemes"))    install.packages("hrbrthemes");    library(hrbrthemes)

rm(list=ls())

onecolor  <- "#2c7a27"
twocolors <- c("#ACACAC", "#2c7a27")


# ========================================================================= #
# GRAPHS     --------------------------------------------------------------
# ========================================================================= #

## Choques ELCA ----------------------------------------------------------
setwd("C:/Users/userecon10/Desktop/Raquel Sofia Zapata/")
shocks_elca_data <- read_dta("./00. Processed data/Base_choques_ELCA_R.dta")

png("./04_Plots/Shocks_ELCA.png", width = 1200, height = 800)
ggplot(shocks_elca_data, aes(x=Region, y=shockd, fill=Shock)) +
  geom_bar(position="dodge", stat = "identity", color="#FFFFFF", alpha = 0.5) +
  scale_fill_manual(values=c("#0a0a0a", "#8a8a8a","#d4d4d4", "#2c7a27", "#4dd644")) +
  xlab("Region") + ylab("Average rate of perceived shoks") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20) +
  theme(legend.position = 'right', legend.text = element_text(size=20), legend.title = element_text(size=20),
        axis.text.x = element_text(size=20))
dev.off()


## Composite indicators histogram -----------------------------------------
indicators_database <- read_dta("./00. Processed data/indicadoresCompuestos.dta")

#Robustness
png("./04_Plots/Histogram_R_ci_3.png", width = 1200, height = 800)
  ggplot(indicators_database, aes(x=R_ci_3)) +
  geom_histogram(fill=onecolor, color="#FFFFFF", size=0, alpha=0.5) + #, bins=20
  xlab("") + ylab("Frequency") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20)
dev.off()

#Adaptation
png("./04_Plots/Histogram_A_ci_4.png", width = 1200, height = 800)
ggplot(indicators_database, aes(x=A_ci_4)) +
  geom_histogram(fill=onecolor, color="#FFFFFF", size=0, alpha=0.5) +
  xlab("") + ylab("Frequency") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20)
dev.off()

#Transformation
png("./04_Plots/Histogram_T_ci_1.png", width = 1200, height = 800)
ggplot(indicators_database, aes(x=T_ci_1)) +
  geom_histogram(fill=onecolor, color="#FFFFFF", size=0, alpha=0.5) +
  xlab("") + ylab("Frequency") +
  theme_ipsum(axis_title_size = 20, axis_text_size = 20)
dev.off()
  












