
# --------------------------
# Script Name ; 01_Cleaning
# Script Author: Jofrey Segeza Amos
# Script Purpose: Data Analysis for a masters dissertation
# Last Edited; 12-June-2026
# --------------------------


# --- Loading Libraries ---
pacman::p_load(
  dplyr,           # For data manipulation
  haven,           # For data importing(different file types)
  ggplot2,         # For data visualization
  gtsummary,       # For publication ready summary tables
  flextable,       # for publication ready summary tables
  skimr           # For skimming through the data
)


# -- Importing Data ---
raw_data <- read_dta("data/Adol_DUCS_Botnar_22 Baseline.dta")

# -- Preliminary Data Exploration ---
View(raw_data)


# -- Data Cleaning ---
# -- Exploratiory Data Analysis ---
