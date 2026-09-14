
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
  skimr,           # For skimming through the data
  lubridate,       # For dates and durations calculations
  readxl,           # For reading excel files
  stats,            # For regression analysis
  rio               # exporting data sets
)


# -- Importing Data ---
raw_data <- read_dta("data/Adol_DUCS_Botnar_22 Baseline.dta")

# -- Preliminary Data Exploration ---
skim(raw_data)


# -- Data Cleaning ---
# Selecting variables that will be used only
new_data <- raw_data %>% 
  select( calc_id1, calc_id2, date_interview, dob, hhid, sd01, sd02, sd03, sd04,sd05,sd06, sd07, sd11,
          nu01, nu02, nu03, nu04, nu04, nu05, nu06, nu07, nu08, nu09, nu10,
          nu11, nu12, nu13, nu14, nu15, nu16, nu17,  nu18, nu19, nu20, nu21,
          nu22, nu23, nu24, nu25, ph01, ph02,ph03, m01, m02, m03, m04, su01,
          su02, su03, su04, su05, su06, su07, su08, su09, hu01, hu02, hu05, 
          hu05_3, hu06, n03c, n04c, n05
          )

# Converting dates 

new_data$dob <- as.Date(new_data$dob, format = "%B %d, %Y")
new_data$date_interview <- as.Date(new_data$date_interview, format = "%B %d, %Y")

# Creating new variables to address the intended objectives
clean_data <- new_data %>% 
  mutate(sex = factor(sd02, levels = c('1','2'), labels = c('M', 'F')),
         in_school = factor(sd03, levels = c('1', '2'), labels = c('Yes', 'No')),
         living_with = case_when(sd07 == 1 | sd07 == 2 ~ 'Parents', 
                                 sd07 == 6 | sd07 == 7 | sd07 == 8 ~ 'Guardians',
                                 sd07 == 4 | sd07 == 5 | sd07 == 3 | sd07 == 94 
                                 | sd07 == 99  ~ 'Others'),
         # Coding the GDQS score
         nug_legumes = case_when(nu01 == 1 ~ 0, nu01 == 2 ~ 2, nu01 == 3 | nu01 == 4 ~ 4),
         nug_dov = case_when(nu02 == 1 ~ 0, nu02 == 2 ~ 0.25, nu02 == 3 | nu02 == 4 ~ 0.5),
         nug_dgv = case_when(nu03 == 1 ~ 0, nu03 == 2 ~ 2, nu03 == 3 | nu03 == 4  ~ 4),
         nug_crucveg = case_when(nu04 == 1 ~ 0, nu04 == 2 ~ 0.25, nu04 == 3 | nu04 == 4 ~ 0.5),
         nug_otherveg = case_when(nu05 == 1 ~ 0, nu05 == 2 ~ 0.25, nu05 == 3 | nu05 == 4 ~ 0.5),
         nug_citrus = case_when(nu06 == 1 ~ 0, nu06 == 2 ~ 1, nu06 == 3 | nu06 == 4 ~ 2),
         nug_dof = case_when(nu07 == 1 ~ 0, nu07 == 2 ~ 1, nu07 == 3 | nu07  == 4  ~ 2),
         nug_otherfru = case_when(nu08 == 1 ~ 0, nu08 == 2 ~ 1, nu08 == 3 | nu08 == 4  ~ 2),
         nug_whtroots = case_when(nu09 == 1 ~ 2, nu09 == 2 ~ 1, nu09 == 3 | nu09 == 4  ~ 0),
         nug_dot = case_when(nu10 == 1 ~ 0, nu10 == 2 ~ 0.25, nu10 == 3 | nu10 == 4  ~ 0.5),
         nug_nutseeds = case_when(nu11 == 1 ~ 0, nu11 == 2 ~ 2, nu11 == 3 | nu11 == 4 ~ 4),
         nug_liqoils = case_when(nu12 == 1 ~ 0, nu12 == 2 ~ 1, nu12 == 3 | nu12 == 4 ~ 2),
         nug_refined = case_when(nu13 == 1 ~ 2, nu13 == 2 ~ 1, nu13 == 3 | nu13 == 4 ~ 0),
         nug_wholeg = case_when(nu14 == 1 ~ 0, nu14 == 2 ~ 1, nu14 == 3 | nu14 == 4 ~ 2),
         nug_unprocsd = case_when(nu15 == 1 ~ 0, nu15 == 2 ~ 1, nu15 == 3 | nu15 == 4 ~ 0),
         nug_procesd = case_when(nu16 == 1 ~ 2, nu16 == 2 ~ 1, nu16 == 3 | nu16 == 4 ~ 0),
         nug_poultry = case_when(nu17 == 1 ~ 0, nu17 == 2 ~ 1, nu17 == 3 | nu17 == 4 ~ 2),
         nug_fish = case_when(nu18 == 1 ~ 0, nu18 == 2 ~ 1, nu18 == 3 | nu18 == 4 ~ 2),
         nug_eggs = case_when(nu19 == 1 ~ 0, nu19 == 2 ~ 1, nu19 == 3 | nu19 == 4 ~ 2),
         nug_sweetsice = case_when(nu20 == 1 ~ 2, nu20 == 2 ~ 1, nu20 == 3 | nu20 == 4 ~ 0),
         nug_sugarsweet = case_when(nu21 == 1 ~ 2, nu21 == 2 ~ 1, nu21 == 3 | nu21 == 4 ~ 0),
         nug_juice = case_when(nu22 == 1 ~ 2, nu22 == 2 ~ 1, nu22 == 3 | nu22 == 4 ~ 0),
         nug_deepfri = case_when(nu23 == 1 ~ 2, nu23 == 2 ~ 1, nu23 == 3 | nu23 == 4 ~ 0),
         nug_lowfatd = case_when(nu24 == 1 ~ 0, nu24 == 2 ~ 1, nu24 == 3 | nu10 == 4 ~ 2),
         nug_highfatd = case_when(nu25 == 1 ~ 0, nu25 == 2 ~ 1, nu25 == 3 ~ 2, nu25 == 4 ~ 0),
         
         # Healthy food score 
         gdqs_healthy = round(rowSums(cbind(nu01,nu02,nu03,nu04,nu05, nu07, nu08, nu09, nu10,
                                nu11, nu12, nu13, nu14,nu17, nu18, nu19))),
         # Healthy food score (levels by median)
         healthy_foods = ifelse(gdqs_healthy > 30, 'High', 'Low'),
         
         # Unhealthy food score
         gdqs_unhealthy = rowSums(cbind(nu16, nu20, nu21, nu22, nu23, nu24)),
         # Unhealthy food score (levels by median)
         unhealthy_foods = ifelse(gdqs_unhealthy > 10, 'High', 'Low'),
         
         # Unhealthy when consumed in excess
         gdqs_excess_unhealthy = rowSums(cbind(nu25,nu15)),
         # Unhealthy when excess food score (levels by median)
         excess_unhealthy_foods = ifelse(gdqs_excess_unhealthy > 3, 'High', 'Low'),
         
         # Total Score
         gdqs_score = rowSums(across(starts_with('nug'))),
         # Other variables
         age = sd01,
         # Age in months (for WHO BMI for age scores)
         age_interval = interval(dob, date_interview),
         age_months = round(as.numeric(age_interval, unit = 'months'),0),
         age_group = case_when( sd01 <= 14 ~ '11-14', sd01 > 14 ~ '15-19'),
         substance_use = case_when(su07 == 1 ~ 'Yes', su07 == 2 ~ 'No'),
         alcohol_use = case_when(su05 == 1 ~ 'Yes', su05 == 2 ~ 'No'),
         cigarette_smoking = case_when(su01 == 1 ~ 'Yes', su01 == 2 ~ 'No'),
         literacy = case_when(sd05 == 1 ~ 'Read only', 
                              sd05 == 2 ~ 'Write only', 
                              sd05 == 3 ~ 'Write and Read', 
                              sd05 == 4 ~ 'No'),
         #Physical activity (from WHO references - everyday(7 days))
         physical_activity = case_when(ph03 < 7 ~ 'Low', ph03 >= 7 ~ 'High'),
         #Depression Symptoms, (if there is any?)
         depression_symptoms = ifelse( m01 == 0 | m02 == 0, 'No', 'Yes'),
         manual_labor = ifelse(sd06 == 2, 'Yes', 'No'),
         # SES score
         ses = sd11,
         ses_level = case_when(sd11 >= 0 & sd11 <= 3 ~ 'Low',
                               sd11 >= 4 & sd11 <= 7 ~ 'Middle',
                               sd11 > 7 ~ 'High'),
         # BMI classification 
         bmi = n05 # (Further calculations below)
         )


# Obesity estimation using z-scores --------------------

boys <- read_excel("who_tables/boys.xlsx")
girls <- read_excel("who_tables/girls.xlsx")

obesity_sd <- boys %>% 
  full_join(girls, by = join_by(Month)) %>% 
  mutate( age_sd = Month,
          girls_sd01 = SD1.y,
          girls_sd02 = SD2.y,
          boys_sd01 = SD1.x,
          boys_sd02 = SD2.x
          ) %>% 
  select(age_sd, girls_sd01, boys_sd01,boys_sd02, girls_sd02)


obesity_data <- clean_data %>% 
  mutate(age_sd = age_months) %>% 
  left_join(obesity_sd, by = join_by(age_sd)) %>% 
  mutate(overweight_pr = case_when(sex == "M" & bmi > boys_sd01 ~ 'Yes',
                                   sex == "M" & bmi <= boys_sd01 ~ 'No',
                                   sex == 'F' & bmi > girls_sd01 ~ 'Yes',
                                   sex == 'F' & bmi <= girls_sd01 ~ 'No',
                                   age_sd > 228 & bmi >= 25 ~ 'Yes',
                                   age_sd > 228 & bmi < 25 ~ 'No'),
         bmi_category = case_when(sex == "M" & bmi > boys_sd01 & bmi <= boys_sd02 ~ 'Overweight',
                                  sex == "M" & bmi > boys_sd02 ~ 'Obesity',
                                  sex == "M" & bmi <= boys_sd01 ~ 'Normal',
                                  sex == 'F' & bmi > girls_sd01 & bmi <= girls_sd02 ~ 'Overweight',
                                  sex == 'F' & bmi > girls_sd02 ~ 'Obesity',
                                  sex == 'F' & bmi <= girls_sd01 ~ 'Normal',
                                  age_sd > 228 & bmi >= 25 & bmi < 30  ~ 'Overweight',
                                  age_sd > 228 & bmi >= 30 ~ 'Obesity',
                                  age_sd > 228 & bmi < 25 ~ 'Normal' )
         )


data <- obesity_data %>% 
  select(sex, in_school, living_with, gdqs_score, gdqs_unhealthy, gdqs_healthy,
         healthy_foods, unhealthy_foods, excess_unhealthy_foods, 
         age, age_group, substance_use, alcohol_use, cigarette_smoking,
         literacy, physical_activity, depression_symptoms, manual_labor, 
         ses, ses_level, overweight_pr, bmi_category)

data$ses_level <- factor(data$ses_level, levels = c('High', 'Middle', 'Low'))
data$bmi_category <- factor(data$bmi_category, levels = c('Obesity', 'Overweight', 'Normal'))
data$living_with <- factor(data$living_with, levels = c('Parents', 'Guardians', 'Others'))
data$literacy <- factor(data$literacy, levels = c('No', 'Read only', 'Write only', 'Write and Read'))
data$overweight_pr <- factor(data$overweight_pr, levels = c('No', 'Yes'))

# Exploratory Data Analysis --------------------------------------------

# Quick check on the missing data of the final dataset
sum(is.na(data))
sapply(data, function(x) sum(is.na(x)) )

# Distribution of SES scores


# Distribution of BMI scores








