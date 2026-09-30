# --------------------------
# Script Name ; 02_Analysis
# Script Author: Jofrey Segeza Amos
# Script Purpose: Data Analysis for a masters dissertation
# Last Edited; 11-Aug-2026
# --------------------------

source('01_Cleaning.R')


# SES profile among adolescents (by basic participants characteristics) -----------

(table011 <- data %>% 
   select(sex,age_group, in_school, physical_activity,  
          manual_labor, healthy_foods, unhealthy_foods, 
          excess_unhealthy_foods,
          ses_level, 
          bmi_category) %>% 
   tbl_summary( by = ses_level, percent = 'column',
                label = list(
     sex ~ 'Sex',
     age_group ~ 'Age group',
     in_school ~ 'Currently in School',
     physical_activity ~ 'Physical Activity',
     manual_labor ~ 'Manual Labor',
     healthy_foods ~ 'Healthy foods consumption',
     unhealthy_foods ~ 'Unhealthy foods consumption',
     excess_unhealthy_foods ~ 'Unhealthy when in excess foods consumption',
     bmi_category ~ 'BMI' # work for the BMI
   )) %>% 
   bold_labels() %>% 
   add_overall() %>% 
   as_flex_table() 
)



# Lifestyle factors description ------------------
(table02 <- data %>% 
   select(living_with, substance_use, alcohol_use, cigarette_smoking,
          literacy, depression_symptoms,
          ses_level, overweight_pr) %>% 
   tbl_summary( by = ses_level, percent = 'column', 
                label = list(
     living_with ~ 'Adults living with',
     substance_use ~ 'Substance use',
     alcohol_use ~ 'Alcohol use',
     cigarette_smoking ~ 'Cigarette smoking',
     literacy ~ 'Literacy',
     depression_symptoms ~ 'Depression Symptoms',
     overweight_pr~ 'Overweight/Obesity'
   )) %>% 
   bold_labels() %>% 
   add_overall() %>% 
   as_flex_table() 
)


data |> 
  ggplot()+
  geom_boxplot(aes( x= ses, y= living_with))
  
data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = substance_use))

data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = alcohol_use))

data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = cigarette_smoking))

data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = literacy))

data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = depression_symptoms))

data |> 
  ggplot()+
  geom_boxplot(aes( x = ses, y = bmi_category))

data |> 
  ggplot()+
  geom_histogram(aes( x = ses))



# 01. Physical activity  & Dietary intake Vs SES (by sex)
data |> 
  ggplot(aes(x=gdqs_score, y = ses_level)) +
  geom_boxplot() +
  coord_flip()+
  theme_classic()

# 02. Dietary intake & Physical activity Vs Obesity/Overweight (by sex)


# Association between SES and Overweight/Obesity ----------------

# 01. Univariable

# Sex
(table30 <- glm(overweight_pr ~ sex , data = data, family = binomial()) |> 
   tbl_regression(exponentiate = TRUE)
 )


# Age
(table31 <- glm(overweight_pr ~ age_group, data = data,
               family = binomial()) |> 
    tbl_regression(exponentiate = T)
  )

# School status
(table32 <- glm(overweight_pr ~ in_school, data = data,
               family = binomial()) |> 
    tbl_regression(exponentiate = T )
  )

# People living with
(table33 <- glm(overweight_pr ~ living_with, data = data,
               family = binomial()) |> 
  tbl_regression(exponentiate = T)
  )

# Substance use
(table34 <- glm(overweight_pr ~ substance_use, data = data,
               family = binomial()) |> 
  tbl_regression(exponentiate = T)
  )

# Alcohol use
(table35 <- glm(overweight_pr ~ alcohol_use, data = data,
               family = binomial()) |> 
  tbl_regression(exponentiate = T)
  )

# Literacy
(table36 <- glm(overweight_pr ~ literacy, data = data,
               family = binomial()) |> 
    tbl_regression(exponentiate = T)
  )

# Depression symptoms
(table37 <- glm(overweight_pr ~ depression_symptoms, data = data,
               family = binomial()) |> 
    tbl_regression(exponentiate = T)
  )

# Manual labor
(table38 <- glm(overweight_pr ~ manual_labor, data = data,
               family = binomial()) |> 
    tbl_regression(exponentiate = T)
  )

# SES
(table39 <- glm(overweight_pr ~  ses_level, data = data,
               family = binomial()) |> 
  tbl_regression(exponentiate = T)
  )



# 02. Multivariable
(model_multi <- glm(overweight_pr ~ sex + age_group + in_school +
                     alcohol_use + 
                   #substance_use +
                     depression_symptoms + manual_labor + 
                   ses_level, data = data, family = binomial())
  )
broom::tidy(model_multi, conf.int = T, exponentiate = T)


# Alternatively with GDQS scores included
(model_multi_alt <- glm(overweight_pr ~ sex + age_group + in_school +
                         alcohol_use + healthy_foods + unhealthy_foods +
                         #substance_use + 
                          depression_symptoms + manual_labor + 
                         ses_level + excess_unhealthy_foods,
                       data = data, family = binomial() )
  )
broom::tidy(model_multi_alt, conf.int = T, exponentiate = T)

# Checking for multicollinearlity
car::vif(model_multi)
car::vif(model_multi_alt)

# Combining all the regression tables -------------------------

# Univariable table

(table_univ <- data |>
    tbl_uvregression(
      method = glm,
      y = overweight_pr,
      method.args = list(family = binomial),
      exponentiate = TRUE,
      include = c(ses_level, sex, age_group, in_school, substance_use, 
                  alcohol_use, depression_symptoms, manual_labor),
      hide_n = T,
      label = list(
        ses_level~ 'SES',
        sex ~ 'Sex',
        age_group ~ 'Age group',
        in_school ~ 'Currently in School',
        substance_use ~ 'Substance use',
        alcohol_use ~ 'Alcohol use',
        depression_symptoms ~ 'Depression Symptoms',
        manual_labor ~ 'Manual Labor'
      )
    ))

# Univariable alt
(table_univ_alt <- data |>
    tbl_uvregression(
      method = glm,
      y = overweight_pr,
      method.args = list(family = binomial),
      exponentiate = TRUE,
      include = c(ses_level, sex, age_group, in_school, substance_use, 
                  alcohol_use, depression_symptoms, manual_labor,
                  healthy_foods, unhealthy_foods, excess_unhealthy_foods),
      hide_n = T,
      label = list(
        ses_level~ 'SES',
        sex ~ 'Sex',
        age_group ~ 'Age group',
        in_school ~ 'Currently in School',
        substance_use ~ 'Substance use',
        alcohol_use ~ 'Alcohol use',
        depression_symptoms ~ 'Depression Symptoms',
        manual_labor ~ 'Manual Labor',
        healthy_foods ~ 'Healthy foods consumption',
        unhealthy_foods ~ 'Unhealthy foods consumption',
        excess_unhealthy_foods ~ 'Unhealthy when in excess foods consumption'
      )
    ))




# Multivariable table
(table_multi <- glm(overweight_pr ~ sex + age_group + in_school +
                      alcohol_use + 
                      substance_use + depression_symptoms + manual_labor + 
                      ses_level, data = data, family = binomial()) |> 
    tbl_regression(exponentiate = T ,
                   label = list(
                     sex ~ 'Sex',
                     age_group ~ 'Age group',
                     in_school ~ 'Currently in School',
                     substance_use ~ 'Substance use',
                     alcohol_use ~ 'Alcohol use',
                     depression_symptoms ~ 'Depression Symptoms',
                     manual_labor ~ 'Manual Labor',
                     ses_level~ 'SES'
                   ))
)

# Althernative multivariable table
(table_multi_alt <- glm(overweight_pr ~ sex + age_group + in_school +
                      alcohol_use + healthy_foods + unhealthy_foods + excess_unhealthy_foods + 
                      substance_use + depression_symptoms + manual_labor + 
                      ses_level, data = data, family = binomial()) |> 
    tbl_regression(exponentiate = T ,
                   label = list(
                     sex ~ 'Sex',
                     age_group ~ 'Age group',
                     in_school ~ 'Currently in School',
                     substance_use ~ 'Substance use',
                     alcohol_use ~ 'Alcohol use',
                     depression_symptoms ~ 'Depression Symptoms',
                     manual_labor ~ 'Manual Labor',
                     ses_level~ 'SES',
                     healthy_foods ~ 'Healthy foods consumption',
                     unhealthy_foods ~ 'Unhealthy foods consumption',
                     excess_unhealthy_foods ~ 'Unhealthy when in excess foods consumption'
                   ))
  )


list02 <- list(table_univ, table_multi)
list_alt <- list(table_univ_alt, table_multi_alt)

# Final regression table

(table06 <- tbl_merge(list02, tab_spanner = c("**Unadjusted**", "**Adjusted**")) |> 
    bold_labels() |> 
    as_flex_table())

(table06_alt <- tbl_merge(list_alt, tab_spanner = c("**Unadjusted**", "**Adjusted**")) |> 
    bold_labels() |> 
    as_flex_table())


# 03. Mediation Model

model00 <- glm(overweight_pr ~ ses_level,
               data = data, family = binomial())
summary(model00)
round(exp(coef(model00)),2) 
round(exp(confint(model00)),2)


model000 <- glm(overweight_pr ~ ses_level + healthy_foods,
               data = data, family = binomial())
summary(model000)
round(exp(coef(model000)),2) 
round(exp(confint(model000)),2)

model001 <- glm(overweight_pr ~ ses_level + unhealthy_foods,
               data = data, family = binomial())
summary(model001)
round(exp(coef(model001)),2) 
round(exp(confint(model001)),2)

model002 <- glm(overweight_pr ~ ses_level + excess_unhealthy_foods,
                data = data, family = binomial())
summary(model002)
round(exp(coef(model002)),2) 
round(exp(confint(model002)),2)


# 04. Final Model diagnostics

# Binary outputs
visreg::visreg(model_multi)

acf(residuals(model_multi))
anova(model_multi, model_multi_alt)

car::vif(model_multi) # Variance inflation factor (to assess multicollinearity)


# Saving all the tables and outputs ----------------------

save_as_docx(table011, path = 'output/table01.docx')
save_as_docx(table02, path = 'output/table02.docx')
save_as_docx(table06, path = 'output/table06.docx')
save_as_docx(table06_alt, path = 'output/table06_alt.docx')
save_as_docx(table011, path = 'output/table011.docx')

# Lara's Subset
names(raw_data)
data_nutrition <- raw_data [1:40,c(15,70:94)]
export(data_nutrition, 'output/data_nutrition.dta')


  

