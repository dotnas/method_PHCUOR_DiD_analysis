## -------PHCUOR DiD analsis----------------------------------------------------
 
# Load Required Libraries
library(tidyverse)
library(stringmagic)
library(dreamerr)
library(fixest)
library(mice)
library(survey)
library(srvyr)
library(haven)
library(Hmisc)
library(sjPlot)
library(dplyr) 
library(labelled)
library(flextable)
library(gt)
library(kableExtra)
library(modelsummary)


# Data Acquisition and Cleaning - MICS3 2007
## Child's Data (mics3_ch.sav)

# Load and Prepare mics3_ch Data 
# Read SPSS file
mics3_ch_raw1 <- read_sav("Data/mics3_ch.sav")

# Checking variable labels and structure
label(mics3_ch_raw1)
view_df(mics3_ch_raw1)

# Select and Rename mics3_ch Variables

mics3_ch_raw2 <- mics3_ch_raw1 %>%
  mutate(
    state = hh7, 
    cluster_id = UF1,
    household_id = UF2,
    child_id = UF4, 
    carer_id = UF6,
    child_uid = paste(cluster_id, household_id, child_id, sep = "_"),
    mother_uid = paste(cluster_id, household_id, carer_id, sep = "_"),
    dob_day = UF10D, 
    dob_month = UF10M, 
    dob_year = UF10Y, 
    age_child_years = UF11, 
    age_child_months6 = CAGE_6, 
    age_child_months12 = CAGE_11, 
    sex_child = HL4, 
    education_mom = MELEVEL,
    chweight_raw = chweight,
    survey_weightch = chweight / 1e6,
    wealth_quintile = wlthind5, 
    wealth_score = wlthscor, 
    dpt1_equivalent = IM15, 
    measles = IM17, 
    bcg = IM11
  ) %>%
  select(
    state, cluster_id, child_id, carer_id, child_uid, mother_uid,  
    dob_day, dob_month, dob_year, 
    age_child_years, age_child_months6, age_child_months12, 
    sex_child, education_mom, 
    wealth_quintile, wealth_score, 
    dpt1_equivalent, measles, bcg
  )

# Wrangling mics3_ch Data

mics3_ch_raw3 <- mics3_ch_raw2 %>%
  
  # Filter states
  filter(state %in% c(1, 2, 5, 7, 10, 11, 15, 16, 17, 18, 19, 20, 
                      21, 25, 26, 27, 28, 30, 31, 32, 33, 34, 35, 3, 
                      4, 6, 8, 9, 12, 13, 14, 22, 23, 24, 29, 36, 37)) %>%
  
  # Define adopter type (treatment/control)
  mutate(
    state_lbl = as_factor(state),
    adopter_type_bin = if_else(state_lbl %in% c("Abia", "Adamawa", "Bauchi", "Benue", "Delta", "Ebonyi", "Gombe", "Imo", 
                                                "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Nasarawa", "Niger", "Ogun", 
                                                "Ondo", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe"), 1, 0),
    adopter_type_fac = factor(adopter_type_bin, 
                              levels = c(0, 1), 
                              labels = c("late_adopters", "early_adopters"))
  ) %>%
  select(-state_lbl) %>%
  mutate(
    dob_day   = na_if(dob_day, 97) %>% na_if(98) %>% na_if(99),
    dob_month = na_if(dob_month, 97) %>% na_if(98) %>% na_if(99),
    dob_year  = na_if(dob_year, 9997) %>% na_if(9998) %>% na_if(9999)
  ) %>%
  
  # Filter children aged <= 23 months
  filter(age_child_months6 >= 1 & age_child_months6 < 4) %>%
  mutate(
    sex_child = na_if(sex_child, 9),
    sex_bin   = if_else(sex_child == 1, 1, 0),  # Male = 1, Female = 0
    sex_fac   = factor(sex_bin, levels = c(0, 1), labels = c("female", "male"))
  ) %>%
  mutate(
    education_mom = na_if(education_mom, 9),
    education_bin = case_when(
      education_mom == 1 ~ 0,               
      education_mom %in% c(2, 3, 4) ~ 1,    
      TRUE ~ NA_integer_
    ),
    education_fac = factor(education_bin, 
                           levels = c(0, 1), 
                           labels = c("no_education", "any_education"))
  ) %>%
  mutate(
    across(c(dpt1_equivalent, measles, bcg), ~ na_if(., 8) %>% na_if(9)),
    fully_vaccinated_bin = if_else(
      dpt1_equivalent == 1 & measles == 1 & bcg == 1, 1, 0, NA_real_
    ),
    fully_vaccinated_fac = factor(
      fully_vaccinated_bin, 
      levels = c(0, 1), 
      labels = c("not_fully_vaccinated", "fully_vaccinated")
    )
  ) %>%
  
  # Year identifier mics3_ch
  mutate(year = 2007)

# Taking out labels for analysis
mics3_ch_clean <- zap_labels(mics3_ch_raw3)


## Women's Data (mics3_wm.sav)

# Load and Inspect Raw mics3_wm Data

# Read SPSS file
mics3_wm_raw1 <- read_sav("Data/mics3_wm.sav")

# Checking variable labels and structure
label(mics3_wm_raw1)
view_df(mics3_wm_raw1)
head(mics3_wm_raw1)

# Select and Rename mics3_wm Variables

mics3_wm_raw2 <- mics3_wm_raw1 %>% 
  mutate(
    cluster_id = WM1,
    household_id = WM2,
    rural_urban = HH6, 
    woman_id = WM4,
    mother_uid = paste(cluster_id, household_id, woman_id, sep = "_"),
    year_interview = WM6Y,  
    mum_agecat = WAGE, 
    mum_agenum = WM9, 
    delivery_place = MN8,
    livebirth_last2yrs = CM12,
    ever_hivtest = HA15,
    postnatal_check = MN13A,
    ever_childdeath = CM7,
    wmweight_raw = wmweight,
    survey_weightwm = wmweight / 1e6
  ) %>% 
  select(
    cluster_id, household_id, rural_urban, woman_id, mother_uid, 
    year_interview, mum_agecat, mum_agenum, delivery_place, 
    livebirth_last2yrs, ever_hivtest, postnatal_check, 
    ever_childdeath, wmweight_raw, survey_weightwm
  ) 

# Wrangling mics3_wm Data

# Filtering women with live birth in the last 2 yrs
mics3_wm_raw3 <- mics3_wm_raw2 %>% 
  filter(livebirth_last2yrs == "Y")
mics3_wm_raw3 <- mics3_wm_raw3 %>%
  mutate(
    rural_urban = na_if(rural_urban, 9),
    rural_urban_bin = case_when(
      rural_urban == 1 ~ 0,  
      rural_urban == 2 ~ 1,  
      TRUE ~ NA_real_
    ),
    rural_urban_fac = factor(rural_urban_bin, 
                             levels = c(0, 1), 
                             labels = c("Rural", "Urban")),
    mum_agecat_fac = factor(
      mum_agecat,
      levels = 1:7,
      labels = c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49")
    ),
    delivery_place = na_if(delivery_place, 99),
    facility_birth_bin = case_when(
      delivery_place %in% c(21, 22, 26, 31, 32, 33, 36, 96) ~ 1,  
      delivery_place %in% c(11, 12) ~ 0,  
      TRUE ~ NA_real_
    ),
    facility_birth_fac = factor(facility_birth_bin, 
                                levels = c(0, 1), 
                                labels = c("Home", "Facility")),
    ever_hivtest = na_if(ever_hivtest, 9),
    ever_hivtest_bin = case_when(
      ever_hivtest == 1 ~ 1,
      ever_hivtest == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_hivtest_fac = factor(ever_hivtest_bin, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")),
    
    postnatal_check = na_if(postnatal_check, 9),
    postnatal_check_bin = case_when(
      postnatal_check == 1 ~ 1,
      postnatal_check == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    postnatal_check_fac = factor(postnatal_check_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes")),
    
    ever_childdeath = na_if(ever_childdeath, 9),
    ever_childdeath_bin = case_when(
      ever_childdeath == 1 ~ 1,
      ever_childdeath == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_childdeath_fac = factor(ever_childdeath_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes"))
  )

# Removing labels for analysis
mics3_wm_clean <- zap_labels(mics3_wm_raw3)

# Merging mics3_ch & mics3_wm Data

# Removing cluster_id (already in child data)
mics3_wm_clean <- mics3_wm_clean %>%
  select(-cluster_id)

# Merging child and women's data by mother_uid
mics3_chwn_merged <- mics3_ch_clean %>% 
  left_join(mics3_wm_clean, by = "mother_uid")

# Check merged data structure

glimpse(mics3_chwn_merged)

# Data Acquisition and Cleaning - MICS4 2011

## Child’s Data (mics4_ch.sav)

# Load and Prepare mics4_ch Data

# Read SPSS file
mics4_ch_raw1 <- read_sav("Data/mics4_ch.sav")

# Checking variable labels and structure
label(mics4_ch_raw1)
view_df(mics4_ch_raw1)

# Select and Rename mics4_ch Variables

mics4_ch_raw2 <- mics4_ch_raw1 %>%
  mutate(
    state = HH7, 
    cluster_id = UF1,
    household_id = UF2,
    child_id = UF4, 
    carer_id = UF6,
    child_uid = paste(cluster_id, household_id, child_id, sep = "_"),
    mother_uid = paste(cluster_id, household_id, carer_id, sep = "_"),
    dob_day = AG1D, 
    dob_month = AG1M, 
    dob_year = AG1Y, 
    age_child_years = CAGE, 
    age_child_months6 = CAGE_6, 
    age_child_months12 = CAGE_11, 
    sex_child = HL4, 
    education_mom = melevel,
    chweight_raw = chweight,
    survey_weightch = chweight / 1e6,
    wealth_quintile = windex5, 
    wealth_score = wscore, 
    dpt1_equivalent = IM11, 
    measles = IM16, 
    bcg = IM7
  ) %>%
  select(
    state, cluster_id, child_id, carer_id, child_uid, mother_uid,  
    dob_day, dob_month, dob_year, 
    age_child_years, age_child_months6, age_child_months12, 
    sex_child, education_mom, 
    wealth_quintile, wealth_score, 
    dpt1_equivalent, measles, bcg
  )

# Initial data checks
glimpse(mics4_ch_raw2)
head(mics4_ch_raw2)
view_df(mics4_ch_raw2)

#  Wrangling mics4_ch Data

mics4_ch_raw3 <- mics4_ch_raw2 %>%
  
  # Filter states
  filter(state %in% c(1, 2, 5, 7, 10, 11, 15, 16, 17, 18, 19, 20, 
                      21, 25, 26, 27, 28, 30, 31, 32, 33, 34, 35, 3, 
                      4, 6, 8, 9, 12, 13, 14, 22, 23, 24, 29, 36, 37)) %>%
  
  # Define adopter type (treatment/control)
  mutate(
    state_lbl = as_factor(state),
    adopter_type_bin = if_else(state_lbl %in% c("Abia", "Adamawa", "Bauchi", "Benue", "Delta", "Ebonyi", "Gombe", "Imo", 
                                                "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Nasarawa", "Niger", "Ogun", 
                                                "Ondo", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe"), 1, 0),
    adopter_type_fac = factor(adopter_type_bin, 
                              levels = c(0, 1), 
                              labels = c("late_adopters", "early_adopters"))
  ) %>%
  select(-state_lbl) %>%
  mutate(
    dob_day   = na_if(dob_day, 97) %>% na_if(98) %>% na_if(99),
    dob_month = na_if(dob_month, 97) %>% na_if(98) %>% na_if(99),
    dob_year  = na_if(dob_year, 9997) %>% na_if(9998) %>% na_if(9999)
  ) %>%
  
  # Filter children aged <= 23 months
  filter(age_child_months6 >= 1 & age_child_months6 < 4) %>%
  mutate(
    sex_child = na_if(sex_child, 9),
    sex_bin   = if_else(sex_child == 1, 1, 0),  # Male = 1, Female = 0
    sex_fac   = factor(sex_bin, levels = c(0, 1), labels = c("female", "male"))
  ) %>%
  mutate(
    education_mom = na_if(education_mom, 9),
    education_bin = case_when(
      education_mom == 1 ~ 0,                
      education_mom %in% c(2, 3, 4) ~ 1,     
      TRUE ~ NA_integer_
    ),
    education_fac = factor(education_bin, 
                           levels = c(0, 1), 
                           labels = c("no_education", "any_education"))
  ) %>%
  mutate(
    across(c(dpt1_equivalent, measles, bcg), ~ na_if(., 8) %>% na_if(9)),
    fully_vaccinated_bin = if_else(
      dpt1_equivalent == 1 & measles == 1 & bcg == 1, 1, 0, NA_real_
    ),
    fully_vaccinated_fac = factor(
      fully_vaccinated_bin, 
      levels = c(0, 1), 
      labels = c("not_fully_vaccinated", "fully_vaccinated")
    )
  ) %>%
  
  # Year identifier mics4_ch
  mutate(year = 2011)

# Less labels for analysis
mics4_ch_clean <- zap_labels(mics4_ch_raw3)

# Final checks mics4_ch
glimpse(mics4_ch_clean)
head(mics4_ch_clean)
view_df(mics4_ch_clean)

## Women's Data (mics4_wm.sav)

# Load and Prepare mics4_wm Data 

# Read SPSS file
mics4_wm_raw1 <- read_sav("Data/mics4_wm.sav")

# View variable labels and structure
label(mics4_wm_raw1)
view_df(mics4_wm_raw1)
head(mics4_wm_raw1)

# Select and Rename mics4_wm Variables

mics4_wm_raw2 <- mics4_wm_raw1 %>% 
  mutate(
    cluster_id = WM1,
    household_id = WM2,
    rural_urban = HH6, 
    woman_id = WM4,
    mother_uid = paste(cluster_id, household_id, woman_id, sep = "_"),
    year_interview = WM6Y,  
    mum_agecat = WAGE, 
    mum_agenum = WB2, 
    delivery_place = MN18,
    livebirth_last2yrs = CM13,
    ever_hivtest = HA24,
    postnatal_check = MN1,
    ever_childdeath = CM8,
    wmweight_raw = wmweight,
    survey_weightwm = wmweight / 1e6
  ) %>% 
  select(
    cluster_id, household_id, rural_urban, woman_id, mother_uid, 
    year_interview, mum_agecat, mum_agenum, delivery_place, 
    livebirth_last2yrs, ever_hivtest, postnatal_check, 
    ever_childdeath, wmweight_raw, survey_weightwm
  ) 

# Wrangling mics4_wm Data

# Filter by women with live birth in the last 2 yrs
mics4_wm_raw3 <- mics4_wm_raw2 %>% 
  filter(livebirth_last2yrs == "Y")
mics4_wm_raw3 <- mics4_wm_raw3 %>%
  mutate(
    rural_urban = na_if(rural_urban, 9),
    rural_urban_bin = case_when(
      rural_urban == 1 ~ 0,  # Rural
      rural_urban == 2 ~ 1,  # Urban
      TRUE ~ NA_real_
    ),
    rural_urban_fac = factor(rural_urban_bin, 
                             levels = c(0, 1), 
                             labels = c("Rural", "Urban")),
    
    # Age category
    mum_agecat_fac = factor(
      mum_agecat,
      levels = 1:7,
      labels = c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49")
    ),
    delivery_place = na_if(delivery_place, 99),
    facility_birth_bin = case_when(
      delivery_place %in% c(21, 22, 26, 31, 32, 33, 36, 96) ~ 1,  
      delivery_place %in% c(11, 12) ~ 0,  
      TRUE ~ NA_real_
    ),
    facility_birth_fac = factor(facility_birth_bin, 
                                levels = c(0, 1), 
                                labels = c("Home", "Facility")),
    
    ever_hivtest = na_if(ever_hivtest, 9),
    ever_hivtest_bin = case_when(
      ever_hivtest == 1 ~ 1,
      ever_hivtest == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_hivtest_fac = factor(ever_hivtest_bin, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")),
    
    postnatal_check = na_if(postnatal_check, 9),
    postnatal_check_bin = case_when(
      postnatal_check == 1 ~ 1,
      postnatal_check == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    postnatal_check_fac = factor(postnatal_check_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes")),
    
    ever_childdeath = na_if(ever_childdeath, 9),
    ever_childdeath_bin = case_when(
      ever_childdeath == 1 ~ 1,
      ever_childdeath == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_childdeath_fac = factor(ever_childdeath_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes"))
  )

# Remove labels for analysis
mics4_wm_clean <- zap_labels(mics4_wm_raw3)

# Check final women's data
glimpse(mics4_wm_clean)

# Merging mics4_ch & mics4_wm Data

# Removing cluster_id (already in child data)
mics4_wm_clean <- mics4_wm_clean %>%
  select(-cluster_id)

# Merging child and women's data by mother_uid
mics4_chwn_merged <- mics4_ch_clean %>% 
  left_join(mics4_wm_clean, by = "mother_uid")

# Check merged data structure
glimpse(mics4_chwn_merged)

# Data Acquisition and Cleaning - MICS5 2016

## Child's Data (mics5_ch.sav)

# Load and Prepare mics5_ch Data 

mics5_ch_raw1 <- read_sav("Data/mics5_ch.sav")

# View variable labels and structure
label(mics5_ch_raw1)
view_df(mics5_ch_raw1)

# Select and Rename mics5_ch Variables

mics5_ch_raw2 <- mics5_ch_raw1 %>%
  mutate(
    state = HH7, 
    cluster_id = UF1,
    household_id = UF2,
    child_id = LN, 
    carer_id = UF6,
    child_uid = paste(cluster_id, household_id, child_id, sep = "_"),
    mother_uid = paste(cluster_id, household_id, carer_id, sep = "_"),
    dob_day = AG1D, 
    dob_month = AG1M, 
    dob_year = AG1Y, 
    age_child_years = CAGE, 
    age_child_months6 = CAGE_6, 
    age_child_months12 = CAGE_11, 
    sex_child = HL4, 
    education_mom = melevel,
    chweight_raw = chweight,
    survey_weightch = chweight / 1e6,
    wealth_quintile = windex5, 
    wealth_score = wscore, 
    dpt1_equivalent = IM12A, 
    measles = IM16, 
    bcg = IM7
  ) %>%
  select(
    state, cluster_id, child_id, carer_id, child_uid, mother_uid,  
    dob_day, dob_month, dob_year, 
    age_child_years, age_child_months6, age_child_months12, 
    sex_child, education_mom, 
    wealth_quintile, wealth_score, 
    dpt1_equivalent, measles, bcg
  )

#  Wrangling mics5_ch Data

mics5_ch_raw3 <- mics5_ch_raw2 %>%
  
  # Filter states
  filter(state %in% c(1, 2, 5, 7, 10, 11, 15, 16, 17, 18, 19, 20, 
                      21, 25, 26, 27, 28, 30, 31, 32, 33, 34, 35, 3, 
                      4, 6, 8, 9, 12, 13, 14, 22, 23, 24, 29, 36, 37)) %>%
  
  # Define adopter type (treatment/control)
  mutate(
    state_lbl = as_factor(state),
    adopter_type_bin = if_else(state_lbl %in% c("Abia", "Adamawa", "Bauchi", "Benue", "Delta", "Ebonyi", "Gombe", "Imo", 
                                                "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Nasarawa", "Niger", "Ogun", 
                                                "Ondo", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe"), 1, 0),
    adopter_type_fac = factor(adopter_type_bin, 
                              levels = c(0, 1), 
                              labels = c("late_adopters", "early_adopters"))
  ) %>%
  select(-state_lbl) %>%
  mutate(
    dob_day   = na_if(dob_day, 97) %>% na_if(98) %>% na_if(99),
    dob_month = na_if(dob_month, 97) %>% na_if(98) %>% na_if(99),
    dob_year  = na_if(dob_year, 9997) %>% na_if(9998) %>% na_if(9999)
  ) %>%
  
  # Filter children aged <= 23 months
  filter(age_child_months6 >= 1 & age_child_months6 < 4) %>%
  
  mutate(
    sex_child = na_if(sex_child, 9),
    sex_bin   = if_else(sex_child == 1, 1, 0),  # Male = 1, Female = 0
    sex_fac   = factor(sex_bin, levels = c(0, 1), labels = c("female", "male"))
  ) %>%
  mutate(
    education_mom = na_if(education_mom, 9),
    education_bin = case_when(
      education_mom == 1 ~ 0,                
      education_mom %in% c(2, 3, 4) ~ 1,     # 
      TRUE ~ NA_integer_
    ),
    education_fac = factor(education_bin, 
                           levels = c(0, 1), 
                           labels = c("no_education", "any_education"))
  ) %>%
  mutate(
    across(c(dpt1_equivalent, measles, bcg), ~ na_if(., 8) %>% na_if(9)),
    fully_vaccinated_bin = if_else(
      dpt1_equivalent == 1 & measles == 1 & bcg == 1, 1, 0, NA_real_
    ),
    fully_vaccinated_fac = factor(
      fully_vaccinated_bin, 
      levels = c(0, 1), 
      labels = c("not_fully_vaccinated", "fully_vaccinated")
    )
  ) %>%
  
  # Year identifier mics5_ch
  mutate(year = 2016)

# Remove labels for analysis
mics5_ch_clean <- zap_labels(mics5_ch_raw3)

## Women's Data (mics5_wm.sav)

# Load and Inspect Raw mics5_wm Data

# Read SPSS file
mics5_wm_raw1 <- read_sav("Data/mics5_wm.sav")

# View variable labels and structure
label(mics5_wm_raw1)
view_df(mics5_wm_raw1)
head(mics5_wm_raw1)

# Select and Rename mics5_wm Variables

mics5_wm_raw2 <- mics5_wm_raw1 %>% 
  mutate(
    cluster_id = WM1,
    household_id = WM2,
    rural_urban = HH6, 
    woman_id = LN,
    mother_uid = paste(cluster_id, household_id, woman_id, sep = "_"),
    year_interview = WM6Y,  
    mum_agecat = WAGE, 
    mum_agenum = WB2, 
    delivery_place = MN18,
    livebirth_last2yrs = CM13,
    ever_hivtest = HA24,
    postnatal_check = PN19,
    ever_childdeath = CM8,
    wmweight_raw = wmweight,
    survey_weightwm = wmweight / 1e6
  ) %>% 
  select(
    cluster_id, household_id, rural_urban, woman_id, mother_uid, 
    year_interview, mum_agecat, mum_agenum, delivery_place, 
    livebirth_last2yrs, ever_hivtest, postnatal_check, 
    ever_childdeath, wmweight_raw, survey_weightwm
  ) 

# Wrangling mics5_wm Data

# Filter women with live birth in the last 2 yrs
mics5_wm_raw3 <- mics5_wm_raw2 %>% 
  filter(livebirth_last2yrs == "Y")

mics5_wm_raw3 <- mics5_wm_raw3 %>%
  mutate(
    rural_urban = na_if(rural_urban, 9),
    rural_urban_bin = case_when(
      rural_urban == 1 ~ 0,  
      rural_urban == 2 ~ 1,  # 
      TRUE ~ NA_real_
    ),
    rural_urban_fac = factor(rural_urban_bin, 
                             levels = c(0, 1), 
                             labels = c("Rural", "Urban")),
    
    mum_agecat_fac = factor(
      mum_agecat,
      levels = 1:7,
      labels = c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49")
    ),
    
    delivery_place = na_if(delivery_place, 99),
    facility_birth_bin = case_when(
      delivery_place %in% c(21, 22, 26, 31, 32, 33, 36, 96) ~ 1,  
      delivery_place %in% c(11, 12) ~ 0,  
      TRUE ~ NA_real_
    ),
    facility_birth_fac = factor(facility_birth_bin, 
                                levels = c(0, 1), 
                                labels = c("Home", "Facility")),
    
    ever_hivtest = na_if(ever_hivtest, 9),
    ever_hivtest_bin = case_when(
      ever_hivtest == 1 ~ 1,
      ever_hivtest == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_hivtest_fac = factor(ever_hivtest_bin, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")),
    
    postnatal_check = na_if(postnatal_check, 9),
    postnatal_check_bin = case_when(
      postnatal_check == 1 ~ 1,
      postnatal_check == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    postnatal_check_fac = factor(postnatal_check_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes")),
    
    ever_childdeath = na_if(ever_childdeath, 9),
    ever_childdeath_bin = case_when(
      ever_childdeath == 1 ~ 1,
      ever_childdeath == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ever_childdeath_fac = factor(ever_childdeath_bin, 
                                 levels = c(0, 1), 
                                 labels = c("No", "Yes"))
  )

# Remove labels for analysis
mics5_wm_clean <- zap_labels(mics5_wm_raw3)

view_df(mics5_ch_clean)

# Check final mics5_wm data
glimpse(mics5_wm_clean)
view_df(mics5_wm_clean)

# Merging mics5_ch & mics5_wm Data

# Removing cluster_id (already in child data)
mics5_wm_clean <- mics5_wm_clean %>%
  select(-cluster_id)

# Merge child and women's data by mother_uid
mics5_chwn_merged <- mics5_ch_clean %>% 
  left_join(mics5_wm_clean, by = "mother_uid")

# Checking merged data structure
glimpse(mics5_chwn_merged)

# Combine All MICS Waves

# Final datasets for each wave
mics3_final <- mics3_chwn_merged
mics4_final <- mics4_chwn_merged
mics5_final <- mics5_chwn_merged

# All datasets together
mics_stacked <- bind_rows(
  mics3_final,
  mics4_final,
  mics5_final
)

# Save combined dataset
saveRDS(mics_stacked, "mics_stacked_backup.rds")

# Peek at final stacked data
glimpse(mics_stacked)
view_df(mics_stacked)

# Exploratory data analysis

# Load, Prepare and Setup

# Final stacked data
mics_stacked <- readRDS("mics_stacked_backup.rds") %>% 
  mutate(
    wave = factor(case_when(
      year == 2007 ~ "MICS3",
      year == 2011 ~ "MICS4",
      year == 2016 ~ "MICS5"
    )),
    
    state_id = as.numeric(factor(state)),
    
    time = year - min(year)
  )

# Figure 1: Facility Birth Trends

ggplot(mics_stacked, aes(x = wave, y = facility_birth_bin, 
                         fill = adopter_type_fac)) +
  stat_summary(geom = "col", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal, 
               width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Facility Birth by Wave and Adopter Type",
       y = "Proportion of Facility Birth",
       x = "Survey Wave") +
  scale_fill_brewer(palette = "Set1", name = "Adopter Type")

# Figure 2: Pre-Trends Visualization

ggplot(mics_stacked, 
       aes(x = wave, y = facility_birth_bin, 
           group = adopter_type_fac, color = adopter_type_fac)) +
  stat_summary(geom = "line", fun = "mean", size = 1.5) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal, width = 0.2) +
  labs(title = "Facility Birth Trends by Adopter Type (Pre-Trend Check)",
       y = "Proportion of Facility Birth",
       x = "Survey Wave") +
  theme_minimal()

# Setup data for further analysis

mics_stacked_clean <- mics_stacked %>%
  mutate(
    
    treated = if_else(adopter_type_bin == 1, 1, 0),
    post = if_else(year >= 2011, 1, 0),
    did_interaction = treated * post,
    rural_urban = factor(rural_urban, levels = 1:2, labels = c("Rural", "Urban")),
    mum_agecat = factor(mum_agecat, levels = 1:7,
                        labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")),
    
    survey_weightwm = as.numeric(survey_weightwm),
    survey_weightwm = if_else(survey_weightwm <= 0, NA_real_, survey_weightwm)
  ) %>%
  filter(
    !is.na(facility_birth_bin),
    !is.na(rural_urban),
    !is.na(survey_weightwm)
  )

summary(mics_stacked_clean$survey_weightwm)

# svy Design set up

svy_design <- svydesign(
  ids = ~cluster_id,          
  weights = ~survey_weightwm, 
  strata = ~year,             
  data = mics_stacked_clean,
  nest = TRUE                 
)

# Difference-in-Differences Analysis

# Standard DiD (Unweighted)


did_unweighted <- feols(
  facility_birth_bin ~ i(post, treated, ref = 0) +
    education_bin + rural_urban + mum_agecat + factor(wealth_quintile) |
    cluster_id,
  data = mics_stacked_clean,
  vcov = "cluster"
)
etable(did_unweighted, title = "Unweighted DiD Model")

# DiD Survey-Weighted (svyglm)


formula <- facility_birth_bin ~ treated * post + did_interaction + 
  education_bin + rural_urban + mum_agecat + factor(wealth_quintile)

did_weighted <- svyglm(
  formula,
  design = svy_design,
  family = quasibinomial()  
)

print(broom::tidy(did_weighted))

tidy_weighted <- broom::tidy(did_weighted) %>%
  filter(term == "did_interaction") %>%
  mutate(
    estimate = estimate,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

kable(tidy_weighted, caption = "Survey-Weighted DiD Estimate") %>%
  kable_styling(bootstrap_options = "striped")

print(tidy_weighted)

## Unweighted vs. Weighted

# DiD Unweighted results from feols

unweighted_term_name <- "i(factor_var = post, var = treated, ref = 0)"

# Unweighted results
unweighted_estimate <- coef(did_unweighted)[unweighted_term_name]
unweighted_se <- se(did_unweighted)[unweighted_term_name]

# CI for unweighted model
unweighted_ci_low <- unweighted_estimate - 1.96 * unweighted_se
unweighted_ci_high <- unweighted_estimate + 1.96 * unweighted_se

# DiD Weighted results from svyglm

weighted_term_name <- "did_interaction" 
weighted_estimate <- coef(did_weighted)[weighted_term_name]
weighted_se <- se(did_weighted)[weighted_term_name]

# Comparison Unweighted vs. weighted results

# Comparison data frame
comparison <- data.frame(
  Model = c("Unweighted", "Survey-Weighted"),
  Estimate = c(unweighted_estimate, tidy_weighted$estimate),
  SE = c(unweighted_se, tidy_weighted$std.error),
  CI_Low = c(unweighted_ci_low, tidy_weighted$conf.low),
  CI_High = c(unweighted_ci_high, tidy_weighted$conf.high)
)

print(comparison)

# Plot Unweighted vs. weighted results

ggplot(comparison, aes(x = Model, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), width = 0.2) +
  labs(title = "DiD Estimates: Unweighted vs. Survey-Weighted",
       y = "Treatment Effect (Percentage Points)", x = "") +
  theme_minimal()

# Models comparison

modelsummary::msummary(
  list("unweighted_term_name" = did_unweighted, "Survey-Weighted" = did_weighted),
  output = "table.html"
)

# Comparison Table

library(modelsummary)
library(flextable)
library(dplyr)

# Models to compare
models <- list(
  "Unweighted Model" = did_unweighted,
  "Survey-Weighted Model" = did_weighted
)

msummary_table <- msummary(
  models,
  title = "Comparison of DiD Estimates",
  stars = TRUE,
  output = "flextable",
  coef_map = c(
    "i(factor_var = post, var = treated, ref = 0)" = "DiD Estimate (Unweighted)",
    "did_interaction" = "DiD Estimate (Survey-Weighted)"
  )
)

#print(msummary_table)

msummary_table <- msummary_table %>%
  flextable::autofit() 

msummary_table

# Models (feols unweighted and svyglm survey weighted) with covariates

# Example of coef_map with more terms
msummary_table <- msummary(
  models,
  title = "Comparison of DiD Estimates",
  stars = TRUE,
  output = "flextable",
  coef_map = c(
    "i(factor_var = post, var = treated, ref = 0)" = "DiD Estimate (Unweighted)",
    "did_interaction" = "DiD Estimate (Survey-Weighted)",
    "education_bin" = "Education (Binary)",
    "rural_urbanUrban" = "Urban Residence",
    "mum_agecat20-24" = "Mother's Age: 20-24", 
    "mum_agecat25-29" = "Mother's Age: 25-29",
    "mum_agecat30-34" = "Mother's Age: 30-34",
    "mum_agecat35-39" = "Mother's Age: 35-39",
    "mum_agecat40-44" = "Mother's Age: 40-44",
    "mum_agecat45-49" = "Mother's Age: 45-49",
    "factor(wealth_quintile)2" = "Wealth Quintile 2", 
    "factor(wealth_quintile)3" = "Wealth Quintile 3",
    "factor(wealth_quintile)4" = "Wealth Quintile 4",
    "factor(wealth_quintile)5" = "Wealth Quintile 5"
  )
)

#print(msummary_table)

msummary_table <- msummary_table %>%
  flextable::autofit() 

msummary_table


