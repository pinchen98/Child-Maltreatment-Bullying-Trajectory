# ====================== MCS Data Preparation ====================== #
# Load libraries
library(tidyverse) # for data manipulation and visualization
library(dplyr) # for data manipulation
library(psych) # for data manipulation
library(haven) # for reading SPSS files
library(skimr) # for descriptives
library(writexl) # for writing output to .xlsx
library(naniar)
library(forcats) # for factor manipulation (conversion)
library(mice) # for multiple imputation
library(lavaan) # for identifying auxiliary variables
library(semTools) # for identifying auxiliary variables

# ====== Load data ======
# Baseline demographics and auxiliary variables for imputation
mcs1_cm_derived <- read_sav("mcs1_cm_derived.sav")
mcs1_parent_derived <- read_sav("mcs1_parent_derived.sav")
mcs1_family_derived <- read_sav("mcs1_family_derived.sav")
mcs1_parent_interview <- read_sav("mcs1_parent_interview.sav")

mcs3_cm_derived <- read_sav("mcs3_cm_derived.sav")
mcs3_hhgrid <- read_sav("mcs3_hhgrid.sav")
mcs3_parent_interview <- read_sav("mcs3_parent_interview.sav")
mcs3_family_derived <- read_sav("mcs3_family_derived.sav")
mcs3_parent_derived <- read_sav("mcs3_parent_derived.sav")

# Study variables
mcs3_parent_cm_interview <- read_sav("mcs3_parent_cm_interview.sav")
mcs4_cm_interview <- read_sav("mcs4_cm_interview.sav")
mcs4_parent_cm_interview <- read_sav("mcs4_parent_cm_interview.sav")
mcs5_cm_interview <- read_sav("mcs5_cm_interview.sav")
mcs5_parent_cm_interview <- read_sav("mcs5_parent_cm_interview.sav")
mcs6_cm_interview <- read_sav("mcs6_cm_interview.sav")
mcs6_parent_cm_interview <- read_sav("mcs6_parent_cm_interview.sav")

# ====== Select variables - Demographics ======
# mcs1_cm_derived: child ethnicity (`ADC06E00`), birthweight (`ADBWGT00`), gestational age (`ADGEST00`)
Demo_mcs1_cm_derived <- mcs1_cm_derived %>% 
    select(MCSID, ACNUM00, 
           CM_ethnicity = ADC06E00,
           CM_birthweight = ADBWGT00,
           CM_gestational_age = ADGEST00)

# mcs1_parent_derived: respondent age at birth (`ADDAGB00`), respondent weight (`ADWGTK00`)
Demo_mcs1_parent_derived <- mcs1_parent_derived %>% 
    select(MCSID, APNUM00, AELIG00,
           res_age_at_birth = ADDAGB00,
           res_weight = ADWGTK00)

# mcs1_family_derived: respondent home ownership (`ADROOW00`), mother BMI (`ADMBMI00`)
Demo_mcs1_family_derived <- mcs1_family_derived %>% 
    select(MCSID, 
           res_home_ownership = ADROOW00,
           mother_BMI = ADMBMI00)

# mcs1_parent_interview: marital status at birth (`APFCIN00`), parent highest academic level (`APACQU00`)
Demo_mcs1_parent_interview <- mcs1_parent_interview %>% 
    select(MCSID, APNUM00, AELIG00,
           marital_status_at_birth = APFCIN00,
           parent_academic = APACQU00)

# mcs3_cm_derived: child SDQ (`CEBDTOT`)
Demo_mcs3_cm_derived <- mcs3_cm_derived %>% 
    select(MCSID, CCNUM00, 
           CM_SDQ = CEBDTOT)

# mcs3_hhgrid: child sex (`CHCSEX00`), age (`CHCDBM00`, `CHCDBY00`)
Demo_mcs3_hhgrid <- mcs3_hhgrid %>% 
    select(MCSID, CCNUM00, CELIG00,
           CM_sex = CHCSEX00, 
           CM_age_month = CHCDBM00, 
           CM_age_year = CHCDBY00,
           CM_age = CHCAGE00)

# mcs3_family_derived: family type (`CDHTYS00`), household parent relationship (`CDRELP00`), 
# income (level: `CDOEDP00`, number: `COEDEX00`, quintile: `COECDUK0`) 
Demo_mcs3_family_derived <- mcs3_family_derived %>% 
    select(MCSID, 
           family_type = CDHTYS00, 
           parent_relationship = CDRELP00,
           income_level = CDOEDP00,
           income_number = COEDEX00,
           income_quintile = COECDUK0)

# mcs3_parent_interview: marital status (`CPFCIN00`)
Demo_mcs3_parent_interview <- mcs3_parent_interview %>% 
    select(MCSID, CPNUM00, CELIG00,
           marital_status = CPFCIN00)

# mcs3_parent_derived: respondent-CM relationship (`CDDREL00`), respondent age (`CDDAGI00`),
# respondent ethnicity (`CDD06E00`), NVQ (`CDDNVQ00`), religion (`CDDRLG00`)
Demo_mcs3_parent_derived <- mcs3_parent_derived %>% 
    select(MCSID, CPNUM00, CELIG00,
           res_CM_relationship = CDDREL00,
           res_age = CDDAGI00,
           res_ethnicity = CDD06E00,
           res_NVQ = CDDNVQ00,
           res_religion = CDDRLG00)

# ====== Merge files - Demographics ======
# Merge all demographic data
Demo_mcs <- Demo_mcs3_cm_derived %>%
    full_join(Demo_mcs3_hhgrid, by = c("MCSID", "CCNUM00")) %>%
    full_join(Demo_mcs3_family_derived, by = "MCSID") %>%
    full_join(Demo_mcs3_parent_interview, by = "MCSID", "CELIG00") %>%
    full_join(Demo_mcs3_parent_derived, by = c("MCSID", "CPNUM00", 
                                               "CELIG00.y" = "CELIG00")) %>% 
    full_join(Demo_mcs1_cm_derived, by = c("MCSID", 
                                           "CCNUM00" = "ACNUM00")) %>%
    full_join(Demo_mcs1_parent_derived, by = c("MCSID", 
                                               "CPNUM00" = "APNUM00", 
                                               "CELIG00.y" = "AELIG00")) %>%
    full_join(Demo_mcs1_family_derived, by = "MCSID") %>%
    full_join(Demo_mcs1_parent_interview, by = c("MCSID", 
                                                 "CPNUM00" = "APNUM00", 
                                                 "CELIG00.y" = "AELIG00"))

# Filter Cohort Member (`CCNUM00`) >= 1 (n = 29,953; unchanged)
Demo_mcs <- Demo_mcs %>% 
    filter(CCNUM00 >= 1)

# Remove the duplicated column for `CELIG00.x` 
###### Note: `CELIG00.x` was removed because most values were -1 (Not applicable)
Demo_mcs <- Demo_mcs %>% 
    select(-CELIG00.x)

# Change `CELIG00.y` to `CELIG00`
Demo_mcs <- Demo_mcs %>% 
    rename(CELIG00 = CELIG00.y)

# Check number of family (N = 19,099)
Demo_mcs %>% count(MCSID)

# Check for duplicates in MCSID
Demo_mcs %>% count(MCSID) %>% 
    filter(n > 0) %>% # Check for duplicates
    count(n)

## n = 1 (no duplicate; only one parent respond; n = 8,531)
## n = 2 (two parents/guardians respond; n = 10,425)
## n = 4 (three parents/guardians respond; n = 143)

# ====== Select variables - Study variables ======

# Child maltreatment ======

# mcs3_parent_interview: domestic violence (`CPFORC00`)
## Partner ever used force in relationship (1 = yes, 0 = no)
Study_mcs3_parent_interview <- mcs3_parent_interview %>% 
    select(MCSID, CPNUM00, CELIG00,
           domestic_violence = CPFORC00)

# mcs3_parent_cm_interview: harsh parenting and physical punishment (`CPDISM00`)
## `CPDIIG00`: How often ignores CM when naughty   
## `CPDISM00`: How often smacks CM when naughty
## `CPDISH00`: How often shouts at CM when naughty 
## `CPDIBN00`: How often sends CM to bedroom/naughty chair 
## `CPDITR00`: How often takes away treats from CM when naughty
## `CPDITE00`: How often tells CM off when naughty 
## `CPDIBR00`: How often bribes CM when naughty
Study_mcs3_parent_cm_interview <- mcs3_parent_cm_interview %>% 
    select(MCSID, CPNUM00, CCNUM00, CELIG00,
           CPDIIG00, CPDISM00, CPDISH00, CPDIBN00, CPDITR00, CPDITE00, CPDIBR00)

# Child-reported bullying ======

# mcs4_cm_interview
## Victimisation (`DCSC0036`): How often do other children bully you?  
## Perpetration (`DCSC0037`): How often are you horrible to other children at school? 
Study_mcs4_cm_interview <- mcs4_cm_interview %>% 
    select(MCSID, DCNUM00, DCSC0036, DCSC0037)

# mcs5_cm_interview
## Victimisation (`ECQ56X00`): How often do other children hurt you or pick on you on purpose? 
## Perpetration (`ECQ57X00`): How often do you hurt or pick on other children on purpose? 
Study_mcs5_cm_interview <- mcs5_cm_interview %>% 
    select(MCSID, ECNUM00, ECQ56X00, ECQ57X00)

# mcs6_cm_interview
## Victimisation (`FCHURT00`): How often other children hurt or pick on CM 
## Perpetration (``FCPCKP00``): How often CM hurts or picks on other children   
Study_mcs6_cm_interview <- mcs6_cm_interview %>% 
    select(MCSID, FCNUM00, FCHURT00, FCPCKP00)

# Parent-reported bullying ======

# mcs4_parent_cm_interview
## Victimisation (`DPSDPB00`): CM picked on or bullied by other children   
## Perpetration (`DPSDFB00`): CM fights with or bullies other children
Study_mcs4_parent_cm_interview <- mcs4_parent_cm_interview %>% 
    select(MCSID, DPNUM00, DCNUM00, DELIG00, DPSDPB00, DPSDFB00)

# mcs5_parent_cm_interview
## Victimisation (`EPSDPB00`): CM picked on or bullied by other children
## Perpetration (`EPSDFB00`): CM fights with or bullies other children
Study_mcs5_parent_cm_interview <- mcs5_parent_cm_interview %>% 
    select(MCSID, EPNUM00, ECNUM00, EELIG00, EPSDPB00, EPSDFB00)

# mcs6_parent_cm_interview
## Victimisation (`FPSDPB00`): Picked on or bullied by other children  
## Perpetration (`FPSDFB00`): Often fights with other children or bullies them
Study_mcs6_parent_cm_interview <- mcs6_parent_cm_interview %>% 
    select(MCSID, FPNUM00, FCNUM00, FELIG00, FPSDPB00, FPSDFB00)

# Covariate: parental psychopathology ======

# mcs3_parent_interview: Kessler (K6) Scale
K6_mcs3_parent_interview <- mcs3_parent_interview %>% 
    select(MCSID, CPNUM00, CELIG00,
           CPPHDE00, CPPHHO00, CPPHRF00, CPPHEE00, CPPHWO00, CPPHNE00)

# ====== Merge files - Study variables ======
# Merge all study variable files by ID of cohort member (CM) and parent
Study_mcs3 <- Study_mcs3_parent_interview %>%
    full_join(Study_mcs3_parent_cm_interview, by = c("MCSID", "CPNUM00", "CELIG00")) %>%
    full_join(Study_mcs4_cm_interview, by = c("MCSID", 
                                              "CCNUM00" = "DCNUM00")) %>%
    full_join(Study_mcs5_cm_interview, by = c("MCSID", 
                                              "CCNUM00" = "ECNUM00")) %>%
    full_join(Study_mcs6_cm_interview, by = c("MCSID", 
                                              "CCNUM00" = "FCNUM00")) %>%
    full_join(Study_mcs4_parent_cm_interview, by = c("MCSID", 
                                                     "CCNUM00" = "DCNUM00",
                                                     "CPNUM00" = "DPNUM00",
                                                     "CELIG00" = "DELIG00")) %>%
    full_join(Study_mcs5_parent_cm_interview, by = c("MCSID", 
                                                     "CCNUM00" = "ECNUM00", 
                                                     "CPNUM00" = "EPNUM00",
                                                     "CELIG00" = "EELIG00")) %>%
    full_join(Study_mcs6_parent_cm_interview, by = c("MCSID", 
                                                     "CCNUM00" = "FCNUM00",
                                                     "CPNUM00" = "FPNUM00",
                                                     "CELIG00" = "FELIG00")) %>%
    full_join(K6_mcs3_parent_interview, by = c("MCSID", "CPNUM00", "CELIG00"))

# Filter Cohort Member (`CCNUM00`) >= 1 (n = 33,118; unchanged)
Study_mcs3 <- Study_mcs3 %>% 
    filter(CCNUM00 >= 1)

# Check number of family (N = 16,347)
Study_mcs3 %>% count(MCSID)

# Check for duplicates in MCSID
Study_mcs3 %>% count(MCSID) %>% 
    filter(n > 0) %>% # Check for duplicates
    count(n)

## n = 1 (no duplicate; only one parent respond; n = 2,854)
## n = 2 (two parents/guardians respond; n = 11,251)
## n = 3 (three parents/guardians respond; n = 1,307)
## n = 4 (four parents/guardians respond; n = 878)
## n = 5 (five parents/guardians respond; n = 35)
## n = 6 (six parents/guardians respond; n = 12)
## n = 8 (eight parents/guardians respond; n = 9)
## n = 10 (ten parents/guardians respond; n = 1)

# ====== Merge files - Demographics and study variables ======
# Merge demographic and study variable data 
# by ID of family (`MCSID`), CM (`CCNUM00`), caregiver (`CPNUM00`), and main caregiver (`CELIG00`)
MCS_data <- Demo_mcs %>%
    full_join(Study_mcs3, by = c("MCSID", "CCNUM00", "CPNUM00", "CELIG00"))

###### 16-06-25 Lucy: check other respondent

# Check for duplicates in MCSID
MCS_data %>% count(MCSID) %>% 
    filter(n > 0) %>% # Check for duplicates
    count(n)

## n = 1 (no duplicate; only one parent respond; n = 5,618) --> joined by Demo_mcs3
## n = 2 (two parents/guardians respond; n = 11,294)
## n = 3 (three parents/guardians respond; n = 1,307)
## n = 4 (four parents/guardians respond; n = 878)
## n = 5 (five parents/guardians respond; n = 35)
## n = 6 (six parents/guardians respond; n = 12)
## n = 8 (eight parents/guardians respond; n = 9)
## n = 10 (ten parents/guardians respond; n = 1)

###### Note: Now we have the data for main caregivers and partners;
###### Next step is to separate caregiver and partner response

# ====== Separate files for caregiver and partner and make it to wide data ======

# Check the number of main caregivers (n = 17,801) and partners (n = 14,151)
MCS_data %>% count(CELIG00)

# Separate for caregiver (N = 17,801)
MCS_data_caregiver <- MCS_data %>% 
    filter(CELIG00 == 1)

# Check duplicates in MCS_data_caregiver
## n = 1 (no duplicate; 15,263); n = 2 (1,248); n = 3 (14)
MCS_data_caregiver %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)

# Separate for partner (N = 14,151)
MCS_data_partner <- MCS_data %>%
    filter(CELIG00 == 2)

# Check duplicates in MCS_data_partner
## n = 1 (no duplicate; 11,850); n = 2 (1,128); n = 3 (15)
MCS_data_partner %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)

# Given that there are duplicates in MCSID, prioritise the row with most response;
# if there is missingness, fill in the values from other rows

# Define child-related variables that duplicated rows can contribute to
child_var <- c("DCSC0036", "DCSC0037", "ECQ56X00", "ECQ57X00", "FCHURT00", "FCPCKP00", "DPSDPB00", "DPSDFB00", "EPSDPB00", "EPSDFB00", "FPSDPB00", "FPSDFB00")

# Main caregivers ======
# Fill in the missing values for child-related variables from other duplicated rows
filled_MCS_data_caregiver <- MCS_data_caregiver %>%
    group_by(MCSID, CCNUM00) %>%
    mutate(across(all_of(child_var), ~ {
        # For each variable, fill from non-missing values within the group
        filled <- .[!is.na(.)]
        if (length(filled) == 0) NA else filled[1]
    })) %>% 
    ungroup()

# Choose the row with most complete data as the main row per MCSID
filled_MCS_data_caregiver <- filled_MCS_data_caregiver %>%
    group_by(MCSID, CCNUM00) %>%
    slice(which.max(rowSums(!is.na(cur_data())))) %>%
    ungroup()

# Check duplicates in MCS_data_caregiver
## No duplicates; N = 16,525
filled_MCS_data_caregiver %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)

# Check source of main caregivers
filled_MCS_data_caregiver %>% count(res_CM_relationship)

## 1 (natural mother): n = 14915
## 2 (natural father): n = 393
## 4 (adoptive mother): n = 13
## 5 (adoptive father): n = 1
## 7 (foster mother): n = 1
## 10 (step mother): n = 3
## 11 (step father): n = 1
## 13 (grandmother): n = 37
## 16 (other; female): n = 3

# Partner of main caregivers ======
# Fill in the missing values for child-related variables from other duplicated rows
filled_MCS_data_partner <- MCS_data_partner %>%
    group_by(MCSID, CCNUM00) %>%
    mutate(across(all_of(child_var), ~ {
        # For each variable, fill from non-missing values within the group
        filled <- .[!is.na(.)]
        if (length(filled) == 0) NA else filled[1]
    })) %>% 
    ungroup()

# Choose the row with most complete data as the main row per MCSID
filled_MCS_data_partner <- filled_MCS_data_partner %>%
    group_by(MCSID, CCNUM00) %>%
    slice(which.max(rowSums(!is.na(cur_data())))) %>%
    ungroup()

# Check duplicates in MCS_data_partner
## No duplicates; N = 12,993
filled_MCS_data_partner %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)

# Check source of main caregivers
filled_MCS_data_partner %>% count(res_CM_relationship)

## 1 (natural mother): n = 245
## 2 (natural father): n = 9795
## 4 (adoptive mother): n = 1
## 5 (adoptive father): n = 19
## 10 (step mother): n = 23
## 11 (step father): n = 449
## 14 (grandfather): n = 8
## 16 (other; female): n = 2
## 17 (other; male): n = 82

# Now we have the separate data files for caregiver and partner,
# we want to change the variable names to indicate the response source,
# and then merge the caregiver and partner files into one dataset.

# Change variable names and merge caregiver and partner files
## filled_MCS_data_caregiver
filled_MCS_data_caregiver <- filled_MCS_data_caregiver %>%
    rename_with(~ paste0("CG_", .), -c(MCSID, CCNUM00))
## filled_MCS_data_partner
filled_MCS_data_partner <- filled_MCS_data_partner %>%
    rename_with(~ paste0("PN_", .), -c(MCSID, CCNUM00))

# Merge caregiver and partner files
Merged_MCS_data <- filled_MCS_data_caregiver %>%
    full_join(filled_MCS_data_partner, by = c("MCSID", "CCNUM00"))

# Check duplicates in MCS_data_partner
## No duplicates; N = 16,535
Merged_MCS_data %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)
## 10 cases were added from filled_data_partner; identify which cases they are
anti_join(filled_MCS_data_partner, filled_MCS_data_caregiver, by = c("MCSID", "CCNUM00"))

# Now check and filter out rows with variables of interest all missing;
# Reason: it makes no sense for either FIML or imputation to work if there's no response. 

# Identify the range of variables to check for missingness
vars_range <- names(Merged_MCS_data)

## Caregiver response: `CG_domestic_violence` to `CG_FPSDFB00`
cg_var <- vars_range[which(vars_range == "CG_domestic_violence") :
                        which(vars_range == "CG_FPSDFB00")]

## Partner response: `PN_domestic_violence` to `PN_FPSDFB00`
pn_var <- vars_range[which(vars_range == "PN_domestic_violence") :
                         which(vars_range == "PN_FPSDFB00")]

# Combine the range of variables to check for missingness
vars_to_check <- c(cg_var, pn_var)

# Check for all missing cases in the specified variables (n = 110)
all_missing_cases <- Merged_MCS_data %>% 
    filter(if_all(all_of(vars_to_check), is.na))

# Remove these all missing cases from the merged dataset
Merged_MCS_data <- Merged_MCS_data %>%
    filter(!if_all(all_of(vars_to_check), is.na))

# Compute child age
Merged_MCS_data <- Merged_MCS_data %>%
    mutate(CM_age = CG_CM_age / 365.25)

# Now we have a merged dataset in a wide format (N = 16,425);
# This dataset includes caregiver and partner responses with no duplicated child;
# Next step is to create a new dataset with only variables of interest.

# ====== Create a dataset with only variables of interest ======
# Select variables of interest
MCS_analysis <- Merged_MCS_data %>%
    select(MCSID, CCNUM00, CG_CPNUM00, CG_CELIG00, PN_CPNUM00, PN_CELIG00, 
           CG_CM_ethnicity, CG_CM_gestational_age,
           CG_res_age_at_birth, CG_res_weight,
           CG_res_home_ownership, CG_mother_BMI,
           CG_marital_status_at_birth, CG_parent_academic,
           CG_CM_SDQ,
           CG_CM_sex, CG_CM_age_month, CG_CM_age_year, CG_CM_age, CM_age, # child sex and age
           CG_family_type, PN_family_type, # family type
           CG_parent_relationship, PN_parent_relationship, # parent relationship
           CG_income_level, CG_income_number, CG_income_quintile, # caregiver household income
           PN_income_level, PN_income_number, PN_income_quintile, # partner household income
           CG_marital_status, PN_marital_status, # parental marital status
           CG_res_CM_relationship, PN_res_CM_relationship, # parent-CM relationship
           CG_res_age, PN_res_age, # parental age
           CG_res_ethnicity, PN_res_ethnicity, # parental ethnicity
           CG_res_NVQ, PN_res_NVQ, # parental NVQ
           CG_res_religion, PN_res_religion, # parental religion
           # Main Caregiver- and partner-reported domestic violence
           CG_domestic_violence, # Caregiver - domestic violence
           PN_domestic_violence, # Partner - domestic violence
           # Main Caregiver-reported harsh parenting
           CG_CPDIIG00, # Caregiver - How often ignores CM when naughty
           CG_CPDISM00, # Caregiver - How often smacks CM when naughty
           CG_CPDISH00, # Caregiver - How often shouts at CM when naughty
           CG_CPDIBN00, # Caregiver - How often sends CM to bedroom/naughty chair
           CG_CPDITR00, # Caregiver - How often takes away treats from CM when naughty
           CG_CPDITE00, # Caregiver - How often tells CM off when naughty
           CG_CPDIBR00, # Caregiver - How often bribes CM when naughty
           # Child-reported bullying
           CG_DCSC0036, # Sweep 4: How often do other children bully you? (victimisation)
           CG_DCSC0037, # Sweep 4: How often are you horrible to other children at school? (perpetration)
           CG_ECQ56X00, # Sweep 5: How often do other children hurt you or pick on you on purpose? (victimisation)
           CG_ECQ57X00, # Sweep 5: How often do you hurt or pick on other children on purpose? (perpetration)
           CG_FCHURT00, # Sweep 6: How often other children hurt or pick on CM (victimisation)
           CG_FCPCKP00, # Sweep 6: How often CM hurts or picks on other children (perpetration)
           # Main Caregiver-reported bullying
           CG_DPSDPB00, # Sweep 4: CM picked on or bullied by other children (victimisation)
           CG_DPSDFB00, # Sweep 4: CM fights with or bullies other children (perpetration)
           CG_EPSDPB00, # Sweep 5: CM picked on or bullied by other children (victimisation)
           CG_EPSDFB00, # Sweep 5: CM fights with or bullies other children (perpetration)
           CG_FPSDPB00, # Sweep 6: Picked on or bullied by other children (victimisation)
           CG_FPSDFB00, # Sweep 6: Often fights with other children or bullies them (perpetration)
           # Main caregiver - Kessler (K6) Scale
           CG_CPPHDE00, # Caregiver - Kessler (K6) Scale: Depression
           CG_CPPHHO00, # Caregiver - Kessler (K6) Scale: Hopelessness
           CG_CPPHRF00, # Caregiver - Kessler (K6) Scale: Restlessness
           CG_CPPHEE00, # Caregiver - Kessler (K6) Scale: Emotional exhaustion
           CG_CPPHWO00, # Caregiver - Kessler (K6) Scale: Worthlessness
           CG_CPPHNE00, # Caregiver - Kessler (K6) Scale: Nervousness
           # Partner - Kessler (K6) Scale
           PN_CPPHDE00, # Partner - Kessler (K6) Scale: Depression
           PN_CPPHHO00, # Partner - Kessler (K6) Scale: Hopelessness
           PN_CPPHRF00, # Partner - Kessler (K6) Scale: Restlessness
           PN_CPPHEE00, # Partner - Kessler (K6) Scale: Emotional exhaustion
           PN_CPPHWO00, # Partner - Kessler (K6) Scale: Worthlessness
           PN_CPPHNE00, # Partner - Kessler (K6) Scale: Nervousness
           )

# N = 16,425

# ====== Check descriptive statistics ======
# Check the summary of the dataset
summary(MCS_analysis)

# Check the number of rows (N = 16,245) and columns (N = 74)
dim(MCS_analysis)

# Check duplicated children - no duplicates
MCS_analysis %>% count(MCSID, CCNUM00) %>% 
    filter(n > 0) %>%
    count(n)

# Check the number of family structures (1-child: n = 16,023; 2-children: n = 201)
MCS_analysis %>% count(MCSID) %>% 
    filter(n > 0) %>%
    count(n)

# Check child sex
MCS_analysis %>% count(CG_CM_sex) # male: 7807, female: 7487, NA: 1131

# Check child age
describe(MCS_analysis$CM_age) # Min: 4,40, Max: 6.13, Mean: 5.22, SD: 0.25 (NA: 1132)

# Check child ethnicity
MCS_analysis %>% count(CG_CM_ethnicity) # Caregiver - white: 12389, others: 2306, NA: 1730

# Check family type
MCS_analysis %>% count(CG_family_type) # Caregiver - two-parents: 12282, one-parent: 3011, NA = 1132
MCS_analysis %>% count(PN_family_type) # Partner - two-parents: 10609, NA = 5816

# Check inter-parental relationship (relationship between parents/carers in the household)
MCS_analysis %>% count(CG_parent_relationship) # Caregiver - married: 9492, cohabiting: 2790, Neither: 1, NA: 4142
MCS_analysis %>% count(PN_parent_relationship) # Partner - married: 8317, cohabiting: 2292, NA: 5816

# Check parental income
MCS_analysis %>% count(CG_income_level) # Caregiver - above 60% median: 10110, below 60% median: 5136, NA: 1179
describe(MCS_analysis$CG_income_number) # Caregiver - mean: 497.30 (NA: 1179)
MCS_analysis %>% count(CG_income_quintile) # Caregiver quintile
MCS_analysis %>% count(PN_income_level) # Partner - above 60% median: 8241, below 60% median: 2333, NA: 5851
describe(MCS_analysis$PN_income_number) # Partner - mean: 580.64 (NA: 5851)
MCS_analysis %>% count(PN_income_quintile) # Partner quintile

# Check respondent marital status
MCS_analysis %>% count(CG_marital_status) # Caregiver - married: 9972, separated/divorced: 1468
MCS_analysis %>% count(PN_marital_status) # Partner - married: 8559, separated/divorced: 468

# Check parent-CM relationship: natural mother/father, adoptive mother/father, etc.
MCS_analysis %>% count(CG_res_CM_relationship) # Caregiver
MCS_analysis %>% count(PN_res_CM_relationship) # Partner

# Check parental age
describe(MCS_analysis$CG_res_age) # Caregiver - Min: 18, Max: 66, Mean: 33.98 (NA: 1132)
describe(MCS_analysis$PN_res_age) # Partner - Min: 16, Max: 77, Mean: 37.19 (NA: 5824)

# Check parental ethnicity: white, mixed, Indian, etc.
MCS_analysis %>% count(CG_res_ethnicity) # Caregiver
MCS_analysis %>% count(PN_res_ethnicity) # Partner

# Parental NVQ: level 1/2/3/4/5, overseas qual only, etc.
MCS_analysis %>% count(CG_res_NVQ) # Caregiver
MCS_analysis %>% count(PN_res_NVQ) # Partner

# Parental religion: Christian, Muslim, Hindu, etc.
MCS_analysis %>% count(CG_res_religion) # Caregiver
MCS_analysis %>% count(PN_res_religion) # Partner

# ====== Check study variable statistics ======
# Check domestic violence: 1 (yes), 2 (no), 3 (don't want to answer)
## Golombok Rust Inventory: Partner ever used force in relationship 
MCS_analysis %>% count(CG_domestic_violence) # Caregiver - yes: 445, no: 10884, NA: 4781
MCS_analysis %>% count(PN_domestic_violence) # Partner - yes: 809, no: 9027, NA: 6332

# Check harsh parenting: 1 (never) to 5 (daily), 6 (can't say)
MCS_analysis %>% count(CG_CPDIIG00) # Caregiver - ignoring CM
describe(MCS_analysis$CG_CPDIIG00)
MCS_analysis %>% count(CG_CPDISH00) # Caregiver - shouting at CM
describe(MCS_analysis$CG_CPDISH00)
MCS_analysis %>% count(CG_CPDIBN00) # Caregiver - Sending CM to bedroom/naughty chair
describe(MCS_analysis$CG_CPDIBN00)
MCS_analysis %>% count(CG_CPDITR00) # Caregiver - Taking away treats from CM
describe(MCS_analysis$CG_CPDITR00)
MCS_analysis %>% count(CG_CPDITE00) # Caregiver - Telling CM off
describe(MCS_analysis$CG_CPDITE00)
MCS_analysis %>% count(CG_CPDIBR00) # Caregiver - Bribing CM
describe(MCS_analysis$CG_CPDIBR00)

# Check physical punishment: 1 (never) to 5 (daily), 6 (can't say)
MCS_analysis %>% count(CG_CPDISM00) # Caregiver - smacking CM
describe(MCS_analysis$CG_CPDISM00)

# Check child-reported bullying
MCS_analysis %>% count(CG_DCSC0036) # Sweep 4 - victimisation: 1 (all of the time) to 3 (never)
describe(MCS_analysis$CG_DCSC0036)
MCS_analysis %>% count(CG_DCSC0037) # Sweep 4 - perpetration: 1 (all of the time) to 3 (never)
describe(MCS_analysis$CG_DCSC0037)
MCS_analysis %>% count(CG_ECQ56X00) # Sweep 5 - victimisation: 1 (most days) to 6 (never)
describe(MCS_analysis$CG_ECQ56X00)
MCS_analysis %>% count(CG_ECQ57X00) # Sweep 5 - perpetration: 1 (most days) to 6 (never)
describe(MCS_analysis$CG_ECQ57X00)
MCS_analysis %>% count(CG_FCHURT00) # Sweep 6 - victimisation: 1 (most days) to 6 (never)
describe(MCS_analysis$CG_FCHURT00)
MCS_analysis %>% count(CG_FCPCKP00) # Sweep 6 - perpetration: 1 (most days) to 6 (never)
describe(MCS_analysis$CG_FCPCKP00)

# Check parent-reported bullying
MCS_analysis %>% count(CG_DPSDPB00) # Sweep 4 - victimisation: 1 (not true) to 3 (certainly true), 4 (can't say)
describe(MCS_analysis$CG_DPSDPB00)
MCS_analysis %>% count(CG_DPSDFB00) # Sweep 4 - perpetration: 1 (not true) to 3 (certainly true), 4 (can't say)
describe(MCS_analysis$CG_DPSDFB00)
MCS_analysis %>% count(CG_EPSDPB00) # Sweep 5 - victimisation: 1 (not true) to 3 (certainly true), 4 (don't know)
describe(MCS_analysis$CG_EPSDPB00)
MCS_analysis %>% count(CG_EPSDFB00) # Sweep 5 - perpetration: 1 (not true) to 3 (certainly true), 4 (don't know)
describe(MCS_analysis$CG_EPSDFB00)
MCS_analysis %>% count(CG_FPSDPB00) # Sweep 6 - victimisation: 1 (not true) to 3 (certainly true)
describe(MCS_analysis$CG_FPSDPB00)
MCS_analysis %>% count(CG_FPSDFB00) # Sweep 6 - perpetration: 1 (not true) to 3 (certainly true)
describe(MCS_analysis$CG_FPSDFB00)

# Check Kessler (K6) Scale: 1 (all of the time) to 5 (none of the time), 6 (can't say)
MCS_analysis %>% count(CG_CPPHDE00) # Caregiver - Depression
describe(MCS_analysis$CG_CPPHDE00)
MCS_analysis %>% count(CG_CPPHHO00) # Caregiver - Hopelessness
describe(MCS_analysis$CG_CPPHHO00)
MCS_analysis %>% count(CG_CPPHRF00) # Caregiver - Restlessness
describe(MCS_analysis$CG_CPPHRF00)
MCS_analysis %>% count(CG_CPPHEE00) # Caregiver - Emotional exhaustion
describe(MCS_analysis$CG_CPPHEE00)
MCS_analysis %>% count(CG_CPPHWO00) # Caregiver - Worthlessness
describe(MCS_analysis$CG_CPPHWO00)
MCS_analysis %>% count(CG_CPPHNE00) # Caregiver - Nervousness
describe(MCS_analysis$CG_CPPHNE00)
MCS_analysis %>% count(PN_CPPHDE00) # Partner - Depression
describe(MCS_analysis$PN_CPPHDE00)
MCS_analysis %>% count(PN_CPPHHO00) # Partner - Hopelessness
describe(MCS_analysis$PN_CPPHHO00)
MCS_analysis %>% count(PN_CPPHRF00) # Partner - Restlessness
describe(MCS_analysis$PN_CPPHRF00)
MCS_analysis %>% count(PN_CPPHEE00) # Partner - Emotional exhaustion
describe(MCS_analysis$PN_CPPHEE00)
MCS_analysis %>% count(PN_CPPHWO00) # Partner - Worthlessness
describe(MCS_analysis$PN_CPPHWO00)
MCS_analysis %>% count(PN_CPPHNE00) # Partner - Nervousness
describe(MCS_analysis$PN_CPPHNE00)

# Now we have all study variables ready;
# To make the most of the information, we want to impute and derive scores for child maltreatment

# Imputation reason: We will derive maltreatment scores by summing scores for each maltreatment type;
# if any item is missing, the sum will be NA, which is losing information;
# >> imputation will allow us to derive scores for child maltreatment even if some items are missing

# Note: As imputation only applies to child maltreatment and its prediction (regression);
# We will first prepare for other variables, conduct trajectory analysis in STATA, and come back to R for imputation and regression

# 07-10-25 For re-scaled variables, we have a separate script #

# ====== Derive study variables - bullying perpetration and victimisation ======
# Child-reported bullying ======
# Recode child-reported bullying scores
## Original
### (1) Sweep 4: 1 (all of the time), 2 (some of the time), 3 (never)
### (2) Sweep 5 & 6: 1 (most days), 2 (once a week), 3 (once a month), 4 (every few months), 5 (less often), 6 (never)
## New
### (1) Sweep 4: 1 (never), 2 (some of the time), 3 (all of the time)
### (2) Sweep 5 & 6: 1 (never), 2 (less often), 3 (every few months), 4 (once a month), 5 (once a week), 6 (most days)
MCS_analysis <- MCS_analysis %>%
    mutate(
        CG_DCSC0036 = recode(as.numeric(CG_DCSC0036), `1` = 3, `2` = 2, `3` = 1), # Sweep 4: victimisation
        CG_DCSC0037 = recode(as.numeric(CG_DCSC0037), `1` = 3, `2` = 2, `3` = 1), # Sweep 4: perpetration
        CG_ECQ56X00 = recode(as.numeric(CG_ECQ56X00), `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1), # Sweep 5: victimisation
        CG_ECQ57X00 = recode(as.numeric(CG_ECQ57X00), `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1), # Sweep 5: perpetration
        CG_FCHURT00 = recode(as.numeric(CG_FCHURT00), `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1), # Sweep 6: victimisation
        CG_FCPCKP00 = recode(as.numeric(CG_FCPCKP00), `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1) # Sweep 6: perpetration
    )

# Check recoding of child-reported bullying scores
## Sweep 4 - victimisation: 1 (never) to 3 (all of the time)
MCS_analysis %>% count(CG_DCSC0036)
describe(MCS_analysis$CG_DCSC0036) # Min: 1, Max: 3, Mean: 1.58, SD: 0.65

## Sweep 4 - perpetration: 1 (never) to 3 (all of the time)
MCS_analysis %>% count(CG_DCSC0037)
describe(MCS_analysis$CG_DCSC0037) # Min: 1, Max: 3, Mean: 1.17, SD: 0.44

## Sweep 5 - victimisation: 1 (never) to 6 (most days)
MCS_analysis %>% count(CG_ECQ56X00)
describe(MCS_analysis$CG_ECQ56X00) # Min: 1, Max: 6, Mean: 2.29, SD: 1.58

## Sweep 5 - perpetration: 1 (never) to 6 (most days)
MCS_analysis %>% count(CG_ECQ57X00)
describe(MCS_analysis$CG_ECQ57X00) # Min: 1, Max: 6, Mean: 1.46, SD: 0.95

## Sweep 6 - victimisation: 1 (never) to 6 (most days)
MCS_analysis %>% count(CG_FCHURT00)
describe(MCS_analysis$CG_FCHURT00) # Min: 1, Max: 6, Mean: 1.99, SD: 1.41

## Sweep 6 - perpetration: 1 (never) to 6 (most days)
MCS_analysis %>% count(CG_FCPCKP00)
describe(MCS_analysis$CG_FCPCKP00) # Min: 1, Max: 6, Mean: 1.43, SD: 0.89

# Parent-reported bullying ======
# Recode parent-reported bullying scores
## Sweep 5 & 6: Recode 4 (can't say / don't know) to NA
MCS_analysis$CG_DPSDPB00[MCS_analysis$CG_DPSDPB00 == 4] <- NA # Sweep 4 - victimisation
MCS_analysis$CG_DPSDFB00[MCS_analysis$CG_DPSDFB00 == 4] <- NA # Sweep 4 - perpetration
MCS_analysis$CG_EPSDPB00[MCS_analysis$CG_EPSDPB00 == 4] <- NA # Sweep 5 - victimisation
MCS_analysis$CG_EPSDFB00[MCS_analysis$CG_EPSDFB00 == 4] <- NA # Sweep 5 - perpetration

# Check recoding of parent-reported bullying scores
# Sweep 4 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_DPSDPB00) 
describe(MCS_analysis$CG_DPSDPB00) # Min: 1, Max: 3, Mean: 1.24, SD: 0.50

# Sweep 4 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_DPSDFB00) 
describe(MCS_analysis$CG_DPSDFB00) # Min: 1, Max: 3, Mean: 1.09, SD: 0.32

# Sweep 5 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_EPSDPB00) 
describe(MCS_analysis$CG_EPSDPB00) # Min: 1, Max: 3, Mean: 1.30, SD: 0.55

# Sweep 5 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_EPSDFB00) 
describe(MCS_analysis$CG_EPSDFB00) # Min: 1, Max: 3, Mean: 1.09, SD: 0.32

# Sweep 6 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_FPSDPB00) 
describe(MCS_analysis$CG_FPSDPB00) # Min: 1, Max: 3, Mean: 1.29, SD: 0.57

# Sweep 6 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(CG_FPSDFB00) 
describe(MCS_analysis$CG_FPSDFB00) # Min: 1, Max: 3, Mean: 1.09, SD: 0.33

# Rename variables ======
MCS_analysis <- MCS_analysis %>% 
    rename(
        # Child-reported bullying
        child_vic_s4 = CG_DCSC0036,
        child_per_s4 = CG_DCSC0037,
        child_vic_s5 = CG_ECQ56X00,
        child_per_s5 = CG_ECQ57X00,
        child_vic_s6 = CG_FCHURT00,
        child_per_s6 = CG_FCPCKP00,
        # Parent-reported bullying
        parent_vic_s4 = CG_DPSDPB00,
        parent_per_s4 = CG_DPSDFB00,
        parent_vic_s5 = CG_EPSDPB00,
        parent_per_s5 = CG_EPSDFB00,
        parent_vic_s6 = CG_FPSDPB00,
        parent_per_s6 = CG_FPSDFB00
    )

colnames(MCS_analysis)

# Child-reported bullying ======
# Recode / rescale child-reported bullying scores
## Original
## (1) Sweep 4: 1 (never), 2 (some of the time), 3 (all of the time)
### (2) Sweep 5 & 6: 1 (never), 2 (less often), 3 (every few months), 4 (once a month), 5 (once a week), 6 (most days)
## New
### (1) Sweep 4: 0 (never), 1 (some of the time), 2 (all of the time)
### (2) Sweep 5 & 6
#### 1 (never), 2 (less often) >> 0 (never / almost never)
#### 3 (every few months), 4 (once a month) >> 1 (every few months / once a month)
#### 4 (once a week), 5 (most days) >> 2 (once a week / most days)

## Sweep 4 - victimisation: 1 (never) to 3 (all of the time)
MCS_analysis %>% count(child_vic_s4) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_vic_s4 = recode(as.numeric(child_vic_s4), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_child_vic_s4)
describe(MCS_analysis$rec_child_vic_s4) # Min: 0, Max: 2, Mean: 0.58, SD: 0.65

## Sweep 4 - perpetration: 1 (never) to 3 (all of the time)
MCS_analysis %>% count(child_per_s4) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_per_s4 = recode(as.numeric(child_per_s4), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_child_per_s4)
describe(MCS_analysis$rec_child_per_s4) # Min: 0, Max: 2, Mean: 0.17, SD: 0.44

## Sweep 5 - victimisation: 1 (never) to 6 (most days)
MCS_analysis %>% count(child_vic_s5) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_vic_s5 = recode(as.numeric(child_vic_s5), 
                                  `1` = 0, `2` = 0, 
                                  `3` = 1, `4` = 1, 
                                  `5` = 2, `6` = 2), 
    )
MCS_analysis %>% count(rec_child_vic_s5)
describe(MCS_analysis$rec_child_vic_s5) # Min: 0, Max: 2, Mean: 0.44, SD: 0.74

## Sweep 5 - perpetration: 1 (never) to 6 (most days)
MCS_analysis %>% count(child_per_s5) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_per_s5 = recode(as.numeric(child_per_s5), 
                                  `1` = 0, `2` = 0, 
                                  `3` = 1, `4` = 1, 
                                  `5` = 2, `6` = 2), 
    )
MCS_analysis %>% count(rec_child_per_s5)
describe(MCS_analysis$rec_child_per_s5) # Min: 0, Max: 2, Mean: 0.12, SD: 0.40

## Sweep 6 - victimisation: 1 (never) to 6 (most days)
MCS_analysis %>% count(child_vic_s6) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_vic_s6 = recode(as.numeric(child_vic_s6), 
                                  `1` = 0, `2` = 0, 
                                  `3` = 1, `4` = 1, 
                                  `5` = 2, `6` = 2), 
    )
MCS_analysis %>% count(rec_child_vic_s6)
describe(MCS_analysis$rec_child_vic_s6) # Min: 0, Max: 2, Mean: 0.31, SD: 0.65

## Sweep 6 - perpetration: 1 (never) to 6 (most days)
MCS_analysis %>% count(child_per_s6) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_child_per_s6 = recode(as.numeric(child_per_s6), 
                                  `1` = 0, `2` = 0, 
                                  `3` = 1, `4` = 1, 
                                  `5` = 2, `6` = 2), 
    )
MCS_analysis %>% count(rec_child_per_s6)
describe(MCS_analysis$rec_child_per_s6) # Min: 0, Max: 2, Mean: 0.10, SD: 0.37

# Parent-reported bullying ======
# Recode / rescale parent-reported bullying scores
## Original
### Sweep 4, 5 & 6: 1 (not true), 2 (somewhat true), 3 (certainly true)
## New
### Sweep 4, 5 & 6: 0 (not true), 1 (somewhat true), 2 (certainly true)

## Sweep 4 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_vic_s4) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_vic_s4 = recode(as.numeric(parent_vic_s4), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_vic_s4)
describe(MCS_analysis$rec_parent_vic_s4) # Min: 0, Max: 2, Mean: 0.24, SD: 0.50

## Sweep 4 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_per_s4) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_per_s4 = recode(as.numeric(parent_per_s4), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_per_s4)
describe(MCS_analysis$rec_parent_per_s4) # Min: 0, Max: 2, Mean: 0.09, SD: 0.32

## Sweep 5 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_vic_s5) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_vic_s5 = recode(as.numeric(parent_vic_s5), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_vic_s5)
describe(MCS_analysis$rec_parent_vic_s5) # Min: 0, Max: 2, Mean: 0.30, SD: 0.55

## Sweep 5 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_per_s5) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_per_s5 = recode(as.numeric(parent_per_s5), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_per_s5)
describe(MCS_analysis$rec_parent_per_s5) # Min: 0, Max: 2, Mean: 0.09, SD: 0.32

## Sweep 6 - victimisation: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_vic_s6) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_vic_s6 = recode(as.numeric(parent_vic_s6), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_vic_s6)
describe(MCS_analysis$rec_parent_vic_s6) # Min: 0, Max: 2, Mean: 0.29, SD: 0.57

## Sweep 6 - perpetration: 1 (not true) to 3 (certainly true)
MCS_analysis %>% count(parent_per_s6) # Check original coding
MCS_analysis <- MCS_analysis %>%
    mutate(
        rec_parent_per_s6 = recode(as.numeric(parent_per_s6), `1` = 0, `2` = 1, `3` = 2), 
    )
MCS_analysis %>% count(rec_parent_per_s6)
describe(MCS_analysis$rec_parent_per_s6) # Min: 0, Max: 2, Mean: 0.09, SD: 0.33

# Check correlations between child and parent reports ======
# Victimisation
cor.test(MCS_analysis$rec_child_vic_s4, MCS_analysis$rec_parent_vic_s4, use = "pairwise.complete.obs") # r = 0.26
cor.test(MCS_analysis$rec_child_vic_s5, MCS_analysis$rec_parent_vic_s5, use = "pairwise.complete.obs") # r = 0.32
cor.test(MCS_analysis$rec_child_vic_s6, MCS_analysis$rec_parent_vic_s6, use = "pairwise.complete.obs") # r = 0.26

# Perpetration
cor.test(MCS_analysis$rec_child_per_s4, MCS_analysis$rec_parent_per_s4, use = "pairwise.complete.obs") # r = 0.15
cor.test(MCS_analysis$rec_child_per_s5, MCS_analysis$rec_parent_per_s5, use = "pairwise.complete.obs") # r = 0.17
cor.test(MCS_analysis$rec_child_per_s6, MCS_analysis$rec_parent_per_s6, use = "pairwise.complete.obs") # r = 0.10

# Check summary of bullying scores ======
hist(MCS_analysis$rec_child_vic_s4)
hist(MCS_analysis$rec_child_vic_s5)
hist(MCS_analysis$rec_child_vic_s6)
hist(MCS_analysis$rec_child_per_s4)
hist(MCS_analysis$rec_child_per_s5)
hist(MCS_analysis$rec_child_per_s6)

hist(MCS_analysis$rec_parent_vic_s4)
hist(MCS_analysis$rec_parent_vic_s5)
hist(MCS_analysis$rec_parent_vic_s6)
hist(MCS_analysis$rec_parent_per_s4)
hist(MCS_analysis$rec_parent_per_s5)
hist(MCS_analysis$rec_parent_per_s6)

# Combine child and parent reports ======
# Now we have rescaled child- and parent-reported scores for bullying perpetration and victimisation for three waves
# We want to combine child- and parent-reported scores using the following rules:
# (1) Combine (average) if both child- and parent-reported scores are present
# (2) If child- or parent-reported score is missing, replace with the other report score

# Average standardised child- and parent-reported bullying scores
MCS_analysis <- MCS_analysis %>%
    mutate(Victimisation_s4 = rowMeans(select(., rec_child_vic_s4, rec_parent_vic_s4), na.rm = TRUE),
           Victimisation_s5 = rowMeans(select(., rec_child_per_s5, rec_parent_vic_s5), na.rm = TRUE),
           Victimisation_s6 = rowMeans(select(., rec_child_vic_s6, rec_parent_vic_s6), na.rm = TRUE),
           Perpetration_s4 = rowMeans(select(., rec_child_per_s4, rec_parent_per_s4), na.rm = TRUE),
           Perpetration_s5 = rowMeans(select(., rec_child_per_s5, rec_parent_per_s5), na.rm = TRUE),
           Perpetration_s6 = rowMeans(select(., rec_child_per_s6, rec_parent_per_s6), na.rm = TRUE))

# Check summary of bullying scores
describe(MCS_analysis$Victimisation_s4) # Victimisation_s4 - Min: 0, Max: 2, Mean: 0.41, SD: 0.48
describe(MCS_analysis$Victimisation_s5) # Victimisation_s5 - Min: 0, Max: 2, Mean: 0.22, SD: 0.39
describe(MCS_analysis$Victimisation_s6) # Victimisation_s6 - Min: 0, Max: 2, Mean: 0.30, SD: 0.50
describe(MCS_analysis$Perpetration_s4) # Perpetration_s4 - Min: 0, Max: 2, Mean: 0.13, SD: 0.31
describe(MCS_analysis$Perpetration_s5) # Perpetration_s5 - Min: 0, Max: 2, Mean: 0.10, SD: 0.30
describe(MCS_analysis$Perpetration_s6) # Perpetration_s6 - Min: 0, Max: 2, Mean: 0.10, SD: 0.29

# Subset separate and composite measures to have a separate dataset ======
bullying <- select(MCS_analysis, 
                   # Child and parent reports
                   rec_child_vic_s4, rec_parent_vic_s4,
                   rec_child_vic_s5, rec_parent_vic_s5,
                   rec_child_vic_s6, rec_parent_vic_s6,
                   rec_child_per_s4, rec_parent_per_s4,
                   rec_child_per_s5, rec_parent_per_s5,
                   rec_child_per_s6, rec_parent_per_s6,
                   # Combined child and parent reports
                   Victimisation_s4, Victimisation_s5, Victimisation_s6,
                   Perpetration_s4, Perpetration_s5, Perpetration_s6)
colnames(bullying)
View(bullying)

# Complete n for each variable
describe(bullying)$n
## Victimisation_s4 - 13805
## Victimisation_s5 - 13306
## Victimisation_s6 - 11727
## Perpetration_s4 - 13855
## Perpetration_s5 - 13321
## Perpetration_s6 - 11732

# Number of missing data by variable
sapply(bullying, function(x) sum(is.na(x)))

# Proportion of missing data across all variables
prop_miss(bullying)

# Combine into table
bullying_missing <- as.data.frame(cbind(Complete = describe(bullying),
                                        Missing = sapply(bullying, function(x) sum(is.na(x))),
                                        Prop_miss = prop_miss(bullying)))

# Now we have all variables, except for child maltreatment, prepared;
# First, we will export this data for trajectory analysis in STATA;
# Second, after trajectory analysis, we will obtain a new dataset with class assignment for each case;
# Third, we will come back to R to impute child maltreatment variables and derive scores for child maltreatment

# ====== Export data in .dat ======
# Export the data for trajectory analysis in STATA
write_dta(MCS_analysis, "RS_MCS_analysis.dta")

# ====== Stop preparation here and proceed with trajectory analysis in STATA ======

# ====== Import data with class assignment into R ======
# Import the data with class assignment from STATA ======
MCS_analysis_with_class <- read_dta("RS_MCS_analysis_class_assignment.dta")

# Check distribution of bullying scores ======
# Unstadardised scores
hist(MCS_analysis_with_class$rec_child_vic_s4, main = "Sweep 4 - Child-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_child_per_s4, main = "Sweep 4 - Child-reported Perpetration", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_child_vic_s5, main = "Sweep 5 - Child-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_child_per_s5, main = "Sweep 5 - Child-reported Perpetration", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_child_vic_s6, main = "Sweep 6 - Child-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_child_per_s6, main = "Sweep 6 - Child-reported Perpetration", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_vic_s4, main = "Sweep 4 - Parent-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_per_s4, main = "Sweep 4 - Parent-reported Perpetration", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_vic_s5, main = "Sweep 5 - Parent-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_per_s5, main = "Sweep 5 - Parent-reported Perpetration", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_vic_s6, main = "Sweep 6 - Parent-reported Victimisation", xlab = "Score", breaks = 10)
hist(MCS_analysis_with_class$rec_parent_per_s6, main = "Sweep 6 - Parent-reported Perpetration", xlab = "Score", breaks = 10)

# Examine the profile of the top quartile (for perpetration)
## We examine the profile of the top quartile, especially for perpetration, as some people get very large positive z-scores;
## we want to see who these people are
MCS_analysis_with_class$top25_perp_s4 <- ifelse(MCS_analysis_with_class$Perpetration_s4 > quantile(MCS_analysis_with_class$Perpetration_s4, 0.75, na.rm = T), 1, 0)
MCS_analysis_with_class$top25_perp_s5 <- ifelse(MCS_analysis_with_class$Perpetration_s5 > quantile(MCS_analysis_with_class$Perpetration_s5, 0.75, na.rm = T), 1, 0)
MCS_analysis_with_class$top25_perp_s6 <- ifelse(MCS_analysis_with_class$Perpetration_s6 > quantile(MCS_analysis_with_class$Perpetration_s6, 0.75, na.rm = T), 1, 0)
MCS_analysis_with_class$top25_vict_s4 <- ifelse(MCS_analysis_with_class$Victimisation_s4 > quantile(MCS_analysis_with_class$Victimisation_s4, 0.75, na.rm = T), 1, 0)
MCS_analysis_with_class$top25_vict_s5 <- ifelse(MCS_analysis_with_class$Victimisation_s5 > quantile(MCS_analysis_with_class$Victimisation_s5, 0.75, na.rm = T), 1, 0)
MCS_analysis_with_class$top25_vict_s6 <- ifelse(MCS_analysis_with_class$Victimisation_s6 > quantile(MCS_analysis_with_class$Victimisation_s6, 0.75, na.rm = T), 1, 0)

# Number of children falling in top 25% at each wave for perpetration: s4 (n = 2541), s5 (n = 1895), s6 (n = 1538)
colSums(MCS_analysis_with_class[, c("top25_perp_s4", "top25_perp_s5", "top25_perp_s6")], na.rm = T)

# Consistency across waves for perpetration (i.e., how many time each child appears in the top 25%): 0 (n = 11809), 1 (n = 3466), 2 (n = 942), 3 (n = 208)
MCS_analysis_with_class$top25_perp_total <- rowSums(
    MCS_analysis_with_class[, c("top25_perp_s4", "top25_perp_s5", "top25_perp_s6")], 
    na.rm = T)
table(MCS_analysis_with_class$top25_perp_total)

# Comparison between perpetration and victimisation
## Wave 4
table(MCS_analysis_with_class$top25_perp_s4, 
      MCS_analysis_with_class$top25_vict_s4)

## Wave 5
table(MCS_analysis_with_class$top25_perp_s5, 
      MCS_analysis_with_class$top25_vict_s5)

## Wave 6
table(MCS_analysis_with_class$top25_perp_s6, 
      MCS_analysis_with_class$top25_vict_s6)

# Box plots
boxplot(Perpetration_s4 ~ top25_vict_s4,
        data = MCS_analysis_with_class,
        names = c("Not top 25%", "Top 25%"),
        main = "Sweep 4 - Perpetration by Victimisation Top 25% Status")

boxplot(MCS_analysis_with_class$Perpetration_s4 ~ MCS_analysis_with_class$top25_perp_s4, 
        main = "Sweep 4 - Perpetration by Top 25% Status", 
        xlab = "Top 25% Status (0 = No, 1 = Yes)", 
        ylab = "Perpetration Score")

# Check descriptive statistics by groups ======
skim_descriptives_all <- MCS_analysis_with_class %>% 
    skim()

write_xlsx(skim_descriptives_all, 
           "Skim_descriptives.xlsx")

skim_descriptives <- MCS_analysis_with_class %>% 
    group_by(`_traj_Group`) %>%
    skim()

write_xlsx(skim_descriptives, 
           "Skim_descriptives_by_group.xlsx")

# Create a dataset without variable labels for easier manipulation
MCS_label_strip <- zap_labels(MCS_analysis_with_class)

demographics_by_group <- MCS_label_strip %>% 
    group_by(`_traj_Group`) %>%
    summarise(
        n = n(),
        child_age = mean(CM_age, na.rm = TRUE),
        child_age_sd = sd(CM_age, na.rm = TRUE),
        child_sex = mean(CG_CM_sex, na.rm = TRUE),
        child_ethnicity = mean(CG_CM_ethnicity == 1, na.rm = TRUE), # % White
        family_type = mean (CG_family_type == 1, na.rm = TRUE), # % two-parents
        parent_relationship = mean(CG_parent_relationship == 1, na.rm = TRUE), # % married
        caregiver_income = mean(CG_income_level == 0, na.rm = TRUE),
        caregiver_child_relationship = mean(CG_res_CM_relationship == 1, na.rm = TRUE), # % natural mother/father
        caregiver_age = mean(CG_res_age, na.rm = TRUE),
        caregiver_age_sd = sd(CG_res_age, na.rm = TRUE),
        caregiver_ethnicity = mean(CG_res_ethnicity == 1, na.rm = TRUE) # % White
    )

# Check number and percentage of people in each group
table(MCS_analysis_with_class$`_traj_Group`)
prop.table(table(MCS_analysis_with_class$`_traj_Group`))

# We first create a dataset without imputation;
# then create another dataset with imputation

# ====== Derive study variables - child maltreatment ======

# Procedure for deriving harsh parenting scores (Bevilacqua et al., 2021)
# 1. Binarise items for harsh parenting and physical punishment
## (1) 3-5 (“Once a month” to “Daily”) >> 1 (yes; present)
## (2) 1-2 (“Never” to “rarely”) >> 0 (no; absent)
# 2. Sum the binary scores to get a cumulative score (0-6) - RQ3
# 3. Create a binary score for presence of child maltreatment (0-1) - RQ2

# Recode missingness of child maltreatment variables ======
# Recode harsh parenting: 6 (can't say) >> NA
MCS_analysis_with_class$CG_CPDIIG00[MCS_analysis_with_class$CG_CPDIIG00 == 6] <- NA # How often ignores CM when naughty
MCS_analysis_with_class$CG_CPDISH00[MCS_analysis_with_class$CG_CPDISH00 == 6] <- NA # How often shouts at CM when naughty
MCS_analysis_with_class$CG_CPDIBN00[MCS_analysis_with_class$CG_CPDIBN00 == 6] <- NA # How often sends CM to bedroom/naughty chair
MCS_analysis_with_class$CG_CPDITR00[MCS_analysis_with_class$CG_CPDITR00 == 6] <- NA # How often takes away treats from CM when naughty
MCS_analysis_with_class$CG_CPDITE00[MCS_analysis_with_class$CG_CPDITE00 == 6] <- NA # How often tells CM off when naughty
MCS_analysis_with_class$CG_CPDIBR00[MCS_analysis_with_class$CG_CPDIBR00 == 6] <- NA # How often bribes CM when naughty

# Recode physical punishment: 6 (can't say) >> NA
MCS_analysis_with_class$CG_CPDISM00[MCS_analysis_with_class$CG_CPDISM00 == 6] <- NA # How often smacks CM when naughty

# Recode domestic violence: 3 (Don t want to answer) >> NA
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(
        CG_domestic_violence = na_if(CG_domestic_violence, 3),
        PN_domestic_violence = na_if(PN_domestic_violence, 3)
    )

# Derive scores for harsh parenting ======

# Check recoding: 1 (never) to 5 (daily)
MCS_analysis_with_class %>% count(CG_CPDIIG00) # Caregiver - How often ignores CM when naughty
MCS_analysis_with_class %>% count(CG_CPDISH00) # Caregiver - How often shouts at CM when naughty
MCS_analysis_with_class %>% count(CG_CPDIBN00) # Caregiver - How often sends CM to bedroom/naughty chair
MCS_analysis_with_class %>% count(CG_CPDITR00) # Caregiver - How often takes away treats from CM when naughty
MCS_analysis_with_class %>% count(CG_CPDITE00) # Caregiver - How often tells CM off when naughty
MCS_analysis_with_class %>% count(CG_CPDIBR00) # Caregiver - How often bribes CM when naughty

# Procedure for deriving harsh parenting scores (Bevilacqua et al., 2021):
# 1. Binarise items
## (1) 3-5 (“Once a month” to “Daily”) >> 1 (yes; present)
## (2) 1-2 (“Never” to “rarely”) >> 0 (no; absent)
# 2. Sum the binary scores to get a cumulative score (0-6)

# Binarise harsh parenting items
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(
        CG_CPDIIG00_bi = ifelse(is.na(CG_CPDIIG00), NA,
                                ifelse(CG_CPDIIG00 %in% c(3, 4, 5), 1, 0)),
        CG_CPDISH00_bi = ifelse(is.na(CG_CPDISH00), NA,
                                ifelse(CG_CPDISH00 %in% c(3, 4, 5), 1, 0)),
        CG_CPDIBN00_bi = ifelse(is.na(CG_CPDIBN00), NA,
                                ifelse(CG_CPDIBN00 %in% c(3, 4, 5), 1, 0)),
        CG_CPDITR00_bi = ifelse(is.na(CG_CPDITR00), NA,
                                ifelse(CG_CPDITR00 %in% c(3, 4, 5), 1, 0)),
        CG_CPDITE00_bi = ifelse(is.na(CG_CPDITE00), NA,
                                ifelse(CG_CPDITE00 %in% c(3, 4, 5), 1, 0)),
        CG_CPDIBR00_bi = ifelse(is.na(CG_CPDIBR00), NA,
                                ifelse(CG_CPDIBR00 %in% c(3, 4, 5), 1, 0))
    )

# Sum binary scores for harsh parenting
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(harsh_parenting = rowSums(select(.,
                                            CG_CPDIIG00_bi,
                                            CG_CPDISH00_bi,
                                            CG_CPDIBN00_bi,
                                            CG_CPDITR00_bi,
                                            CG_CPDITE00_bi,
                                            CG_CPDIBR00_bi
    ), na.rm = FALSE))

# Check summary of harsh_parenting
describe(MCS_analysis_with_class$harsh_parenting) # Min: 0, Max: 6, Mean: 3.71, SD: 1.58

# Binarise sum scores (i.e., `harsh_parenting`) for presence of child maltreatment
## 0-4 >> 0 (absent); 5-6 >> 1 (present)
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(harsh_parenting_bi = ifelse(is.na(harsh_parenting), NA, 
                                       ifelse(harsh_parenting > 4, 1, 0)
    ))

# Check summary of harsh parenting binary scores
MCS_analysis_with_class %>% count(harsh_parenting_bi) # 0 (absent): 9030, 1 (present): 5040, NA: 2355

# Derive scores for physical punishment ======

# Check recoding: 1 (never) to 5 (daily)
MCS_analysis_with_class %>% count(CG_CPDISM00) # Caregiver - How often smacks CM when naughty

# Procedure for deriving physical punishment scores (Bevilacqua et al., 2021):
# Binarise items
## (1) 3-5 (“Once a month” to “Daily”) >> 1 (yes; present)
## (2) 1-2 (“Never” to “rarely”) >> 0 (no; absent)

# Binarise physical punishment items
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(
        CG_CPDISM00_bi = ifelse(is.na(CG_CPDISM00), NA,
                                ifelse(CG_CPDISM00 %in% c(3, 4, 5), 1, 0))
    )

# Check summary of physical punishment
MCS_analysis_with_class %>% count(CG_CPDISM00_bi) # 0 (absent): 12679, 1 (present): 1742, NA: 2004

# Derive scores for domestic violence ======
# Recode domestic violence
## Original: 1 (yes), 2 (no), 3 (don't want to answer)
## New: 0 (no), 1 (yes)
## Note: recode 3 (don't want to answer) to NA
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(
        rec_CG_domestic_violence = ifelse(CG_domestic_violence == 3 | is.na(CG_domestic_violence), NA,
                                          ifelse(CG_domestic_violence == 2, 0, 1)),
        rec_PN_domestic_violence = ifelse(PN_domestic_violence == 3 | is.na(PN_domestic_violence), NA,
                                          ifelse(PN_domestic_violence == 2, 0, 1))
    )

# Check recoding: 0 (no), 1 (yes)
MCS_analysis_with_class %>% count(rec_CG_domestic_violence) # Caregiver - domestic violence: 0 (no): 10884, 1 (yes): 445, NA: 5096
MCS_analysis_with_class %>% count(rec_PN_domestic_violence) # Partner - domestic violence: 0 (no): 9027, 1 (yes): 809, NA: 6589

# Combine the above and derive scores for child maltreatment ======
# Presence for child maltreatment - RQ2 (i.e., presence / binary scores)
## 
MCS_analysis_with_class <- MCS_analysis_with_class %>% 
    mutate(
        child_maltreatment_bi = case_when(
            rowSums(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence), na.rm = FALSE) > 0 ~ 1, # present
            rowSums(is.na(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence))) > 0 ~ NA_real_, # NA
            TRUE ~ 0 # absent
        )
    )

# Check summary of presence of child maltreatment
###### 18-06-25 Pin: doesn't make sense to me that number of presence almost equals absence; maybe because of how NA is accounted for
MCS_analysis_with_class %>% count(child_maltreatment_bi) # 0 (absent): 4845, 1 (present): 4273, NA: 7307

# Sum scores for child maltreatment - RQ3 (i.e., cumulative scores)
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(child_maltreatment = rowSums(select(.,
                                               harsh_parenting_bi, # harsh parenting
                                               CG_CPDISM00_bi, # physical punishment
                                               rec_CG_domestic_violence, # caregiver-reported domestic violence
                                               rec_PN_domestic_violence # partner-reported domestic violence
    ), na.rm = FALSE))

# Check summary of child maltreatment scores
describe(MCS_analysis_with_class$child_maltreatment) # Min: 0, Max: 4, Mean: 0.60, SD: 0.73

# This is the dataset without imputation, which can be used for 
## (1) identifying auxiliary variables using correlation with missingness and
## (2) sensitivity analysis;
# Now we will proceed with imputation

# Now we are ready for imputation and logistic regression analysis

# Imputation rule:
# (1) Impute based on raw scores (continuous) >> pool imputed values
# (2) Derive child maltreatment scores: binarising imputed scores >> sum binarised scores

# ======================================================= #
# ====== Impute study variables - child maltreatment ======
# ======================================================= #

# ====== Derive scores for Kessler (K6) Scale ======
# Recode Kessler (K6) Scale: 6 (can't say) >> NA
MCS_analysis_with_class$CG_CPPHDE00[MCS_analysis_with_class$CG_CPPHDE00 == 6] <- NA # Caregiver - Depression
MCS_analysis_with_class$CG_CPPHHO00[MCS_analysis_with_class$CG_CPPHHO00 == 6] <- NA # Caregiver - Hopelessness
MCS_analysis_with_class$CG_CPPHRF00[MCS_analysis_with_class$CG_CPPHRF00 == 6] <- NA # Caregiver - Restlessness
MCS_analysis_with_class$CG_CPPHEE00[MCS_analysis_with_class$CG_CPPHEE00 == 6] <- NA # Caregiver - Emotional exhaustion
MCS_analysis_with_class$CG_CPPHWO00[MCS_analysis_with_class$CG_CPPHWO00 == 6] <- NA # Caregiver - Worthlessness
MCS_analysis_with_class$CG_CPPHNE00[MCS_analysis_with_class$CG_CPPHNE00 == 6] <- NA # Caregiver - Nervousness
MCS_analysis_with_class$PN_CPPHDE00[MCS_analysis_with_class$PN_CPPHDE00 == 6] <- NA # Partner - Depression
MCS_analysis_with_class$PN_CPPHHO00[MCS_analysis_with_class$PN_CPPHHO00 == 6] <- NA # Partner - Hopelessness
MCS_analysis_with_class$PN_CPPHRF00[MCS_analysis_with_class$PN_CPPHRF00 == 6] <- NA # Partner - Restlessness
MCS_analysis_with_class$PN_CPPHEE00[MCS_analysis_with_class$PN_CPPHEE00 == 6] <- NA # Partner - Emotional exhaustion
MCS_analysis_with_class$PN_CPPHWO00[MCS_analysis_with_class$PN_CPPHWO00 == 6] <- NA # Partner - Worthlessness
MCS_analysis_with_class$PN_CPPHNE00[MCS_analysis_with_class$PN_CPPHNE00 == 6] <- NA # Partner - Nervousness

# Recode Kessler (K6) Scale
## Original: 1 (all of the time) to 5 (none of the time)
## New: 1 (none of the time) to 5 (all of the time)
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(
        CG_CPPHDE00 = recode(as.numeric(CG_CPPHDE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Depression
        CG_CPPHHO00 = recode(as.numeric(CG_CPPHHO00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Hopelessness
        CG_CPPHRF00 = recode(as.numeric(CG_CPPHRF00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Restlessness
        CG_CPPHEE00 = recode(as.numeric(CG_CPPHEE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Emotional exhaustion
        CG_CPPHWO00 = recode(as.numeric(CG_CPPHWO00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Worthlessness
        CG_CPPHNE00 = recode(as.numeric(CG_CPPHNE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Caregiver - Nervousness
        PN_CPPHDE00 = recode(as.numeric(PN_CPPHDE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Partner - Depression
        PN_CPPHHO00 = recode(as.numeric(PN_CPPHHO00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Partner - Hopelessness
        PN_CPPHRF00 = recode(as.numeric(PN_CPPHRF00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Partner - Restlessness
        PN_CPPHEE00 = recode(as.numeric(PN_CPPHEE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Partner - Emotional exhaustion
        PN_CPPHWO00 = recode(as.numeric(PN_CPPHWO00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), # Partner - Worthlessness
        PN_CPPHNE00 = recode(as.numeric(PN_CPPHNE00), `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1) # Partner - Nervousness
    )

# Check Kessler (K6) Scale: 1 (none of the time) to 5 (all of the time)
# Caregiver - Depression
MCS_analysis_with_class %>% count(CG_CPPHDE00) 
describe(MCS_analysis_with_class$CG_CPPHDE00) # Min: 1, Max: 5, Mean: 1.48, SD: 0.81

# Caregiver - Hopelessness
MCS_analysis_with_class %>% count(CG_CPPHHO00) 
describe(MCS_analysis_with_class$CG_CPPHHO00) # Min: 1, Max: 5, Mean: 1.40, SD: 0.75

# Caregiver - Restlessness
MCS_analysis_with_class %>% count(CG_CPPHRF00) 
describe(MCS_analysis_with_class$CG_CPPHRF00) # Min: 1, Max: 5, Mean: 1.62, SD: 0.86

# Caregiver - Emotional exhaustion
MCS_analysis_with_class %>% count(CG_CPPHEE00) 
describe(MCS_analysis_with_class$CG_CPPHEE00) # Min: 1, Max: 5, Mean: 1.82, SD: 0.93

# Caregiver - Worthlessness
MCS_analysis_with_class %>% count(CG_CPPHWO00) 
describe(MCS_analysis_with_class$CG_CPPHWO00) # Min: 1, Max: 5, Mean: 1.37, SD: 0.77

# Caregiver - Nervousness
MCS_analysis_with_class %>% count(CG_CPPHNE00) 
describe(MCS_analysis_with_class$CG_CPPHNE00) # Min: 1, Max: 5, Mean: 1.55, SD: 0.82

# Partner - Depression
MCS_analysis_with_class %>% count(PN_CPPHDE00) 
describe(MCS_analysis_with_class$PN_CPPHDE00) # Min: 1, Max: 5, Mean: 1.38, SD: 0.73

# Partner - Hopelessness
MCS_analysis_with_class %>% count(PN_CPPHHO00) 
describe(MCS_analysis_with_class$PN_CPPHHO00) # Min: 1, Max: 5, Mean: 1.27, SD: 0.64

# Partner - Restlessness
MCS_analysis_with_class %>% count(PN_CPPHRF00) 
describe(MCS_analysis_with_class$PN_CPPHRF00) # Min: 1, Max: 5, Mean: 1.80, SD: 0.91

# Partner - Emotional exhaustion
MCS_analysis_with_class %>% count(PN_CPPHEE00) 
describe(MCS_analysis_with_class$PN_CPPHEE00) # Min: 1, Max: 5, Mean: 1.77, SD: 0.89

# Partner - Worthlessness
MCS_analysis_with_class %>% count(PN_CPPHWO00) 
describe(MCS_analysis_with_class$PN_CPPHWO00) # Min: 1, Max: 5, Mean: 1.24, SD: 0.63

# Partner - Nervousness
MCS_analysis_with_class %>% count(PN_CPPHNE00) 
describe(MCS_analysis_with_class$PN_CPPHNE00) # Min: 1, Max: 5, Mean: 1.51, SD: 0.75

# Sum scores for Kessler (K6) Scale
MCS_analysis_with_class <- MCS_analysis_with_class %>%
    mutate(CG_Kessler = rowMeans(select(., CG_CPPHDE00, CG_CPPHHO00, CG_CPPHRF00, CG_CPPHEE00, CG_CPPHWO00, CG_CPPHNE00), na.rm = TRUE),
           PN_Kessler = rowMeans(select(., PN_CPPHDE00, PN_CPPHHO00, PN_CPPHRF00, PN_CPPHEE00, PN_CPPHWO00, PN_CPPHNE00), na.rm = TRUE))

# Check summary of Kessler (K6) Scale scores
describe(MCS_analysis_with_class$CG_Kessler) # Caregiver - Min: 1, Max: 5, Mean: 1.54, SD: 0.65
describe(MCS_analysis_with_class$PN_Kessler) # Partner - Min: 1, Max: 5, Mean: 1.50, SD: 0.56

# ====== Check amount and percentage of missingness ======
# Check percentage of missingness of the whole dataset, excluding class assignment - 19.16%
pct_miss(MCS_analysis_with_class)

# Check amount of complete data - n = 107
describe(MCS_analysis_with_class)$n

# Check amount of missing data per variable
colSums(is.na(MCS_analysis_with_class)) 

# Find percentage of missing data per variable 
missing_pct <- as.data.frame((colMeans(is.na(MCS_analysis_with_class)))*100)

missing_pct <- missing_pct %>% 
    rename(percentage_missing = "(colMeans(is.na(MCS_analysis_with_class))) * 100") # rename column name

missing_pct %>% arrange(desc(percentage_missing)) # descending order

# Study variable with most missingness percentage: child_maltreatment(/_bi) (44.49%), PN_domestic_violence (40.12%)
# Study variable with least missingness percentage: CG_CPDIBN00, CG_CPDIBN00_bi (11.95%)

# As no variable has more than 70% missingness, no need to remove any (according to Chow et al., 2023)

# Select variables to be placed in imputation model ======
vars_to_impute <- c(
    "MCSID", "CCNUM00",
    # Auxiliary variables
    "CG_CM_ethnicity", # child ethnicity
    "CG_CM_gestational_age", # gestational age
    "CG_res_age_at_birth", # caregiver age at birth
    "CG_res_weight", # caregiver weight
    "CG_res_home_ownership", # caregiver home ownership
    "CG_mother_BMI", # caregiver BMI
    "CG_marital_status_at_birth", # caregiver marital status at birth
    "CG_parent_academic", # caregiver academic qualifications
    "CG_CM_SDQ", # child SDQ score at age 5
    "CG_CM_sex", "CM_age", # child sex and age
    "CG_family_type", # family type
    "CG_parent_relationship", # parent relationship
    "CG_income_quintile", # caregiver household income
    "CG_marital_status", # parental marital status
    "CG_res_CM_relationship", "PN_res_CM_relationship", # parent-CM relationship
    # Child maltreatment
    "child_maltreatment_bi", # Presence of child maltreatment (binary)
    # Main Caregiver- and partner-reported domestic violence
    "CG_domestic_violence", # Caregiver - domestic violence
    "PN_domestic_violence", # Partner - domestic violence
    # Main Caregiver-reported harsh parenting and physical punishment
    "CG_CPDIIG00", # Caregiver - How often ignores CM when naughty
    "CG_CPDISM00", # Caregiver - How often smacks CM when naughty
    "CG_CPDISH00", # Caregiver - How often shouts at CM when naughty
    "CG_CPDIBN00", # Caregiver - How often sends CM to bedroom/naughty chair
    "CG_CPDITR00", # Caregiver - How often takes away treats from CM when naughty
    "CG_CPDITE00", # Caregiver - How often tells CM off when naughty
    "CG_CPDIBR00", # Caregiver - How often bribes CM when naughty
    # Bullying class assignment
    "_traj_Group", # Trajectory group assignment
    # Main caregiver - Kessler (K6) Scale
    "CG_Kessler", # Caregiver - Kessler (K6) Scale
    "PN_Kessler" # Partner - Kessler (K6) Scale
)

# Subset the data
impute_data <- MCS_analysis_with_class[vars_to_impute]

# Change variable name `_traj_Group` to `traj_Group`
impute_data <- impute_data %>%
    rename(traj_Group = `_traj_Group`) # Rename column

# ====== Check counts of categorical variable ======
###### By checking counts of categorical variables, we want to know if there are few people 
###### in a specific groups; if so, we will collapse the category, to make the model
###### of identifying auxiliary variables easier

# Check counts of each single categorical variables
impute_data %>% count(CG_CM_ethnicity) # no need to collapse
impute_data %>% count(CG_res_home_ownership) 
# 9 (squatting): n = 2 >> collapse to 10 (other)
impute_data %>% count(CG_marital_status_at_birth) # no need to collapse
impute_data %>% count(CG_parent_academic) # no need to collapse
impute_data %>% count(CG_CM_sex) # no need to collapse
impute_data %>% count(CG_family_type) # no need to collapse
impute_data %>% count(CG_parent_relationship) 
# 1 (neither): n = 1 >> collapse to NA
impute_data %>% count(CG_income_quintile) # no need to collapse
impute_data %>% count(CG_marital_status) # no need to collapse
impute_data %>% count(CG_res_CM_relationship) 
# 4 (adoptive mother): n = 13 >> collapse to 30 (adoptive / foster / step parent / other); 
# 5 (adoptive father): n = 1 >> collapse to 30 (adoptive / foster / step parent / other);
# 7 (foster mother): n = 1 >> collapse to 30 (adoptive / foster / step parent / other);
# 10 (step mother): n = 3 >> collapse to 30 (adoptive / foster / step parent / other); 
# 11 (step father): n = 1 >> collapse to 30 (adoptive / foster / step parent / other); 
# 16 (other, female): n = 3 >> collapse to 30 (adoptive / foster / step parent / other)
impute_data %>% count(PN_res_CM_relationship)
# 4 (adoptive mother): n = 1 >> collapse to 30 (adoptive / foster / step parent / other); 
# 5 (adoptive father): n = 19 >> collapse to 30 (adoptive / foster / step parent / other); 
# 10 (step mother): n = 23 >> collapse to 30 (adoptive / foster / step parent / other);
# 11 (step father): n = 449 >> collapse to 30 (adoptive / foster / step parent / other);
# 14 (grandfather): n = 8 >> collapse to 30 (adoptive / foster / step parent / other); 
# 16 (other, female): n = 2 >> collapse to 30 (adoptive / foster / step parent / other); 
# 17 (other, male): n = 82 >> collapse to 30 (adoptive / foster / step parent / other)
impute_data %>% count(child_maltreatment_bi) # no need to collapse
impute_data %>% count(CG_domestic_violence) # no need to collapse

# Collapse categories into another / a new one
## Keeps value labels as factor levels
impute_data$CG_res_home_ownership <- as_factor(impute_data$CG_res_home_ownership)  
impute_data$CG_parent_relationship <- as_factor(impute_data$CG_parent_relationship)  
impute_data$CG_res_CM_relationship <- as_factor(impute_data$CG_res_CM_relationship)  
impute_data$PN_res_CM_relationship <- as_factor(impute_data$PN_res_CM_relationship)  

## Collapse categories 
### Respondent home ownership (`CG_res_home_ownership`)
impute_data$CG_res_home_ownership <- fct_collapse(
    impute_data$CG_res_home_ownership,
    "Other" = c("Squatting", "Other")
)

### Parent relationship (`CG_parent_relationship`)
###### Need a different code to assign case to NA
impute_data <- impute_data %>% 
    mutate(CG_parent_relationship = na_if(CG_parent_relationship, "Neither")) # Recode "Neither" to NA

### Respondent-child relationship (`CG_res_CM_relationship`)
impute_data$CG_res_CM_relationship <- fct_collapse(
    impute_data$CG_res_CM_relationship,
    "Other" = c("Adoptive mother", "Adoptive father", "Foster mother", "Step mother", "Step father", "Grandmother", "Other, female")
)

### Partner-child relationship (`PN_res_CM_relationship`)
impute_data$PN_res_CM_relationship <- fct_collapse(
    impute_data$PN_res_CM_relationship,
    "Other" = c("Adoptive mother", "Adoptive father", "Step mother", "Step father", "Grandfather", "Other, female", "Other, male")
)

# ====== Identify auxiliary variables for imputation ======
# Auxiliary variables will be identified following Craig Enders's recommendation
# 1. Predicted by / predicting missing data indicator of variable A
# 2. Residual correlates with variable A

# In my model, auxiliary variables will be identified with
# Predictor: child maltreatment presence
# Outcome: bullying trajectory membership

# Correlates of missingness using cohen's d effect size ======
## We will fit each regression and standardise the coefficients, with predictors with salient impact identified (|d| ≥ .20; Cohen, 2016)
## Note: Instead of testing all models together in sem using lavaan, as shown by Enders; 
## we tested each model separately, as we have variables of mixed types.

# Create missing data indicator
impute_data$mis.CM_bi <- is.na(impute_data$child_maltreatment_bi)

# Predict missingness with potential auxiliaries
## Child ethnicity: est.std = 0.501
standardizedSolution(sem("CG_CM_ethnicity ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver gestational age: est.std = -0.049
standardizedSolution(sem("CG_CM_gestational_age ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver age at birth: est.std = -0.411
standardizedSolution(sem("CG_res_age_at_birth ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver weight: est.std = -0.072
standardizedSolution(sem("CG_res_weight ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver home ownership: est.std = 0.593
impute_data$CG_res_home_ownership <- as.ordered(impute_data$CG_res_home_ownership) # Convert to ordered factor
standardizedSolution(sem("CG_res_home_ownership ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Mother BMI: est.std = 0.018
standardizedSolution(sem("CG_mother_BMI ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver marital status at birth: est.std = 0.402
standardizedSolution(sem("CG_marital_status_at_birth ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Parent academic level: est.std = 0.481
standardizedSolution(sem("CG_parent_academic ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Child SDQ score: est.std = 0.379
standardizedSolution(sem("CG_CM_SDQ ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Child sex: est.std = -0.001
standardizedSolution(sem("CG_CM_sex ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Child age: est.std = 0.074
standardizedSolution(sem("CM_age ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Family type: est.std = 1.226
standardizedSolution(sem("CG_family_type ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Parent relationship: est.std = 0.106
impute_data$CG_parent_relationship <- as.ordered(impute_data$CG_parent_relationship) # Convert to ordered factor
standardizedSolution(sem("CG_parent_relationship ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver income quintile: est.std = -0.874
standardizedSolution(sem("CG_income_quintile ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver marital status: est.std = 0.547
standardizedSolution(sem("CG_marital_status ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Caregiver-child relationship: est.std = 0.402
impute_data$CG_res_CM_relationship <- as.ordered(impute_data$CG_res_CM_relationship) # Convert to ordered factor
standardizedSolution(sem("CG_res_CM_relationship ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

## Partner-child relationship: est.std = -0.391
impute_data$PN_res_CM_relationship <- as.ordered(impute_data$PN_res_CM_relationship) # Convert to ordered factor
standardizedSolution(sem("PN_res_CM_relationship ~ mis.CM_bi", impute_data, fixed.x = T), 
                     type = "std.nox")

# The following variables are of salient impact (|est.std| >= .20); n = 11;
## (1) Child ethnicity (`CG_CM_ethnicity`): est.std = 0.501
## (2) Caregiver age at birth (`CG_res_age_at_birth`): est.std = -0.411
## (3) Caregiver home ownership (`CG_res_home_ownership`): est.std = 0.593 
## (4) Caregiver marital status at birth (`CG_marital_status_at_birth`): est.std = 0.402
## (5) Parent academic level (`CG_parent_academic`): est.std = 0.481 
## (6) Child SDQ score (`CG_CM_SDQ`): est.std = 0.379
## (7) Family type (`CG_family_type`): est.std = 1.226 
## (8) Caregiver income quintile (`CG_income_quintile`): est.std = -0.874
## (9) Caregiver marital status (`CG_marital_status`): est.std = 0.547
## (10) Caregiver-child relationship (`CG_res_CM_relationship`): est.std = 0.402
## (11) Partner-child relationship (`PN_res_CM_relationship`): est.std = -0.391

# Semi-partial / residual correlations (|Std.all| of covariance >= .30 will be interpreted as saliant; Collins et al., 2001) ======
# Convert categorical variables to numeric vector (as `sem.auxiliary` requires only numeric vector)
impute_data$child_maltreatment_bi_num <- as.numeric(impute_data$child_maltreatment_bi)
impute_data$traj_Group_num <- as.numeric(impute_data$traj_Group)
impute_data$CG_CM_ethnicity_num <- as.numeric(impute_data$CG_CM_ethnicity)
impute_data$CG_res_home_ownership_num <- as.numeric(impute_data$CG_res_home_ownership)
impute_data$CG_marital_status_at_birth_num <- as.numeric(impute_data$CG_marital_status_at_birth)
impute_data$CG_parent_academic_num <- as.numeric(impute_data$CG_parent_academic)
impute_data$CG_family_type_num <- as.numeric(impute_data$CG_family_type)
impute_data$CG_income_quintile_num <- as.numeric(impute_data$CG_income_quintile)
impute_data$CG_marital_status_num <- as.numeric(impute_data$CG_marital_status)
impute_data$CG_res_CM_relationship_num <- as.numeric(impute_data$CG_res_CM_relationship)
impute_data$PN_res_CM_relationship_num <- as.numeric(impute_data$PN_res_CM_relationship)

# Define auxiliary variables (so all these variables will correlate incomplete variables residual)
auxvars <- c("CG_CM_ethnicity_num",
             "CG_res_age_at_birth",
             "CG_res_home_ownership_num",
             "CG_marital_status_at_birth_num",
             "CG_parent_academic_num",
             "CG_CM_SDQ",
             "CG_family_type_num",
             "CG_income_quintile_num",
             "CG_marital_status_num",
             "CG_res_CM_relationship_num",
             "PN_res_CM_relationship_num")

# Residualise variable on the other variables in the model
## Note: no need to model outcome (i.e., traj_Group) as it has complete data

## Residualise child maltreatment presence (no salient aux.)
### Note: we can use FIML when outcome is nominal (4 categories)
model.CM_bi <- 'child_maltreatment_bi_num ~ traj_Group_num + CG_Kessler + PN_Kessler'

### Chile ethnicity: Std.all = -0.018
fit.CM_eth <- sem.auxiliary(model.CM_bi, impute_data, 
              fixed.x = F, missing = "fiml",
              aux = "CG_CM_ethnicity",
              em.h1.iter.max = 500)
summary(fit.CM_eth, rsquare = T, standardize = T)

### Child age at birth: Std.all = -0.056
fit.CM_age_birth <- sem.auxiliary(model.CM_bi, impute_data, 
                            fixed.x = F, missing = "fiml",
                            aux = "CG_res_age_at_birth",
                            em.h1.iter.max = 500)
summary(fit.CM_age_birth, rsquare = T, standardize = T)

### Caregiver home ownership: Std.all = -0.012
fit.res_home <- sem.auxiliary(model.CM_bi, impute_data, 
                                  fixed.x = F, missing = "fiml",
                                  aux = "CG_res_home_ownership_num",
                                  em.h1.iter.max = 500)
summary(fit.res_home, rsquare = T, standardize = T)

### Caregiver marital status at birth: Std.all = 0.020
fit.CG_ms_birth <- sem.auxiliary(model.CM_bi, impute_data, 
                              fixed.x = F, missing = "fiml",
                              aux = "CG_marital_status_at_birth_num",
                              em.h1.iter.max = 500)
summary(fit.CG_ms_birth, rsquare = T, standardize = T)

### Parent academic: Std.all = -0.076
###### Note: I didn't use the numeric version as (1) R gives warning (2) original variable has 8 categories, which is big enough for `fiml`
fit.parent_acmc <- sem.auxiliary(model.CM_bi, impute_data, 
                           fixed.x = F, missing = "fiml",
                           aux = "CG_parent_academic",
                           em.h1.iter.max = 500)
summary(fit.parent_acmc, rsquare = T, standardize = T)

### Child SDQ score: Std.all = 0.148
fit.CM_SDQ <- sem.auxiliary(model.CM_bi, impute_data, 
                                 fixed.x = F, missing = "fiml",
                                 aux = "CG_CM_SDQ",
                                 em.h1.iter.max = 500)
summary(fit.CM_SDQ, rsquare = T, standardize = T)

### Family type: Std.all = 0.008
##### R gives warning --> when checking cross-tabs I found that no one in one-parent family gives response to child maltreatment presence >> decided to remove family type as aux.
fit.fam_type <- sem.auxiliary(model.CM_bi, impute_data, 
                            fixed.x = F, missing = "fiml",
                            aux = "CG_family_type",
                            em.h1.iter.max = 500)
summary(fit.fam_type, rsquare = T, standardize = T)

table(impute_data$CG_family_type, useNA = "ifany") # 1 (nuclear): 0.154, 2 (extended): 0.154, 3 (blended): 0.154, 4 (single parent): 0.154, 5 (other): 0.154
cor(impute_data[ , c("CG_family_type", "child_maltreatment_bi")], use = "pairwise.complete.obs")

sum(complete.cases(impute_data[ , c("CG_family_type", "child_maltreatment_bi")])) 

### Income quintile: Std.all = 0.043
fit.income <- sem.auxiliary(model.CM_bi, impute_data, 
                              fixed.x = F, missing = "fiml",
                              aux = "CG_income_quintile_num",
                              em.h1.iter.max = 500)
summary(fit.income, rsquare = T, standardize = T)

### Marital status: Std.all = 0.007
fit.ms <- sem.auxiliary(model.CM_bi, impute_data, 
                            fixed.x = F, missing = "fiml",
                            aux = "CG_marital_status_num",
                            em.h1.iter.max = 500)
summary(fit.ms, rsquare = T, standardize = T)

### Caregiver-child relationship: Std.all = -0.004
fit.CG_CM_relationship <- sem.auxiliary(model.CM_bi, impute_data, 
                        fixed.x = F, missing = "fiml",
                        aux = "CG_res_CM_relationship_num",
                        em.h1.iter.max = 500)
summary(fit.CG_CM_relationship, rsquare = T, standardize = T)

### Partner-child relationship: Std.all = -0.003
fit.PN_CM_relationship <- sem.auxiliary(model.CM_bi, impute_data, 
                                        fixed.x = F, missing = "fiml",
                                        aux = "PN_res_CM_relationship_num",
                                        em.h1.iter.max = 500)
summary(fit.PN_CM_relationship, rsquare = T, standardize = T)

###### As no auxiliary variable was selected with |Std.all| >= .30, so we will not use residual correlation in the imputation model
###### Instead, we proceed with treating these 11 variables as auxiliaries

# Convert variables in imputation model ======
# Select variables to be placed in imputation model and subset the data
# Create unique ID for each participant
impute_data <- impute_data %>%
    group_by(MCSID, CCNUM00) %>%
    mutate(child_ID = cur_group_id()) %>%
    ungroup()

impute_data_with_aux <- impute_data[c(
    "child_ID",
    "CG_CM_sex",
    # Auxiliary variables
    "CG_CM_ethnicity", # child ethnicity
    "CG_res_age_at_birth", # caregiver age at birth
    "CG_res_home_ownership", # caregiver home ownership
    "CG_marital_status_at_birth", # caregiver marital status at birth
    "CG_parent_academic", # caregiver academic qualifications
    "CG_CM_SDQ", # child SDQ score at birth
    "CG_family_type", # family type
    "CG_income_quintile", # caregiver household income
    "CG_marital_status", # parental marital status
    "CG_res_CM_relationship", # Caregiver-child relationship
    "PN_res_CM_relationship", # Partner-child relationship
    # Main Caregiver- and partner-reported domestic violence
    "CG_domestic_violence", # Caregiver - domestic violence
    "PN_domestic_violence", # Partner - domestic violence
    # Main Caregiver-reported harsh parenting and physical punishment
    "CG_CPDIIG00", # Caregiver - How often ignores CM when naughty
    "CG_CPDISM00", # Caregiver - How often smacks CM when naughty
    "CG_CPDISH00", # Caregiver - How often shouts at CM when naughty
    "CG_CPDIBN00", # Caregiver - How often sends CM to bedroom/naughty chair
    "CG_CPDITR00", # Caregiver - How often takes away treats from CM when naughty
    "CG_CPDITE00", # Caregiver - How often tells CM off when naughty
    "CG_CPDIBR00", # Caregiver - How often bribes CM when naughty
    # Child maltreatment
    "child_maltreatment_bi", # Presence of child maltreatment (binary)
    # Bullying class assignment
    "traj_Group", # Trajectory group assignment
    # Main caregiver - Kessler (K6) Scale
    "CG_Kessler", # Caregiver - Kessler (K6) Scale
    "PN_Kessler" # Partner - Kessler (K6) Scale
)]

# Check types
str(impute_data_with_aux)

# Check counts of each single categorical variables
impute_data_with_aux %>% count(CG_CM_sex) # no need to collapse
impute_data_with_aux %>% count(CG_CM_ethnicity) # no need to collapse
impute_data_with_aux %>% count(CG_res_home_ownership) 
impute_data_with_aux %>% count(CG_marital_status_at_birth) # can collapse windowed (n = 27)
impute_data_with_aux %>% count(CG_parent_academic) # no need to collapse
impute_data_with_aux %>% count(CG_family_type) # no need to collapse
impute_data_with_aux %>% count(CG_income_quintile) # no need to collapse
impute_data_with_aux %>% count(CG_marital_status) # can collapse windowed (n = 69)
impute_data_with_aux %>% count(CG_res_CM_relationship) # no need to collapse
impute_data_with_aux %>% count(PN_res_CM_relationship) # no need to collapse

# Check cross-tabulations of each variable with child maltreatment presence
## List of variables to check
vars <- c(
    "CG_CM_ethnicity",
    "CG_res_home_ownership",
    "CG_marital_status_at_birth",
    "CG_parent_academic",
    "CG_family_type",
    "CG_income_quintile",
    "CG_marital_status",
    "CG_res_CM_relationship",
    "PN_res_CM_relationship"
)

## Variable (i.e., `child_maltreatment_bi`) to cross-tabulate with
target <- "child_maltreatment_bi"

## Loop through variables and print cross-tabs
for (var in vars) {
    cat("\n========================\n")
    cat("Cross-tab:", var, "vs", target, "\n")
    cat("========================\n")
    
    print(
        impute_data %>%
            count(!!sym(var), !!sym(target)) %>%
            tidyr::pivot_wider(
                names_from = !!sym(target),
                values_from = n,
                values_fill = 0
            )
    )
}

### `CG_CM_ethnicity` --> ok
### `CG_res_home_ownership` --> 3 (Part rent/part mortgage (shared equity)) and 8 (Live rent free) >> 9 (Other)
### `CG_marital_status_at_birth` --> 2 (married) and 3 (remarried) >> 1 (in a relationship); 1 (separated), 4 (single), 5 (divorced), and 6 (widowed) to 2 (not in a relationship)
### `CG_parent_academic` --> 95 (Other academic qualifications) and 96 (None of these qualifications) to 7 (Other)
### `CG_family_type` --> no variance (i.e., for one parent/carer families, no response is given to `child_maltreatment_bi`); remove this from aux.
### `CG_income_quintile` --> ok
### `CG_marital_status` --> 2 (married) and 3 (remarried) >> 1 (in a relationship); 1 (separated), 4 (single), 5 (divorced), and 6 (widowed) to 2 (not in a relationship)
### `CG_res_CM_relationship` --> 2 (natural father) >> 3 (Other)
### `PN_res_CM_relationship` --> 1 (natural mother) >> 3 (Other)

# Variables that need to be collapsed / adjusted
## Caregiver home ownership: 3 (Part rent/part mortgage (shared equity)) and 8 (Live rent free) >> 9 (Other)
impute_data_with_aux$CG_res_home_ownership_collapsed <- fct_collapse(
    impute_data_with_aux$CG_res_home_ownership,
    "Other" = c("Part rent/part mortgage (shared equity)", "Live rent free")
)

## Marital status at birth: 2 (married) and 3 (remarried) >> 1 (in a relationship); 1 (separated), 4 (single), 5 (divorced), and 6 (widowed) to 2 (not in a relationship)
impute_data_with_aux <- impute_data_with_aux %>% 
    mutate(
        CG_marital_status_at_birth = as.numeric(CG_marital_status_at_birth), # strip haven labels
        CG_marital_status_at_birth_collapsed = case_when(
            CG_marital_status_at_birth %in% c(2, 3) ~ 1 , # in a relationship
            CG_marital_status_at_birth %in% c(1, 4, 5, 6) ~ 2, # not in a relationship
            TRUE ~ NA_real_
        ),
        CG_marital_status_at_birth_collapsed = factor(
            CG_marital_status_at_birth_collapsed,
            levels = c(1, 2),
            labels = c("In a relationship", "Not in a relationship")
        )
    )

## Parent academic qualifications: 95 (Other academic qualifications) and 96 (None of these qualifications) to 7 (Other)
impute_data_with_aux <- impute_data_with_aux %>% 
    mutate(
        CG_parent_academic = as.numeric(CG_parent_academic), # strip haven labels
        CG_parent_academic_collapsed = case_when(
            CG_parent_academic %in% c(95, 96) ~ 7, # Other
            TRUE ~ CG_parent_academic
        ),
        CG_parent_academic_collapsed = factor(
            CG_parent_academic_collapsed,
            levels = 1:7,
            labels = c("Higher degree", "First degree", "Diplomas in higher education", 
                       "A / AS / S levels", "O level / GCSE grades A-C", 
                       "GCSE grades D-G", "Other")
        )
    )

## Marital status: 2 (married) and 3 (remarried) >> 1 (in a relationship); 1 (separated), 4 (single), 5 (divorced), and 6 (widowed) to 2 (not in a relationship)
impute_data_with_aux <- impute_data_with_aux %>% 
    mutate(
        CG_marital_status = as.numeric(CG_marital_status), # strip haven labels
        CG_marital_status_collapsed = case_when(
            CG_marital_status %in% c(2, 3) ~ 1 , # in a relationship
            CG_marital_status %in% c(1, 4, 5, 6) ~ 2, # not in a relationship
            TRUE ~ NA_real_
        ),
        CG_marital_status_collapsed = factor(
            CG_marital_status_collapsed,
            levels = c(1, 2),
            labels = c("In a relationship", "Not in a relationship")
        )
    )

## Caregiver-child relationship: 2 (natural father) >> 3 (Other)
impute_data_with_aux <- impute_data_with_aux %>% 
    mutate(
        CG_res_CM_relationship = as.numeric(CG_res_CM_relationship), # strip haven labels
        CG_res_CM_relationship_collapsed = case_when(
            CG_res_CM_relationship == 3 ~ 5, # collapse "natural father" to "Other"
            TRUE ~ CG_res_CM_relationship
        ),
        CG_res_CM_relationship_collapsed = factor(
            CG_res_CM_relationship_collapsed,
            levels = c(2, 5),
            labels = c("Natural mother", "Other")
        )
    )

# Partner-child relationship: 1 (natural mother) >> 3 (Other)
impute_data_with_aux <- impute_data_with_aux %>% 
    mutate(
        PN_res_CM_relationship = as.numeric(PN_res_CM_relationship), # strip haven labels
        PN_res_CM_relationship_collapsed = case_when(
            PN_res_CM_relationship == 2 ~ 5, # collapse "natural mother" to "Other"
            TRUE ~ PN_res_CM_relationship
        ),
        PN_res_CM_relationship_collapsed = factor(
            PN_res_CM_relationship_collapsed,
            levels = c(2, 3, 5),
            labels = c("Natural mother", "Natural father", "Other")
        )
    )

# Check cross-tabulations again to make sure variables are collapsed correctly
## List of variables to check
vars <- c(
    "CG_CM_ethnicity",
    "CG_res_home_ownership_collapsed",
    "CG_marital_status_at_birth_collapsed",
    "CG_parent_academic_collapsed",
    #"CG_family_type", # removed due to no variance
    "CG_income_quintile",
    "CG_marital_status_collapsed",
    "CG_res_CM_relationship_collapsed",
    "PN_res_CM_relationship_collapsed"
)

## Variable (i.e., `child_maltreatment_bi`) to cross-tabulate with
target <- "child_maltreatment_bi"

## Loop through variables and print cross-tabs
for (var in vars) {
    cat("\n========================\n")
    cat("Cross-tab:", var, "vs", target, "\n")
    cat("========================\n")
    
    print(
        impute_data_with_aux %>%
            count(!!sym(var), !!sym(target)) %>%
            tidyr::pivot_wider(
                names_from = !!sym(target),
                values_from = n,
                values_fill = 0
            )
    )
}

# Check types
str(impute_data_with_aux)

# Impute child maltreatment ======
# Update the imputation dataset after collapsed / adjusted variables
impute_data_with_aux <- impute_data_with_aux[c(
    "child_ID",
    "CG_CM_sex",
    # Auxiliary variables
    "CG_CM_ethnicity", # child ethnicity
    "CG_res_age_at_birth", # caregiver age at birth
    "CG_res_home_ownership_collapsed", # caregiver home ownership
    "CG_marital_status_at_birth_collapsed", # caregiver marital status at birth
    "CG_parent_academic_collapsed", # caregiver academic qualifications
    "CG_CM_SDQ", # child SDQ score at birth
    #"CG_family_type", # family type (removed due to zero covariance with child maltreatment presence)
    "CG_income_quintile", # caregiver household income
    "CG_marital_status_collapsed", # parental marital status
    "CG_res_CM_relationship_collapsed", # Caregiver-child relationship
    "PN_res_CM_relationship_collapsed", # Partner-child relationship
    # Main Caregiver- and partner-reported domestic violence
    "CG_domestic_violence", # Caregiver - domestic violence
    "PN_domestic_violence", # Partner - domestic violence
    # Main Caregiver-reported harsh parenting and physical punishment
    "CG_CPDIIG00", # Caregiver - How often ignores CM when naughty
    "CG_CPDISM00", # Caregiver - How often smacks CM when naughty
    "CG_CPDISH00", # Caregiver - How often shouts at CM when naughty
    "CG_CPDIBN00", # Caregiver - How often sends CM to bedroom/naughty chair
    "CG_CPDITR00", # Caregiver - How often takes away treats from CM when naughty
    "CG_CPDITE00", # Caregiver - How often tells CM off when naughty
    "CG_CPDIBR00", # Caregiver - How often bribes CM when naughty
    # Child maltreatment
    "child_maltreatment_bi", # Presence of child maltreatment (binary)
    # Bullying class assignment
    "traj_Group", # Trajectory group assignment
    # Main caregiver - Kessler (K6) Scale
    "CG_Kessler", # Caregiver - Kessler (K6) Scale
    "PN_Kessler" # Partner - Kessler (K6) Scale
)]

# Create variables for derived variables after imputation
## We create these variables to make regression pooling easier after imputation
imp_with_drvd_var <- impute_data_with_aux %>%
        # Binarising each type of child maltreatment
        mutate(
            # Caregiver ignore child
            CG_CPDIIG00_bi = ifelse(is.na(CG_CPDIIG00), NA,
                                    ifelse(CG_CPDIIG00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver shouts at child
            CG_CPDISH00_bi = ifelse(is.na(CG_CPDISH00), NA,
                                    ifelse(CG_CPDISH00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver sends child to bedroom/naughty chair
            CG_CPDIBN00_bi = ifelse(is.na(CG_CPDIBN00), NA,
                                    ifelse(CG_CPDIBN00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver takes away treats from child
            CG_CPDITR00_bi = ifelse(is.na(CG_CPDITR00), NA,
                                    ifelse(CG_CPDITR00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver tells child off
            CG_CPDITE00_bi = ifelse(is.na(CG_CPDITE00), NA,
                                    ifelse(CG_CPDITE00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver bribes child
            CG_CPDIBR00_bi = ifelse(is.na(CG_CPDIBR00), NA,
                                    ifelse(CG_CPDIBR00 %in% c(3, 4, 5), 1, 0)),
            # Caregiver experiences domestic violence
            rec_CG_domestic_violence = ifelse(CG_domestic_violence == 3 | is.na(CG_domestic_violence), NA,
                                              ifelse(CG_domestic_violence == 2, 0, 1)),
            # Partner experiences domestic violence
            rec_PN_domestic_violence = ifelse(PN_domestic_violence == 3 | is.na(PN_domestic_violence), NA,
                                              ifelse(PN_domestic_violence == 2, 0, 1))) %>% 
        # Summing binary scores for harsh parenting
        mutate(
            harsh_parenting = rowSums(select(.,
                                             CG_CPDIIG00_bi,
                                             CG_CPDISH00_bi,
                                             CG_CPDIBN00_bi,
                                             CG_CPDITR00_bi,
                                             CG_CPDITE00_bi,
                                             CG_CPDIBR00_bi
            ), na.rm = FALSE)) %>% 
        # Binarising sum scores for harsh parenting
        mutate(
            harsh_parenting_bi = ifelse(is.na(harsh_parenting), NA, 
                                        ifelse(harsh_parenting > 4, 1, 0)
            )) %>%
        # Binarising physical punishment
        mutate(
            CG_CPDISM00_bi = ifelse(is.na(CG_CPDISM00), NA,
                                    ifelse(CG_CPDISM00 %in% c(3, 4, 5), 1, 0))) %>% 
        # Binarising presence of child maltreatment
        mutate(
            child_maltreatment_bi = case_when(
                rowSums(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence), na.rm = FALSE) > 0 ~ 1, # present
                rowSums(is.na(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence))) > 0 ~ NA_real_, # NA
                TRUE ~ 0 # absent
            )) %>% 
        # Derive cumulative child maltreatment scores by summing each type of child maltreatment
        mutate(
            child_maltreatment = rowSums(select(.,
                                                harsh_parenting_bi, # harsh parenting
                                                CG_CPDISM00_bi, # physical punishment
                                                rec_CG_domestic_violence, # caregiver-reported domestic violence
                                                rec_PN_domestic_violence # partner-reported domestic violence
            ), na.rm = FALSE))

# Check the structure of auxiliary variables; convert variables to appropriate types
str(imp_with_drvd_var)

## Child sex
str(imp_with_drvd_var$CG_CM_sex) # Numeric
imp_with_drvd_var$CG_CM_sex <- factor(imp_with_drvd_var$CG_CM_sex,
                                      levels = c(1, 2),
                                      labels = c("Male", "Female"))
str(imp_with_drvd_var$CG_CM_sex) # Factor

## Child ethnicity
str(imp_with_drvd_var$CG_CM_ethnicity) # Numeric
imp_with_drvd_var$CG_CM_ethnicity <- factor(imp_with_drvd_var$CG_CM_ethnicity,
                                      levels = c(1,2,3,4,5,6),
                                      labels = c("White","Mixed","Indian","Pakistani and Bangladeshi","Black or Black British","Other Ethnic group (inc Chinese,Other)"))
str(imp_with_drvd_var$CG_CM_ethnicity) # Factor

## Caregiver age at birth
str(imp_with_drvd_var$CG_res_age_at_birth) # Numeric with haven labels
imp_with_drvd_var$CG_res_age_at_birth <- as.numeric(imp_with_drvd_var$CG_res_age_at_birth)
str(imp_with_drvd_var$CG_res_age_at_birth) # Numeric

## Caregiver home ownership
str(imp_with_drvd_var$CG_res_home_ownership_collapsed) # Ordered factor
imp_with_drvd_var$CG_res_home_ownership_collapsed <- factor(imp_with_drvd_var$CG_res_home_ownership_collapsed,
                                                            ordered = FALSE)
str(imp_with_drvd_var$CG_res_home_ownership_collapsed) # (Unordered) factor

## Caregiver marital status at birth
str(imp_with_drvd_var$CG_marital_status_at_birth_collapsed) # Factor

## Parent academic qualifications
str(imp_with_drvd_var$CG_parent_academic_collapsed) # Factor

## Child SDQ score
str(imp_with_drvd_var$CG_CM_SDQ) # Numeric with haven labels
imp_with_drvd_var$CG_CM_SDQ <- as.numeric(imp_with_drvd_var$CG_CM_SDQ)
str(imp_with_drvd_var$CG_CM_SDQ) # Numeric

## Caregiver income quintile
str(imp_with_drvd_var$CG_income_quintile) # Numeric with haven labels
imp_with_drvd_var$CG_income_quintile <- factor(imp_with_drvd_var$CG_income_quintile,
                                            levels = c(1,2,3,4,5),
                                            labels = c("Lowest","Second","Third","Fourth","Highest"),
                                            ordered = TRUE)
str(imp_with_drvd_var$CG_income_quintile) # Ordered factor

## Parental marital status
str(imp_with_drvd_var$CG_marital_status_collapsed) # Factor

## Caregiver-child relationship
str(imp_with_drvd_var$CG_res_CM_relationship_collapsed) # Factor

## Partner-child relationship
str(imp_with_drvd_var$PN_res_CM_relationship_collapsed) # Factor

## Caregiver-reported domestic violence
str(imp_with_drvd_var$CG_domestic_violence) # Numeric with haven labels
imp_with_drvd_var$CG_domestic_violence <- factor(imp_with_drvd_var$CG_domestic_violence,
                                                  levels = c(1,2),
                                                  labels = c("Yes","No"))
str(imp_with_drvd_var$CG_domestic_violence) # Factor

## Partner-reported domestic violence
str(imp_with_drvd_var$PN_domestic_violence) # Numeric with haven labels
imp_with_drvd_var$PN_domestic_violence <- factor(imp_with_drvd_var$PN_domestic_violence,
                                                 levels = c(1,2),
                                                 labels = c("Yes","No"))
str(imp_with_drvd_var$PN_domestic_violence) # Factor

## Main Caregiver-reported harsh parenting and physical punishment
### Caregiver - How often ignores CM when naughty
str(imp_with_drvd_var$CG_CPDIIG00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDIIG00 <- factor(imp_with_drvd_var$CG_CPDIIG00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDIIG00) # Ordered factor

### Caregiver - How often smacks CM when naughty
str(imp_with_drvd_var$CG_CPDISM00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDISM00 <- factor(imp_with_drvd_var$CG_CPDISM00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDISM00) # Ordered factor

### Caregiver - How often shouts at CM when naughty
str(imp_with_drvd_var$CG_CPDISH00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDISH00 <- factor(imp_with_drvd_var$CG_CPDISH00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDISH00) # Ordered factor

### Caregiver - How often sends CM to bedroom/naughty chair
str(imp_with_drvd_var$CG_CPDIBN00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDIBN00 <- factor(imp_with_drvd_var$CG_CPDIBN00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDIBN00) # Ordered factor

### Caregiver - How often takes away treats from CM when naughty
str(imp_with_drvd_var$CG_CPDITR00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDITR00 <- factor(imp_with_drvd_var$CG_CPDITR00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDITR00) # Ordered factor

### Caregiver - How often tells CM off when naughty
str(imp_with_drvd_var$CG_CPDITE00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDITE00 <- factor(imp_with_drvd_var$CG_CPDITE00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDITE00) # Ordered factor

### Caregiver - How often bribes CM when naughty
str(imp_with_drvd_var$CG_CPDIBR00) # Numeric with haven labels
imp_with_drvd_var$CG_CPDIBR00 <- factor(imp_with_drvd_var$CG_CPDIBR00,
                                               levels = c(1,2,3,4,5),
                                               labels = c("Never","Rarely","Sometimes (about once a month)","Often (about once a week or more)","Daily"),
                                               ordered = TRUE)
str(imp_with_drvd_var$CG_CPDIBR00) # Ordered factor

### Bullying trajectory group assignment
str(imp_with_drvd_var$traj_Group) # Numeric with haven labels
imp_with_drvd_var$traj_Group <- factor(imp_with_drvd_var$traj_Group)
str(imp_with_drvd_var$traj_Group) # Factor

## Caregiver - Kessler (K6) Scale
str(imp_with_drvd_var$CG_Kessler) # Numeric with haven labels
imp_with_drvd_var$CG_Kessler <- as.numeric(imp_with_drvd_var$CG_Kessler)
str(imp_with_drvd_var$CG_Kessler) # Numeric

## Partner - Kessler (K6) Scale
str(imp_with_drvd_var$PN_Kessler) # Numeric with haven labels
imp_with_drvd_var$PN_Kessler <- as.numeric(imp_with_drvd_var$PN_Kessler)
str(imp_with_drvd_var$PN_Kessler) # Numeric

# Check types
str(imp_with_drvd_var)

# Before imputation, we run an imputation with 0 iterations to check the predictor matrix and imputation methods

# Predictor matrix: variables to be used to predict other variables, with values 1 and 0
# 1: a predictor is used to predict the other variable
# 0: a predictor is not used to predict the other variable

# Imputation methods, empty cell "" are the variable that will not be imputed

# Check predictor matrix
imputed_MCS <- mice(imp_with_drvd_var, maxit=0)
imputed_MCS$loggedEvents # Inspect logged events --> two logged events showing that `rec_CG_domestic_violence` and `rec_PN_domestic_violence` are collinear with other variables, which is fine as they're not used in imputation
predM <- imputed_MCS$predictorMatrix
meth <- imputed_MCS$method
predM # Diagonal is always 0 as as a predictor can't predict itself
meth # Which imputation method is assigned to each variable; `child_ID` and `traj_Group` are empty as they have complete data; this is desirable for our imputation

# Child ID → set both row & column to 0
predM["child_ID", ] <- 0
predM[, "child_ID"] <- 0

# Derived / recoded variables → set both row & column to 0
exclude_vars <- c(
    "CG_CPDIIG00_bi",       # Caregiver ignores child
    "CG_CPDISH00_bi",       # Caregiver shouts at child
    "CG_CPDIBN00_bi",       # Caregiver sends child to bedroom/naughty chair
    "CG_CPDITR00_bi",       # Caregiver takes away treats from child
    "CG_CPDITE00_bi",       # Caregiver tells child off
    "CG_CPDIBR00_bi",       # Caregiver bribes child
    "rec_CG_domestic_violence", # Caregiver experiences domestic violence
    "rec_PN_domestic_violence", # Partner experiences domestic violence
    "harsh_parenting",      # Sum of harsh parenting
    "harsh_parenting_bi",   # Binarised sum of harsh parenting
    "CG_CPDISM00_bi",       # Caregiver smacks child
    "child_maltreatment_bi",# Presence of child maltreatment (binary)
    "child_maltreatment"    # Cumulative child maltreatment score
)

for (v in exclude_vars) {
    predM[v, ] <- 0
    predM[, v] <- 0
}

# Specify imputation methods: exclude prediction on derived variables
meth[("CG_CPDIIG00_bi")]=""
meth[("CG_CPDISH00_bi")]=""
meth[("CG_CPDIBN00_bi")]=""
meth[("CG_CPDITR00_bi")]=""
meth[("CG_CPDITE00_bi")]=""
meth[("CG_CPDIBR00_bi")]=""
meth[("rec_CG_domestic_violence")]=""
meth[("rec_PN_domestic_violence")]=""
meth[("harsh_parenting")]=""
meth[("harsh_parenting_bi")]=""
meth[("CG_CPDISM00_bi")]=""
meth[("child_maltreatment_bi")]=""
meth[("child_maltreatment")]=""

# Re-run `mice` with custom setup
imputed_MCS <- mice(
    imp_with_drvd_var,
    maxit = 0, # Number of iterations
    predictorMatrix = predM, # Custom predictor matrix
    method = meth, # Custom imputation methods
)

## Inspect which variables will be imputed
imputed_MCS$method
imputed_MCS$predictorMatrix

# Impute converted variables
imputed_MCS <- mice(imp_with_drvd_var, 
                    m = 2, 
                    maxit = 2,
                    predictorMatrix = predM, # Custom predictor matrix
                    method = meth, # Custom imputation methods
                    seed = 123,
                    print = TRUE)

# Inspect warning message (n = 84)
imputed_MCS$loggedEvents
table(imputed_MCS$loggedEvents$dep) # 4 logged events for each imputed variable
table(imputed_MCS$loggedEvents$out) # Logged events related to: `CG_res_CM_relationship_collapsed`, `PN_res_CM_relationship_collapsed`

# Collapse levels of categorical variables
imp_with_drvd_var$CG_res_CM_relationship_collapsed <- forcats::fct_collapse(
    imp_with_drvd_var$CG_res_CM_relationship_collapsed,
    Other = c("Other", "Natural father")
)

imp_with_drvd_var$PN_res_CM_relationship_collapsed <- forcats::fct_collapse(
    imp_with_drvd_var$PN_res_CM_relationship_collapsed,
    Other = c("Other", "Natural mother") # if that's ever an issue
)

# Test imputation again
imputed_MCS <- mice(imp_with_drvd_var, 
                    m = 2, 
                    maxit = 2,
                    predictorMatrix = predM, # Custom predictor matrix
                    method = meth, # Custom imputation methods
                    seed = 123,
                    print = TRUE)

# Impute converted variables
imputed_MCS <- mice(imp_with_drvd_var, 
                    m = 40, 
                    maxit = 10,
                    predictorMatrix = predM, # Custom predictor matrix
                    method = meth, # Custom imputation methods
                    seed = 123,
                    print = TRUE)

# Check imputed datasets ======
# Extract complete imputed datasets only to create a list
completed_list <- mice::complete(imputed_MCS, action = "all")

# Check summary of imputed datasets
summary(completed_list) # 40 datasets; 37 variables

# Check missingness of imputed datasets
colSums(is.na(mice::complete(imputed_MCS, action = 2)))

# Now we have imputed datasets ready;
# we will proceed with study variable transformation and logistic regression

# Export imputed datasets to R data file (as backup)
save(imputed_MCS, file = "imputed_MCS.RData")

# Extract imputed datasets in long format
imputed_MCS_long <- mice::complete(imputed_MCS, action = "long", include = TRUE)

# Checking the structure
str(imputed_MCS_long) # 40 datasets; 37 variables + 2 indicator variables

# Transform imputed variables ======
# Custom function to transform each dataset
## First: Binarise each type of child maltreatment (i.e., harsh discipline, phyiscal punishment, domestic violence)
## Second: Binarise sum scores obtained to derive "presence of child maltreatment" (0-1)
## Third: Sum binarised scores to derive "cumulative scores for child maltreatment"

# Create function for binarising items
trnsfrm_imputed_MCS_long <- imputed_MCS_long %>%
        # Binarising each type of child maltreatment
        mutate(
            # Caregiver ignore child
            CG_CPDIIG00_bi = ifelse(is.na(CG_CPDIIG00), NA,
                                    ifelse(CG_CPDIIG00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0)),
            # Caregiver shouts at child
            CG_CPDISH00_bi = ifelse(is.na(CG_CPDISH00), NA,
                                    ifelse(CG_CPDISH00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0)),
            # Caregiver sends child to bedroom/naughty chair
            CG_CPDIBN00_bi = ifelse(is.na(CG_CPDIBN00), NA,
                                    ifelse(CG_CPDIBN00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0)),
            # Caregiver takes away treats from child
            CG_CPDITR00_bi = ifelse(is.na(CG_CPDITR00), NA,
                                    ifelse(CG_CPDITR00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0)),
            # Caregiver tells child off
            CG_CPDITE00_bi = ifelse(is.na(CG_CPDITE00), NA,
                                    ifelse(CG_CPDITE00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0)),
            # Caregiver bribes child
            CG_CPDIBR00_bi = ifelse(is.na(CG_CPDIBR00), NA,
                                    ifelse(CG_CPDIBR00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0))) %>% 
        # Summing binary scores for harsh parenting
        mutate(
            harsh_parenting = rowSums(select(.,
                                             CG_CPDIIG00_bi,
                                             CG_CPDISH00_bi,
                                             CG_CPDIBN00_bi,
                                             CG_CPDITR00_bi,
                                             CG_CPDITE00_bi,
                                             CG_CPDIBR00_bi
            ), na.rm = FALSE)) %>% 
        # Binarising sum scores for harsh parenting
        mutate(
            harsh_parenting_bi = ifelse(is.na(harsh_parenting), NA, 
                                        ifelse(harsh_parenting > 4, 1, 0)
            )) %>%
        # Binarising physical punishment
        mutate(
            CG_CPDISM00_bi = ifelse(is.na(CG_CPDISM00), NA,
                                    ifelse(CG_CPDISM00 %in% c("Sometimes (about once a month)", "Often (about once a week or more)", "Daily"), 1, 0))) %>% 
        # Derive binarised domestic violence scores
        mutate(rec_CG_domestic_violence = ifelse(CG_domestic_violence == 3 | is.na(CG_domestic_violence), NA,
                                                 ifelse(CG_domestic_violence == "No", 0, 1)),
               rec_PN_domestic_violence = ifelse(PN_domestic_violence == 3 | is.na(PN_domestic_violence), NA,
                                                 ifelse(PN_domestic_violence == "No", 0, 1))) %>% 
        # Binarising presence of child maltreatment
        mutate(
            child_maltreatment_bi = case_when(
                rowSums(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence), na.rm = FALSE) > 0 ~ 1, # present
                rowSums(is.na(select(., harsh_parenting_bi, CG_CPDISM00_bi, rec_CG_domestic_violence, rec_PN_domestic_violence))) > 0 ~ NA_real_, # NA
                TRUE ~ 0 # absent
            )) %>% 
        # Derive cumulative child maltreatment scores by summing each type of child maltreatment
        mutate(
            child_maltreatment = rowSums(select(.,
                                                harsh_parenting_bi, # harsh parenting
                                                CG_CPDISM00_bi, # physical punishment
                                                rec_CG_domestic_violence, # caregiver-reported domestic violence
                                                rec_PN_domestic_violence # partner-reported domestic violence
            ), na.rm = FALSE))

# Check the summary of transformed variables
View(trnsfrm_imputed_MCS_long)
summary(trnsfrm_imputed_MCS_long) # Check summary statistics of transformed variables
summary(subset(trnsfrm_imputed_MCS_long, .imp != 0)) # Check summary statistics of transformed variables excluding the original dataset

# ====================== End of MCS Data Preparation ====================== #

# ====================== MCS Data Analysis ====================== #

# ====== Logistic regression ======
library(nnet)

# Convert back from long data to mids ======
# Set Group 1 (i.e., `traj_Group` == 1) as the reference group
trnsfrm_imputed_MCS_long$traj_Group <- relevel(trnsfrm_imputed_MCS_long$traj_Group, ref = "1")

# Convert the transformed long data back to `mids` object
mids_new <- as.mids(trnsfrm_imputed_MCS_long)

# Check `mids_new` for summary and missingness
## Check summary of imputed datasets
summary(mice::complete(mids_new, action = "all")) # 40 datasets; 37 variables

## Check missingness of imputed datasets
colSums(is.na(mice::complete(mids_new, action = 2)))

# Unadjusted logistic regression ======
# Predictor: child maltreatment presence
CM_presence_multinom <- with(mids_new, 
                        multinom(traj_Group ~ child_maltreatment_bi))

summary(pool(CM_presence_multinom)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Predictor: child maltreatment cumulative score
CM_cumulative_multinom <- with(mids_new, 
                               multinom(traj_Group ~ child_maltreatment))

summary(pool(CM_cumulative_multinom)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Check if the results of all significance are correct ======
# Convergence: no massive coefficients (e.g., >10 or <-10) --> ok
CM_presence_multinom$analyses[[1]]

# Raw distributions: no cells tiny or empty --> ok
with(mids_new, table(traj_Group, child_maltreatment_bi))

# Scaling of test statistics (i.e., check effect sizes and CIs) --> ok (with reasonable effect sizes, between 0.5 and 5, and finite CIs)
## `conf.int = TRUE`: gives 95% confidence intervals
## `exponentiate = TRUE`: gives odds ratios instead of log-odds
summary(pool(CM_presence_multinom), conf.int = TRUE, exponentiate = TRUE)

# Adjusted logistic regression ======
# Predictor: child maltreatment presence, adjusted for caregiver and partner Kessler (K6) Scale scores and child sex
CM_presence_multinom_adj <- with(mids_new, 
                             multinom(traj_Group ~ child_maltreatment_bi + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_presence_multinom_adj)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom_adj), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Predictor: child maltreatment presence, adjusted for caregiver and partner Kessler (K6) Scale scores, child sex, and SES
CM_presence_multinom_adj_2 <- with(mids_new, 
                                 multinom(traj_Group ~ child_maltreatment_bi + CG_CM_sex + CG_Kessler + PN_Kessler + CG_income_quintile))

summary(pool(CM_presence_multinom_adj_2)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom_adj_2), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

## Check reference group and the comparison groups
rownames(coef(CM_presence_multinom_adj$analyses[[1]]))

# Predictor: child maltreatment cumulative score, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_cumulative_multinom_adj <- with(mids_new, 
                               multinom(traj_Group ~ child_maltreatment + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_cumulative_multinom_adj)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom_adj), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Predictor: child maltreatment cumulative score, adjusted for caregiver and partner Kessler (K6) Scale scores, child sex, and SES
CM_cumulative_multinom_adj_2 <- with(mids_new, 
                                   multinom(traj_Group ~ child_maltreatment + CG_CM_sex + CG_Kessler + PN_Kessler + CG_income_quintile))

summary(pool(CM_cumulative_multinom_adj_2)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom_adj_2), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Adjusted logistic regression with reference group as 2 (i.e., "Childhood victims") ======
# Predictor: child maltreatment presence, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_presence_multinom_adj_ref2 <- with(mids_new, 
                                 multinom(relevel(traj_Group, ref = "2") ~ child_maltreatment_bi + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_presence_multinom_adj_ref2)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom_adj_ref2), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

## Check reference group and the comparison groups
rownames(coef(CM_presence_multinom_adj_ref2$analyses[[1]]))

# Predictor: child maltreatment cumulative score, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_cumulative_multinom_adj_ref2 <- with(mids_new, 
                                   multinom(relevel(traj_Group, ref = "2") ~ child_maltreatment + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_cumulative_multinom_adj_ref2)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom_adj_ref2), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Adjusted logistic regression with reference group as 3 (i.e., "Mid-adolescence victims") ======
# Predictor: child maltreatment presence, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_presence_multinom_adj_ref3 <- with(mids_new, 
                                      multinom(relevel(traj_Group, ref = "3") ~ child_maltreatment_bi + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_presence_multinom_adj_ref3)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom_adj_ref3), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

## Check reference group and the comparison groups
rownames(coef(CM_presence_multinom_adj_ref3$analyses[[1]]))

# Predictor: child maltreatment cumulative score, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_cumulative_multinom_adj_ref3 <- with(mids_new, 
                                        multinom(relevel(traj_Group, ref = "3") ~ child_maltreatment + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_cumulative_multinom_adj_ref3)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom_adj_ref3), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# Adjusted logistic regression with reference group as 4 (i.e., "Victim-perpetrators") ======
# Predictor: child maltreatment presence, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_presence_multinom_adj_ref4 <- with(mids_new, 
                                      multinom(relevel(traj_Group, ref = "4") ~ child_maltreatment_bi + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_presence_multinom_adj_ref4)) # Summary of pooled logistic regression results

summary(pool(CM_presence_multinom_adj_ref4), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

## Check reference group and the comparison groups
rownames(coef(CM_presence_multinom_adj_ref4$analyses[[1]]))

# Predictor: child maltreatment cumulative score, adjusted for caregiver and partner Kessler (K6) Scale scores
CM_cumulative_multinom_adj_ref4 <- with(mids_new, 
                                        multinom(relevel(traj_Group, ref = "4") ~ child_maltreatment + CG_CM_sex + CG_Kessler + PN_Kessler))

summary(pool(CM_cumulative_multinom_adj_ref4)) # Summary of pooled logistic regression results

summary(pool(CM_cumulative_multinom_adj_ref4), conf.int = TRUE, exponentiate = TRUE) # Effect sizes and 95% CIs

# ====== Write regression results to CSV ======
write_xlsx(
    list(
        "CM presence model_ref1" = summary(pool(CM_presence_multinom_adj), conf.int = TRUE, exponentiate = TRUE), # Presence; ref = 1
        "CM presence model_ref2" = summary(pool(CM_presence_multinom_adj_ref2), conf.int = TRUE, exponentiate = TRUE), # Presence; ref = 2
        "CM presence model_ref3" = summary(pool(CM_presence_multinom_adj_ref3), conf.int = TRUE, exponentiate = TRUE), # Presence; ref = 3
        "CM presence model_ref4" = summary(pool(CM_presence_multinom_adj_ref4), conf.int = TRUE, exponentiate = TRUE), # Presence; ref = 4
        "CM cumulative model_ref1" = summary(pool(CM_cumulative_multinom_adj), conf.int = TRUE, exponentiate = TRUE), # Cumulative; ref = 1
        "CM cumulative model_ref2" = summary(pool(CM_cumulative_multinom_adj_ref2), conf.int = TRUE, exponentiate = TRUE), # Cumulative; ref = 2
        "CM cumulative model_ref3" = summary(pool(CM_cumulative_multinom_adj_ref3), conf.int = TRUE, exponentiate = TRUE), # Cumulative; ref = 3
        "CM cumulative model_ref4" = summary(pool(CM_cumulative_multinom_adj_ref4), conf.int = TRUE, exponentiate = TRUE) # Cumulative; ref = 4
),
    "/Users/chenping/Library/CloudStorage/OneDrive-Nexus365/Pin-DPhil/DPhil-Study-1/Result-Study-1/RS-combined/multinom_results_allrefs.xlsx"
)

# Controlling for SES
write_xlsx(
    list(
        "CM presence model_ref1" = summary(pool(CM_presence_multinom_adj_2), conf.int = TRUE, exponentiate = TRUE), # Presence; ref = 1
        "CM cumulative model_ref1" = summary(pool(CM_cumulative_multinom_adj_2), conf.int = TRUE, exponentiate = TRUE) # Cumulative; ref = 1
    ),
    "/Users/chenping/Library/CloudStorage/OneDrive-Nexus365/Pin-DPhil/DPhil-Study-1/Result-Study-1/RS-combined/SES_multinom_results_allrefs.xlsx"
)

# ====================== End of MCS Data Analysis ====================== #

# ====== Variable information ======

# Main variables ======

# Bullying perpetration (`Perpetration_s4`, `Perpetration_s5`, `Perpetration_s6`) - RQ1 - RQ3
## Scale: continuous - rescaled score (0-2)

# Bullying victimisation (`Victimisation_s4`, `Victimisation_s5`, `Victimisation`) - RQ1 - RQ3
## Scale: continuous - rescaled score (0-2)

# Child maltreatment presence (`child_maltreatment_bi`) - RQ2
## Scale: binary - 0 (absent), 1 (present)

# Child maltreatment cumulative score (`child_maltreatment`) - RQ3
## Scale: continuous - range: 0 - 4

# Covariates ======
# Child sex (`CG_CM_sex`)
## Scale: binary - 1 (male), 2 (female)

# Parental psychopathology / mental health (`CG_Kessler` & `PN_Kessler`)
## Scale: continuous - range: 6 - 30
