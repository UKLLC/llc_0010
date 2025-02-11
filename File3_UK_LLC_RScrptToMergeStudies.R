#This file is run to put together the cohort and NHS data within the UK LLC. 
# This file creates  the main spine of the data. Multiple files will need to be run in advance.
# Note this version was written on the 03May2023 
# Updated 11 Feb 2025 to correct typos. 
# NB I, Richard Shaw, have dyslexia and there may be a few typos I miss. 


library(haven) 
library(tidyverse)
library(readr)
library(expss)
library(feather)
library(lubridate)
library(stringr)

#### User created functions 

#This takes selected column names from a data frame and creates the frequencies and proportions for them 
frequencies_selected <- function(data_frame, column_names, decimal_places = 1) {
  select(data_frame, column_names)|>
    pivot_longer(column_names)  |>
    group_by(name) |>
    count( value) |>
    mutate(percent = round(n /sum(n)*100 , digits = decimal_places)) 
}

  

#### Importing data files 
# Core denominator data 
core_denominator <- readRDS("S:/LLC_0010/data/core/core_denominator_2022-11-04.rds")

### Contains ethnicity data
core_nhsd_derived <- readRDS("S:/LLC_0010/data/core/core_indicator_2023-02-08.rds")|>
drop_all_labels()

ethnic_valid <- core_nhsd_derived |> 
  select(llc_0010_stud_id, ethnic)


ethnic_valid$e_1 <-NA
ethnic_valid$e_2 <-NA

ethnic_valid$e_1[ethnic_valid$ethnic ==  "A"] <- 1
ethnic_valid$e_2[ethnic_valid$ethnic ==  "A"] <- 1


ethnic_valid$e_1[ethnic_valid$ethnic ==  "B"] <- 1
ethnic_valid$e_2[ethnic_valid$ethnic ==  "B"] <- 1

ethnic_valid$e_1[ethnic_valid$ethnic ==  "C"] <- 1
ethnic_valid$e_2[ethnic_valid$ethnic ==  "C"] <- 1

ethnic_valid$e_1[ethnic_valid$ethnic ==  "D"] <- 4
ethnic_valid$e_2[ethnic_valid$ethnic ==  "D"] <- 2

ethnic_valid$e_1[ethnic_valid$ethnic ==  "E"] <- 4
ethnic_valid$e_2[ethnic_valid$ethnic ==  "E"] <- 2

ethnic_valid$e_1[ethnic_valid$ethnic ==  "F"] <- 4
ethnic_valid$e_2[ethnic_valid$ethnic ==  "F"] <- 2

ethnic_valid$e_1[ethnic_valid$ethnic ==  "G"] <- 4
ethnic_valid$e_2[ethnic_valid$ethnic ==  "G"] <- 2

ethnic_valid$e_1[ethnic_valid$ethnic ==  "H"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "H"] <- 3

ethnic_valid$e_1[ethnic_valid$ethnic ==  "H"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "H"] <- 3

ethnic_valid$e_1[ethnic_valid$ethnic ==  "J"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "J"] <- 4

ethnic_valid$e_1[ethnic_valid$ethnic ==  "K"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "K"] <- 5

ethnic_valid$e_1[ethnic_valid$ethnic ==  "L"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "L"] <- 8

ethnic_valid$e_1[ethnic_valid$ethnic ==  "M"] <- 3
ethnic_valid$e_2[ethnic_valid$ethnic ==  "M"] <- 6

ethnic_valid$e_1[ethnic_valid$ethnic ==  "N"] <- 3
ethnic_valid$e_2[ethnic_valid$ethnic ==  "N"] <- 7

ethnic_valid$e_1[ethnic_valid$ethnic ==  "P"] <- 3
ethnic_valid$e_2[ethnic_valid$ethnic ==  "P"] <- 8

ethnic_valid$e_1[ethnic_valid$ethnic ==  "R"] <- 2
ethnic_valid$e_2[ethnic_valid$ethnic ==  "R"] <- 8

ethnic_valid$e_1[ethnic_valid$ethnic ==  "S"] <- 5
ethnic_valid$e_2[ethnic_valid$ethnic ==  "S"] <- 8

ethnic.df <- ethnic_valid |>
  select(llc_0010_stud_id, e_1, e_2)

glimpse(ethnic_valid)

# NHS Covid data files 
first_test <- readRDS("S:/LLC_0010/data/multiple/first_pos_test_2022-10-31.rds")
first_covid_hosp <- readRDS("S:/LLC_0010/data/multiple/first_hospital_covid_record_2022-11-04.rds")
first_covid_gp <- readRDS("S:/LLC_0010/data/multiple/first_gp_covid_record_2022-11-04.rds")

#Will probably need a vaccination date but for most people it may be after the outcome wave. 



#Cohort data currently available
elsa <- read_dta("S:/LLC_0010/data/flow_chart/ELSA_flowchart_sample_26Apr23.dta")
usoc <- read_dta("S:/LLC_0010/data/flow_chart/USoc_flowchart_sample_26Apr23.dta")
bcs70 <- read_dta("S:/LLC_0010/data/flow_chart/BCS70_flowchart_sample_26Apr23.dta")
ncds58 <- read_dta("S:/LLC_0010/data/flow_chart/NCDS_flowchart_sample_23Apr23.dta")
nextsteps <- read_dta("S:/LLC_0010/data/flow_chart/nextsteps_flowchart_sample_23Apr23.dta")

# at a later stage factor order gets muddled up not using this although may have some advantages in some context so making a note of the code  usoc_labeled <- as_factor(usoc, levels = "labels")

#Generation Scotland is being converted as labels and factors are problematic. 

#cls cohorts 
bcs70_convert <- bcs70 |>
  mutate(survey_date  = ymd(
    paste0("2021-",
           str_pad(as.character(cw3_enddatem), width = 2 , side = 'left', pad = '0'),
           "-",
           str_pad(as.character(cw3_enddated), width = 2 , side = 'left', pad = '0'))))

ncds58_convert <- ncds58 |>
  mutate(survey_date  = ymd(
    paste0("2021-",
           str_pad(as.character(cw3_enddatem), width = 2 , side = 'left', pad = '0'),
           "-",
           str_pad(as.character(cw3_enddated), width = 2 , side = 'left', pad = '0'))))

nextsteps_convert <- nextsteps |>
  mutate(survey_date  = ymd(
    paste0("2021-",
           str_pad(as.character(cw3_enddatem), width = 2 , side = 'left', pad = '0'),
           "-",
           str_pad(as.character(cw3_enddated), width = 2 , side = 'left', pad = '0'))))

cls_combined <- bcs70_convert |>
  rbind(ncds58_convert)|>
  rbind(nextsteps_convert) |>
  select(!c(cw3_enddated, cw3_enddatem )) |>
  drop_all_labels()

core_vars <- names(cls_combined)
class(core_vars)
names(nextsteps_convert)


# ELSA
elsa_convert <- elsa |>
  mutate(survey_date  = ymd(
    paste0("2020-",
           str_pad(as.character(cintdatm_w2), width = 2 , side = 'left', pad = '0'),
           "-",
           str_pad(as.character(cintdatd_w2), width = 2 , side = 'left', pad = '0')))) |>
  select(c(core_vars))|>
  drop_all_labels()
view(elsa_convert)

#usoc will need_to_versions depending on March on January
usoc_jan_convert <- usoc|>
  mutate(survey_date = ymd_hms(survey_alt),
         age = age_alt ,
         employment_status = employment_status_alt) |>
  select(c(core_vars))|>
    drop_all_labels()





#Combining the data sets. 

combined_data <- bind_rows(cls_combined, elsa_convert) |>
  bind_rows(usoc_jan_convert) |>
  mutate(survey_date = as.Date(survey_date)) 



#Alteration as the employment status currently includes furlough
combined_data$employment_status[combined_data$employment_status == 1] <- 0
combined_data$employment_status[combined_data$employment_status == 2] <- 1



### add in the additional ethnicity variables, English NHS data and then code English NHS data.  
combined_data <- left_join(combined_data, ethnic.df,  by = "llc_0010_stud_id" ) |>
  left_join(first_test, by = "llc_0010_stud_id" ) |>
  left_join(first_covid_hosp, by = "llc_0010_stud_id" ) |>
  left_join(first_covid_gp, by = "llc_0010_stud_id" ) |>
  mutate(
    Test_positive = if_else(survey_date > test_date , 1, 0, missing = 0),
    Covid_hospital = if_else(survey_date > acute_covid_hosp , 1, 0, missing = 0),
    COVID19_GP = if_else(survey_date > COVID19_Date , 1, 0, missing = 0),
  )




###############################################################################
# Now attaching the spine
### note previous version had done this before adding NHS data but because including Generation Scotland comes later. 
core_denominator$in_spine <- 1


Spine <- full_join(core_denominator, combined_data, by = "llc_0010_stud_id")



##Saving a file for use in another package 
write_dta(Spine,"S:/LLC_0010/data/Flow_Chart/FlowChart_24Apr2023.dta" )




