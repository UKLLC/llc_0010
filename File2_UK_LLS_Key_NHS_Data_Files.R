#This file was used to derive the NHS data files used in the analyss in the project. 

#NB This file has been produced by Richard Shaw @ Univeristy of Glasgow but it  is based on the R helper file
# Date Last Update: 03 May 2023

########################################################################################
# Title: UK LLC R value and variable labelling helper
#
# This uses the expss package to apply variable and value labels to datasets, this can
# be done on individual datasets or multiple datasets at once. Due to performance issues 
# it is not recommended to load and apply to all datasets at once (if you have a large
# number available as part of your project) as the loading will be slow
#
# ACTIONS: 1) UPDATE project number at ACTION1
# ACTIONS: 2) Decide which method (A,B,C) to use to label data:
#             A: one table at a time - AND UPDATE data_source and table variables
#             B: one data_source at a time - AND UPDATE data_source variable
#             C: all project data in one go - NO UPDATE required
#         
# Date: 04/03/2022 (version 1) 
#######################################################################################
#I think this was edited by me so may not work library(tidyverse)
#################################
# ACTION1: enter/update project number in form "LLC_0001"
#################################
proj_no = "LLC_0010"


####################################################################################################################
# set the dir to where this script is save and source the labelling function (assumes both scripts in same location)
####################################################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("r_label_functions.R")

##############
# packages-
# Note this starts with the original LLC code and I am now adding my own
#############
packages = c("tidyr", "dplyr", "RODBC", "expss", "stringr", "tidyr")
# run function to load and install packages
load_install_packages(packages)

library(readxl)
library(tidyverse)
library(feather)
library(lubridate)
################################################################################
# Environment setting commands 
################################################################################
options(scipen = 999)



################################################################################
# User defined functions. 




#########################################
# DB connection and project information
#########################################
# setup connection
conn <- odbcConnect(dsn="LLC_DB") 
# Get all table/viewnames avail in connection
allviews <- sqlTables(conn)
# just take ones attached to project schema
viewnames <- tolower(allviews[allviews$TABLE_SCHEM %in% proj_no,"TABLE_NAME"])
# and print to sense-check
print(viewnames) 

#RJS addition write file name into a text file. 
#Folder location for text file 
output_loc <- "S:/LLC_0010/outputs/Prelim/"
write(viewnames, paste0(output_loc , "file_Names_", Sys.Date(), ".txt"))




#This is necessary as many things such as ID and codes are imported as numbers and defaultin R is to present
# them as scientific notation. 
options(scipen = 999)

###################################################################################
#Using one table method to extract the data 
####################################################################################
# core_denominator_file1_20220623
# NOte this shows linkage permisions 


data_source <- "core"
table <- "denominator_file1_20220623"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

head(core_denominator_file1_20220623)

saveRDS(core_denominator_file1_20220623 , file = paste0("S:/LLC_0010/data/core/core_denominator_", Sys.Date() ,".rds"))



####################################################################
# core_nhsd_geo_indicator_v0004_20221028

data_source <- "core"
table <- "nhsd_geo_indicator_v0004_20221028"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

glimpse(core_nhsd_geo_indicator_v0004_20221028)

saveRDS(core_nhsd_geo_indicator_v0004_20221028 , file = paste0("S:/LLC_0010/data/core/core_geo_indicator_", Sys.Date() ,".rds"))


##########################################################################
# The older version of this has problems with the BCS70 and unliked peole and should note be used.
# So does version 3. So will need to use version 3. 


data_source <- "core"
table <- "nhsd_derived_indicator_v0005_20221217"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)


saveRDS(core_nhsd_derived_indicator_v0005_20221217 , file = paste0("S:/LLC_0010/data/core/core_indicator_", Sys.Date() ,".rds"))

core_nhsd_derived <- readRDS("S:/LLC_0010/data/core/core_indicator_2023-02-08.rds")


#################################################################################
# core_nhsd_presence_v0001_20220803
# This has individuals presence in each nshd data sets. 
data_source <- "core"
table <- "nhsd_presence_v0005_20221028"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

glimpse(core_nhsd_presence_v0005_20221028)


saveRDS(core_nhsd_presence_v0005_20221028 , file = paste0("S:/LLC_0010/data/core/core_presence_", Sys.Date() ,".rds"))


################################################################################
## nhsd_chess_v0001


data_source <- "nhsd"
table <- "chess_v0001"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

#Save with date issue to keep track of old-files
saveRDS(nhsd_chess_v0001 , file = paste0("S:/LLC_0010/data/nhsd/nhsd_chess_", Sys.Date() , ".rds"))

################################################################################
### nhsd_covidsgss_v0001
#This is pilar testing and one and two one of the more important files. Note that only positive tests
# Are included. 

data_source <- "nhsd"
table <- "covidsgss_v0001"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

saveRDS(nhsd_covidsgss_v0001 , file = paste0("S:/LLC_0010/data/nhsd/nhsd_covidsgss_", Sys.Date() , ".rds"))




#############################################################################
data_source <- "nhsd"
table <- "demographics_sub_20220716"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)
View(nhsd_demographics_sub_20220716)

### Save ####
saveRDS(nhsd_demographics_sub_20220716 , file = "S:/LLC_0010/data/nhsd/nhsd_demographcs.rds")


###############################################################################
###### nhsd_ielisa_v0001

data_source <- "nhsd"
table <- "ielisa_v0001"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
# assign name to dataset
assign(name_t1, data_t1)

saveRDS(nhsd_ielisa_v0001 , file = paste0("S:/LLC_0010/data/nhsd/nhsd_ielisa_" , Sys.Date() , ".rds"))



################################################################################
#### nhsd_npex_v0001

data_source <- "nhsd"
table <- "npex_v0001"
# run labelling function
data_t1 <- lab_func(proj_no,data_source,table)
# create df name
name_t1 <- paste0(data_source,"_",table)
assign(name_t1, data_t1)


#Using feather as it is noticably faster. 
write_feather(nhsd_npex_v0001 , path = paste0("S:/LLC_0010/data/nhsd/nhsd_npex_", Sys.Date() ,".feather"))



#Needed data datafiles 
covidsgss <- readRDS("S:/LLC_0010/data/nhsd/nhsd_covidsgss_2022-10-24.rds")
ielisa   <- readRDS("S:/LLC_0010/data/nhsd/nhsd_ielisa_2022-10-24.rds" )
npex <- read_feather(path = "S:/LLC_0010/data/nhsd/nhsd_npex_2022-10-24.feather") 
chess <- readRDS("S:/LLC_0010/data/nhsd/nhsd_chess_2022-10-24.rds")

#Need to add in generation scotland file 
ecoss <-readRDS(  "S:/LLC_0010/data/genscot/genscot_ecoss_2022-10-28.rds")

### Taking the specifc subsets
## Covidsgss 
#Note Covidsgss only has postive tests so somewhat simpler than the rest. 

covidsgss_FirstPosTest <- covidsgss |>
  select(c("llc_0010_stud_id", "specimen_date")) |>
  rename(test_date = specimen_date)|>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))

ielisa_FirstPosTest <- ielisa |>
  select(c("llc_0010_stud_id", "testresult"    ,   "teststartdate"  )) |>
  filter(testresult == "SCT:1321541000000108" ) |>
  rename(test_date = teststartdate) |>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))

npex_FirstPosTest <- npex |>
  select(c("llc_0010_stud_id", "testresult"    ,   "teststartdate"  )) |>
  filter(testresult == "SCT:1240581000000104" | testresult == "SCT:1322781000000102" ) |>
  rename(test_date = teststartdate) |>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))

chess_FirstPosTest <- chess |>
  select(c("llc_0010_stud_id", "labtestdate")) |>
  rename(test_date = labtestdate) |>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))

ecoss_FirstPosTest <- ecoss |>
  select(c("LLC_0010_stud_id", "covid"    ,   "dt"  )) |>
  filter(covid == 1)  |>
  rename(llc_0010_stud_id = LLC_0010_stud_id, 
         test_date = dt) |>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))

FirstPosTest <- rbind(covidsgss_FirstPosTest, ielisa_FirstPosTest ) |>
  rbind(ielisa_FirstPosTest) |>
  rbind(npex_FirstPosTest) |>
  rbind(chess_FirstPosTest) |>
  rbind(ecoss_FirstPosTest) |>
  group_by(llc_0010_stud_id) |>
  arrange(test_date, by_group = TRUE) |>
  summarise(test_date = min(test_date))


saveRDS(FirstPosTest , file = paste0("S:/LLC_0010/data/multiple/", "first_pos_test_" , Sys.Date(),".rds"))




