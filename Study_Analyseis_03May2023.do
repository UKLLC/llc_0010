use "S:\LLC_0010\data\flow_chart\FlowChart_24Apr2023.dta" , clear

*Creating necessary variables 

*Encode cohort so the first stage is easier
encode cohort, gen(cohort_num)

*           1 BCS70
*           2 ELSA
*           3 NCDS
*           4 NextSteps (See LSTYPE)
*           5 USoc (See UKHLS)
*           6 ALSPAC
*           7 BIB
*           8 EPIC
*           9 EXCEED
*          10 GENSCOT
*          11 GLAD
*          12 LSTYPE
*          13 MCS_CM
*          14 NICOLA
*          15 NIHRBIO
*          16 NSHD
*          17 SABRE
*          18 TRACKCOVID
*          19 TWINSUK
*          20 UKHLS




*Recoding education and employment status as missing. 
tab nssec7, miss
recode nssec7 . = 9
tab education, miss
recode education . = 4
rename employment_status employm

*Note to address missing ethnicity data I am replacing ethnicity  and ethnic_bin with e_1
replace ethnicity = e_1 if ethnicity == . 
tab ethnicity e_1, miss
recode ethnicity_bin . = 0 if e_1 == 1
recode ethnicity_bin . = 1 if e_1 >=2 & e_1 <= 5



*Note commands to just focus data on the cohorts for which I have data. 
gen in_analyses = 1 
foreach var in Test_positive age sex hh_comp ethnicity shielding sr_health nssec7 education cohort_num  {
	replace in_analyses = . if `var' == . 
}




*Creating moderator variabels 
egen age_temp = cut(age), at(25,50,67)
recode age_temp (25  = 0 "Under 50") (50  = 1 "50"), gen(age_bin)

*** Stratified analyses 
*Note changes used combine ethnic groups. 
*drop those unclassiable 
recode nssec7 9 = . , gen(class_alt)
recode education 4 = . , gen(educat_alt)


*Ethnicity groups too small to stratify 


*NS-SEC
recode nssec7 (1/2 = 0 "Higher") (3/7 = 1 "Intermediate & lower") , gen(nssec2)



*** Age variable for descriptive 
egen age_cat = cut(age), at(25, 30, 40,  50, 60, 67)


*Selecting sample with tabulations to enable calculation of flow chart
tab sex, miss
keep if cohort_num == 2 | cohort_num == 4 | cohort_num == 9 |  cohort_num == 11 | cohort_num == 18 
tab sex, miss
keep if study_selection == 1
tab sex, miss
keep if age_selection == 1
tab sex, miss
keep if employment_selection == 1
tab sex, miss
keep if country == 1 
tab sex, miss
keep if nhs_e_linkage_permission == "1"
tab sex, miss
keep if in_analyses == 1
tab sex, miss





*** Descriptive statistics 
tab1 econ_act employm covid_sr Test_positive age_cat sex hh_comp ethnicity shielding sr_health nssec7 education cohort_num if in_analyses==1, miss

*** Cross tabe of self-reported versus test_positive
tab covid_sr Test_positive if in_analyses==1

*NHS records

*** Economic activity unadjusted 
logistic econ_act i.Test_positive  if in_analyses==1

**** Economic activity full model 
logistic econ_act i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1


**** employment status unadjusted 
logistic employm i.Test_positive  if in_analyses==1

**** employment status full model 
logistic employm i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1


* Self-reported measures 

*** Economic activity unadjusted 
logistic econ_act i.covid_sr  if in_analyses==1

**** Economic activity full model 
logistic econ_act i.covid_sr c.age##c.age i.sex i.hh_comp i.ethnicity i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1


**** employment status unadjusted 
logistic employm i.covid_sr  if in_analyses==1

**** employment status full model 
logistic employm i.covid_sr c.age##c.age i.sex i.hh_comp i.ethnicity i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1








**** Age stratified 
logistic econ_act i.Test_positive  i.sex  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & age_bin == 0


logistic econ_act i.Test_positive  i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & age_bin == 1


**** Sex stratified 
logistic econ_act i.Test_positive c.age##c.age  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & sex == 0


logistic econ_act i.Test_positive c.age##c.age  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & sex == 1




**** class stratified 
logistic econ_act i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.educat_alt i.cohort_num if in_analyses==1 & nssec2 == 0


logistic econ_act i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.educat_alt i.cohort_num if in_analyses==1 & nssec2 == 1


*** Self-rated health 

logistic econ_act i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding  i.nssec7 i.educat_alt i.cohort_num if in_analyses==1 & sr_health < 4

logistic econ_act i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding  i.nssec7 i.educat_alt i.cohort_num if in_analyses==1 & sr_health > 3



**** Age stratified 
logistic employm i.Test_positive  i.sex  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & age_bin == 0


logistic employm i.Test_positive  i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & age_bin == 1


**** Sex stratified 
logistic employm i.Test_positive c.age##c.age  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & sex == 0


logistic employm i.Test_positive c.age##c.age  i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.class_alt i.educat_alt i.cohort_num if in_analyses==1 & sex == 1




**** Xlass stratified 
logistic employm i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.educat_alt i.cohort_num if in_analyses==1 & nssec2 == 0


logistic employm i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding i.sr_health i.educat_alt i.cohort_num if in_analyses==1 & nssec2 == 1



**** Self-rated health
logistic employm i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding  i.nssec7 i.educat_alt i.cohort_num if in_analyses==1 & sr_health < 4

logistic employm i.Test_positive c.age##c.age i.sex i.hh_comp i.ethnicity_bin i.shielding  i.nssec7 i.educat_alt i.cohort_num if in_analyses==1 & sr_health > 3
