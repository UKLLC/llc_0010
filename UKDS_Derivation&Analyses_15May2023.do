*** Note this Stata file has been complied by Richard Shaw (richard.shaw@glasgow.ac.uk) Other contributors to work include  
* Jingmin Zhu (ELSA) & Rebecca Rhead (NCDS, BCS70 Next steps) and undoubtedly been informed by the work of many others. 

clear all
set maxvar 30000

*NOte reasembling files in their original form. 
global source "D:\Data\ARQ7P4\Original"
global destination "D:\Data\ARQ7P4\Derived"


*******************************************************************************8
*** ELSA *** Covid wave 1 
* 

use  "$source\elsa_covid_w1_eulv2.dta"

*Coding all variables 
rename _all, lower

foreach var of varlist _all { 
						rename `var' `var'_w1
						}

gen inCVWave1  = 1 
rename idauniq_w1 idauniq

save "$destination\tempCV_w1.dta" , replace


* Covid wave 2 

use "$source\elsa_covid_w2_eulv2.dta", clear
rename _all, lower
foreach var of varlist _all { 
						rename `var' `var'_w2
						}
generate inCVWave2 = 1
rename idauniq_w2 idauniq
save "$destination\tempCV_w2.dta", replace

*** Assemblng ELSA wave 9 data which has been broken into many components. 

*Import the first section
use "$source\wave_9_elsa_data_eul_v1.dta" , clear 
*Adding the other wave 9 variables needed 
merge 1:1 idauniq using "$source\wave_9_financial_derived_variables.dta", keepusing(eqtotinc_bu_s nettotnhw_bu_s tnhwq5_bu_s)
drop _merge
merge 1:1 idauniq using "$source\wave_9_ifs_derived_variables.dta", keepusing(smoker* edqual difjobm)
drop _merge

gen inWave9 = 1 

save "$destination\temp_w9.dta", replace

*Importing NS-sec from wave 8, 6 and 5 wave 7 does not appear to have the variable 


use "$source\wave_8_elsa_data_eul_v2.dta" , clear 

keep idauniq w8nssec8
save "$destination\temp_w8.dta", replace



use "$source\wave_6_elsa_data_v2.dta" , clear 

keep idauniq w6nssec8
save "$destination\temp_w6.dta", replace



use "$source\wave_5_elsa_data_v4.dta" , clear 

keep idauniq w5nssec8
save "$destination\temp_w5.dta", replace


global source "D:\Data\ARQ7P4\Original"
global destination "D:\Data\ARQ7P4\Derived"
********************************************************************************
*Assemblng files 
use "$destination\tempCV_w1", clear
merge 1:1 idauniq using "$destination\tempCV_w2"
drop _merge
merge 1:1 idauniq using "$destination\temp_w9"
*drop if _merge==2
drop _merge
egen ELSA_inwave = concat(inWave9 inCVWave1 inCVWave2)
tab ELSA_inwave
merge  1:1 idauniq using "$destination\temp_w8"
drop _merge
merge  1:1 idauniq using "$destination\temp_w6"
drop _merge
merge  1:1 idauniq using "$destination\temp_w5"
drop _merge
*******************************************************************************8


gen study_selection = 1  if corepartner_w1==1 | corepartner_w2==1
replace study_selection = . if inWave9 != 1 | inCVWave1 != 1 | inCVWave2 != 1

gen age_selection = 1 
replace age_selection = . if age_arch_w2 >= 67
replace age_selection = . if age_arch_w1 >= 67 & age_arch_w2==.
gen employment_selection = 1  if cvpred_w1 == 2 | cvpred_w1== 3





******************************************************************************
***** outcome variable
*** employment status
* Following discussions with Daniel Kopasker (Economist) 
* The self-employed but not currently working have been classified as economically active
* but unemployed. 
gen employ = cvpstd_w2
recode employ (1=2)(2=3)(3=1)(4 =3)(5/8=2)(-9/-8=.)
label define employ 1"1:furloygh" 2"2:unemployed" 3"3:employed/self-employed",replace
label values employ employ


*** Economic activity 
label define econ_act 0 "Active" 1 "In active"
gen econ_act = . 
recode econ_act . = 0 if inlist(cvpstd_w2, 2, 3, 4, 5)
recode econ_act . = 1 if inlist(cvpstd_w2, 1, 7 , 8)
recode econ_act . = 0 if cvpstd_w2 == 6 & cvsearch_w2 == 1
recode econ_act . = 1 if cvpstd_w2 == 6 & cvsearch_w2 == 2

* financial difficulty
gen subfin=cvpostfn_w2
label define subfin 1"1:much worse" 2"2:little worse" 3"3:same" 4"4:little better" 5"5:much better",replace
label val subfin subfin
gen subfin_bi=(subfin==3 | subfin==4 | subfin==5)



*** changing in working time

* number of hours worked
recode cvpsth_w2 cvpreh_w1 (-9/-1=.)
gen workhour_w2 = cvpsth_w2

* pre_pandemic working hours
gen workhour_pre=cvpreh_w1

* work hour change
gen workhour_low = workhour_pre*0.9
gen workhour_high = workhour_pre*1.1
gen workhour_change=1 if workhour_w2 > workhour_high
replace workhour_change=2 if workhour_w2 < workhour_low
replace workhour_change=3 if workhour_low <= workhour_w2 <=workhour_high
replace workhour_change=4 if employ==1
replace workhour_change=5 if employ==2 | cvpsth_w2==5
label define change 1"1:increase" 2"2:decrease" 3"3:same" 4"4:non-working furlough" 5"5:non-working non-furlough",replace
label values workhour_change change

*worktime_change
gen worktime_change  = 1 if (workhour_w2 > workhour_high ) & workhour_w2 !=. 
recode worktime_change . = 2 if (workhour_w2 < workhour_low) & workhour_low !=. 
recode worktime_change .  = 3 if (workhour_w2 >= workhour_low) &  (workhour_w2  <= workhour_high) & workhour_w2 ! =. & workhour_w2 !=. 
replace worktime_change =4 if employ==1
replace worktime_change =5 if employ==2 | cvpsth_w2==5 
label values worktime_change change

*** exposure
* self-reported covid
** definition 1: positive covid test/ hospitalisation/ one of three core symptoms 
** cw1
egen srcovid_w1= rowtotal (cvsymp01_w1 cvsymp02_w1 cvsymp05_w1)
tab srcovid_w1, missing 

gen srcovid1_w1=(srcovid_w1>=1) if srcovid_w1 !=.
tab srcovid1_w1

tab cvtestb_w1
tab  cvhosp_w1, nolab
recode cvhosp_w1 (-9/-8=.)
    
gen covcase_w1 =(cvtestb_w1 == 1 | cvhosp_w1 == 1 | srcovid1_w1 == 1) if cvtestb_w1 !=. | cvhosp_w1 !=. | srcovid1_w1 !=.
tab covcase_w1 , missing

*Note all those who had postive tests had been hospitalized.
*Note using the coreset of symptoms as the question for ELSA slightly different and 
* may prompt people to report fatigue irrespective of whether they thought it was due to covid. 
gen covid_confirmed_w1 = . 
recode covid_confirmed_w1 . = 2 if cvtestb_w1 == 1 
recode covid_confirmed_w1 . = 0 if cvtestb_w1 == 2
recode covid_confirmed_w1 . = 1 if srcovid_w1 > 0 
recode covid_confirmed_w1 . = 0 if srcovid_w1 == 0
label define covid_confirmed_w1  0 "No" 1 "Suspected" 2 "Confirmed"
label values covid_confirmed_w1 covid_confirmed_w1


** cw2
gen covcase_w2=0
replace covcase_w2=1 if (cvtestb_w2 == 1 | cvhosp_w2 == 1)
replace covcase_w2=1  if cvtestwhy_final001_w2==1 & (cvtestb_w2 == 3 | cvtestb_w2 == 4 )
tab covcase_w2

*Test because they had covid symptoms 
tab cvtestwhy_final001_w2

*Note no hospital stays in analytic sample 
gen covid_confirmed_w2 = .
*Postive tests
recode covid_confirmed_w2 . = 2 if cvtestb_w2 ==1
*Negative tests
recode covid_confirmed_w2  . = 0 if cvtestb_w2 ==2
*Those who had a test due to symptoms
recode  covid_confirmed_w2 . = 1 if cvtestwhy_final001_w2 ==1
* remaineder not suspected 
recode  covid_confirmed_w2 . = 0 
* People whose test results are not being reported. 
replace covid_confirmed_w2 = .  if cvtestb_w2 == -9
label define covid_confirmed_w2  0 "No" 1 "Suspected" 2 "Confirmed"
label values covid_confirmed_w2 covid_confirmed_w2

*coding the remaining cases 
gen covid_sr  = . 
recode covid_sr . = 2 if covid_confirmed_w1 ==2 |  covid_confirmed_w2 == 2
recode covid_sr . = 1 if covid_confirmed_w1 ==1 |  covid_confirmed_w2 == 1
recode covid_sr . = 0 
label define covid_sr  0 "No" 1 "Suspected" 2 "Confirmed"
label values covid_sr covid_sr


*What was the result of the covid test 
 tab cvtestb_w2
* -9 prefer not to answer
*-1 not applicale 
* 1 positive
* 2 negative
* 3 inconclusive
* 4 waiting 


 
* timing of covid infection
gen covtime=0 if covcase_w1==0 & covcase_w2==0
replace covtime=1 if covcase_w1==1 & covcase_w2==0
replace covtime=2 if covcase_w1==0 & covcase_w2==1
replace covtime=3 if covcase_w1==1 & covcase_w2==1


*** MAIN COVARIATES
* Age
gen age_gr_w2=age_arch_w2
recode age_gr_w2 (52/54=1) (55/66=2)
label define age 1"1:45-54" 2"2:55-66",replace
label values age_gr_w2 age


* Ethnicity 
gen ethnicity =. 

gen ethnicity_temp = ethnicity_arch_w1
replace ethnicity_temp = ethnicity_arch_w2 if  ethnicity_temp == . 
replace ethnicity_temp = fqethnm if  ethnicity_temp == . 

recode ethnicity_temp (-9/-1 = . ) (1 = 0  "White") (2 = 1 "Non White")  , gen(ethnicity_bin)
label variable ethnicity_bin "Ethnicity Binary"



gen ethnicity1 = . 


* Education
label define education 0 "NVQ 4 or 5" 1 "NVQ 3" 2 "NVQ 2 & 1" 3 "None"
recode  w9edqual (-9/-1=.) (1/2 = 0 ) (3 = 1) (4/5 = 2) (6/7 = 3), gen(education)
label variable education "Education NVQ eqvs"
label values education education

recode education (0 = 0 "NVQ 4 or 5") (1/3 = 1 "NVQ3 or less") , gen(education_bin)

*Soc 2000



/*Commenting out Soc2000 as data is not available. 
* Soc2000 - for this variable, we need to gather info from previous waves (issues with w7!)
		* Note1 -- I created in ELSAw1 a soc2000 var, and for w7 I derived it from nssec
		* Note2 -- we also gather number of room for 'overcrowding' 
		* In ELSA w6, for the loop to work, you first need to rename HoRoom as horoom
  cd "/Users/jm/OneDrive - University College London/ELSA_data/loop"
		foreach h in 1 2 3 4 5 6 7 8 {
		merge 1:1 idauniq using "wave_`h'_elsa_data", keepusing(w`h'soc2000 horoom)
		rename horoom horoom_w`h'
		drop if _merge==2
		drop _merge
		}
		recode w*soc2000* (-9/-1=.)
		generate occupation_1d=floor(w9soc2000r/10)
		foreach h in 8 6 5 4 3 2 1 7 {
		replace occupation_1d=floor(w`h'soc2000/10) if occupation_1d==.
		}
		gen occupation_2d=w9soc2000r_w9
		foreach h in 8 6 5 4 3 2 1 7 {
		replace occupation_2d=w`h'soc2000 if occupation_2d==.
		}	
	* industry - SIC2003-2 digits -for this variable, we need to gather info from previous waves
		foreach h in 4 5 6 7 8 {
		merge 1:1 idauniq using "wave_`h'_elsa_data", keepusing(w`h'sic2003)
		drop if _merge==2
		drop _merge
		}
        recode w9sic2003r_w9 w8sic2003 w7sic2003 w6sic2003 w5sic2003 w4sic2003 (-3/-1=.)(-8=.)
		rename w9sic2003r_w9 w9sic2003
		gen sic2003_2d=.
		foreach h in 9 8 7 6 5 4 {
		replace sic2003_2d=w`h'sic2003 if sic2003_2d==.
		}
		merge m:m sic2003_2d using /Users/jasmine/Documents/UCL/NCS_7HS_P4_employment/sic2003_2007.dta
		drop if _merge==2
		drop _merge
*/



* household composition
*Note that cvnump_w`k' indicate number in houshold 
* Target variable will have the following groups Alone, partner, partner & children, lone parent, other person 
* Note to be consistent with CLS and USoc  this has been amended to include grandchilren as offspring, not just children

egen offspring_w1=anymatch(demographics_*_cvrelp_w1), values(2 3 4)
egen offspring_w2=anymatch(demographics_*_cvrelp_w2), values(2 3 4)
egen partinhh_w1=anymatch(demographics_*_cvrelp_w1), values(1)
egen partinhh_w2=anymatch(demographics_*_cvrelp_w2), values(1)

recode partinhh_w1 -1=0
forvalues k=1/2 {
generate cvhhcomp_w`k'=.
replace cvhhcomp_w`k'=1 if cvnump_w`k'==1
replace cvhhcomp_w`k'=2 if cvnump_w`k'==2 & partinhh_w`k'==1
replace cvhhcomp_w`k'=3 if cvnump_w`k'>=2 & partinhh_w`k'==1 & offspring_w`k'==1
replace cvhhcomp_w`k'=4 if cvnump_w`k'>=2 & partinhh_w`k'==0 & offspring_w`k'==1
replace cvhhcomp_w`k'=5 if cvnump_w`k'==-1 | (cvnump_w`k'>=2 & partinhh_w`k'==0 & offspring_w`k'==0) | (cvnump_w`k'>=3 & partinhh_w`k'==1 & offspring_w`k'==0)
}

forvalues k=1/2 {
recode cvhhcomp_w`k' (2=1)(3=2)(4=3)(1=4)(5=4)
label define cvhhcomp 1"1:only partner" 2"2:partner+kids" 3"3:single parent" 4"4:alone or other", replace
label values cvhhcomp_w* cvhhcomp
}

*New varibale  hh_comp with categoies as used for the CLS studies. 
label define hh_comp 0 "alone" 1 "Partner" 2 "Partner & children" 3 "lone parent" 4 "other person" 
gen hh_comp =. 
replace hh_comp = 0 if cvnump_w1 == 1
replace hh_comp = 1 if partinhh_w1 == 1
recode hh_comp 1 = 2 if offspring_w1 == 1
replace hh_comp = 3 if offspring_w1 == 1 & partinhh_w1 == 0
recode hh_comp . = 4  if cvnump_w1 >1 & cvnump_w1 !=. 
label values hh_comp hh_comp


* Self-rated health 
tab hehelf
clonevar sr_health = hehelf 
recode sr_health -1 = . 

*lim_ill
gen lim_ill = . 
replace lim_ill = 0 if heill == 2 | helim == 2
replace lim_ill = 1 if helim == 1 
label define lim_ill 0 "No" 1 "Yes"
label values lim_ill lim_ill


* keyworker
recode cvkey_w* (-9/-1=.)
gen keyworker = cvkey_w1
replace keyworker =  cvkey_w2  if keyworker == . 
recode keyworker 2 = 0 . = 0 
label define keyworker 0 "No" 1 "Yes"
label values keyworker keyworker 





* pre-pandemic mental health - ces-d in wave9
recode pscedd pscedf (2=1) (1=0) (-9/-1=.)
recode psceda pscedb pscedc pscede pscedg pscedh (2=0) (-9/-1=.)
egen depression_w9=rowtotal(psced*), missing
generate depressed_w9=(depression_w9>=4)
replace depressed_w9=. if depression_w9==.




* lli
*generate lli=(helim_w9==1)
*label define lli 0"no long-standing limiting illness" 1"lli", replace
*label values lli lli


		
* shielding at first pandemic wave
recode cvvuln_w1 (-9/-1=.)
gen shielding=(cvvuln_w1==1) if cvvuln_w1!=.	
	
		


*** Five health conditions: pre-pandemic wave combined with first pandemic wave
**Note other pre_covid waves are added the suffix _w9 will need to be addded to some vars or other ways of selecting variables found

* cancer
gen cancer=hedbsca
recode cancer (-8=.)(-1=0)(2=0)
replace cancer=1 if hedibca ==1
replace cancer=1 if cvhecond09_w1==1



* heart disease and high blood pressure
recode hedas95 hedasar hedashf hedashm heyra hediahf hediahm hedim85 hedim86 (-9/-1=.)
gen heartd=0
foreach k in hedas95 hedasar hedashf hedashm heyra hediahf hediahm hedim85 hedim86 {
replace heartd=1 if `k'==1
}
replace heartd=1 if cvhecond02_w1==1
replace heartd=1 if cvhecond03_w1==1

//high blood pressure part//
replace heartd=1 if hedasbp==1
replace heartd=1 if hediabp==1
replace heartd=1 if cvhecond01_w1==1

* obesity at first pandemic wave
recode wtimperial_wtstones_w1 wtimperial_wtpounds_w1 wtkilos_w1 dvheight_w2 (-9/-1=.)
gen cvweight_w1=6.35*wtimperial_wtstones_w1
replace cvweight_w1=cvweight_w1+0.45*wtimperial_wtpounds_w1 if wtimperial_wtpounds_w1!=.
gen cvestwt_w1=wtkilos_w1
replace cvestwt_w1=cvweight_w1 if cvestwt_w1==. & cvweight_w1!=.
gen cvbmi_w1 = round(cvestwt_w1/[(dvheight_w2/100)^2], .1)
gen obesity=(cvbmi_w1>=30) if cvbmi_w1!=.


* respiratory disorders
recode hedblu hediblu hedbsas hedibas (-9/-1=.)
gen respiratory=0
foreach k in hedblu hediblu hedbsas hedibas {
replace respiratory=1 if `k'==1
}
replace respiratory=1 if cvhecond06_w1==1
replace respiratory=1 if cvhecond07_w1==1



********************************************************************************
*****Variables for analysis 

*** Study mannagement 
* Person id
*rename llc_0010_stud_id LLC_0010_stud_id
* Cohort id 
gen cohort_id = "ELSA"



*** Outcomes
*Employment status:  employ  -> employment_status

tab employ
label define employment_status 0 "Employed" 1 "Furloughed" 2 "Not in employment"
recode employ 3 = 0 , gen(employment_status)
label values employment_status employment_status



*Financial difficulties: subfin subfin_bi  -> (finance_change finance_change_bi)
clonevar finance_change = subfin
recode subfin_bi 0 = 1 1 = 0, gen(finance_change_bin)
label define finance_change_bi   0 "Same - better" 1 "Much worse - little worse" 

*Change in working hours:  worktime_change -> worktime_change 
tab worktime_change 
label define worktime_change 0 "Stayed the same"  1 "Increased" 2 "Decreased" 3 "Furloughed" 4 "Not in employed"
recode worktime_change 3 = 0 4 = 3 5 =4 
label values worktime_change worktime_change


***Exposures 
*Any covid symptoms: covtime -> covid_ever
recode covtime 0 = 0 1/3 = 1 , gen(covid_ever)
label define covid_ever 0 "No" 1 "Yes"
label values covid_ever covid_ever

*Timing of Covid: covtime    -> covid_timing 
clonevar covid_timing = covtime
label define covid_timing   0 "No Covid" 1 "First Wave" 2 "Second Wave" 3 "Both Waves" 
label values covid_timing covid_timing

*LongCovid: long_covid_out        -> long_covid 
*Going to leave now as there are problems
gen long_covid = . 



***Control variables 
*Country variable does not appear to ba available so using 
gen country =  rgn_arch_w2
recode country -1 = . 
replace country = rgn_arch_w1 if rgn_arch_w2 == .
recode country -1 =.  1/9 = 1 10 = 2 11 = 3 
label define country 1 "England" 2 "Scotland" 3 "Wales" 4 "Northern Ireland" 
label values country country 



*Age: ch_age ch_agecat -? age age_group
clonevar age = age_arch_w2
recode age_arch_w2 25/39 = 0 40/54 = 1 55/66 = 2, gen(age_group)
label define age_group 0 "25 to 39" 1 "40 to 54" 2 "55 +"
label values age_group age_group 


*sex: Sex_w2   >- sex
gen temp_sex = sex_w2
replace temp_sex = sex_w1 if temp_sex == . 
gen sex  = temp_sex - 1
label define sex 0 "Male" 1 "Female"
label values sex sex 




*Household composition: ????? -> hh_comp
*Will need recoding when looking at other cohorts 
* Note using wave 1 as it won't be consequence of the outcome. 
*clonevar hh_comp = cvhhcomp_w1


* NS-SEC "Seven category" to be consistent across the studies. 
* Using seven categories as the ELSA 8 categories are different from the other cohorts. 
recode w5nssec8 (-9/-1 99 = 9 "Other Unclassifiable")  (1 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 =8 "longterm unemployed" ) , gen(nssec7w5)
label variable nssec7w5 "NS-SEC 7 Categories w6"


recode w6nssec8 (-9/-1 99 = 9 "Other Unclassifiable")  (1 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 =8 "longterm unemployed" ) , gen(nssec7w6)
label variable nssec7w6 "NS-SEC 7 Categories w6"

recode w8nssec8 (-9/-1 99 = 9 "Other Unclassifiable")  (1 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 =8 "longterm unemployed" ) , gen(nssec7w8)
label variable nssec7w8 "NS-SEC 7 Categories w8"


recode w9nssec8 (-9/-1 99 = 9 "Other Unclassifiable")  (1 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 =8 "longterm unemployed" ) , gen(nssec7w9)
label variable nssec7w9 "NS-SEC 7 Categories w9"

* NS-SEC "Five category" to be consistent 
clonevar nssec7 = nssec7w9
replace nssec7 = nssec7w8 if nssec7 == 9 | nssec7 == .
replace nssec7 = nssec7w6 if nssec7 == 9 | nssec7 == .
replace nssec7 = nssec7w5 if nssec7 == 9 | nssec7 == .




*SOC 2010/2007 pre-pandemic: soc_2d  soc_designation    ->  soc_2d  soc_designation  
gen soc_2d = . 
gen soc_designation = . 

*SIC 2007 : lob_sic07_sec                  -> sic_1d
gen  sic_1d = . 

*Key worker status

tab keyworker



*Mental health: depressed_w9 -> mental_health 
clonevar mental_health = depressed_w9



*Shielding:  ?????? -> shielding
tab shielding

*clonevar shielding = shield 

*Cancer: ??????? -> cancer
tab  cancer  

*Heart disease
gen  heart  = heartd

*Depression 
gen depression = . 

* Obesity 
gen obese =  obesity

* respiratory 
tab  respiratory 

tab1 employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity_bin ethnicity1 country hh_comp nssec7 sr_health keyworker mental_health shielding

*Creating a String id variable for anlaysis 
tostring idauniq, gen(id_temp)
gen id_string = "E_" + id_temp
*Note removoing interview date variables as not eeded 
keep id_string cohort_id study_selection age_selection  employment_selection  ///
employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity_bin ethnicity1 country hh_comp nssec7 sr_health keyworker mental_health shielding



save  "$destination\ELSA_analytic_sample_15May23.dta" , replace 

************************************************************************************************************************************************
*** USoc Code 

clear all

*Recoding all covid waves with single file in same loop. 
*NB this is all waves apart from e and f, and there are two versions of f. 
*NB there are current two versions of wave cf I am using the single file version that drops a couple of varibales at the end. 
* The variables are not needed in this analysis and the two file version appear to drop variale lables. 

foreach x in a b c d e f g h {
use "$source\c`x'_indresp_w.dta" , clear
rename (racel_dv psu strata) (c`x'_racel_dv c`x'_psu c`x'_strata)
gen in_c`x' = 1
save "$destination\c`x'_indresp_w_racel.dta", replace
}


global source "D:\Data\ARQ7P4\Original"
global destination "D:\Data\ARQ7P4\Derived"

*This is wave g 
use "$source\g_indresp.dta" , clear
save "$destination\g_indresp.dta", replace

*This is wave h 
use "$source\h_indresp.dta" , clear
save "$destination\h_indresp.dta", replace


*This is wave i 
use "$source\i_indresp.dta" , clear
save "$destination\i_indresp.dta", replace



*This is wave j 
use "$source\j_indresp.dta" , clear
keep if j_intdaty_dv <= 2019 | (j_intdatm_dv <= 2 & j_intdaty_dv == 2020)
save "$destination\j_indresp.dta", replace



*Adding wave k
use "$source\k_indresp.dta" , clear
drop if k_intdaty_dv == -9
keep if k_intdaty_dv <= 2019 | (k_intdatm_dv <= 2 & k_intdaty_dv == 2020)
save "$destination\k_indresp.dta", replace






*****Importing data 
*Data files to be used at this point are the web survey Pandemic waves c`x'_indresp_w ca - ch (April 2020 to March 2021) and prepandemic waves `x'_indresp g - j (2015 to 2020)


***Setting File locations 
**
clear all
set maxvar 30000
set more off


**File Path

*** Merging data files 
clear 

**Pre-pandemic waves 
*Wave 10
use "$destination\j_indresp.dta"
gen in_w10 = 1
*add wave 11
merge 1:1 pidp using "$destination\k_indresp.dta", gen(w11mrg)
*Add wave 9 
merge 1:1 pidp using "$destination\i_indresp.dta", gen(w9mrg)
*Add wave 8 
merge 1:1 pidp using "$destination\h_indresp.dta", gen(w8mrg)
*Add wave 7
merge 1:1 pidp using "$destination\g_indresp.dta", gen(w7mrg)
**Covid data 
*add covid data

foreach x in a b c d e f g h {
merge 1:1 pidp using "$destination\c`x'_indresp_w_racel.dta", gen(c`x'_mrg)
tab c`x'_mrg
}



********************************************************************************
label define no_yes 0 "No" 1 "Yes"
***** Deriving outcomes 
*** Employment status 


*** Employment status (latest C19 Sweep) 
*This is derived from ch_sempderived and ch_newfurlough
*ch_sempderived is a derived varible. Universe is ALL, but around 1.5% are  inapplicable. Not clear why
*ch_newfurloughis  is asked of those who are employed or both employed and self-employed from ch_sempderived  
* Note if self-employed hours = 0 classifed as as not working. Not doing this for both emplyed and self-employed due to being employed. 

gen ch_employment_stat = . 
replace ch_employment_stat = 2 if ch_newfurlough == 1 // Recodes those reporting as furloughed
replace ch_employment_stat = 1 if ch_newfurlough == 2 // recodes those not furloughed as employed 
recode  ch_employment_stat . = 3 if  ch_sempderived == 2 & ch_hours == 0 // Selfemployed and 0 hours
recode  ch_employment_stat . = 1 if  ch_sempderived == 2  & ch_hours != 0   & ch_hours !=. // Selfemployed are coded as in employment. 
recode  ch_employment_stat . = 3 if  ch_sempderived == 4 
label define ch_employment_stat 1 "In employment"  2 "Furloughed"  3 "Not Employed" 
label values ch_employment_stat ch_employment_stat

* Note that using 0 -  working and 2 working as to be consistent with other cohorts. 
* Not coding furlough for this paper as that not the relevant data. 
gen cg_employment_stat = . 
recode  cg_employment_stat . = 0 if cg_sempderived == 1 | cg_sempderived == 3
recode  cg_employment_stat . = 0 if cg_sempderived == 2  & cg_hours > 0  & cg_hours !=. 
recode  cg_employment_stat . = 2 if cg_sempderived == 2  & cg_hours == 0  
recode cg_employment_stat . = 2 if cg_sempderived == 4
label variable cg_employment_stat "Working or not January"
label define  cg_employment_stat 0 "Working " 2 "Not working"
label values cg_employment_stat cg_employment_stat



*** economic activity status form cg 
gen econ_act = . 
recode econ_act . = 0 if cg_sempderived ==1 | cg_sempderive == 2 | cg_sempderived == 3
recode econ_act . = 0 if cg_sempderive ==4 & cg_julk4wk == 1
recode econ_act . = 1 if cg_sempderive ==4 & cg_julk4wk == 2
label define econ_act 0 "Active" 1 "Inactive"
label values econ_act econ_act




*******************************************************************************
**** Exposures

gen usoc_main = ch_surveystart 
gen usoc_alt = cg_surveystart

label define c19_confirmed 0 "No" 1 "Suspected" 2 "Confirmed"

*** Covid confirmed 
foreach wave in a b c d e f {
	gen c`wave'_c19_confirmed = . 
	recode c`wave'_c19_confirmed . = 2 if c`wave'_testresult == 1
	recode c`wave'_c19_confirmed . = 0 if c`wave'_testresult == 2
	recode c`wave'_c19_confirmed . = 1 if c`wave'_hadsymp == 1
	recode c`wave'_c19_confirmed . = 0 if c`wave'_hadsymp == 2
	label values c`wave'_c19_confirmed c19_confirmed
}


gen cg_c19_confirmed = . 
recode cg_c19_confirmed . = 2 if cg_testresult_test1 == 1 | cg_testresult_test2 == 1 | cg_testresult_test3 == 1
recode cg_c19_confirmed . = 0 if cg_testresult_test1 == 2 | cg_testresult_test2 == 2 | cg_testresult_test3 ==2 
recode cg_c19_confirmed . = 1 if cg_hadsymp == 1 
recode cg_c19_confirmed . = 0 if cg_hadsymp == 2
label values cg_c19_confirmed c19_confirmed 

gen ch_c19_confirmed = . 
recode ch_c19_confirmed . = 2 if ch_testpos == 1
recode ch_c19_confirmed . = 0 if ch_testpos == 2
recode ch_c19_confirmed . = 1 if ch_hadsymp == 1
recode ch_c19_confirmed . = 0 if ch_hadsymp == 2
label variable ch_c19_confirmed "ch C19 SR confirmed"
label values ch_c19_confirmed c19_confirmed

gen covid_sr = . 
foreach wave in a b c d e f g {
	recode covid_sr . = 2 if c`wave'_c19_confirmed == 2
}

foreach wave in a b c d e f g {
	recode covid_sr . = 1 if c`wave'_c19_confirmed == 1	
}

recode covid_sr . = 0 if cg_c19_confirmed == 0 
replace covid_sr = . if cg_c19_confirmed ==.

*********************************************************************************
*****Control Variables 
*** UK Country of residence 
gen pan_region = ca_gor_dv 
recode pan_region -9 = .
foreach wave in  b c d e f {
replace pan_region = c`wave'_gor_dv if pan_region == . 
recode pan_region -9 = .
}
recode pan_region  1/9 = 1  10 = 3  11 = 2 12 = 4 , gen(country)
label define country 1 "England" 2 "Scotland" 3 "Wales" 4 "Northern Ireland" 
label values country country 

*Country for ch_wave
recode ch_gor_dv 1/9 = 1 10 = 3 11 = 2 12 = 4, gen(country_main)
label values country_main pan_country

*Country for cg_wave
recode cg_gor_dv 1/9 = 1 10 = 3 11 = 2 12 = 4, gen(country_alt)
label values country_alt pan_country



***Age  - Pandemic outcome  
recode cg_age 25/39 = 0 40/54 = 1 55/66 = 2, gen(cg_agecat)
replace cg_agecat = . if ch_age < 25 | cg_age >66
label variable cg_agecat "Age March 2021 in categories"
label define cg_agecat 0 "25 to 39" 1 "40 to 54" 2 "55 +"
label values cg_agecat cg_agecat 

gen age_main = ch_age
gen age_alt = cg_age

*** age first covid wave with corrections 

*Note that there are some inconsitencies with ca_age which outside the TRE
* I have previously addressed with ca_pidpcorrected which is not available. 
* Note solution is I am coding as missing in ca_age and then replacing with next available
* or year of birth. 


label define age_entry 1 "16-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "65-74"  7 "75+" 

gen lob_birthy = k_birthy
recode lob_birthy (-9/-1 = .)
replace lob_birthy = j_birthy if lob_birthy == . 
recode lob_birthy (-9/-1 = .)
replace lob_birthy = i_birthy if lob_birthy == . 
recode lob_birthy (-9/-1 = .)
replace lob_birthy = h_birthy if lob_birthy == . 
recode lob_birthy (-9/-1 = .)
replace lob_birthy = g_birthy if lob_birthy == . 
recode lob_birthy (-9/-1 = .)
gen dob_age = 2020 - lob_birthy

*Note that there are some inconsitencies with ca_age which 
* I have previously addressed with ca_pidpcorrected which is not available. 
* Note that I am recoding ca_ages 
gen ca_age_temp = ca_age
foreach wave in b c d e f g h {
replace ca_age_temp = . if c`wave'_age < ca_age & ca_age != .
replace ca_age_temp  =. if ca_age + 1 < c`wave'_age & c`wave'_age !=.
}


gen  age_at_start = ca_age_temp
foreach wave in  b c d e f g h {
replace age_at_start = c`wave'_age if age_at_start == . 
}
replace age_at_start = dob_age if age_at_start ==. 


*** Sex - Pandemic 
 gen temp_sex = cg_sex 
 foreach wave in h f e d c b a {
 replace temp_sex = c`wave'_sex if temp_sex == 3
 }
gen sex = temp_sex - 1
label define sex 0 "Male" 1 "Female" 
label values sex sex 
drop temp_sex

*** Household composition - pandemic 
* Note that wave ca is missing relationshp data so will be classified on the basis 
* of first valid data person has from remaining waves. Designed to be consistent with CLS

label define household_comp 0 "Alone" 1 "Partner" 2 "Partner & children" 3 "Lone parent" 4 "Other person" 

foreach wave in cb cc cd ce cf cg ch  {
	egen `wave'_offspring = anymatch(`wave'_relation*), values(3 6)
	egen `wave'_partner = anymatch(`wave'_relation*), values(1 2)
	gen `wave'_household_comp = . 
	recode  `wave'_household_comp  .  = 2 if `wave'_partner == 1 & `wave'_offspring == 1 & `wave'_couple != . 
	recode  `wave'_household_comp  .  = 1 if `wave'_partner == 1 & `wave'_offspring == 0 & `wave'_couple != . 
	recode  `wave'_household_comp  .  = 3 if `wave'_partner == 0 & `wave'_offspring == 1 & `wave'_couple != . 
	recode  `wave'_household_comp  .  = 4 if `wave'_partner == 0 &  `wave'_hhnum >=2 & `wave'_hhnum <= 20 & `wave'_couple != . 
	recode  `wave'_household_comp  .  = 0 if `wave'_partner == 0  & `wave'_couple != . 
	label values  `wave'_household_comp household_comp
}

gen hh_comp = cb_household_comp
foreach wave in cc cd ce cf cg ch {
	replace hh_comp = `wave'_household_comp if hh_comp == . 
}
label values hh_comp household_comp
tab cb_household_comp, miss

	tab cb_couple,miss
	
*** Self-rated health 
label define sr_health 1 "Excellent" 2 "Very Good" 3 "Good" 4 "Fair" 5 "Poor"
label define sr_health_bin 0 "Excellent-Good" 1 "Fair-Poor"
foreach wave in g h i j k cf cg ch {
	recode `wave'_scsf1 (-9/0 = . ) , gen(`wave'_sr_health)
	label values `wave'_sr_health sr_health
	recode `wave'_sr_health (1/3 = 0) (4/5 = 1), gen(`wave'_sr_health_bin)
	label values `wave'_sr_health_bin sr_health_bin
}

gen sr_health = . 
foreach wave in k j i h g {
replace sr_health = `wave'_sr_health if sr_health == . 
} 
label values sr_health sr_health

*** Ethnicity - pandemic



recode cg_racel_dv (-9/0=.) (1/4=1) (5/8=4) (9/13=2) (14/16=3) (17 97=5), gen(ethnicity)
label variable ethnicity "Ethnicity broad " 
label define ethnicity 1 "White" 2 "Asian" 3 "Black" 4 "Mixed" 5 "Other" 
label values ethnicity  ethnicity

recode ethnicity (1 = 0 "White") (2/5 = 1 "Non White") , gen(ethnicity_bin)
label variable ethnicity_bin "Ethnicity Binary"



label define ethnicity1 1 "White" 2 "Mixed" 3 "Indian" 4 "Pakistani" 5 "Bangladeshi" 6 "Black Caribean" 7 "Black African" 8 "Other" 
recode ch_racel_dv(-9/-1 = . ) (1/4 = 1) (5/8 = 2 ) (9 = 3) (10 = 4) (11 = 5) (12/13 = 8) (14 = 6) (15 = 7) (16/97 = 8)  , gen(ethnicity1)
label values ethnicity1 ethnicity1 






**** NSSEC 7
* Note inorder to be consistent with wave9 for ELSa this will being
* Note that I am focusing on ELSA firt. 


*These are a derived variable showing last jub NSSEC
foreach x in k j i h g {
gen `x'_nssec8 = `x'_jbnssec8_dv 
replace `x'_nssec8 = `x'_jlnssec8_dv if `x'_jbnssec8_dv == -8 & `x'_jlnssec8_dv > 0 & `x'_jlnssec8_dv !=. 
}
gen nssec8 = . 
foreach x in k j i h g {
replace nssec8 = `x'_nssec8 if nssec8 == . 
recode nssec8 -9/-1 = . 
}
recode nssec8 . = 9

label define nssec8 1 "Large employers & higher mangement" 2 "Higher professional" 3 "Lower management & professional" 4 "Intermediate" 5 "Small employers & own acccount" 6 "Lower supervisory & technical" 7 "Semi-routine" 8 "routine" 9 "Unclassifiable"
label values nssec8 nssec8
*Note some of the unclassifiable might be coded as longterm unemployed at this point to distingush those in educaiton or training. 

recode nssec8 (1/2 = 1 "Higher management or professional") (3 = 2 "Lower management and professional" ) (4 = 3 "Intermediate") (5 = 4 "Small Employer") (6 = 5 "Lower supervisory") (7 = 6 "Semi-routine") (8 = 7 "Routine") (999 = 8 "longterm unemployed" ) ( 9 = 9 "Other Unclassifiable")  , gen(nssec7)
label variable nssec7 "NS-SEC 7 Categories"


recode nssec7  (1/2 = 1 "Management & professional ") (3 = 2 "Intermediate") (4 = 3 "Small Employer") (5 = 4 "Lower supervisory & Technical") (6/7 = 5 "Semi-routine & Routine")  (8 = 6 "longterm unemployed" ) ( 9 = 9 "Other Unclassifiable")  , gen(nssec5)
label variable nssec5 "NS-SEC 5 Categories"




*** SOC - pre-pandemic
*NB not sure this is necessarily the most efficient, and have corrected some of Mikes code. 
*Namely d2 needs both `y' and `z' and d3 needs both `y' `z' `k' 
*Can probably just divide the three digit version by ten for the 2 digit version then 

*SOC codes
foreach job in soc00 soc10 {
gen lob_`job'_d3=.
}

foreach x in k j i h g {
	foreach job in jbsoc00 jlsoc00 jbsoc10 jlsoc10{
	recode `x'_`job'_cc (-9/-1=.), gen(`x'_`job')
	}
	foreach code in soc00 soc10{
	replace lob_`code'_d3=`x'_jb`code' if lob_`code'==.
	replace lob_`code'_d3=`x'_jl`code' if lob_`code'==.
	}
}
gen lob_soc00_d2 = floor(lob_soc00_d3/10) 
gen lob_soc00_d1 = floor(lob_soc00_d2/10) 


gen lob_soc10_d2 = floor(lob_soc10/10) 
gen lob_soc10_d1 = floor(lob_soc10_d2/10) 

*** soc_designation
label define soc_designation 0 "SOC2000" 1 "SOC2010" 2 "Missing" 

gen soc_designation = 1 if lob_soc10_d1 ! =. 
recode soc_designation . = 0 if lob_soc00_d1  !=. 
recode soc_designation . = 2 
label values soc_designation soc_designation


*** soc_1d 
gen soc_1d = lob_soc10_d1
replace soc_1d = lob_soc00_d1 if soc_1d == . 


*** soc_2d
gen soc_2d = lob_soc10_d2
replace soc_2d = lob_soc00_d2 if soc_2d == .


*** soc_3d
gen soc_3d = lob_soc10_d3
replace soc_3d = lob_soc00_d3 if soc_3d == .








*** SIC - Prepandemic 

label define sic2007_sec ///
1 "A AGRICULTURE, FORESTRY AND FISHING 1-3" /// 
2 "B MINING AND QUARRYING 5-9" /// 
3 "C MANUFACTURING 10 - 33" ///
4 "D ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY 35" /// 35
5 "E WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES 36 - 39" ///
6 "F CONSTRUCTION 41 - 43" ///
7 "G WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES 45 - 47" ///
8 "H TRANSPORTATION AND STORAGE 49 - 53" ///
9 "I ACCOMMODATION AND FOOD SERVICE ACTIVITIES 55 - 56" ///
10 "J INFORMATION AND COMMUNICATION 58 - 63" ///
11 "K FINANCIAL AND INSURANCE ACTIVITIES 64 - 66" ///
12 "L REAL ESTATE ACTIVITIES 68" ///
13 "M PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES 69 - 75" ///
14 "N ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES 77 - 82" ///
15 "O PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY 84" ///
16 "P EDUCATION 85" ///
17 "Q HUMAN HEALTH AND SOCIAL WORK ACTIVITIES 86 - 88" ///
18 "R ARTS, ENTERTAINMENT AND RECREATION 90 - 93" ///
19 "S OTHER SERVICE ACTIVITIES 94 - 96" ///
20 "T ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS; UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE 97 - 98" ///
21 "U ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES 99" ///
22 "Missing" 																

gen lob_sic07 = . 
foreach wave in k j h i g {
replace lob_sic07 = `wave'_jbsic07_cc if lob_sic07 == . 
recode lob_sic07 -9/-1 = . 
replace lob_sic07 = `wave'_jlsic07_cc if lob_sic07 == . 
recode lob_sic07 -9/-1 = . 
}
label values lob_sic07 j_jbsic07_cc

recode lob_sic07 (1/3 = 1) (5/9 = 2) (10/33 = 3) (35 = 4) (36/39 = 5) (41/43 = 6) (45/47 = 7) (49/53 = 8) (55/56 = 9) (58/63 = 10) (64/66 = 11) ///
(68 = 12) (69/75 = 13) (77/82 = 14) (84 = 15) (85 = 16) (86/88 = 17) (90/93 = 18) (94/96 = 19) (97/98 = 20) (99 = 21) (. = 22) , gen(lob_sic07_sec)
label values lob_sic07_sec sic2007_sec


*** Education - pre-pandemic status 


foreach x in g h i j k {
	recode  `x'_qfhigh_dv (-9/-1 = . ) (1/6 = 0 "NVQ 4 or 5") (7/12 = 1 "NVQ 3") (13/16 = 2 "NVQ 1 or 2") (96 = 3 "None"), gen(`x'_educat)
}


gen education = k_educat
replace education = j_educat if education ==. 
replace education = i_educat if education ==. 
replace education = h_educat if education ==. 
replace education = g_educat if education ==. 
label define education 0 "NVQ 4 or 5" 1 "NVQ 3" 2 "NVQ 2 & 1" 3 "None"
label values education education 

recode education (0 = 0 "NVQ 4 or 5") (1/3 = 1 "NVQ3 or less") , gen(education_bin)






*** Keyworker status - first valid response to pandemic UP to September 2020 everybody had a chance to join.  
* I am going with the later wave version first as it was better defined and more likely to be consistent with other studies. 
* Then fill in first wave where blank. 
* Then assume those who still don't have a valid response aren't keyworkers. 
gen keyworker = .
foreach wave in cb cc cd ce  {
recode keyworker  . = 1 if `wave'_keyworksector >=1 & `wave'_keyworksector <= 8
recode keyworker  . = 0 if `wave'_keyworksector == 9
}
recode keyworker . = 1 if ca_keyworker == 1
recode keyworker . = 0 if ca_keyworker == 2
recode keyworker . = 0 
label values keyworker no_yes


*Mental health - Prepandemic + pandemic waves) 

*Derive GHQ-12 caseness
foreach x in g h i j k ca cb cc cd ce cf cg ch{
clonevar `x'_ghq12=`x'_scghq2_dv
replace `x'_ghq12=. if `x'_ghq12<0
clonevar `x'_ghq36=`x'_scghq1_dv
replace `x'_ghq36=. if `x'_ghq36<0
gen `x'_ghqcase=.
replace `x'_ghqcase=0 if `x'_ghq12>=0 & `x'_ghq12<4
replace `x'_ghqcase=1 if `x'_ghq12>=4 & `x'_ghq12<13
label var `x'_ghqcase "GHQ Caseness (4+)"
label values `x'_ghqcase yesno 
label var `x'_ghq12 "GHQ-12 (12 point scoring)"
label var `x'_ghq36 "GHQ-12 (36 point scoring)"
}



gen ghqcase = k_ghqcase 
replace ghqcase = j_ghqcase if ghqcase == . 
replace ghqcase = i_ghqcase if ghqcase == . 
replace ghqcase = h_ghqcase if ghqcase == . 
replace ghqcase = g_ghqcase if ghqcase == . 
label variable ghqcase "GHQ caseness most recent prepandemic wave" 
label define ghqcase 0 "No" 1 "Yes" 
label values ghqcase  ghqcase 


*** Health Conditions - Pandemic 



************************************************
*Coding the Shield variables 

gen shield = . 
label values shield no_yes
gen cancer = . 
label values cancer no_yes
gen diabetes = . 
label values diabetes no_yes
gen hbp = . 
label values hbp no_yes
gen respiratory =. 
label values respiratory  no_yes
gen cvd = .
label values cvd  no_yes
gen obesity = . 
label values obesity  no_yes
gen depression = . 
label values depression  no_yes

foreach wave in a b c d e {

*Shielding 
recode c`wave'_nhsshield -9/-1 = -9 2 = 0 , gen(c`wave'_shield)
label values c`wave'_shield NoYes
replace shield = c`wave'_shield if shield == . 

*Cancer
gen c`wave'_cancer_f = . 
recode c`wave'_cancer_f . = 0 if c`wave'_hcond_cv13 == 0 
recode c`wave'_cancer_f . = 1 if c`wave'_hcond_cv13 == 1 
label values c`wave'_cancer_f NoYes
replace cancer = c`wave'_cancer_f if cancer == . 



*Diabetes 
gen c`wave'_diabetes_f = . 
recode c`wave'_diabetes_f . = 0 if c`wave'_hcond_cv14 == 0 
recode c`wave'_diabetes_f . = 1 if c`wave'_hcond_cv14 == 1 
label values c`wave'_diabetes_f NoYes
replace diabetes = c`wave'_diabetes_f if diabetes == . 


*High Blood pressure
gen c`wave'_highblood_f = . 
recode c`wave'_highblood_f . = 0 if c`wave'_hcond_cv16 == 0 
recode c`wave'_highblood_f . = 1 if c`wave'_hcond_cv16 == 1 
label values c`wave'_highblood_f NoYes
replace hbp = c`wave'_highblood_f if hbp == . 

*Respiratory problems
gen c`wave'_resp_f = . 
recode c`wave'_resp_f . = 1 if c`wave'_hcond_cv1 == 1 | c`wave'_hcond_cv8 == 1 | c`wave'_hcond_cv11 == 1 | c`wave'_hcond_cv21 == 1
recode c`wave'_resp_f . = 0 if  c`wave'_hcond_cv1 == 0 | c`wave'_hcond_cv8 == 0 | c`wave'_hcond_cv11 == 0 | c`wave'_hcond_cv21 == 0
label values c`wave'_resp_f NoYes
replace respiratory = c`wave'_resp_f if respiratory == . 


*CVD 
gen c`wave'_cvd_f = . 
recode c`wave'_cvd_f . = 1 if c`wave'_hcond_cv3 == 1 | c`wave'_hcond_cv4 == 1 | c`wave'_hcond_cv5 == 1 | c`wave'_hcond_cv6 == 1 | c`wave'_hcond_cv7 == 1
recode c`wave'_cvd_f . = 0 if  c`wave'_hcond_cv3 == 0 | c`wave'_hcond_cv4 == 0 | c`wave'_hcond_cv5 == 0 | c`wave'_hcond_cv6 == 0 | c`wave'_hcond_cv7 == 0
label values c`wave'_cvd_f NoYes
replace cvd = c`wave'_cvd_f if cvd == . 

*Alternative heart diseasev vairable. 
gen c`wave'_heartdis_f = . 
recode c`wave'_heartdis_f . = 1 if c`wave'_hcond_cv3 == 1 | c`wave'_hcond_cv4 == 1 | c`wave'_hcond_cv5 == 1 | c`wave'_hcond_cv6 == 1 
recode c`wave'_heartdis_f . = 0 if  c`wave'_hcond_cv3 == 0 | c`wave'_hcond_cv4 == 0 | c`wave'_hcond_cv5 == 0 | c`wave'_hcond_cv6 == 0 
label values c`wave'_heartdis_f NoYes

*Obesity
gen c`wave'_obesity_f = . 
recode c`wave'_obesity_f . = 0 if c`wave'_hcond_cv27 == 0 
recode c`wave'_obesity_f . = 1 if c`wave'_hcond_cv27 == 1 
label values c`wave'_obesity_f NoYes
replace obesity = c`wave'_obesity_f if obesity == . 


*Depression 
gen c`wave'_depression_f = . 
recode c`wave'_depression_f . = 0 if c`wave'_hcond_cv22 == 0 
recode c`wave'_depression_f . = 1 if c`wave'_hcond_cv22 == 1 
label values c`wave'_depression_f NoYes
replace depression = c`wave'_depression_f if depression == .
}


*Deriving pre pandemic employment measure for selection. 


**** had pre-pandemic employment 
gen pre_pandemic_emp = ca_blwork
recode pre_pandemic_emp -9/-1 = . 1/3 = 1 4 = 0 
foreach letter in b c d e {
recode pre_pandemic_emp . = 1 if c`letter'_blwork > 0  &  c`letter'_blwork < 4
recode pre_pandemic_emp . = 0 if c`letter'_blwork == 4
}


*Selection


gen study_selection = in_cg
gen age_selection = 1 if age_at_start >= 25 & cg_age < 67
gen employment_selection = 1 if pre_pandemic_emp == 1 


********************************************************************************
*****Variables for analysis 
*Converting ID 
*rename   llc_0010_stud_id LLC_0010_stud_id
gen cohort_id = "USoc"

*This is to use to select correct date for healthcare data
gen survey_date =   ch_surveystart
gen survey_main = ch_surveystart
gen survey_alt = cg_surveystart

*** Outcomes
*Employment status:  ch_employment_stat -> employment_status
*gen employment_status_main = ch_employment_stat - 1
*label define employment_status_main 0 "Employed" 1 "Furloughed" 2 "Not in employment"
*label values employment_status_main employment_status

clonevar employment_status = cg_employment_stat
**** 
tab econ_act 


*Age
gen age = age_alt

*sex: sex   >- sex
*Leave sex as is 
tab sex

*Education 
tab education 
tab education_bin

*Ethnicity: ethnicity1  ethnicity2   -> ethnicity ethnicity_bin 
tab ethnicity 
tab ethnicity_bin
tab ethnicity1

***Control variables 
*Country of residence: pan_country -> country
tab country
tab country_alt
tab country_main


*Household composition: household_comp -> hh_comp
tab hh_comp 


*NSSEC7
tab nssec7

*Self_rated health 
tab sr_health 

*Key worker status: keyworker -> keyworker
tab keyworker

*Shielding:  shield -> shielding
clonevar shielding = shield 
recode shielding -9 = . 
label values shielding no_yes

* mental_health
clonevar mental_health = ghqcase


*do not tab these vars as they are dates survey_date survey_main survey_alt
tab1 cohort_id    employment_status econ_act covid_sr age_main age 
tab1 sex education education_bin ethnicity  ethnicity1 country   hh_comp nssec7 sr_health keyworker shielding  mental_health
*New Idstring variable
tostring pidp, gen(id_temp)
gen id_string = "U_" + id_temp

keep id_string cohort_id  study_selection  age_selection employment_selection   employment_status  econ_act covid_sr age_main age sex education education_bin ethnicity ethnicity_bin ethnicity1 country   hh_comp nssec7 sr_health keyworker shielding  mental_health

tab age_selection employment_selection if study_selection == 1 & country == 1




save "$destination\USoc_analytic_sample_05May23.dta", replace


***********************************************************************************************************************************************
*** BCS70 Code 


clear all



*First reasemble main bcs10 file and select necessary variables 
use  "$source\bcs_age46_main.dta", clear
rename _all, lower

keep bcsid bd10achq1 bd10anvq1 bd10bmi bd10bmic bd10cns8 bd10cnsscc bd10cnssec bd10cntry bd10ecact bd10emwb bd10enfa bd10genh bd10hnvq bd10mbmi bd10mbmic bd10nvq1 bd10wemwb bd10mal bd10malg  

save  "$destination\core_bcs10_vars.dta" , replace 

use  "$source\bcs2000.dta", clear
keep bcsid ethnic
save  "$destination\bcs6_ethnic.dta" , replace 



*Note core demographic file is quite small so add serperately 
* use  "$source\BCS70_basic_demographic_v0001_20211101.dta", clear


use  "$source\covid-19_wave1_survey_cls.dta", clear
drop if BCSID == ""
save  "$destination\temp1.dta" , replace 
use  "$source\covid-19_wave2_survey_cls.dta", clear
drop if BCSID == ""
save  "$destination\temp2.dta" , replace 
use  "$source\covid-19_wave3_survey_cls.dta", clear
drop if BCSID == ""
save  "$destination\temp3.dta" , replace 

use  "$destination\temp3.dta", clear
merge 1:1 BCSID using "$destination\temp2.dta", gen(_merge2)
merge 1:1 BCSID using "$destination\temp1.dta", gen(_merge1)
merge 1:1 BCSID using "$source\bcs70_response_1970-2016.dta", gen(_demog)
rename _all, lower

merge 1:1 bcsid using "$destination\core_bcs10_vars.dta", gen(_core)
merge 1:1 bcsid using "$destination\bcs6_ethnic.dta", gen(_ethnic)


* RESTRICTIONS 


gen study_selection = 1 if cw3_outcome !=  . 
gen age_selection  = 1 
gen employment_selection = 1 if inlist(cw1_econactivityb, 1, 2)


* Cohort management variables 

*Note these to be copied through to R to derive them. 
desc cw3_enddated cw3_enddatem // These are deate of survey completion and will need to add the year 2021 to produce the final variable 
gen cohort_id = "BCS70"




*OUTCOME MEASURES

*====================================================================================================================================
* employment status
* -----------------
* Note RJS I have amemnded this as aprentices are employees and getting paid a wage and holidya pay. 
recode cw3_econactivityd (1 3 4 6 = 0 "employed / self-employed") ///
                         (2 = 1 "furloughed") (5 7/13 = 2 "Not Employed") ///
                         (-99/-1=.), gen(employment_status)
						 
codebook employment_status						 


recode cw3_econactivityd (1/4 6/8 = 0 "Active") ///
                         (5 9/13 = 1 "Inactive") ///
                         (-99/-1=.), gen(econ_act)

codebook econ_act
codebook econ_act


* finance_change
* --------------
recode cw3_financialmand (-99 / -1 =.) 
rename cw3_financialmand finance_change
codebook finance_change

* finance_change_bin
* ------------------
recode finance_change (3/5 = 0 "same/better") (1/2 = 1 "worse"), gen (finance_change_bin)
codebook finance_change_bin


* worktime_change
* ---------------
gen pre_hours=cw1_wrkhoursb if cw1_wrkhoursb>0
gen post_hours=cw3_wrkhoursd if cw3_wrkhoursd>0

gen ch_hours_prop = post_hours / pre_hours
tab ch_hours_prop

cap drop change_hours_cat
recode ch_hours_prop (0.90/1.10 = 0 "stayed the same (90% to 110%)") ///
					 (0/0.90 = 1 "Decreased") ///
					 (1.10/40 = 2 "Increased"), gen(change_hours_cat) 

*Five categoies of change in working time as described in the paper. 
recode change_hours_cat (0 = 0) (2 = 1 )  (1 = 2), gen(worktime_change)
replace worktime_change = 3 if  employment_status ==1
replace  worktime_change = 4 if employment_status ==2
label define worktime_change 0 "stayed the same" 1 "increased" 2 "decreased" 3 "furloughed" 4 "not employed" 
label values worktime_change worktime_change
tab worktime_change 

tab employment_status, nolabel

* EXPOSURES
* ====================================================================================================================================

* covid_ever -  suspeced covid
* ------------------------------
cap drop covid_ever
gen covid_ever = 1 if (inlist(cw3_covid19,1, 2) | inlist(cw2_covid19,1, 2) | inlist(cw1_covid19,1,2)) 
recode covid_ever . = 0 if inlist(cw3_covid19,3, 4) == 1 

cap label drop covid_ever
label define covid_ever	///
		0 "No"	///
		1 "Yes"	
label values covid_ever covid_ever
label variable covid_ever "Covid ever - suspected"
tab covid_ever


tab cw3_covid19pos cw3_covid19, miss

* covid_confirmed 

cap drop covid_confirmed
gen covid_confirmed = 1 if (inlist(cw3_covid19,1) | inlist(cw2_covid19,1) | inlist(cw1_covid19,1)) 
recode covid_confirmed . = 0 if inlist(cw3_covid19, 2, 3, 4) == 1 
cap label drop covid_confirmed
label define covid_confirmed	///
		0 "No"	///
		1 "Yes"	
label values covid_confirmed covid_confirmed
tab covid_confirmed

gen covid_sr = covid_confirmed 
recode covid_sr 1 = 2
recode covid_sr 0 = 1 if covid_ever == 1
label define covid_sr ///
	0 "No" ///
	1 "Suspected" ///
	2 "Test confirmed"
label values covid_sr covid_sr 



* covid_timing suspected 
* ------------


label define covid_period 0 "No Covid" 1 "Before September 1st 2020" 2 "After September 1st 2020" 3 "Before and after 23 september"

gen survey1 = 1 if inlist(cw1_covid19,1, 2)  == 1
recode survey1 . = 0 if inlist(cw1_covid19, 3, 4)  == 1
label values survey1 covid_period 

gen survey2 = . 
replace survey2 = 1 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos < 8
replace survey2 = 2 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos >=8 & cw2_covid19pos !=. 
replace survey2 = 0 if  inlist(cw2_covid19, 3, 4) 
label values survey2 covid_period


gen survey3 = . 
replace survey3 = 1 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos < 8
replace survey3 = 2 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos >=8 & cw3_covid19pos !=. 
replace survey3 = 0 if  inlist(cw3_covid19, 3, 4) 
label values survey3 covid_period


cap drop covid_timing
gen covid_timing = 0 if covid_ever==0
recode covid_timing .  = 1 if (survey1 == 1 | survey2 ==1 | survey3 == 1) & (survey2 !=2  & survey3 != 2)
recode covid_timing .  = 2 if (survey2 == 2 | survey3 ==2 ) & survey1 !=1 & survey2 !=1
recode covid_timing .  = 3 if (survey2 == 2 | survey3 == 2)

cap label drop covid_timing
label define covid_timing ///
        0 "no cov"  ///
        1 "before sept 1st" ///
        2 "after sept 1st"  ///
        3 "both"
label values covid_timing covid_timing

tab covid_timing

* long_covid
* ----------
*gen long_covid = 0 if covid_ever == 0 /// Note this may need changing due to the difference suspected and confirmed things. 
*replace long_covid = 1 if inlist(cw3_covfunc, 1, 2, 3, 4, 5)
*replace long_covid = 2 if inlist(cw3_covfunc, 6)
*replace long_covid = 3 if inlist(cw3_covfunc, 7)

*label define long_covid ///
           0 "no covid" ///
           1 "<4weeks" ///
           2 "4-<12 weeks" ///
           3  "12+ weeks" 
*label values long_covid long_covid

* MODIFIERS
* ====================================================================================================================================

* age
* ---
gen age_group = 2 

* sex
* ---
cap label drop sex
recode sex (1=0 "male") (2=1 "female") (3=.), gen (sex2)
drop sex 
rename sex2 sex 

* education

* Education
label define education 0 "NVQ 4 or 5" 1 "NVQ 3" 2 "NVQ 2 & 1" 3 "None"
recode  bd10hnvq  (-9/-1=.) (0 = 3) (1/2 = 2 ) (3 = 1) (4/5 = 0) ,gen(education)
label variable education "Education NVQ eqvs"
label values education education

recode education (0 = 0 "NVQ 4 or 5") (1/3 = 1 "NVQ3 or less") , gen(education_bin)




* ethnicity - Next steps only (other cohort are white)
* ----------------------------------------------------
** merge with the first wave of Next Steps to extract ethnicity data 



* Ethnicity 

recode  ethnic (1/3 = 1 "White") (4/7 = 4 "Mixed") (12/14= 3 "Black") (8/11 15 = 2 "Asian") (16 = 5 "Other") (98/99 = . ) , gen(ethnicity)
label variable ethnicity "Ethnicity Broad"



recode ethnicity (1 = 0 "White") (2/5 = 1 "Non White") , gen(ethnicity_bin)
label variable ethnicity_bin "Ethnicity Binary"


tab ethnic
label define ethnicity1 1 "White" 2 "Mixed" 3 "Indian" 4 "Pakistani" 5 "Bangladeshi" 6 "Black Caribean" 7 "Black African" 8 "Other" 
recode ethnic (1/3 = 1) (4/7 = 2 ) (8 = 3) (9 = 4) (10 = 5) (11 = 8) (12 = 6) (13 = 7) (14 = 8) (15 = 8) (16 = 8) (98/99 = .) , gen(ethnicity1)
label values ethnicity1 ethnicity1 



* soc
gen soc_2d = floor(cw1_soc2010/10 )
gen soc_1d = floor(cw1_soc2010/100 )

*soc_designation 
label define soc_designation 0 "SOC2000" 1 "SOC2010" 2 "Missing" 
gen soc_designation = 1 if soc_1d ! =. 





* sic
* ---
tab cw3_sic3
gen sic_1d = . 

* nssec 7 (pre-C19 )
recode cw1_nssec2010an (1/1.9 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 = 8 "longterm unemployed" ) (-1 9 = 9 "Other Unclassifiable")  , gen(nssec7)
label variable nssec7 "NS-SEC 7 Categories"

* nssec  5 (pre-C19 ) 
recode nssec7  (1/2 = 1 "Management & professional ") (3 = 2 "Intermediate") (4 = 3 "Small Employer") (5 = 4 "Lower supervisory & Technical") (6/7 = 5 "Semi-routine & Routine")  (8 = 6 "longterm unemployed" ) ( 9 = 9 "Other Unclassifiable")  , gen(nssec5)
label variable nssec5 "NS-SEC 5 Categories"







* CONTROL VARIABLES
* ====================================================================================================================================
* Age
* change this if needed 
* Note as I htink they are are all born in April this may need changing 
gen age = 50

* country of residence
* --------------------
tab cw3_countres, nolabel 
recode cw3_countres ///
	(-9 = .) /// 
	(1 = 1 "England")  ///
	(2 = 3 "Wales") ///
	(3 = 2 "Scotland") ///
	(4 = 4 "Nothern Ireland") ///
	(5 = 5 "Elsewhere") , gen(country)
	

* household composition
* ---------------------
tab cw3_hhnum 
tab cw3_hhnumwh_1 
tab cw3_hhnumwh_2 
tab cw3_hhnumwh_3 
tab cw3_hhnumwh_4 
tab cw3_hhnumwh_5 
tab cw3_hhnumwh_6 
tab cw3_hhnumwh_7 
tab cw3_hhnumwh_8 
tab cw3_hhnumwh_9 
tab cw3_numrooms

tab cw1_hhnumwh_1, nolabel
*Lives with partner
recode cw1_hhnumwh_1  (2 = 0 "no partner") (1 = 1 "partner") , gen(cw1_partner)

*Lives with children or grandchildren
recode cw1_hhnumwh_2  (2 = 0 "no children") (1 = 1 "children") , gen(cw1_child) 
recode cw1_child 0 = 1 if cw1_hhnumwh_5 == 1 

*Other relative 
recode cw1_hhnumwh_3 (2 = 0 "No other relative")   (1 = 1 "Other relative")   , gen(cw1_relative)
recode cw1_relative 0 = 1 if  cw1_hhnumwh_4 == 1  | cw1_hhnumwh_6 == 1 | cw1_hhnumwh_7 == 1

tab cw1_hhnumwh_6 cw1_relative

*other 
recode cw1_hhnumwh_8 (2 = 0 "No other")   (1 = 1 "Other")   , gen(cw1_other)
recode cw1_other 0 = 1 if  cw1_hhnumwh_9 == 1

label define hh_comp 0 "alone" 1 "Partner" 2 "Partner & children" 3 "lone parent" 4 "other person" 
gen hh_comp = cw1_partner 
recode hh_comp -1 = . 
recode hh_comp  0 = 3 1 = 2 if cw1_child == 1 
recode hh_comp  0 = 4 if cw1_relative == 1
recode hh_comp  0 = 4 if cw1_other == 1 
label values hh_comp hh_comp

tab cw1_hhnum 
tab cw1_hhnumwh_1 
tab cw1_hhnumwh_2 
tab cw1_hhnumwh_3 
tab cw1_hhnumwh_4 
tab cw1_hhnumwh_5 
tab cw1_hhnumwh_6 
tab cw1_hhnumwh_7 
tab cw1_hhnumwh_8 
tab cw1_hhnumwh_9 
tab cw1_numrooms


tab cw1_hhnum


* Self-rated health 
clonevar sr_health = cw1_ghqprecovid
recode sr_health -8 = . 


* mental health
* -------------
*tab cw1_ghqprecovid 
*tab cw2_ghqprecovid 
*tab cw3_ghqprecovid 
recode bd10malg (1 = 0 "Low malaise") (2 = 1 "High malaise") , gen(mental_health) 


* keyworker at any point (across the pandemic - nonemployed are sep category)
* ---------------------------------------------------------------------------
* RJS not I revised thos code to be first incidence of keyworker and theen use later waves
* to fill in small number of misisng. 
* Ideally you would it defined pre-pandemic but also the concept did not exist prior this is the subtitute. 



* only asked of employed people so you may want to create additional category for non-employed
cap drop keyworker
gen keyworker = cw1_keyworkerd 
replace keyworker = cw2_keyworkerd if keyworker==.
replace keyworker = cw3_keyworkerd if keyworker ==. 
recode keyworker (2=0)
recode keyworker . = 0 
codebook keyworker

codebook keyworker



* shielded at any point
* ---------------------
* Shielding has smilar issues to keyworker up switching to using the first wave to be consistent with USoc. 
* 



forvalues j = 1 (1) 3 {

    recode cw`j'_shield (-10/-1 = .)
    
}  

gen shield = cw1_shield
replace shield = cw2_shield if shield ==.
replace shield = cw3_shield if shield ==.
recode shield (2=0)
tab shield


* health conditions 
* ------------------

* Cancer
gen cancer = cw1_lli_1
replace cancer = cw2_lli_1 if cancer ==.
replace cancer = cw3_lli_1 if cancer ==.
recode cancer (2=0) (-8/-1=.)
tab cancer

* Heart
gen heart = cw1_lli_10
replace heart = cw2_lli1_10 if heart ==.
replace heart = cw3_lli1_10 if heart ==.
recode heart (2=0) (-9/-1=.)
tab heart

* Depression
cap drop depression
gen depression = 1 if cw1_lli_16 ==1 | cw1_lli_11 ==1
replace depression = 1 if cw2_lli2_5 ==1 | cw2_lli2_1 ==1 
replace depression = 1 if cw3_lli2_5 ==1 | cw3_lli2_1 ==1 
recode depression . = 0 if cw1_lli_16 == 2
tab depression

* Obesity
gen obesity = cw1_lli_12
replace obesity = cw2_lli2_2 if obesity ==.
replace obesity = cw3_lli2_2 if obesity ==.
recode obesity (2=0) (-9/-1=.)
tab obesity

* respiratory
gen respiratory = 1 if cw1_lli_2 ==1 | cw1_lli_3 ==1 | cw1_lli_4 ==1 | cw1_lli_5 ==1 | cw1_lli_1 ==13 
replace respiratory = 1 if cw2_lli1_2 ==1 | cw2_lli1_3 ==1 | cw2_lli1_4 ==1 | cw2_lli1_5 ==1
replace respiratory = 1 if cw3_lli1_2 ==1 | cw3_lli1_3 ==1 | cw3_lli1_4 ==1 | cw3_lli1_5 ==1
recode respiratory . = 0 if cw1_lli_2 == 2
tab respiratory

*Additional changes 
clonevar shielding = shield 
clonevar obese = obesity


tab1 employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity1 ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding


gen id_string = bcsid   

* Files to keep 
keep /// 
id_string cohort_id    study_selection age_selection employment_selection  employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity1 ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding

save  "$destination\BCS70_analytic_sample_05May23.dta" , replace 


***********************************************************************************************************************************************
*** Next Steps Code 


use  "$source\covid-19_wave1_survey_cls.dta", clear
drop if NSID == ""
save  "$destination\temp1.dta" , replace 
use  "$source\covid-19_wave2_survey_cls.dta", clear
drop if NSID == ""
save  "$destination\temp2.dta" , replace 
use  "$source\covid-19_wave3_survey_cls.dta", clear
drop if NSID == ""
save  "$destination\temp3.dta" , replace 


clear all


use  "$destination\temp3.dta", clear
merge 1:1 NSID using "$destination\temp2.dta", gen(_merge2)
merge 1:1 NSID using "$destination\temp1.dta", gen(_merge1)
merge 1:1 NSID using "$source\wave_one_lsype_family_background_2020.dta", gen(_mergefamilybck) keepusing(W1ethgrpYP)
*merge 1:1 llc_0010_stud_id using "$source\NEXTSTEP_basic_demographic_data_v0001_20211101.dta", gen(_merge_demo)
merge 1:1 NSID using "$source\ns8_2015_derived.dta", gen(_merge_derived) 


rename _all, lower

* Selector variables 

gen study_selection = 1 if cw3_outcome !=  . 
gen age_selection  = 1 
gen employment_selection = 1 if inlist(cw1_econactivityb, 1, 2)


* Cohort management variables 

*Note these to be copied through to R to derive them. 
desc cw3_enddated cw3_enddatem // These are deate of survey completion and will need to add the year 2021 to produce the final variable 
gen cohort_id = "NextSteps"



*OUTCOME MEASURES

*====================================================================================================================================

* employment status
* -----------------
* Note RJS I have amemnded this as aprentices are employees and getting paid a wage and holidya pay. 
recode cw3_econactivityd (1 3 4 6 = 0 "employed / self-employed") ///
                         (2 = 1 "furloughed") (5 7/13 = 2 "Not Employed") ///
                         (-99/-1=.), gen(employment_status)
						 
codebook employment_status						 


recode cw3_econactivityd (1/4 6/8 = 0 "Active") ///
                         (5 9/13 = 1 "Inactive") ///
                         (-99/-1=.), gen(econ_act)

codebook econ_act
* finance_change
* --------------
recode cw3_financialmand (-99 / -1 =.) 
rename cw3_financialmand finance_change
codebook finance_change

* finance_change_bin
* ------------------
recode finance_change (3/5 = 0 "same/better") (1/2 = 1 "worse"), gen (finance_change_bin)
codebook finance_change_bin


* worktime_change
* ---------------
gen pre_hours=cw1_wrkhoursb if cw1_wrkhoursb>0
gen post_hours=cw3_wrkhoursd if cw3_wrkhoursd>0

gen ch_hours_prop = post_hours / pre_hours
tab ch_hours_prop

cap drop change_hours_cat
recode ch_hours_prop (0.90/1.10 = 0 "stayed the same (90% to 110%)") ///
					 (0/0.90 = 1 "Decreased") ///
					 (1.10/40 = 2 "Increased"), gen(change_hours_cat) 

*Five categoies of change in working time as described in the paper. 
recode change_hours_cat (0 = 0) (2 = 1 )  (1 = 2), gen(worktime_change)
replace worktime_change = 3 if  employment_status ==1
replace  worktime_change = 4 if employment_status ==2
label define worktime_change 0 "stayed the same" 1 "increased" 2 "decreased" 3 "furloughed" 4 "not employed" 
label values worktime_change worktime_change
tab worktime_change 


* EXPOSURES
* ====================================================================================================================================

* covid_ever - confirmed by test
* ------------------------------

cap drop covid_ever
gen covid_ever = 1 if (inlist(cw3_covid19,1, 2) | inlist(cw2_covid19,1, 2) | inlist(cw1_covid19,1,2)) 
recode covid_ever . = 0 if inlist(cw3_covid19,3, 4) == 1 

cap label drop covid_ever
label define covid_ever	///
		0 "No"	///
		1 "Yes"	
label values covid_ever covid_ever
label variable covid_ever "Covid ever - suspected"
tab covid_ever


* covid_confirmed 

cap drop covid_confirmed
gen covid_confirmed = 1 if (inlist(cw3_covid19,1) | inlist(cw2_covid19,1) | inlist(cw1_covid19,1)) 
recode covid_confirmed . = 0 if inlist(cw3_covid19, 2, 3, 4) == 1 
cap label drop covid_confirmed
label define covid_confirmed	///
		0 "No"	///
		1 "Yes"	
label values covid_confirmed covid_confirmed
tab covid_confirmed

gen covid_sr = covid_confirmed 
recode covid_sr 1 = 2
recode covid_sr 0 = 1 if covid_ever == 1
label define covid_sr ///
	0 "No" ///
	1 "Suspected" ///
	2 "Test confirmed"
label values covid_sr covid_sr 




* covid_timing
* ------------

label define covid_period 0 "No Covid" 1 "Before September 1st 2020" 2 "After September 1st 2020" 3 "Before and after 23 september"

gen survey1 = 1 if inlist(cw1_covid19,1, 2)  == 1
recode survey1 . = 0 if inlist(cw1_covid19, 3, 4)  == 1
label values survey1 covid_period 

gen survey2 = . 
replace survey2 = 1 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos < 8
replace survey2 = 2 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos >=8 & cw2_covid19pos !=. 
replace survey2 = 0 if  inlist(cw2_covid19, 3, 4) 
label values survey2 covid_period


gen survey3 = . 
replace survey3 = 1 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos < 8
replace survey3 = 2 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos >=8 & cw3_covid19pos !=. 
replace survey3 = 0 if  inlist(cw3_covid19, 3, 4) 
label values survey3 covid_period


cap drop covid_timing
gen covid_timing = 0 if covid_ever==0
recode covid_timing .  = 1 if (survey1 == 1 | survey2 ==1 | survey3 == 1) & (survey2 !=2  & survey3 != 2)
recode covid_timing .  = 2 if (survey2 == 2 | survey3 ==2 ) & survey1 !=1 & survey2 !=1
recode covid_timing .  = 3 if (survey2 == 2 | survey3 == 2)

cap label drop covid_timing
label define covid_timing ///
        0 "no cov"  ///
        1 "before sept 1st" ///
        2 "after sept 1st"  ///
        3 "both"
label values covid_timing covid_timing


* long_covid
* ----------
gen long_covid = 0 if covid_ever == 0 
replace long_covid = 1 if inlist(cw3_covfunc, 1, 2, 3, 4, 5)
replace long_covid = 2 if inlist(cw3_covfunc, 6)
replace long_covid = 3 if inlist(cw3_covfunc, 7)

label define long_covid ///
           0 "no covid" ///
           1 "<4weeks" ///
           2 "4-<12 weeks" ///
           3  "12+ weeks" 
label values long_covid long_covid

* MODIFIERS
* ====================================================================================================================================

* age
* ---
gen age_group = 1

* sex
* ---
recode cw3_psex (1=0 "male") (2=1 "female"), gen (sex)

* education
* ---------
label define education 0 "NVQ 4 or 5" 1 "NVQ 3" 2 "NVQ 2 & 1" 3 "None"
recode w8dhanvqh (min/-1 = .) (1/2 = 2) (3 = 1)  (4/5 = 0) (95/96 = 3), gen(education)
label values education education 
label variable education "Education NVQ eqvs"

recode education (0 = 0 "NVQ 4 or 5") (1/3 = 1 "NVQ3 or less") , gen(education_bin)


* ethnicity - Next steps only (other cohort are white)
* ----------------------------------------------------

rename (w1ethgrpyp) (ns_ethnic)

* recode ns_ethnic (1=1) (3/5=2) (6/7=3) (2=4) (8=5)

* cap label drop ns_cm_ethnic
* label define ns_cm_ethnic   ///
*        1 "white"   ///
*        2 "south asian" ///
*        3 "black"   ///
*        4 "mixed"   ///
*        5 "other"   
*label values ns_ethnic ns_ethnic


recode  ns_ethnic (1 = 1 "White") (2 = 4 "Mixed") (3/5= 2 "Asian") (6/7 =3 "Black") (8 = 5 "Other") (-999 -92 = .) , gen(ethnicity)
label variable ethnicity "Ethnicity Broad"



recode ethnicity (1 = 0 "White") (2/5 = 1 "Non White") , gen(ethnicity_bin)
label variable ethnicity_bin "Ethnicity Binary"




clonevar ethnicity1 = ns_ethnic





* soc

gen soc_2d = floor(cw1_soc2010/10 )
gen soc_1d = floor(cw1_soc2010/100 )

*soc_designation 
label define soc_designation 0 "SOC2000" 1 "SOC2010" 2 "Missing" 
gen soc_designation = 1 if soc_1d ! =. 

* sic
* ---
tab cw3_sic3
gen sic_1d = . 



* nssec (pre-C19 ) 7
recode cw1_nssec2010an (1/1.9 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 = 8 "longterm unemployed" ) ( -1 9 = 9 "Other Unclassifiable")  , gen(nssec7)
label variable nssec7 "NS-SEC 7 Categories"

* nssec (pre-C19 ) 5
recode nssec7  (1/2 = 1 "Management & professional ") (3 = 2 "Intermediate") (4 = 3 "Small Employer") (5 = 4 "Lower supervisory & Technical") (6/7 = 5 "Semi-routine & Routine")  (8 = 6 "longterm unemployed" ) ( 9 = 9 "Other Unclassifiable")  , gen(nssec5)
label variable nssec5 "NS-SEC 5 Categories"

* CONTROL VARIABLES
* ====================================================================================================================================
* Age
gen age = 31




* country of residence
* --------------------
tab cw3_countres, nolabel 
tab cw3_countres, nolabel 
recode cw3_countres /// 
	(1 = 1 "England")  ///
	(2 = 3 "Wales") ///
	(3 = 2 "Scotland") ///
	(4 = 4 "Nothern Ireland") ///
	(5 = 5 "Elsewhere") , gen(country)
	

* household composition
* ---------------------
tab cw3_hhnum 
tab cw3_hhnumwh_1 
tab cw3_hhnumwh_2 
tab cw3_hhnumwh_3 
tab cw3_hhnumwh_4 
tab cw3_hhnumwh_5 
tab cw3_hhnumwh_6 
tab cw3_hhnumwh_7 
tab cw3_hhnumwh_8 
tab cw3_hhnumwh_9 
tab cw3_numrooms

tab cw1_hhnumwh_1, nolabel
*Lives with partner
recode cw1_hhnumwh_1  (2 = 0 "no partner") (1 = 1 "partner") , gen(cw1_partner)

*Lives with children or grandchildren
recode cw1_hhnumwh_2  (2 = 0 "no children") (1 = 1 "children") , gen(cw1_child) 
recode cw1_child 0 = 1 if cw1_hhnumwh_5 == 1 

*Other relative 
recode cw1_hhnumwh_3 (2 = 0 "No other relative")   (1 = 1 "Other relative")   , gen(cw1_relative)
recode cw1_relative 0 = 1 if  cw1_hhnumwh_4 == 1  | cw1_hhnumwh_6 == 1 | cw1_hhnumwh_7 == 1

tab cw1_hhnumwh_6 cw1_relative

*other 
recode cw1_hhnumwh_8 (2 = 0 "No other")   (1 = 1 "Other")   , gen(cw1_other)
recode cw1_other 0 = 1 if  cw1_hhnumwh_9 == 1

label define hh_comp 0 "alone" 1 "Partner" 2 "Partner & children" 3 "lone parent" 4 "other person" 
gen hh_comp = cw1_partner 
recode hh_comp -8 = . 
recode hh_comp  0 = 3 1 = 2 if cw1_child == 1 
recode hh_comp  0 = 4 if cw1_relative == 1
recode hh_comp  0 = 4 if cw1_other == 1 
label values hh_comp hh_comp

tab cw1_hhnum 
tab cw1_hhnumwh_1 
tab cw1_hhnumwh_2 
tab cw1_hhnumwh_3 
tab cw1_hhnumwh_4 
tab cw1_hhnumwh_5 
tab cw1_hhnumwh_6 
tab cw1_hhnumwh_7 
tab cw1_hhnumwh_8 
tab cw1_hhnumwh_9 
tab cw1_numrooms


tab cw1_hhnum



* Self-rated health 
clonevar sr_health = cw1_ghqprecovid
recode sr_health -1 = . 


* mental health
* -------------
*tab cw1_ghqprecovid 
*tab cw2_ghqprecovid 
*tab cw3_ghqprecovid 
recode  w8dghqsc  (0/3 = 0 "No") (4/12 = 1 "Yes"), gen(mental_health) 



* keyworker at any point (across the pandemic - nonemployed are sep category)
* ---------------------------------------------------------------------------




* only asked of employed people so you may want to create additional category for non-employed
cap drop keyworker
gen keyworker = cw1_keyworkerd 
replace keyworker = cw2_keyworkerd if keyworker==.
replace keyworker = cw3_keyworkerd if keyworker ==. 
recode keyworker (2=0)
recode keyworker . = 0 
codebook keyworker

codebook keyworker



* shielded at any point
* ---------------------
* Shielding has smilar issues to keyworker up switching to using the first wave to be consistent with USoc. 
* 



forvalues j = 1 (1) 3 {

    recode cw`j'_shield (-10/-1 = .)
    
}  

gen shield = cw1_shield
replace shield = cw2_shield if shield ==.
replace shield = cw3_shield if shield ==.
recode shield (2=0)
tab shield

* health conditions 
* ------------------

* Cancer
gen cancer = cw1_lli_1
replace cancer = cw2_lli_1 if cancer ==.
replace cancer = cw3_lli_1 if cancer ==.
recode cancer (2=0) (-8/-1=.)
tab cancer

* Heart
gen heart = cw1_lli_10
replace heart = cw2_lli1_10 if heart ==.
replace heart = cw3_lli1_10 if heart ==.
recode heart (2=0) (-9/-1=.)
tab heart

* Depression
cap drop depression
gen depression = 1 if cw1_lli_16 ==1 | cw1_lli_11 ==1
replace depression = 1 if cw2_lli2_5 ==1 | cw2_lli2_1 ==1 
replace depression = 1 if cw3_lli2_5 ==1 | cw3_lli2_1 ==1 
recode depression . = 0 if cw1_lli_16 == 2
tab depression

* Obesity
gen obesity = cw1_lli_12
replace obesity = cw2_lli2_2 if obesity ==.
replace obesity = cw3_lli2_2 if obesity ==.
recode obesity (2=0) (-9/-1=.)
tab obesity

* respiratory
gen respiratory = 1 if cw1_lli_2 ==1 | cw1_lli_3 ==1 | cw1_lli_4 ==1 | cw1_lli_5 ==1 | cw1_lli_1 ==13 
replace respiratory = 1 if cw2_lli1_2 ==1 | cw2_lli1_3 ==1 | cw2_lli1_4 ==1 | cw2_lli1_5 ==1
replace respiratory = 1 if cw3_lli1_2 ==1 | cw3_lli1_3 ==1 | cw3_lli1_4 ==1 | cw3_lli1_5 ==1
recode respiratory . = 0 if cw1_lli_2 == 2
tab respiratory

*Additional changes 
clonevar shielding = shield 
clonevar obese = obesity



tab1 employment_status econ_act age sex education education_bin ethnicity ethnicity1 ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding

tab covid_sr


gen id_string = nsid   
* Files to keep 
keep /// 
id_string cohort_id   study_selection age_selection  employment_selection  employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity1 ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding



save  "$destination\nextsteps_analytic_sample_05May23.dta" , replace 

**********************************************************************************************************************************************
*** NCDS Code 

clear all



*First reasemble main bcs10 file and select necessary variables 
use  "$source\ncds_2013_flatfile.dta", clear

keep NCDSID N9KHPB08

save  "$destination\ncds9_mental_health.dta" , replace 



clear all
set maxvar 30000

*NOte reasembling files in their original form. 



use  "$source\covid-19_wave1_survey_cls.dta", clear
drop if NCDSID == ""
save  "$destination\temp1.dta" , replace 
use  "$source\covid-19_wave2_survey_cls.dta", clear
drop if NCDSID == ""
save  "$destination\temp2.dta" , replace 
use  "$source\covid-19_wave3_survey_cls.dta", clear
drop if NCDSID == ""
save  "$destination\temp3.dta" , replace 


clear all

use  "$destination\temp3.dta", clear
merge 1:1 NCDSID using "$destination\temp2.dta", gen(_merge2)
merge 1:1 NCDSID using "$destination\temp1.dta", gen(_merge1)
merge 1:1 NCDSID using "$source\ncds_response.dta" , gen(_merge_response)
merge 1:1 NCDSID using "$source\ncds_2013_derived.dta" , gen(_merge_derived)
merge 1:1 NCDSID using "$destination\ncds9_mental_health.dta" , gen(_merge_mental)

rename _all, lower

* Selector variables 

gen study_selection = 1 if cw3_outcome !=  . 
gen age_selection  = 1 
gen employment_selection = 1 if inlist(cw1_econactivityb, 1, 2)


* Cohort management variables 

*Note these to be copied through to R to derive them. 
desc cw3_enddated cw3_enddatem // These are deate of survey completion and will need to add the year 2021 to produce the final variable 
gen cohort_id = "NCDS"



*OUTCOME MEASURES

*====================================================================================================================================

* employment status
* -----------------
* Note RJS I have amemnded this as aprentices are employees and getting paid a wage and holidya pay. 
recode cw3_econactivityd (1 3 4 6 = 0 "employed / self-employed") ///
                         (2 = 1 "furloughed") (5 7/13 = 2 "Not Employed") ///
                         (-99/-1=.), gen(employment_status)
						 
codebook employment_status						 


recode cw3_econactivityd (1/4 6/8 = 0 "Active") ///
                         (5 9/13 = 1 "Inactive") ///
                         (-99/-1=.), gen(econ_act)

codebook econ_act
* finance_change
* --------------
recode cw3_financialmand (-99 / -1 =.) 
rename cw3_financialmand finance_change
codebook finance_change

* finance_change_bin
* ------------------
recode finance_change (3/5 = 0 "same/better") (1/2 = 1 "worse"), gen (finance_change_bin)
codebook finance_change_bin


* worktime_change
* ---------------
gen pre_hours=cw1_wrkhoursb if cw1_wrkhoursb>0
gen post_hours=cw3_wrkhoursd if cw3_wrkhoursd>0

gen ch_hours_prop = post_hours / pre_hours
tab ch_hours_prop

cap drop change_hours_cat
recode ch_hours_prop (0.90/1.10 = 0 "stayed the same (90% to 110%)") ///
					 (0/0.90 = 1 "Decreased") ///
					 (1.10/40 = 2 "Increased"), gen(change_hours_cat) 

*Five categoies of change in working time as described in the paper. 
recode change_hours_cat (0 = 0) (2 = 1 )  (1 = 2), gen(worktime_change)
replace worktime_change = 3 if  employment_status ==1
replace  worktime_change = 4 if employment_status ==2
label define worktime_change 0 "stayed the same" 1 "increased" 2 "decreased" 3 "furloughed" 4 "not employed" 
label values worktime_change worktime_change
tab worktime_change 


* EXPOSURES
* ====================================================================================================================================


cap drop covid_ever
gen covid_ever = 1 if (inlist(cw3_covid19,1, 2) | inlist(cw2_covid19,1, 2) | inlist(cw1_covid19,1,2)) 
recode covid_ever . = 0 if inlist(cw3_covid19,3, 4) == 1 

cap label drop covid_ever
label define covid_ever	///
		0 "No"	///
		1 "Yes"	
label values covid_ever covid_ever
label variable covid_ever "Covid ever - suspected"
tab covid_ever


* covid_confirmed 

cap drop covid_confirmed
gen covid_confirmed = 1 if (inlist(cw3_covid19,1) | inlist(cw2_covid19,1) | inlist(cw1_covid19,1)) 
recode covid_confirmed . = 0 if inlist(cw3_covid19, 2, 3, 4) == 1 
cap label drop covid_confirmed
label define covid_confirmed	///
		0 "No"	///
		1 "Yes"	
label values covid_confirmed covid_confirmed
tab covid_confirmed

gen covid_sr = covid_confirmed 
recode covid_sr 1 = 2
recode covid_sr 0 = 1 if covid_ever == 1
label define covid_sr ///
	0 "No" ///
	1 "Suspected" ///
	2 "Test confirmed"
label values covid_sr covid_sr 



* covid_timing suspected 
* ------------


label define covid_period 0 "No Covid" 1 "Before September 1st 2020" 2 "After September 1st 2020" 3 "Before and after 23 september"

gen survey1 = 1 if inlist(cw1_covid19,1, 2)  == 1
recode survey1 . = 0 if inlist(cw1_covid19, 3, 4)  == 1
label values survey1 covid_period 

gen survey2 = . 
replace survey2 = 1 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos < 8
replace survey2 = 2 if  inlist(cw2_covid19, 1, 2)  == 1 &  cw2_covid19pos >=8 & cw2_covid19pos !=. 
replace survey2 = 0 if  inlist(cw2_covid19, 3, 4) 
label values survey2 covid_period


gen survey3 = . 
replace survey3 = 1 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos < 8
replace survey3 = 2 if  inlist(cw3_covid19, 1, 2)  == 1 &  cw3_covid19pos >=8 & cw3_covid19pos !=. 
replace survey3 = 0 if  inlist(cw3_covid19, 3, 4) 
label values survey3 covid_period


cap drop covid_timing
gen covid_timing = 0 if covid_ever==0
recode covid_timing .  = 1 if (survey1 == 1 | survey2 ==1 | survey3 == 1) & (survey2 !=2  & survey3 != 2)
recode covid_timing .  = 2 if (survey2 == 2 | survey3 ==2 ) & survey1 !=1 & survey2 !=1
recode covid_timing .  = 3 if (survey2 == 2 | survey3 == 2)

cap label drop covid_timing
label define covid_timing ///
        0 "no cov"  ///
        1 "before sept 1st" ///
        2 "after sept 1st"  ///
        3 "both"
label values covid_timing covid_timing


* long_covid
* ----------
gen long_covid = 0 if covid_ever == 0 
replace long_covid = 1 if inlist(cw3_covfunc, 1, 2, 3, 4, 5)
replace long_covid = 2 if inlist(cw3_covfunc, 6)
replace long_covid = 3 if inlist(cw3_covfunc, 7)

label define long_covid ///
           0 "no covid" ///
           1 "<4weeks" ///
           2 "4-<12 weeks" ///
           3  "12+ weeks" 
label values long_covid long_covid

* MODIFIERS
* ====================================================================================================================================

* age
* ---
gen age_group = 3

* sex
* ---
recode cw3_psex (1=0 "male") (2=1 "female"), gen (sex)

* education
* ---------
 
label define education 0 "NVQ 4 or 5" 1 "NVQ 3" 2 "NVQ 2 & 1" 3 "None"
recode nd9hnvq (min/-1 = .)  (0 = 3) (1/2 = 2) (3 = 1)  (4/5 = 0) , gen(education)
label values education education 
label variable education "Education NVQ eqvs"

recode education (0 = 0 "NVQ 4 or 5") (1/3 = 1 "NVQ3 or less") , gen(education_bin)





* ethnicity - Next steps only (other cohort are white)
* ----------------------------------------------------
** merge with the first wave of Next Steps to extract ethnicity data 


recode   ethnicid (1 = 1 "White") (2 = 4 "Mixed") (5= 3 "Black") (3/4 = 2 "Asian") (6 = 5 "Other") (-1 = . ) , gen(ethnicity)
label variable ethnicity "Ethnicity Broad"

recode ethnicity (1 = 0 "White") (2/5 = 1 "Non White") , gen(ethnicity_bin)
label variable ethnicity_bin "Ethnicity Binary"

gen ethnicity1 = . 




* soc
gen soc_2d = floor(cw1_soc2010/10 )
gen soc_1d = floor(cw1_soc2010/100 )

*soc_designation 
label define soc_designation 0 "SOC2000" 1 "SOC2010" 2 "Missing" 
gen soc_designation = 1 if soc_1d ! =. 

* sic
* ---
tab cw3_sic3
gen sic_1d = . 

* nssec (pre-C19 ) 7
recode cw1_nssec2010an (1/1.9 = 1 "Higher management or professional") (2 = 2 "Lower management and professional" ) (3 = 3 "Intermediate") (4 = 4 "Small Employer") (5 = 5 "Lower supervisory") (6 = 6 "Semi-routine") (7 = 7 "Routine") (8 = 8 "longterm unemployed" ) ( -1 9 = 9 "Other Unclassifiable")  , gen(nssec7)
label variable nssec7 "NS-SEC 7 Categories"

* nssec (pre-C19 ) 5
recode nssec7  (1/2 = 1 "Management & professional ") (3 = 2 "Intermediate") (4 = 3 "Small Employer") (5 = 4 "Lower supervisory & Technical") (6/7 = 5 "Semi-routine & Routine")  (8 = 6 "longterm unemployed" ) ( 9 = 9 "Other Unclassifiable")  , gen(nssec5)
label variable nssec5 "NS-SEC 5 Categories"

* CONTROL VARIABLES
* ====================================================================================================================================
* Age
* change this if needed 
* Note as I htink they are are all born in March this may need changing 
gen age = 63

* country of residence
* --------------------
tab cw3_countres, nolabel 
tab cw3_countres, nolabel 
recode cw3_countres /// 
	(1 = 1 "England")  ///
	(2 = 3 "Wales") ///
	(3 = 2 "Scotland") ///
	(4 = 4 "Nothern Ireland") ///
	(5 = 5 "Elsewhere") , gen(country)
	

* household composition
* ---------------------
tab cw3_hhnum 
tab cw3_hhnumwh_1 
tab cw3_hhnumwh_2 
tab cw3_hhnumwh_3 
tab cw3_hhnumwh_4 
tab cw3_hhnumwh_5 
tab cw3_hhnumwh_6 
tab cw3_hhnumwh_7 
tab cw3_hhnumwh_8 
tab cw3_hhnumwh_9 
tab cw3_numrooms

tab cw1_hhnumwh_1, nolabel
*Lives with partner
recode cw1_hhnumwh_1  (2 = 0 "no partner") (1 = 1 "partner") , gen(cw1_partner)

*Lives with children or grandchildren
recode cw1_hhnumwh_2  (2 = 0 "no children") (1 = 1 "children") , gen(cw1_child) 
recode cw1_child 0 = 1 if cw1_hhnumwh_5 == 1 

*Other relative 
recode cw1_hhnumwh_3 (2 = 0 "No other relative")   (1 = 1 "Other relative")   , gen(cw1_relative)
recode cw1_relative 0 = 1 if  cw1_hhnumwh_4 == 1  | cw1_hhnumwh_6 == 1 | cw1_hhnumwh_7 == 1

tab cw1_hhnumwh_6 cw1_relative

*other 
recode cw1_hhnumwh_8 (2 = 0 "No other")   (1 = 1 "Other")   , gen(cw1_other)
recode cw1_other 0 = 1 if  cw1_hhnumwh_9 == 1

label define hh_comp 0 "alone" 1 "Partner" 2 "Partner & children" 3 "lone parent" 4 "other person" 
gen hh_comp = cw1_partner 
recode hh_comp -8 = . 
recode hh_comp  0 = 3 1 = 2 if cw1_child == 1 
recode hh_comp  0 = 4 if cw1_relative == 1
recode hh_comp  0 = 4 if cw1_other == 1 
label values hh_comp hh_comp

tab cw1_hhnum 
tab cw1_hhnumwh_1 
tab cw1_hhnumwh_2 
tab cw1_hhnumwh_3 
tab cw1_hhnumwh_4 
tab cw1_hhnumwh_5 
tab cw1_hhnumwh_6 
tab cw1_hhnumwh_7 
tab cw1_hhnumwh_8 
tab cw1_hhnumwh_9 
tab cw1_numrooms


tab cw1_hhnum



* Self-rated health 
clonevar sr_health = cw1_ghqprecovid
recode sr_health -8 = . 

* mental health
* -------------
recode n9khpb08 2 = 0, gen(mental_health)
label define mental_health 0 "No" 1 "Yes"
label values mental_health mental_health 

* keyworker at any point (across the pandemic - nonemployed are sep category)
* ---------------------------------------------------------------------------
* RJS not I revised thos code to be first incidence of keyworker and theen use later waves
* to fill in small number of misisng. 
* Ideally you would it defined pre-pandemic but also the concept did not exist prior this is the subtitute. 




* only asked of employed people so you may want to create additional category for non-employed
cap drop keyworker
gen keyworker = cw1_keyworkerd 
replace keyworker = cw2_keyworkerd if keyworker==.
replace keyworker = cw3_keyworkerd if keyworker ==. 
recode keyworker (2=0)
recode keyworker . = 0 
codebook keyworker



* shielded at any point
* ---------------------
* Shielding has smilar issues to keyworker up switching to using the first wave to be consistent with USoc. 
* 



forvalues j = 1 (1) 3 {

    recode cw`j'_shield (-10/-1 = .)
    
}  

gen shield = cw1_shield
replace shield = cw2_shield if shield ==.
replace shield = cw3_shield if shield ==.
recode shield (2=0)
tab shield


* health conditions 
* ------------------

* Cancer
gen cancer = cw1_lli_1
replace cancer = cw2_lli_1 if cancer ==.
replace cancer = cw3_lli_1 if cancer ==.
recode cancer (2=0) (-8/-1=.)
tab cancer

* Heart
gen heart = cw1_lli_10
replace heart = cw2_lli1_10 if heart ==.
replace heart = cw3_lli1_10 if heart ==.
recode heart (2=0) (-9/-1=.)
tab heart

* Depression
cap drop depression
gen depression = 1 if cw1_lli_16 ==1 | cw1_lli_11 ==1
replace depression = 1 if cw2_lli2_5 ==1 | cw2_lli2_1 ==1 
replace depression = 1 if cw3_lli2_5 ==1 | cw3_lli2_1 ==1 
recode depression . = 0 if cw1_lli_16 == 2
tab depression

* Obesity
gen obesity = cw1_lli_12
replace obesity = cw2_lli2_2 if obesity ==.
replace obesity = cw3_lli2_2 if obesity ==.
recode obesity (2=0) (-9/-1=.)
tab obesity

* respiratory
gen respiratory = 1 if cw1_lli_2 ==1 | cw1_lli_3 ==1 | cw1_lli_4 ==1 | cw1_lli_5 ==1 | cw1_lli_1 ==13 
replace respiratory = 1 if cw2_lli1_2 ==1 | cw2_lli1_3 ==1 | cw2_lli1_4 ==1 | cw2_lli1_5 ==1
replace respiratory = 1 if cw3_lli1_2 ==1 | cw3_lli1_3 ==1 | cw3_lli1_4 ==1 | cw3_lli1_5 ==1
recode respiratory . = 0 if cw1_lli_2 == 2
tab respiratory

*Additional changes 
clonevar shielding = shield 
clonevar obese = obesity


tab1 employment_status econ_act age covid_sr sex education education_bin ethnicity  ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding

gen id_string = ncdsid

* Files to keep 
keep /// 
id_string cohort_id    study_selection age_selection  employment_selection  employment_status econ_act covid_sr age sex education education_bin ethnicity ethnicity1 ethnicity_bin country hh_comp nssec7 sr_health keyworker mental_health shielding

save  "$destination\NCDS_analytic_sample_05May23.dta" , replace 



global destination "D:\Data\ARQ7P4\Derived"
use  "$destination\ELSA_analytic_sample_15May23.dta", clear
append using  "$destination\USoc_analytic_sample_05May23.dta" 
append using  "$destination\nextsteps_analytic_sample_05May23.dta" 
append using  "$destination\BCS70_analytic_sample_05May23.dta" 
append using  "$destination\NCDS_analytic_sample_05May23.dta" 




*Recoding education and employment status as missing. 
tab nssec7, miss
recode nssec7 . = 9
tab education, miss
recode education . = 4
recode employment_status 1 = 0 2 = 1 , gen(employm)

encode cohort_id, gen(cohort_num)



*** Due to the crude nature of ethnicity for some cohorts plus a comparatively high number missing it is being dropped from these analyses. 

*Note this is dropping ethnicity which currently causes problems ethnicity
gen in_analyses = 1 
foreach var in covid_sr age sex hh_comp country shielding sr_health nssec7 education cohort_num  {
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


*EThnicity too smal to stratify 


*Ns-SEc
recode nssec7 (1/2 = 0 "Higher") (3/7 = 1 "Intermediate & lower") (9 = .), gen(nssec2)

recode sr_health (1/3 = 0 "Excellent or Good") (4/5 = 1 "Fair or poor")  , gen(sr_bin)

*** Age variable for descriptive 
egen age_cat = cut(age), at(25, 30, 40,  50, 60, 67)


tab sex, miss

tab cohort_num, miss
keep if study_selection == 1
tab sex, miss
keep if age_selection == 1
tab sex, miss
keep if employment_selection == 1
tab sex, miss
keep if in_analyses == 1
tab sex, miss

gen age_10 = age/10


tab econ_act employm, miss


*** Descriptive statistics 
tab1 econ_act employm covid_sr  age_cat sex hh_comp  shielding sr_health nssec7 education cohort_num country if in_analyses==1, miss


*** Economic activity unadjusted 
logistic econ_act i.covid_sr  if in_analyses==1

**** Economic activity full model 
logistic econ_act i.covid_sr c.age##c.age i.sex i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num i.country if in_analyses==1


**** employment status unadjusted 
logistic employm i.covid_sr  if in_analyses==1

**** employment status full model 
logistic employm i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding i.sr_health i.nssec7 i.education i.cohort_num i.country  if in_analyses==1




***** England only 

*** Economic activity unadjusted 
logistic econ_act i.covid_sr  if in_analyses==1  & country == 1

**** Economic activity full model 
logistic econ_act i.covid_sr c.age##c.age i.sex i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & country == 1


**** employment status unadjusted 
logistic employm i.covid_sr  if in_analyses==1 & country == 1

**** employment status full model 
logistic employm i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding i.sr_health i.nssec7 i.education i.cohort_num i.country  if in_analyses==1 & country == 1


****************** Stratified analyses

* Economic activity 
**** Age stratified 
logistic econ_act i.covid_sr  i.sex              i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & age_bin == 0


logistic econ_act i.covid_sr  i.sex             i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & age_bin == 1





**** Sex stratified 
logistic econ_act i.covid_sr         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & sex == 0


logistic econ_act i.covid_sr         c.age##c.age  i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & sex == 1





**** NS_sec
logistic econ_act i.covid_sr i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 & nssec2 == 0


logistic econ_act i.covid_sr i.sex c.age##c.age  i.hh_comp i.shielding i.sr_health            i.education i.cohort_num if in_analyses==1 & nssec2 == 1


*** Sr health 
logistic econ_act i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 & sr_bin == 0
logistic econ_act i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 & sr_bin == 1




* Employment status 
**** Age stratified 
logistic employm i.covid_sr  i.sex              i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & age_bin == 0


logistic employm i.covid_sr  i.sex             i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & age_bin == 1





**** Sex stratified 
logistic employm i.covid_sr         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & sex == 0


logistic employm i.covid_sr         c.age##c.age  i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 & sex == 1





**** NS_sec
logistic employm i.covid_sr i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 & nssec2 == 0


logistic employm i.covid_sr i.sex c.age##c.age  i.hh_comp i.shielding i.sr_health            i.education i.cohort_num if in_analyses==1 & nssec2 == 1


*** Sr health 
logistic employm i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 & sr_bin == 0
logistic employm i.covid_sr c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 & sr_bin == 1



*** effects modification 

* Economic activity 
**** Age  
logistic econ_act i.covid_sr##c.age_bin  i.sex i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1
estimates store act_age_int
logistic econ_act i.covid_sr c.age_bin  i.sex i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1
estimates store act_age_base
lrtest act_age_int act_age_base



**** Sex  
logistic econ_act i.covid_sr##c.sex         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 
estimates store act_sex_int 
logistic econ_act i.covid_sr c.sex         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 
estimates store act_sex_base
lrtest act_sex_int act_sex_base


**** NS_sec
logistic econ_act i.covid_sr##nssec2 i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 
estimates store act_nssec_int 
logistic econ_act i.covid_sr nssec2 i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 
estimates store act_nssec_base
lrtest act_nssec_int act_nssec_base 


*** Sr health 
logistic econ_act i.covid_sr##sr_bin c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 
estimates store act_health_int
logistic econ_act i.covid_sr  sr_bin c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1
estimates store act_health_base
lrtest act_health_int act_health_base




* Employment status 
**** Age  
logistic employm i.covid_sr##c.age_bin  i.sex i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1
estimates store act_age_int
logistic employm i.covid_sr c.age_bin  i.sex i.hh_comp i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1
estimates store act_age_base
lrtest act_age_int act_age_base


**** Sex  
logistic employm i.covid_sr##c.sex         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 
estimates store act_sex_int 
logistic employm i.covid_sr c.sex         c.age##c.age  i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num if in_analyses==1 
estimates store act_sex_base
lrtest act_sex_int act_sex_base


**** NS_sec
logistic employm i.covid_sr##nssec2 i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 
estimates store act_nssec_int 
logistic employm i.covid_sr nssec2 i.sex c.age##c.age  i.hh_comp  i.shielding i.sr_health           i.education i.cohort_num if in_analyses==1 
estimates store act_nssec_base
lrtest act_nssec_int act_nssec_base 


*** Sr health 
logistic employm i.covid_sr##sr_bin c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1 
estimates store act_health_int
logistic employm i.covid_sr  sr_bin c.age##c.age i.sex i.hh_comp   i.shielding            i.nssec7 i.education i.cohort_num   if in_analyses==1
estimates store act_health_base
lrtest act_health_int act_health_base

*** Alternative age coding due to small coefficent sizes
egen zage = std(age)

**** Economic activity full model 
logistic econ_act i.covid_sr c.zage##c.zage i.sex i.hh_comp  i.shielding i.sr_health i.nssec7 i.education i.cohort_num i.country if in_analyses==1




**** employment status full model 
logistic employm i.covid_sr c.zage##c.zage i.sex i.hh_comp   i.shielding i.sr_health i.nssec7 i.education i.cohort_num i.country  if in_analyses==1

