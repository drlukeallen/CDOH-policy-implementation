**** Replication code for regression analyses                                                                                                                     ****

**** Article: "Assessing the association between corporate financial influence and implementation of policies to tackle                                           ****
**** commercial determinants of non-communicable diseases: a cross-sectional analysis of 172 countries"                                                           ****
**** Journal: Social Science & Medicine                                                                                                                           ****
**** Authors: Luke Allen, Simon Wigley, and Hampus Holmer                                                                                                         ****
**** Updated: 25 January 2022                                                                                                                                     ****
**** Data and code not to be used or cited without permission of the authors                                                                                      ****

set more off
clear


use "comm_imp_panel.dta" 
xtset country1 year



***Stata version 14.2***
ssc install center, replace
ssc install coefplot, replace
ssc install estout, replace


***PRESERVE ORIGINAL***
preserve

***STANDARDIZE ALL VARIABLES***

center comm_pct tob_pct comm_imp non_comm_imp tob_imp_total alc_imp_total food_imp_total fat child_food_marketing salt sodium_m sodium_f IHS_alc_cons_2015 IHS_smok_tob_prev_2015 ln_hypertension_average ///
IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 cpi_indexaa v2eldonate cfii_panel_inv cfii_panel_lobby_bin_inv cfii_panel_wodisc_inv IHS_elf1 IHS_muslim00 sids landlocked_bin sahel cont_africa cont_asia cont_europe cont_oceania cont_north_america cont_south_america legor_gbr legor_fra legor_soc legor_deu legor_sca v2x_mpi v2x_corr ///
ln_gdppc_id_b2010 energy_kcal_unadj  IHS_mx_warterror_10years tax_gdp_hf ln_tax_gdp_hf ln_pop65_pct_wb ln_urban_un  fdi_inflow_gdp_wb ln_tax_gdp_hf /// 
, inplace standardize nolabel
		  
glo xlist "ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb v2x_mpi sids IHS_muslim00 IHS_elf1 legor_gbr legor_deu legor_soc legor_fra legor_sca cont_africa cont_asia cont_europe cont_north_america cont_south_america cont_oceania"

***COEFFICIENT PLOTS***

*****Regressions for commercial policies (with and without controls)****

***All policies***
xtreg comm_imp cfii_panel_inv yr*, r 
estimates store nocontrolsa

xtreg comm_imp cfii_panel_inv $xlist yr*, r 
estimates store controlsa

coefplot (nocontrolsa, label(Without controls) pstyle(p3)) ///
         (controlsa, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0)  ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall)) coeflabels(, wrap(25)) ///
		 legend(size(tiny)) legend(off) title("A. All commercial policies", size(small)) ///
		 saving(figure1a, replace)
		 
***Food policies***
xtreg comm_imp cfii_panel_inv yr*, r 
estimates store nocontrolsb

xtreg food_imp_total cfii_panel_inv $xlist yr*, r 
estimates store controlsb
  
coefplot (nocontrolsb, label(Without controls) pstyle(p3)) ///
         (controlsb, label(With controls) pstyle(p1))  ///
         , levels(95) drop(yr* _cons)  xline(0) ylabel(,labsize(tiny)) xlabel( ,labsize(tiny))  /// 
		 coeflabels(cfii_panel_inv = " " ln_gdppc_id_b2010 = " " ln_urban_un= " " ln_pop65_pct_wb= " " v2x_mpi= " " sids= " " /// 
IHS_muslim00= " " IHS_elf1= " " legor_gbr= " " legor_deu= " " legor_soc= " " legor_fra= " " legor_sca= " " cont_africa= " " cont_asia= " " cont_europe= " " cont_north_america= " " cont_south_america= " " cont_oceania= " ") ///
		 legend(size(small))  ///
		 title("B. Food policies", size(small)) legend( position(3) cols(1))  ///
		 saving(figure1b, replace)
		  
***Tobacco policies***
xtreg tob_imp_total cfii_panel_inv yr*, r 
estimates store nocontrolsc

xtreg tob_imp_total cfii_panel_inv $xlist yr*, r 
estimates store controlsc
 
coefplot (nocontrolsc, label(Without controls) pstyle(p3)) ///
         (controlsc, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0) ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall)) coeflabels(, wrap(25)) ///
		 legend(size(tiny)) legend(off) title("C. Tobacco policies", size(small)) ///
		 saving(figure1c, replace)

***Alcohol policies***
xtreg alc_imp_total cfii_panel_inv  yr*, r 
estimates store nocontrolsd
xtreg alc_imp_total cfii_panel_inv $xlist yr*, r 
estimates store controlsd
 
coefplot (nocontrolsd, label(Without controls) pstyle(p3)) ///
         (controlsd, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0) ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall))   ///
		 coeflabels(cfii_panel_inv = " " ln_gdppc_id_b2010 = " " ln_urban_un= " " ln_pop65_pct_wb= " " v2x_mpi= " " sids= " " /// 
IHS_muslim00= " " IHS_elf1= " " legor_gbr= " " legor_deu= " " legor_soc= " " legor_fra= " " legor_sca= " " cont_africa= " " cont_asia= " " cont_europe= " " cont_north_america= " " cont_south_america= " " cont_oceania= " ") ///
		 legend(size(small))  ///
		 title("D. Alcohol policies", size(small)) legend( position(3) cols(1)) ///
		 saving(figure1d, replace)
		 
****Combined figure****
gr combine figure1a.gph figure1b.gph figure1c.gph figure1d.gph, xcommon iscale(.5) title("Commercial Policies and Corporate Financial Influence", size(small)) /// 
note("               Notes: Coefficients produced using random effects regressions, with cluster robust standard errors, for 172 countries and the years 2015, 2017, and 2019. Spikes represent 95% CI.""               All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Coefficients for year dummies not reported.", size(tiny))  ///
subtitle("  ", size(tiny)) imargin(0 0 0 0) graphregion(margin(l=0 r=0)) 
graph export comm_coefplot.emf, replace


***RESULTS TABLES***

*****Regressions with random effects for commercial policies (with and without controls)****

eststo clear

xtset country1 year

eststo: xtreg comm_imp cfii_panel_inv yr*, r 
eststo: xtreg comm_imp cfii_panel_inv $xlist yr*, r 

eststo: xtreg food_imp_total cfii_panel_inv yr*, r 
eststo: xtreg food_imp_total cfii_panel_inv $xlist yr*, r 

eststo: xtreg tob_imp_total cfii_panel_inv yr*, r 
eststo: xtreg tob_imp_total cfii_panel_inv $xlist yr*, r 

eststo: xtreg alc_imp_total cfii_panel_inv yr*, r 
eststo: xtreg alc_imp_total cfii_panel_inv $xlist yr*, r 

***Table x***
esttab using comm_imp_table.csv, se  star(* 0.05 ** 0.01 *** 0.001) label title("Commercial Policies and Corporate Financial Influence") ///
order(cfii_panel_inv $xlist yr*) ///
 stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Robust standard errors in parentheses. Reference category for legal origin and continent is Scandinavian and Oceania.") nogap replace


 ***INDIVIDUAL POLICIES***
 *****Regressions with random effects for individual policies (with and without controls)****

eststo clear
  
xtset country1 year

eststo: xtreg salt cfii_panel_inv yr*, r 
eststo: xtreg salt cfii_panel_inv $xlist yr*, r 

eststo: xtreg fat cfii_panel_inv yr*, r 
eststo: xtreg fat cfii_panel_inv $xlist yr*, r 

eststo: xtreg child_food_marketing cfii_panel_inv yr*, r 
eststo: xtreg child_food_marketing cfii_panel_inv $xlist yr*, r 

eststo: xtreg milkcode cfii_panel_inv yr*, r 
eststo: xtreg milkcode cfii_panel_inv $xlist yr*, r

eststo: xtreg tob_tax cfii_panel_inv yr*, r 
eststo: xtreg tob_tax cfii_panel_inv $xlist yr*, r 

eststo: xtreg tob_advert cfii_panel_inv yr*, r 
eststo: xtreg tob_advert cfii_panel_inv $xlist yr*, r 

eststo: xtreg smoke_free cfii_panel_inv yr*, r 
eststo: xtreg smoke_free cfii_panel_inv $xlist yr*, r 

eststo: xtreg alc_sales cfii_panel_inv yr*, r 
eststo: xtreg alc_sales cfii_panel_inv $xlist yr*, r 

eststo: xtreg alc_advert cfii_panel_inv yr*, r 
eststo: xtreg alc_advert cfii_panel_inv $xlist yr*, r 

eststo: xtreg alc_tax cfii_panel_inv yr*, r 
eststo: xtreg alc_tax cfii_panel_inv $xlist yr*, r

***Table***
esttab using individual_imp_table.csv,  se  star(* 0.05 ** 0.01 *** 0.001) label title("Individual Policies and Corporate Financial Influence") ///
order(cfii_panel_inv $xlist  yr*) ///
 stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Robust standard errors in parentheses. Reference category for legal origin and continent is Scandinavian and Oceania.") nogap replace


 
 ***ROBUSTNESS CHECKS***

***Corporate permeation index (Galea & Lima)***
*****Coefplots for CPI ****

xtset country1 year

***All policies***
xtreg comm_imp cpi_indexaa yr*, r 
estimates store nocontrolsa

xtreg comm_imp cpi_indexaa $xlist yr*, r 
estimates store controlsa

coefplot (nocontrolsa, label(Without controls) pstyle(p3)) ///
         (controlsa, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0) ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall)) coeflabels(, wrap(25))  /// 
		 legend(size(tiny)) legend(off) ///
		 title("A. All commercial policies", size(small)) ///
		 saving(figure1a, replace)
		 
***Food policies***
xtreg comm_imp cpi_indexaa yr*, r 
estimates store nocontrolsb

xtreg food_imp_total cpi_indexaa $xlist yr*, r 
estimates store controlsb
  
coefplot (nocontrolsb, label(Without controls) pstyle(p3)) ///
         (controlsb, label(With controls) pstyle(p1))  ///
         , levels(95) drop(yr* _cons)  xline(0) ylabel(,labsize(tiny)) xlabel( ,labsize(tiny)) /// 
		 coeflabels(cpi_indexaa = " " ln_gdppc_id_b2010 = " " ln_urban_un= " " ln_pop65_pct_wb= " " v2x_mpi= " " sids= " " /// 
IHS_muslim00= " " IHS_elf1= " " legor_gbr= " " legor_deu= " " legor_soc= " " legor_fra= " " cont_africa= " " cont_asia= " " cont_europe= " " cont_north_america= " " cont_south_america= " ") ///
		 legend(size(small))  ///
		 title("B. Food policies", size(small)) legend( position(3) cols(1))  ///
		 saving(figure1b, replace)
		  
***Tobacco policies***
xtreg tob_imp_total cpi_indexaa yr*, r 
estimates store nocontrolsc

xtreg tob_imp_total cpi_indexaa $xlist yr*, r 
estimates store controlsc
 
coefplot (nocontrolsc, label(Without controls) pstyle(p3)) ///
         (controlsc, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0) ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall)) coeflabels(, wrap(25))  /// 
		 legend(size(tiny)) legend(off) ///
		 title("C. Tobacco policies", size(small)) ///
		 saving(figure1c, replace)

***Alcohol policies***
xtreg alc_imp_total cpi_indexaa  yr*, r 
estimates store nocontrolsd
xtreg alc_imp_total cpi_indexaa $xlist yr*, r 
estimates store controlsd
 
coefplot (nocontrolsd, label(Without controls) pstyle(p3)) ///
         (controlsd, label(With controls) pstyle(p1))  ///
         , drop(yr* _cons)  xline(0) ylabel(,labsize(vsmall)) xlabel(,labsize(vsmall))   /// 
		 coeflabels(cpi_indexaa = " " ln_gdppc_id_b2010 = " " ln_urban_un= " " ln_pop65_pct_wb= " " v2x_mpi= " " sids= " " /// 
IHS_muslim00= " " IHS_elf1= " " legor_gbr= " " legor_deu= " " legor_soc= " " legor_fra= " " cont_africa= " " cont_asia= " " cont_europe= " " cont_north_america= " " cont_south_america= " ") ///
		 legend(size(small))  ///
		 title("D. Alcohol policies", size(small)) legend( position(3) cols(1)) ///
		 saving(figure1d, replace)
		 
****Combined figure****
gr combine figure1a.gph figure1b.gph figure1c.gph figure1d.gph, xcommon iscale(.5) title("Commercial Policies and Corporate Permeation Index", size(small))  ///
note("               Notes: Coefficients produced using random effects regressions, with cluster robust standard errors, for 145 countriesand the years 2015, 2017, and 2019. Spikes represent 95% CI.""               All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Coefficients for year dummies not reported.", size(tiny))  ///
subtitle("  ", size(tiny)) imargin(0 0 0 0) graphregion(margin(l=0 r=0)) 
graph export comm_coefplot_cpi.emf, replace

***Results table for CPI***
eststo clear

xtset country1 year

eststo: xtreg comm_imp cpi_indexaa yr*, r 
eststo: xtreg comm_imp cpi_indexaa $xlist yr*, r 

eststo: xtreg food_imp_total cpi_indexaa yr*, r 
eststo: xtreg food_imp_total cpi_indexaa $xlist yr*, r 

eststo: xtreg tob_imp_total cpi_indexaa yr*, r 
eststo: xtreg tob_imp_total cpi_indexaa $xlist yr*, r 

eststo: xtreg alc_imp_total cpi_indexaa yr*, r 
eststo: xtreg alc_imp_total cpi_indexaa $xlist yr*, r 


***Table***
esttab using comm_imp_table_cpi.csv,  se  star(* 0.05 ** 0.01 *** 0.001) label title("Commercial Policies and Corporate Permeation Index") ///
order(cpi_indexaa $xlist  yr*) ///
 stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Robust standard errors in parentheses") nogap replace

*****Alernative cfii and controls****
eststo clear

xtset country1 year
eststo: xtreg comm_imp cpi_indexaa $xlist yr*, r 
eststo: xtreg comm_imp cfii_panel_inv $xlist yr*, r 
eststo: xtreg comm_imp cfii_panel_lobby_bin_inv $xlist yr*, r 
eststo: xtreg comm_imp cfii_panel_wodisc_inv $xlist yr*, r
eststo: xtreg comm_imp cfii_panel_inv v2x_corr $xlist  yr*, r
eststo: xtreg comm_imp cfii_panel_inv ln_tax_gdp_hf $xlist  yr*, r
eststo: xtreg comm_imp cfii_panel_inv IHS_mx_warterror_10years $xlist  yr*, r
eststo: xtreg non_comm_imp cfii_panel_inv $xlist yr*, r 


***Table x***
esttab using comm_imp_table_robust.csv,  se  star(* 0.05 ** 0.01 *** 0.001) label title("Robustness checks: Commercial Policies and Corporate Financial Influence") ///
order(cpi_indexaa cfii_panel_inv cfii_panel_lobby_bin_inv cfii_panel_wodisc_inv v2x_corr ln_tax_gdp_hf IHS_mx_warterror_10years $xlist  yr*) ///
 stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Robust standard errors in parentheses.") nogap replace

 ***Results tables for risk factors****
eststo clear

xtset country1 year

eststo: xtreg tob_imp_total IHS_smok_tob_prev_2015 yr*, r 
eststo: xtreg tob_imp_total IHS_smok_tob_prev_2015 cfii_panel_inv $xlist yr*, r  

eststo: xtreg fat IHS_obese_gho_2015 yr*, r 
eststo: xtreg fat IHS_obese_gho_2015 cfii_panel_inv $xlist yr*, r 

eststo: xtreg child_food_marketing IHS_ncd_bmi_plus2c_2015 yr*, r 
eststo: xtreg child_food_marketing IHS_ncd_bmi_plus2c_2015 cfii_panel_inv $xlist yr*, r 

eststo: xtreg salt ln_hypertension_average yr*, r 
eststo: xtreg salt ln_hypertension_average cfii_panel_inv $xlist yr*, r 

eststo: xtreg alc_imp_total IHS_alc_cons_2015 yr*, r 
eststo: xtreg alc_imp_total IHS_alc_cons_2015 cfii_panel_inv $xlist yr*, r 


***Table***
esttab using risk_table.csv,  se  star(* 0.05 ** 0.01 *** 0.001) label title("Risk factors and specific policies") ///
order(IHS_smok_tob_prev_2015  IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 ln_hypertension_average  IHS_alc_cons_2015 cfii_panel_inv $xlist  yr*) /// 
 stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Robust standard errors in parentheses") nogap replace

  
 ***RESTORE TO ORIGINAL***
restore

***BLAND-ALTMAN PLOT***
ssc install concord
xtset country1 year

***PRESERVE ORIGINAL***
preserve

***Random effects regressions for all years: Commercial policies***
xtreg comm_imp cfii_panel_inv ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb v2x_mpi cont_africa cont_asia cont_europe cont_north_america cont_south_america  /// 
sids IHS_muslim00 legor_gbr legor_deu legor_soc legor_fra IHS_elf1  yr* , r

predict predicted_comm 

***Bland-Altman plot for 2019 implementation: Commercial policies***
concord comm_imp predicted_comm if year==2019

gen diff = comm_imp-predicted_comm
gen avg = (comm_imp+predicted_comm)/2

su diff
local mdiff = r(mean)
local lrr = `mdiff' - 2*r(Var)^.5
local urr = `mdiff' + 2*r(Var)^.5
display "Mean difference=` mdiff'"
display "Reference Range= `lrr' to `urr'"

graph twoway (scatter diff avg, mlabel(iso3) mlabsize(tiny) msym(oh) jitter(4))  if year==2019, yline(`mdiff', lc(black) lwidth(vvthin)) yline(`lrr', lc(black) lpattern(dash) lwidth(vvvthin)) ///
yline(`urr', lc(black) lpattern(dash) lwidth(vvvthin)) xtitle(Average of actual and predicted implementation scores, size(small)) ///
ytitle("Difference between actual and" "predicted implementation scores", size(small)) title("Bland-Altman plot for commercial policies in 2019") ylab(-4(2)4) legend(off) scheme(s1color) /// 
note("Points above (below) the zero line do better (worse) than predicted. 95% of all points lie between the dashed lines.""Predicted values based on regression model with all covariates." ///
"The concordance correlation coefficient for actual vs predicted is 0.723 (95% CI 0.659 to 0.787, p<0.0001).", size(vsmall))
graph export baplot_2019_comm.pdf, replace


***RESTORE TO ORIGINAL***
restore


***QUANTITIES OF INTEREST***
preserve
gen ln_comm_imp = ln(comm_imp+1)
gen ln_cfii_panel_inv = ln(cfii_panel_inv+1)

***1 unit increase in cfii is associated with a x% change in commercial policies***
xtreg ln_comm_imp cfii_panel_inv ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb v2x_mpi sids IHS_muslim00 IHS_elf1 legor_gbr legor_deu legor_soc legor_fra cont_africa cont_asia cont_europe cont_north_america cont_south_america yr*, r 

di (exp(_b[cfii_panel_inv])-1)*100

matrix E = r(table)
matrix list E

gen ll2  =  E[5,1]
gen ul2  =  E[6,1]

di (exp(ll2)-1)*100
di (exp(ul2)-1)*100


***10% increase in cfii is associated with a x% change in commercial policies (ln)***
xtreg ln_comm_imp ln_cfii_panel_inv ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb v2x_mpi sids IHS_muslim00 IHS_elf1 legor_gbr legor_deu legor_soc legor_fra cont_africa cont_asia cont_europe cont_north_america cont_south_america yr*, r 

di ((1.10^_b[ln_cfii_panel_inv])-1)*100

matrix E = r(table)
matrix list E

gen ll3  =  E[5,1]
gen ul3  =  E[6,1]

di (1.10^ll3-1)*100
di (1.10^ul3-1)*100


restore

***DESCRIPTIVE STATISTICS***
ssc install fsum, replace
ssc install mdesc, replace

preserve
glo xlist "cfii_panel_inv ln_gdppc_id_b2010 ln_pop65_pct_wb ln_urban_un v2x_mpi IHS_elf1 IHS_muslim00 sids legor_gbr legor_fra legor_soc legor_deu legor_sca cont_africa cont_asia cont_europe cont_north_america cont_south_america cont_oceania"

***Summary statistics ***


***summarize (within and between)***
xtsum comm_imp food_imp_total tob_imp_total alc_imp_total salt fat child_food_marketing milkcode tob_tax smoke_free graphic_warnings tob_advert alc_sales alc_advert alc_tax ///
$xlist IHS_alc_cons_2015 IHS_smok_tob_prev_2015 ln_hypertension_average IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 cpi_indexaa v2x_corr

***summarize***
eststo clear
estpost summarize comm_imp food_imp_total tob_imp_total alc_imp_total salt fat child_food_marketing milkcode tob_tax smoke_free graphic_warnings tob_advert alc_sales alc_advert alc_tax ///
$xlist IHS_alc_cons_2015 IHS_smok_tob_prev_2015 ln_hypertension_average IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 cpi_indexaa v2x_corr, detail   
esttab using summ_table.csv, cells("count mean  sd min max p50 p25 p75") label title("Summary statistics for regression analyses") noobs replace



***missing***
ssc install mdesc, replace
mdesc comm_imp food_imp_total tob_imp_total alc_imp_total salt fat child_food_marketing milkcode tob_tax smoke_free graphic_warnings tob_advert alc_sales alc_advert alc_tax ///
$xlist IHS_alc_cons_2015 IHS_smok_tob_prev_2015 ln_hypertension_average IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 cpi_indexaa v2x_corr


***Variable correlation matrix***
set matsize 700
eststo clear

estpost correlate comm_imp food_imp_total tob_imp_total alc_imp_total $xlist IHS_alc_cons_2015 IHS_smok_tob_prev_2015 ln_hypertension_average IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 yr*, matrix listwise

eststo correlation

esttab correlation using correl_var.csv, unstack not noobs compress nogap label star(* 0.05 ** 0.01 *** 0.001) title("Variable correlation matrix") replace 
 
***Coefficient correlation matrix for baseline regression***
eststo clear
eststo: xtreg comm_imp $xlist  yr*, r

eststo: estat vce, corr
eststo: matrix V = r(V)
eststo: matrix list V

esttab matrix(V, fmt(3)) using correl_coef.csv, unstack not noobs compress nogap label title("Coefficient correlation matrix for baseline regression") star(* 0.05 ** 0.01 *** 0.001) replace
eststo clear

restore
 
***FURTHER ROBUSTNESS CHECKS***

***Regressions based on multiple imputaton of missing values***
***Unreported policy scores also imputed and aggregate implementation then calculated for each imputed data set****
***Auxiliary variables: fedu_yrs_pc_1519 ln_tax_gdp_hf plow_positive_crops ln_oilres_regav_2015 fdi_inflow_gdp_wb sodium_m sodium_f mean_bmi energy_kcal_unadj IHS_mx_warterror_10years ***
***landlocked_bin sahel dd_mode_201519 uhc_scaindex_2015 mp_protect mp_quit mp_warn mp_ban mp_tax mp_mm ***
***cfii inputs included****

preserve

mi set flong
mi xtset country1 year

mi register imputed ln_gdppc_id_b2010  IHS_alc_cons_2015  IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 /// 
mean_bmi_average ln_pop65_pct_wb IHS_muslim00 v2x_mpi v2x_corr  dd_mode_201519 uhc_scaindex_2015 ///
target_ms risk_factor_surveys_ms plan_ms tob_tax_ms tob_mass_media_ms alc_sales_ms alc_advert_ms alc_tax_ms salt_ms fat_ms child_food_marketing_ms milkcode_ms /// 
phys_mass_media_ms clinical_guide_ms cardio_therapies_ms ///
fedu_yrs_pc_1519 ln_tax_gdp_hf plow_positive_crops  ln_oilres_regav_2015 ///
cfii_panel_inv cpi_indexaa v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow fdi_inflow_gdp_wb 

mi impute mvn ln_gdppc_id_b2010  IHS_alc_cons_2015  IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 /// 
mean_bmi_average ln_pop65_pct_wb  IHS_muslim00 v2x_mpi v2x_corr  dd_mode_201519 uhc_scaindex_2015 ///
target_ms  risk_factor_surveys_ms plan_ms tob_tax_ms tob_mass_media_ms alc_sales_ms  alc_advert_ms alc_tax_ms salt_ms fat_ms child_food_marketing_ms milkcode_ms ///
phys_mass_media_ms clinical_guide_ms cardio_therapies_ms ///
fedu_yrs_pc_1519 ln_tax_gdp_hf plow_positive_crops  ln_oilres_regav_2015 ///
cfii_panel_inv cpi_indexaa v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow fdi_inflow_gdp_wb ///
= sodium_m sodium_f mean_bmi energy_kcal_unadj IHS_mx_warterror_10years IHS_smok_tob_prev_2015 /// 
cont_africa cont_asia cont_europe cont_north_america cont_south_america cont_oceania ln_urban_un IHS_elf1 landlocked_bin sahel sids legor_gbr legor_fra legor_soc legor_deu legor_sca yr1 yr2 /// 
mortality_data_ms smoke_free_ms graphic_warnings_ms tob_advert_ms comm_imp ///
mp_monitor mp_protect mp_quit mp_warn mp_ban mp_tax mp_mm, ///
add(30) replace rseed(837568) prior(ridge, df(0.1)) initmcmc(em, iterate(1000))

***Create new total commerical implementation variables for each imputed data set***
mi passive: gen comm_imp_mi =  smoke_free_ms + graphic_warnings_ms + tob_advert_ms + tob_mass_media_ms + alc_sales_ms + alc_advert_ms + alc_tax_ms + salt_ms + fat_ms /// 
+ child_food_marketing_ms + milkcode_ms

mi passive: gen food_imp_mi = salt_ms + fat_ms + child_food_marketing_ms + milkcode_ms

mi passive: gen tob_imp_mi = smoke_free_ms + graphic_warnings_ms + tob_advert_ms + tob_mass_media_ms

mi passive: gen alc_imp_mi = alc_sales_ms + alc_advert_ms + alc_tax_ms 


***Standardize variables***
qui foreach var in  comm_imp_mi food_imp_mi tob_imp_mi alc_imp_mi child_food_marketing_ms salt_ms fat_ms v2x_mpi cfii_panel_inv  ///
 IHS_smok_tob_prev_2015 IHS_alc_cons_2015 ln_hypertension_average IHS_obese_gho_2015 IHS_ncd_bmi_plus2c_2015 ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb  cont_africa cont_asia cont_europe /// 
cont_north_america cont_south_america cont_oceania sids IHS_muslim00 legor_gbr legor_deu legor_soc legor_sca ///
legor_fra IHS_elf1 yr1 yr2 yr3 {
mi passive: egen `var'_z=std(`var')
}





***Run regression on imputed datasets and combine results using Rubin rules: cfii***
glo xlist "ln_gdppc_id_b2010_z ln_urban_un_z ln_pop65_pct_wb_z v2x_mpi_z sids_z IHS_muslim00_z IHS_elf1_z legor_gbr_z legor_deu_z legor_soc_z legor_fra_z legor_sca_z cont_africa_z cont_asia_z cont_europe_z cont_north_america_z cont_south_america_z cont_oceania_z" 

eststo clear

eststo: mi est, post: xtreg comm_imp_mi_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r 

eststo: mi est, post: xtreg food_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r

eststo: mi est, post: xtreg tob_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r

eststo: mi est, post: xtreg alc_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r


esttab using comm_imp_mi.csv, se star(* 0.05 ** 0.01 *** 0.001) label title("Multiple imputation: Commercial policies and Corporate Financial Influence") ///
order(cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z) ///  
stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Robust standard errors in parentheses") nogap replace


***Run regression on imputed datasets and combine results using Rubin rules: Risk factors***
eststo clear
eststo: xtreg tob_imp_mi_z IHS_smok_tob_prev_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r 

eststo: xtreg fat_ms_z IHS_obese_gho_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r  

eststo: xtreg child_food_marketing_ms_z IHS_ncd_bmi_plus2c_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r 

eststo: xtreg salt_ms_z ln_hypertension_average_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r 
 
eststo: xtreg alc_imp_mi_z IHS_alc_cons_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r 


esttab using risk_mi.csv, se star(* 0.05 ** 0.01 *** 0.001) label title("Multiple imputation: Risk factors and specific policies") ///
order(IHS_smok_tob_prev_2015_z IHS_obese_gho_2015_z IHS_ncd_bmi_plus2c_2015_z ln_hypertension_average_z IHS_alc_cons_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z) ///  
stats(N_g r2_o, labels("Countries" "R-squared (overall)"))  addnote("Coefficients produced using random effects regressions. All variables have been standardized. Reference category for legal origin and continent is Scandinavian and Oceania. Robust standard errors in parentheses") nogap replace


**Calculate overall R-squared for each model***
eststo clear

mi query
local M = r(M)
scalar rsq = 0

***cfii***
qui mi xeq 1/`M': xtreg comm_imp_mi_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg food_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg tob_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg alc_imp_mi_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

***Risk factors***
qui mi xeq 1/`M': xtreg tob_imp_mi_z IHS_smok_tob_prev_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg fat_ms_z IHS_obese_gho_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg child_food_marketing_ms_z IHS_ncd_bmi_plus2c_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o) 
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg salt_ms_z ln_hypertension_average_z  cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq

qui mi xeq 1/`M': xtreg alc_imp_mi_z IHS_alc_cons_2015_z cfii_panel_inv_z $xlist yr1_z yr2_z yr3_z, r; ; scalar rsq = rsq + e(r2_o)
scalar rsq = rsq/`M'
di rsq


restore


*****Hausman test for fixed vs random effects  (if Prob>chi2 > 0.05 then use random effects) (see Wooldridge, 2012, p. 496)****
ssc install rhausman, replace

xtset country1 year
preserve

glo xlist "ln_gdppc_id_b2010 ln_urban_un ln_pop65_pct_wb v2x_mpi cont_africa cont_asia cont_europe cont_north_america cont_south_america sids cont_oceania IHS_muslim00 legor_gbr legor_deu legor_soc legor_fra legor_sca IHS_elf1"

qui xtreg food_imp_total cfii_panel_inv $xlist yr*, fe r
estimates store fixed

qui xtreg food_imp_total cfii_panel_inv $xlist yr*, r 
estimates store random

set seed 837568
rhausman fixed random, reps(200) cluster

restore

***Adjusted p-values for multiple testing***
net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/controls-option") replace

wyoung  comm_imp tob_imp_total alc_imp_total food_imp_total, cmd(xtreg OUTCOMEVAR cfii_panel_inv ln_gdppc_id_b2010 ln_pop65_pct_wb ln_urban_un v2x_mpi IHS_elf1 IHS_muslim00 sids legor_gbr legor_fra legor_soc ///
legor_deu legor_sca cont_africa cont_asia cont_europe cont_north_america cont_south_america cont_oceania yr*) ///
familyp(cfii_panel_inv)  cluster(country1) bootstrap(200) seed(837568)

matrix list r(table)


exit
