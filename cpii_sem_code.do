**** Replication code for regression analyses for "Commercial NCD policies and Corporate Political Influence from 2015 to 2020"                                   ****
**** Authors: Luke Allen, Hampus Holmer, and Simon Wigley                                                                                                         ****
**** Updated: 5 September 2021                                                                                                                                    ****
**** Data and code not to be used or cited without permission of the authors                                                                                      ****

set more off
clear

***Stata version 14.2***
ssc install fsum, replace
ssc install corrtable, replace
ssc install heatplot, replace
ssc install palettes, replace
ssc install colrspace, replace

use "cpii_inputs_panel.dta" 
xtset country1 year

***Input summary statistics (excluding micro states)*** 
fsum v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow idea_oecd_bin,  s(n  mean sd  min max p25 median p75 abspct) label

***Input correlation matrix***
eststo clear
estpost correlate v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow idea_oecd_bin, matrix listwise
eststo correlation
esttab correlation using correl_sem.csv, unstack not noobs compress nogap label star(* 0.05 ** 0.01 *** 0.001) title("Input correlation matrix") replace 

***corrtable**
corrtable v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow idea_oecd_bin, flag1(abs(r(rho)) > 0.8) howflag1(mlabsize(*7)) flag2(inrange(abs(r(rho)), 0.6, 0.8)) howflag2(mlabsize(*6)) half

***heatmap***
qui correlate v2eldonate v2lgotovst v2lgcrrpt v2elpubfin disc idea_privfin_narrow idea_oecd_bin
matrix C = r(C)
heatplot C, values(format(%9.3f)) color(hcl, diverging intensity(.6)) legend(off) aspectratio(1) lower nodiagonal  ylabel( ,labsize(vsmall)) xlabel( ,labsize(vsmall))

***Estimate latent using SEM (mlmv)***
sem (CPII -> v2eldonate, ) (CPII -> v2elpubfin, ) (CPII -> idea_privfin_narrow, ) (CPII -> disc, ) (CPII -> v2lgcrrpt, ) (CPII -> v2lgotovst, ), method(mlmv) standardized latent(CPII ) ///
cov(e.v2lgcrrpt*e.v2lgotovst e.idea_privfin_narrow*e.v2eldonate e.idea_privfin_narrow*e.v2lgotovst e.idea_privfin_narrow*e.v2lgcrrpt e.disc*e.v2lgotovst) nocapslatent

***Equation-level goodness of fit***
estat eqgof

***Overall goodness of fit***
estat gof, stats(all)

***Predict***
predict cpii_panel, latent(CPII)

***Invert***
egen max_cpii = max(cpii_panel)
gen cpii_panel_inv = max_cpii-cpii_panel

***Rescale 0-100***
egen max_cpii_inv = max(cpii_panel_inv)
gen cpii_panel_inv_100 = (cpii_panel_inv/max_cpii_inv)*100

***Estimate latent using SEM (mlmv) (w/ lobbying disclosure)***
sem (CPII_lobby -> v2eldonate, ) (CPII_lobby -> v2elpubfin, ) (CPII_lobby -> idea_privfin_narrow, ) (CPII_lobby -> disc, ) (CPII_lobby -> v2lgcrrpt, ) (CPII_lobby -> v2lgotovst, ) ///
(CPII_lobby -> idea_oecd_bin, ), method(mlmv) standardized latent(CPII_lobby ) cov( e.idea_privfin_narrow*e.v2eldonate e.v2lgcrrpt*e.v2eldonate e.v2lgcrrpt*e.idea_privfin_narrow e.v2lgcrrpt*e.disc e.v2lgotovst*e.v2eldonate ///
e.v2lgotovst*e.v2elpubfin e.v2lgotovst*e.idea_privfin_narrow e.v2lgotovst*e.disc e.v2lgotovst*e.v2lgcrrpt e.idea_oecd_bin*e.idea_privfin_narrow e.idea_oecd_bin*e.v2lgcrrpt) nocapslatent

***Predict***
predict cpii_panel_lobby_bin, latent(CPII_lobby)

***Invert***
egen max_cpii_lobby = max(cpii_panel_lobby_bin)
gen cpii_panel_lobby_bin_inv = max_cpii_lobby-cpii_panel_lobby_bin

***Estimate latent using SEM (mlmv) (w/o disclosure)***
sem (CPII_wodisc -> v2eldonate, ) (CPII_wodisc -> v2elpubfin, ) (CPII_wodisc -> idea_privfin_narrow, ) (CPII_wodisc -> v2lgcrrpt, ) (CPII_wodisc -> v2lgotovst, ), method(mlmv) standardized latent(CPII_wodisc ) ///
cov( e.idea_privfin_narrow*e.v2eldonate e.v2lgcrrpt*e.v2eldonate e.v2lgcrrpt*e.idea_privfin_narrow e.v2lgotovst*e.v2eldonate e.v2lgotovst*e.idea_privfin_narrow e.v2lgotovst*e.v2lgcrrpt) nocapslatent

***Predict***
predict cpii_panel_wodisc, latent(CPII_wodisc)

***Invert***
egen max_cpii_wodisc = max(cpii_panel_wodisc)
gen cpii_panel_wodisc_inv = max_cpii_wodisc-cpii_panel_wodisc

***Export results**
export excel country iso3 year cpii_panel_inv  cpii_panel_inv_100 cpii_panel_lobby_bin_inv cpii_panel_wodisc_inv using "cpii_sem_results", firstrow(variables) replace


exit
