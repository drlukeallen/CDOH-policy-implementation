**** Replication code for constructing Corporate Financial Influence Index                                                                                        ****

**** Article: "Assessing the association between corporate financial influence and implementation of policies to tackle                                           ****
**** commercial determinants of non-communicable diseases: a cross-sectional analysis of 172 countries"                                                           ****
**** Journal: Social Science & Medicine                                                                                                                           ****
**** Authors: Luke Allen, Simon Wigley, and Hampus Holmer                                                                                                         ****

**** Updated: 25 January 2022                                                                                                                                     ****
**** Data and code not to be used or cited without permission of the authors                                                                                      ****

set more off
clear

***Stata version 14.2***
ssc install fsum, replace
ssc install corrtable, replace
ssc install heatplot, replace
ssc install palettes, replace
ssc install colrspace, replace

use "cfii_inputs_panel.dta" 
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
sem (CFII -> v2eldonate, ) (CFII -> v2elpubfin, ) (CFII -> idea_privfin_narrow, ) (CFII -> disc, ) (CFII -> v2lgcrrpt, ) (CFII -> v2lgotovst, ), method(mlmv) standardized latent(CFII ) ///
cov(e.v2lgcrrpt*e.v2lgotovst e.idea_privfin_narrow*e.v2eldonate e.idea_privfin_narrow*e.v2lgotovst e.idea_privfin_narrow*e.v2lgcrrpt e.disc*e.v2lgotovst) nocapslatent

***Equation-level goodness of fit***
estat eqgof

***Overall goodness of fit***
estat gof, stats(all)

***Predict***
predict cfii_panel, latent(CFII)

***Invert***
egen max_cfii = max(cfii_panel)
gen cfii_panel_inv = max_cfii-cfii_panel

***Rescale 0-100***
egen max_cfii_inv = max(cfii_panel_inv)
gen cfii_panel_inv_100 = (cfii_panel_inv/max_cfii_inv)*100

***Estimate latent using SEM (mlmv) (w/ lobbying disclosure)***
sem (CFII_lobby -> v2eldonate, ) (CFII_lobby -> v2elpubfin, ) (CFII_lobby -> idea_privfin_narrow, ) (CFII_lobby -> disc, ) (CFII_lobby -> v2lgcrrpt, ) (CFII_lobby -> v2lgotovst, ) ///
(CFII_lobby -> idea_oecd_bin, ), method(mlmv) standardized latent(CFII_lobby ) cov( e.idea_privfin_narrow*e.v2eldonate e.v2lgcrrpt*e.v2eldonate e.v2lgcrrpt*e.idea_privfin_narrow e.v2lgcrrpt*e.disc e.v2lgotovst*e.v2eldonate ///
e.v2lgotovst*e.v2elpubfin e.v2lgotovst*e.idea_privfin_narrow e.v2lgotovst*e.disc e.v2lgotovst*e.v2lgcrrpt e.idea_oecd_bin*e.idea_privfin_narrow e.idea_oecd_bin*e.v2lgcrrpt) nocapslatent

***Predict***
predict cfii_panel_lobby_bin, latent(CFII_lobby)

***Invert***
egen max_cfii_lobby = max(cfii_panel_lobby_bin)
gen cfii_panel_lobby_bin_inv = max_cfii_lobby-cfii_panel_lobby_bin

***Estimate latent using SEM (mlmv) (w/o disclosure)***
sem (CFII_wodisc -> v2eldonate, ) (CFII_wodisc -> v2elpubfin, ) (CFII_wodisc -> idea_privfin_narrow, ) (CFII_wodisc -> v2lgcrrpt, ) (CFII_wodisc -> v2lgotovst, ), method(mlmv) standardized latent(CFII_wodisc ) ///
cov( e.idea_privfin_narrow*e.v2eldonate e.v2lgcrrpt*e.v2eldonate e.v2lgcrrpt*e.idea_privfin_narrow e.v2lgotovst*e.v2eldonate e.v2lgotovst*e.idea_privfin_narrow e.v2lgotovst*e.v2lgcrrpt) nocapslatent

***Predict***
predict cfii_panel_wodisc, latent(CFII_wodisc)

***Invert***
egen max_cfii_wodisc = max(cfii_panel_wodisc)
gen cfii_panel_wodisc_inv = max_cfii_wodisc-cfii_panel_wodisc

***Export results**
export excel country iso3 year cfii_panel_inv  cfii_panel_inv_100 cfii_panel_lobby_bin_inv cfii_panel_wodisc_inv using "cfii_sem_results", firstrow(variables) replace

***Corporate Financial Influence in 2015 and 2019***
tw (scatter cfii_panel_inv_100  L4.cfii_panel_inv_100 if year==2019,  mlabel(iso3) mlabsize(tiny) msym(oh) ytitle("CFII 2019") xtitle("CFII 2015") legend(off)) (line cfii_panel_inv_100 cfii_panel_inv_100)


exit
