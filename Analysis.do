****Priject: life course and cognitive aging
****Author:  Siyun Peng
****Date:    2023/04/17
****Version: 18
****Purpose: Data Analysis


cd "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Distal and proximal\P2P\results"


***************************************************************
**# 1 P2P clean
***************************************************************


use "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Stata Files\demographics",clear
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Stata Files\P2P_clean_3G",keepusing(netsize diverse density b1density minstrength efctsize sole pkin) nogen
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Stata Files\beliefs_attitudes_other_scales",nogen  //ace
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Stata Files\mental_health",nogen //mental_health
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Stata Files\cognitive_function",nogen  //baseline cognitive_function
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\Final Weights\P2P_panel_weights.dta",nogen //weights
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\55+\Data Files\Stata\9 P2P Cog55 scoring", 
keep if _merge==3 //all 55+ samples matched with baseline P2P; keep 688 matched
drop _merge
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\55+\Data Files\Stata\2 P2P Cog55 demographics",nogen keepusing(age time_beg_dem yrs_school)
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\55+\Data Files\Stata\5 P2P Cog55 numbers and fluency",nogen keepusing(animals veggies)
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\55+\Data Files\Stata\8 P2P Cog55 activities reyd",nogen 
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\P2P data\55+\Data Files\Stata\6 P2P Cog55 lifestyle and cognitive",nogen 

recode CHECKERS_FREE SAUCER_FREE TELEGRAM_FREE REDCROSS_FREE GAME_CUED DISH_CUED MESSAGE_CUED ORG_CUED (2 96=0)
egen words_free=rowtotal(CHECKERS_FREE SAUCER_FREE TELEGRAM_FREE REDCROSS_FREE),mi
egen words_cued=rowtotal(GAME_CUED DISH_CUED MESSAGE_CUED ORG_CUED),mi
gen words=2*words_free+words_cued
lab var words "Memory impairment screen"

gen year=year(MENTAL_HEALTH_CAT_SA_FINISH)
lab var year "P2P baseline year"

recode GENDER_ID (1=0) (2 3=1),gen(female) //male at birth transgener to female
label var female "Women"

recode EDUCATION (1=1) (2=2) (3 4=3) (5=4),gen(edu)
lab define edu2 1 "Less than HS" 2 "HS or GED" 3 "Some college" 4 "College"
lab values edu edu2
lab var edu "Education"

fre RACE_*
gen white=1 if RACE_WHITE ==1
replace white=0 if RACE_OTHER ==1
replace white=0 if RACE_ASIAN ==1
replace white=0 if RACE_AMIND ==1
replace white=0 if RACE_NATHAW_PACIS ==1
replace white=0 if RACE_BLACK_AFAM ==1
replace white=0 if ETHNICITY ==1
lab define white 0 "non-White or Hispanic" 1 "White non-Hispanic" 
lab values white white 

recode digif_totalb digif_total (92=.)
egen attention=rowmean(digif_totalb digif_total) //Measures of forward and backward digit span (DS) are among the oldest and most widely used neuropsychological tests of attention and working memory (Richardson, 2007). The total number of lists reported correctly is combined across forward span (FS) and backward span (BS) to produce a Wechsler total correct score.

recode reydecorab_1_n* (89 92 96=.) 
egen rey_d=rownvals(reydecorab_1_n*)  //ssc inst egenmore
replace rey_d=. if reydere_1==92


	/*compute bridging ties*/
	

	sem (Bridging -> netsize diverse density minstrength efctsize sole) if netsize>0 & !missing(netsize), var(Bridging@1) method(mlmv) cov(e.netsize*e.density) cov(e.netsize*e.sole) cov(e.diverse*e.density) cov(e.netsize*e.diverse) cov(e.density*e.efctsize) cov(e.netsize*e.minstrength)
estat gof, stats(all)
estat mindices

predict bridging,latent(Bridging) 
label var bridging "Social bridging"

*adjust bridging when netsize=0 or missing
replace bridging=. if missing(netsize) 
sum bridging
local min=r(min)
replace bridging=`min' if netsize==0 //set bridging to minimum value when netsize=0



*standardized key variables
foreach x of varlist netsize diverse bridging rey_d attention {
	egen `x'_std =std(`x') 
}
label var bridging_std "Social bridging"
label var attention_std "Working memory" //digit span (can also be labeled as working memory)
label var rey_d_std "Episodic memory" //delayed Rey recall

*descriptive table
svyset psu [pw=wt_comb_age55], strata(strata) //psu and strata fix variance
desctable age white female married words i.year bridging rey_d attention, filename("descriptives") stats(svymean svysemean sd n) group(edu) listwise

foreach x of varlist age words bridging rey_d attention {
	reg `x' i.edu if !missing(age, white, female, words, year, bridging, rey_d, attention)
}
foreach x of varlist white female year {
	tab `x' edu if !missing(age, white, female, words, year, bridging, rey_d, attention),chi2
}


***************************************************************
//	#2 analysis
***************************************************************


/*weighted: often inflate the variance*/


drop if missing(rey_d_std)
svyset psu [pw=wt_comb_age55], strata(strata) //psu and strata fix variance

sum netsize diverse b1density minstrength efctsize sole if bridging_std>1
sum netsize diverse b1density minstrength efctsize sole if bridging_std<-1
svy: tab edu

eststo clear
foreach y of varlist rey_d_std attention_std {
eststo `y': svy: reg `y' age white female i.year c.bridging_std##i.edu
eststo `y'2: svy: reg `y' age white female words i.year c.bridging_std##i.edu
}
esttab rey_d_std attention_std rey_d_std2 attention_std2 using "reg.csv",label replace b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted noconstant



svy: reg rey_d_std age white female i.year c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("Social bridging") recastci(rarea) ciopt(color(%10)) saving("plot1",replace)
svy: reg attention_std age white female i.year c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Working memory") xtit("Social bridging") recastci(rarea) ciopt(color(%10)) saving("plot2",replace)

grc1leg "plot1" "plot2", legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bridging_weight.tif", replace

svy: reg rey_d_std age white female words i.year c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("Social bridging") recastci(rarea) ciopt(color(%10)) saving("plot1",replace)
svy: reg attention_std age white female words i.year c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Working memory") xtit("Social bridging") recastci(rarea) ciopt(color(%10)) saving("plot2",replace)

grc1leg "plot1" "plot2" , legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bridging_weight_long.tif", replace


*Mediation: no mediation
*rey
eststo clear
eststo rey1: mediate (rey_d_std age white female i.year) (bridging_std age white female i.year) (edu) [pw=wt_comb_age55],control(4)

eststo rey2: mediate (rey_d_std age white female i.year words) (bridging_std age white female i.year) (edu) [pw=wt_comb_age55],control(4) //adjust for baseline

*attention
eststo att1: mediate (attention_std age white female i.year) (bridging_std age white female i.year) (edu) [pw=wt_comb_age55],control(4)

eststo att2: mediate (attention_std age white female i.year words) (bridging_std age white female i.year) (edu) [pw=wt_comb_age55],control(4) //adjust for baseline

esttab * using "mediate.csv",replace b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted noconstant

/*rwrmed
rwrmed rey_d_std [pw=wt_comb_age55], avar(edu) mvar(bridging_std) cvar(age white female year) cat(year) a(4) astar(1) m(0) mreg(reg)
*/




***************************************************************
//	#3 Sensitivity analysis
***************************************************************




/*GSEM to model 2 outcomes simultaneously*/


*gsem: sem not used because it does not support i. and ## function therefore cannot manually create Bridging##edu
svyset psu [pw=wt_comb_age55], strata(strata) //psu and strata fix variance
eststo clear
eststo m1 : svy: gsem (rey_d_std <- age white female i.year c.bridging_std##i.edu) ///
                  (attention_std <- age white female i.year c.bridging_std##i.edu)
eststo m2 : svy: gsem (rey_d_std <- age white female i.year words c.bridging_std##i.edu)                  (attention_std <- age white female i.year words c.bridging_std##i.edu)
esttab m1 m2 using "sem.csv",label replace b(%5.3f) se(%5.3f) r2 nogap compress nonum noomitted noconstant


	
/*Precovid only*/


gen covid=1
replace covid=0 if MENTAL_HEALTH_CAT_SA_FINISH<date("20200315","YMD")

eststo clear
foreach y of varlist rey_d_std attention_std {
eststo `y' :	svy: reg `y' age white female i.covid c.bridging_std##i.edu 
eststo `y'2 :	svy: reg `y' age white female words i.covid c.bridging_std##i.edu
}
esttab rey_d_std attention_std rey_d_std2 attention_std2 using "reg.csv",label append b(%5.3f) se(%5.3f) r2 nogap compress nonum noomitted noconstant



/*unweighted*/


eststo clear
foreach y of varlist rey_d_std attention_std {
eststo `y' :	reg `y' age white female i.year c.bridging_std##i.edu,vce(robust)
eststo `y'2 :	reg `y' age white female words i.year c.bridging_std##i.edu,vce(robust)

}
esttab rey_d_std attention_std  rey_d_std2 attention_std2 using "reg.csv",label append b(%5.3f) se(%5.3f) r2 nogap compress nonum noomitted noconstant


/*outliers*/


reg rey_d_std age white female words i.year c.bridging_std##i.edu
predict cooks_d1, cooksd
scatter cooks_d1 bridging_std, title("Episodic memory") saving("f1",replace) //4/n
scatter cooks_d1 edu, title("Episodic memory") saving("f2",replace) //4/n

reg attention_std age white female words i.year c.bridging_std##i.edu
predict cooks_d2, cooksd
scatter cooks_d2 bridging_std , title("Working memory") saving("f3",replace) //4/n
scatter cooks_d2 edu , title("Working memory") saving("f4",replace) //4/n

graph combine "f1" "f2" "f3" "f4", title("Influential: Cook's D>1 " "Moderately influential: Cook's D>0.0058")
graph export "outlier.png",replace

twoway (scatter rey_d_std bridging_std if edu==1) ///
       (lfit rey_d_std bridging_std if edu==1), ///
       legend(order(1 "Data" 2 "Fitted Line")) ///
       title("Scatter Plot <HS") ///
       ytitle("Episodic memory") ///
       xtitle("Bridging") legend(off) saving("f1",replace)

twoway (scatter attention_std bridging_std if edu==1) ///
       (lfit attention_std bridging_std if edu==1), ///
       legend(order(1 "Data" 2 "Fitted Line")) ///
       title("Scatter Plot <HS") ///
       ytitle("Working memory") ///
       xtitle("Bridging") legend(off) saving("f2",replace)
graph combine "f1" "f2"
graph export "outlier.png",replace



/*bridging distribution for lower education*/


hist bridging_std, by(edu) percent
graph export "hist_bridging.png",replace


