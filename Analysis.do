****Priject: Distal vs. proximal stimulating
****Author:  Siyun Peng
****Date:    2023/04/17
****Version: 17
****Purpose: Data Analysis


clear
cd "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Distal and proximal\P2P\results"


***************************************************************
**# 1 P2P clean
***************************************************************


use "C:\Users\peng_admin\OneDrive - Indiana University\P2P\55+\Data Files\Stata\9 P2P Cog55 scoring", clear
merge 1:1 record_id using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\55+\Data Files\Stata\5 P2P Cog55 numbers and fluency",nogen keepusing(animals veggies)
merge 1:1 record_id using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\55+\Data Files\Stata\6 P2P Cog55 lifestyle and cognitive",nogen 
merge 1:1 record_id using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\55+\Data Files\Stata\0 P2P Cog55 id crosswalk",nogen //record_id vs. SU_ID, keep 55+ sample
merge 1:1 SU_ID using "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\P2P\Age and network\data\P2P_clean_3G" 
recode _merge (1 2=0) (3=1),gen(cog55)
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\Stata Files\beliefs_attitudes_other_scales",nogen //ace
merge 1:1 SU_ID using "C:\Users\peng_admin\OneDrive - Indiana University\P2P\55+\Data Files\Stata\P2P_panel_weights",nogen //weights

keep if cog55==1 //keep cases done cog55+
recode GENDER_ID (1=0) (2=1) (3=.),gen(female)
label var female "Women"
lab define edu2 1 "Less than HS" 2 "HS or GED" 3 "Some college" 4 "College"
lab values edu edu2
fre RACE_*
gen race=RACE_WHITE 
replace race=6 if RACE_OTHER ==1
replace race=3 if RACE_ASIAN ==1
replace race=4 if RACE_AMIND ==1
replace race=5 if RACE_NATHAW_PACIS ==1
replace race=2 if RACE_BLACK_AFAM ==1
lab define race 1 "White" 2 "Black" 3 "Asian" 4 "Native American" 5 "Pacific Islander" 6 "Other"
lab values race race //688*0.7=481
tab race female if ETHNICITY==2
tab race female if ETHNICITY==1
fre race ETHNICITY if GENDER_ID==3 
recode mocatotal (92=.)
recode rnews_f cwpuzzles_f cardgame_f puzzlegame_f write_f museum_f memthink_f sew_f tv_f radio_f computer_f musicinstr_f famfriends_f visitors_f (92 97 98=.) (6=0) (5=1) (4=2) (3=3) (2=4) (1=5)
egen engage=rowmean(rnews_f cwpuzzles_f cardgame_f puzzlegame_f write_f museum_f memthink_f sew_f tv_f radio_f computer_f musicinstr_f famfriends_f visitors_f)

recode ACES* (92 97 98=.) (1=1) (2=0)
egen ace=rowmean(ACES*)

recode b1density (1=0) (.=.) (else=1),gen(sole)
lab var sole "Sole bridge status"

egen attention=rowmean(digif_totalb digif_total) //Measures of forward and backward digit span (DS) are among the oldest and most widely used neuropsychological tests of attention and working memory (Richardson, 2007). The total number of lists reported correctly is combined across forward span (FS) and backward span (BS) to produce a Wechsler total correct score.

/* interviewers type sentence like "I don't know" in the string, need to be manually cleaned
replace animals="" if animals=="92" | animals=="1" 
replace veggies="" if veggies=="92" | veggies=="1" 
split animals //split long string into one word string variables
split veggies
drop animals veggies
egen animals=rowsvals(animals*) //count unique string values (ssc install egenmore)
egen veggies=rowsvals(veggies*) 
recode animals veggies (0=.)
drop animals? animals?? veggies? veggies??
egen language=rowmean(animals veggies)
*/

recode COG_FUNCTION_* (97 98=.)
egen cci=rowmean(COG_FUNCTION_*)
label var cci "Cognitive complaint index"


	/*compute bridging ties*/
	

	sem (Bridging -> netsize diverse density minstrength efctsize sole) if netsize>0 & !missing(netsize), var(Bridging@1) method(mlmv) cov(e.netsize*e.density) cov(e.netsize*e.sole) cov(e.diverse*e.density) cov(e.netsize*e.diverse) cov(e.density*e.efctsize) cov(e.netsize*e.minstrength)
estat gof, stats(all)
estat mindices

predict bridging,latent(Bridging) 
label var bridging "Bridging capital"

*adjust bridging when netsize=0 or missing
replace bridging=. if missing(netsize) 
sum bridging
local min=r(min)
replace bridging=`min' if netsize==0 //set bridging to minimum value when netsize=0



	/*compute bonding ties*/
	
	
foreach x in listen care advice {
	gen n`x'=p`x'*netsize
}	
 
sem (Bonding -> pkin nlisten ncare nadvice mstrength) if netsize>0 & !missing(netsize), var(Bonding@1) method(mlmv) cov(e.pkin*e.mstrength)
estat gof, stats(all)
estat mindices

predict bonding,latent(Bonding) 
label var bonding "Bonding capital"

*adjust bonding when netsize=0 or missing
replace bonding=. if missing(netsize) 
sum bonding
local min=r(min)
replace bonding=`min' if netsize==0 //set bonding to minimum value when netsize=0


*descriptive table
svyset psu [pw=wt_comb_age55], strata(strata) //psu and strata fix variance
desctable age white female bridging bonding mocatotal_std reyadel_a6_total_std attention_std  cci_std, filename("descriptives") stats(svymean svysemean n) listwise group(edu)

*standardized key variables
foreach x of varlist netsize diverse bridging bonding mocatotal reyadel_a6_total attention cci  {
	egen `x'_std =std(`x') 
}
label var bridging_std "Bridging social capital"
label var bonding_std "Bonding social capital"
label var attention_std "Attention" //digit span (can also be labeled as working memory)
label var reyadel_a6_total_std "Episodic memory" //delayed Rey recall
label var cci_std "Cognitive complaint index"


***************************************************************
//	#2 analysis
***************************************************************



/*unweighted*/


eststo clear
foreach y of varlist mocatotal_std reyadel_a6_total_std attention_std  cci_std {
eststo `y'2 :	reg `y' age white female c.bridging_std##i.edu,vce(robust)
eststo `y':	reg `y' age white female c.bonding_std##i.edu,vce(robust)
}
esttab mocatotal_std reyadel_a6_total_std attention_std  cci_std mocatotal_std2 reyadel_a6_total_std2 attention_std2  cci_std2 using "reg.csv",label replace b(%5.3f) se(%5.3f) r2 nogap compress nonum noomitted noconstant

reg mocatotal_std age white female c.bridging_std##i.edu,vce(robust)
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Global cognition") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot1",replace)
reg reyadel_a6_total_std age white female c.bridging_std##i.edu,vce(robust)
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot2",replace)
reg attention_std age white female c.bridging_std##i.edu,vce(robust)
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Attention") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot3",replace)
reg cci_std age white female c.bridging_std##i.edu,vce(robust)
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Cognitive complaint index") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot4",replace)

grc1leg "plot1" "plot2" "plot3" "plot4", legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bridging.tif", replace


reg mocatotal_std age white female c.bonding_std##i.edu,vce(robust)
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Global cognition") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot1",replace)
reg reyadel_a6_total_std age white female c.bonding_std##i.edu,vce(robust)
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot2",replace)
reg attention_std age white female c.bonding_std##i.edu,vce(robust)
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Attention") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot3",replace)
reg cci_std age white female c.bonding_std##i.edu,vce(robust)
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Cognitive complaint index") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot4",replace)

grc1leg "plot1" "plot2" "plot3" "plot4", legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bonding.tif", replace



/*weighted: often inflate the variance*/


svyset psu [pw=wt_comb_age55], strata(strata) //psu and strata fix variance

eststo clear
foreach y of varlist mocatotal_std reyadel_a6_total_std attention_std  cci_std {
eststo `y'2:	 svy: reg `y' age white female c.bridging_std##i.edu
eststo `y': svy: reg `y' age white female c.bonding_std##i.edu
}
esttab mocatotal_std reyadel_a6_total_std attention_std  cci_std mocatotal_std2 reyadel_a6_total_std2 attention_std2  cci_std2 using "reg.csv",label append b(%5.3f) se(%5.3f) nogap r2 compress nonum noomitted noconstant



svy: reg mocatotal_std age white female c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Global cognition") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot1",replace)
svy: reg reyadel_a6_total_std age white female c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot2",replace)
svy: reg attention_std age white female c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Attention") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot3",replace)
svy: reg cci_std age white female c.bridging_std##i.edu
margins i.edu, at(bridging_std=(-2(1)2))
marginsplot, tit("") ytit("Cognitive complaint index") xtit("Bridging social capital") recastci(rarea) ciopt(color(%5)) saving("plot4",replace)

grc1leg "plot1" "plot2" "plot3" "plot4", legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bridging_weight.tif", replace


svy: reg mocatotal_std age white female c.bonding_std##i.edu
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Global cognition") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot1",replace)
svy: reg reyadel_a6_total_std age white female c.bonding_std##i.edu
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Episodic memory") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot2",replace)
svy: reg attention_std age white female c.bonding_std##i.edu
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Attention") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot3",replace)
svy: reg cci_std age white female c.bonding_std##i.edu
margins i.edu, at(bonding_std=(-2(1)2))
marginsplot, tit("") ytit("Cognitive complaint index") xtit("bonding social capital") recastci(rarea) ciopt(color(%5)) saving("plot4",replace)

grc1leg "plot1" "plot2" "plot3" "plot4", legendfrom("plot1") position(4) ring(1) imargin(0 0 0 0) ycommon 
graph export "plot_bonding_weight.tif", replace
  

  
  
