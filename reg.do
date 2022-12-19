clear
cd "~/Desktop/jotarepos/voting/"
insheet using "data_to_stata.csv", clear

foreach var of varlist * {
cap replace `var' = "" if `var'=="NA"
}

destring playervote playerlama_star playervote_cond playerchange_vote playerbetteryeslag playertlag playervotelag grouppolicylag playerbiddelta total_yeslag best_respond playertlag playerbiddelta, replace force


encode sessioncode, gen(sn)
g player_unique = sn*100+playerid_in_group


drop if sessioncode=="49mqxl65" & subsessionround_number==9
g held_one = 0 
g held_none = 0

replace held_one = 1 if playertlag==0
replace held_none = 1 if playertlag<0

g positive_change = 0 
replace positive_change = 1 if playerbiddelta>0

g high_treatment = 0 
replace high_treatment=1 if groupuniforme==0

g high_treatment_none = held_none*high_treatment 
g high_treatment_one = held_one*high_treatment 

g cost35 = 0 
replace cost35= 1 if groupcosto==35

g cost35_none = held_none*cost35
g cost35_one = held_one*cost35

g pivo_pass = 0 
replace pivo_pass = 1 if total_yeslag==4
replace pivo_pass = 1 if total_yeslag==5

g pivo_pass_none = held_none*pivo_pass 

g pivo_reject = 0 
replace pivo_reject = 1 if total_yeslag==6
replace pivo_reject = 1 if total_yeslag==7


g pivo_reject_none = held_none*pivo_reject 


g trend_none = total_yeslag*held_none
g trend_one = total_yeslag*held_one

g total_yeslag2 = total_yeslag*total_yeslag


keep if groupcosto<60 & mod(marca, 2) == 0

bysort groupuniforme: spearman playerbid playerlama
bysort groupuniforme grouppolicy: spearman playerbid playerlama
bysort groupuniforme grouppolicy: spearman  playert playerlama


keep if groupcosto<60 & mod(marca, 2) == 0 & held_one==0


xtset sn

xtreg playerbiddelta held_none pivo_pass pivo_pass_none if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none pivo_reject pivo_reject_none if playerbetteryeslag==0 & grouppolicylag==1, fe 


keep if playerid_in_group==1 

reg groupprice grouppolicy if groupcosto==20 & groupuniforme==0, vce(cluster sessioncode)
reg groupprice grouppolicy if groupcosto==35 & groupuniforme==0, vce(cluster sessioncode)



reg groupprice grouppolicy if groupcosto==20 & groupuniforme==1, vce(cluster sessioncode)
reg groupprice grouppolicy if groupcosto==35 & groupuniforme==1, vce(cluster sessioncode)








reg playerbiddelta held_none if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
reg playerbiddelta held_none pivo_pass if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
reg playerbiddelta held_none pivo_pass pivo_pass_none if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)

reg playerbiddelta held_none pivo_reject pivo_reject_none if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)


reg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0 & cost35==0, vce(cluster player_unique)
reg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0 & cost35==1, vce(cluster player_unique)

reg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1 & cost35==0, vce(cluster player_unique)
reg playerbiddelta held_none held_one  total_yeslag if playerbetteryeslag==0 & grouppolicylag==1 & cost35==1, vce(cluster player_unique)




xtreg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, fe 
restore

preserve
keep if groupcosto<60 & mod(marca, 2) == 0 & groupuniforme==1

xtset sn
xtreg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, fe 

restore


xtset sn
xtreg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, fe 


keep if groupcosto<60 & mod(marca, 2) == 0 & groupuniforme==1 & cost35==1

xtset sn
xtreg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, fe 
restore

keep if groupcosto<60 & mod(marca, 2) == 0 & groupuniforme==1 & cost35==0

xtset sn
xtreg playerbiddelta held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, fe 

xtreg playerbiddelta held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, fe 
restore







xtreg playerbiddelta held_none held_one playerbetteryeslag if grouppolicylag==1, fe 
xtreg playerbiddelta held_none held_one playerbetteryeslag if grouppolicylag==0, fe 


xtreg playerbiddelta held_none held_one cost35 if playerbetteryeslag==0 & grouppolicylag==1, fe 
xtreg playerbiddelta held_none held_one cost35  if playerbetteryeslag==1 & grouppolicylag==0, fe 


xtreg playerbiddelta held_none held_one cost35 cost35_none cost35_one if playerbetteryeslag==0 & grouppolicylag==1, fe 

xtreg playerbid held_none held_one cost35 cost35_none cost35_one if playerbetteryeslag==0 & grouppolicylag==1, fe 



reg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0 & cost35==0, vce(cluster player_unique)
reg playerbiddelta held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0 & cost35==1, vce(cluster player_unique)






preserve 
keep if groupcosto<60 & mod(marca, 2) == 0 & groupuniforme==1
reg positive_change held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)

est store A1
reg positive_change held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
est store A2

reg positive_change held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)
est store A3
reg positive_change held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)
est store A4

reg positive_change held_none held_one total_yeslag  total_yeslag2 if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
reg positive_change held_none held_one total_yeslag total_yeslag2 if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)

restore 

preserve 
keep if groupcosto<60 & mod(marca, 2) == 0 & groupuniforme==0
reg positive_change held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
est store A5
reg positive_change held_none held_one total_yeslag if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
est store A6

reg positive_change held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)
est store A7
reg positive_change held_none held_one total_yeslag if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)
est store A8

reg positive_change held_none held_one total_yeslag  total_yeslag2 if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
reg positive_change held_none held_one total_yeslag total_yeslag2 if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)


restore 

esttab A1 A2 A3 A4  using tables_reg_voting.tex, p replace f ///
	stats(N r2)


esttab A5 A6 A7 A8  using tables_reg_voting_high.tex, p replace f ///
	stats(N r2)

reg positive_change held_none held_one minoria mino_none mino_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
reg positive_change held_none held_one mayoria mayo_none mayo_one if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)


reg positive_change held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)
*est store A1

reg positive_change held_none held_one if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)
*est store A2




reg positive_change held_none held_one high_treatment high_treatment_none high_treatment_one if playerbetteryeslag==0 & grouppolicylag==1, vce(cluster player_unique)

reg positive_change held_none held_one high_treatment high_treatment_none high_treatment_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)

reg positive_change held_none held_one cost35 cost35_none cost35_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique)

restore


logit positive_change held_none held_one if playerbetteryeslag==1 & grouppolicylag==0, vce(cluster player_unique) 
