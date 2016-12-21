use "/Users/anders1991/Google Drev/Arbejde/Digital Footprints/Data/diffusion_age_clean.dta", clear
graph drop _all

generate ppp = post/acc_firstact // posts per potential participant
label variable ppp "Monthly wallposts per participant (all)"
generate ppp1 = post1/acc_firstact1 // posts per potential participant; agecat1
label variable ppp1 "Monthly wallposts per participant (15-29)"
generate ppp2 = post2/acc_firstact2 // posts per potential participant; agecat2
label variable ppp2 "Monthly wallposts per participant (30-44)"
generate ppp3 = post3/acc_firstact3 // posts per potential participant; agecat3
label variable ppp3 "Monthly wallposts per participant (45-59)"
generate ppp4 = post4/acc_firstact4 // posts per potential participant; agecat4
label variable ppp4 "Monthly wallposts per participant (60+)"

tsset tid //fasts¾tter time var

* All
twoway (tsline acc_firstact actpar, yaxis(1)) ///
		(tsline ppp, yaxis(2)), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(200)1000, labsize(small) angle(0) axis(1)) ///
		ylabel(0(1)15, labsize(small) angle(0) axis(2)) ///
		xtitle("") ///
		ytitle("Participants", axis(1)) ///
		ytitle("Monthly wallposts", axis(2)) ///
		scheme(s2color) legend(size(small) symxsize(3)) ///
		name(all)

* 15-29
twoway (tsline acc_firstact1 actpar1, yaxis(1)) ///
		(tsline ppp1, yaxis(2)), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(50)300, labsize(small) angle(0) axis(1)) ///
		ylabel(0(1)15, labsize(small) angle(0) axis(2)) ///
		xtitle("") ///
		ytitle("Participants", axis(1)) ///
		ytitle("Monthly wallposts", axis(2)) ///
		scheme(s2color) legend(size(small) symxsize(3)) ///
		name(age1)

* 30-44
twoway (tsline acc_firstact2 actpar2, yaxis(1)) ///
		(tsline ppp2, yaxis(2)), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(50)300, labsize(small) angle(0) axis(1)) ///
		ylabel(0(1)15, labsize(small) angle(0) axis(2)) ///
		xtitle("") ///
		ytitle("Participants", axis(1)) ///
		ytitle("Monthly wallposts", axis(2)) ///
		scheme(s2color) legend(size(small) symxsize(3)) ///
		name(age2)

* 45-59
twoway (tsline acc_firstact3 actpar3, yaxis(1)) ///
		(tsline ppp3, yaxis(2)), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(50)300, labsize(small) angle(0) axis(1)) ///
		ylabel(0(1)15, labsize(small) angle(0) axis(2)) ///
		xtitle("") ///
		ytitle("Participants", axis(1)) ///
		ytitle("Monthly wallposts", axis(2)) ///
		scheme(s2color) legend(size(small) symxsize(3)) ///
		name(age3)

* 60+
twoway (tsline acc_firstact4 actpar4, yaxis(1)) ///
		(tsline ppp4, yaxis(2)), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(50)300, labsize(small) angle(0) axis(1)) ///
		ylabel(0(1)15, labsize(small) angle(0) axis(2)) ///
		xtitle("") ///
		ytitle("Participants", axis(1)) ///
		ytitle("Monthly wallposts", axis(2)) ///
		scheme(s2color) legend(size(small) symxsize(3)) ///
		name(age4)
		
		
* Gap between potential and active users

* All
generate gap = acc_firstact - actpar
label variable gap "Potential-active users gap (all)"
twoway (tsline gap), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(gapall)
		
* 15-29
generate gap1 = acc_firstact1 - actpar1
label variable gap1 "Potential-active users gap (15-29)"
twoway (tsline gap1), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(gap1)
		
* 30-44
generate gap2 = acc_firstact2 - actpar2
label variable gap2 "Potential-active users gap (30-44)"
twoway (tsline gap2), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(gap2)

* 45-59
generate gap3 = acc_firstact3 - actpar3
label variable gap3 "Potential-active users gap (45-59)"
twoway (tsline gap3), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(gap3)

* 60+
generate gap4 = acc_firstact4 - actpar4
label variable gap4 "Potential-active users gap (60+)"
twoway (tsline gap4), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(gap4)
		
twoway ///
		(tsline gap1) ///
		(tsline gap2) ///
		(tsline gap3) ///
		(tsline gap4), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) legend(size(vsmall) symxsize(3)) ///
		name(gap_total)

		
* Active percentage of participants
generate actpot = actpar/acc_firstact * 100
recode actpot (. = 0)
label variable actpot "Share active users of potential participants (all)"
twoway (tsline actpot), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(shareall)

generate actpot1 = actpar1/acc_firstact1 * 100
recode actpot1 (. = 0)
label variable actpot1 "Share active users of potential participants (15-29)"
twoway (tsline actpot1), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(share1)
		
generate actpot2 = actpar2/acc_firstact2 * 100
recode actpot2 (. = 0)
label variable actpot2 "Share active users of potential participants (30-44)"
twoway (tsline actpot2), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(share2)
		
generate actpot3 = actpar3/acc_firstact3 * 100
recode actpot3 (. = 0)
label variable actpot3 "Share active users of potential participants (45-59)"
twoway (tsline actpot3), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(share3)
		
generate actpot4 = actpar4/acc_firstact4 * 100
recode actpot4 (. = 0)
label variable actpot4 "Share active users of potential participants (60+)"
twoway (tsline actpot4), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) name(share4)

twoway ///
		(tsline actpot1) ///
		(tsline actpot2) ///
		(tsline actpot3) ///
		(tsline actpot4), ///
		xlabel(552(12)649, format(%tmCCYY) labsize(small)) ///
		ylabel(0(20)100, labsize(small) angle(0)) ///
		xtitle("") ///
		scheme(s2color) legend(size(vsmall) symxsize(3)) ///
		name(share_total)
