/*
#delimit ;
odbc load, exec(
"SELECT parinfo.id AS participant_id, 
	parinfo.year_of_birth, 
	parinfo.gender, 
	parinfo.level_of_education, 
	parinfo.using_real_name_on_fb, 
	municipality.name, 
	CONCAT(YEAR(p.created), 
		' - ', 
		MONTHNAME(p.created)) AS month, 
	COUNT(p.id) AS count 
FROM fb_posts p, 
	fb_user_data ud, 
	fb_actors a, 
	fb_post_from_actors_rel pa, 
	fb_users u, 
	participants par, 
	participant_info parinfo, 
	x_municipalities municipality 
WHERE p.project_id = 61 
	AND p.is_story = 0 
	AND p.category = 'WALL' 
	AND p.id = pa.fb_post_id 
	AND pa.fb_actor_id = a.id 
	AND a.id = ud.self_fb_actor_id 
	AND ud.fb_user_id = u.id 
	AND u.id = par.fb_user_id 
	AND u.id = p.owner_id 
	AND par.participant_info_id = parinfo.id 
	AND parinfo.municipality_id = municipality.id 
	AND p.created < '2014-03-01' 
	AND (p.application IN (
			'Facebook for iPhone', 
			'Facebook for Android', 
			'Facebook for iPad', 
			'Facebook for Windows Phone', 
			'Windows Phone', 
			'Facebook for Every Phone', 
			'Blackberry Smartphones App', 
			'Sony Xperiaª smartphone', 
			'Facebook Exporter for iPhoto', 
			'Facebook mobile by Opentech ENG', 
			'Facebook for Windows', 
			'Facebook Toolbar for Firefox', 
			'Facebook for Blackberry 10', 
			'Birthday Calendar for iPhone', 
			'Chat for iPhone, iPad, iPod Touch', 
			'Huawei Social Phone', 
			'Facebook Messenger for Windows') 
		OR p.application IS NULL) 
GROUP BY parinfo.id, 
	parinfo.year_of_birth, 
	parinfo.gender, 
	parinfo.level_of_education, 
	parinfo.using_real_name_on_fb, 
	municipality.name, 
	month, 
	YEAR(p.created), 
	MONTH(p.created) 
ORDER BY parinfo.id LIMIT 100000000"
) dsn(dfootprints);
#delimit cr
*/

*use "/Users/anders1991/Google Drev/Digital Footprints/Data/wallposts.dta", clear
use "/Users/anders1991/Google Drive/Digital Footprints/Data/wallposts clean.dta", clear
graph drop _all


***********************
*** DATAFORMATERING ***
***********************

* Formatering af ID
generate id = real(participant_id)
drop participant_id

* Tilf¿jer slutmŒneder
set obs `=_N+1036' // udvider matrice
replace month = "2014 - February" if (month == "") // inds¾tter slutmŒned
replace id = _n - 33331 if (id == .) // parrer slutmŒned med id

duplicates tag id month, generate(tag) // tagger duplicates
drop if tag == 1 & count == "" // fjerner 565 duplicates
drop tag // fjerner duplicate tag variabel

* Formatering af tid
generate tid = monthly(month, "YM")
format %tmMonth_CCYY tid // tid med mm/yyyy
label variable tid "Date (month)"
drop month

* Udfyldning af data
xtset id tid //fasts¾tter panel og time vars
tsfill //inds¾tter udeladte tidsenheder


* Formatering af wallposts
generate posts = real(count)
label variable posts "Wallposts (count)"
recode posts (. = 0)
drop count

* Formatering af f¿dselsŒr
replace year_of_birth = L.year_of_birth if year_of_birth >= .
generate f¿dselsdag = year_of_birth + 182.5
format %td f¿dselsdag
label variable f¿dselsdag "Date of birth"
drop year_of_birth

* Formatering af k¿n
encode gender, gen(gender2)
replace gender2 = L.gender2 if gender2 >= . //udfylder udeladte mŒneder med foregŒende v¾rdi
drop gender
generate k¿n = gender2
recode k¿n (1 = 1) (2 = 0)
label variable k¿n "Gender (1=female)"
label define k¿n 0 "male" 1 "female"
label values k¿n k¿n
drop gender

* Formatering af uddannelse
encode level_of_education, gen(edu)
replace edu = L.edu if edu >= . 
drop level_of_education
generate udd = edu
recode udd (3 = 1) (2 = 2) (1 = 3)
label variable udd "Education (1=short)"
label define udd 1 "short" 2 "medium" 3 "long"
label values udd udd
drop edu

* Formatering af FB-navn
replace using_real_name_on_fb = L.using_real_name_on_fb if using_real_name_on_fb >= .
rename using_real_name_on_fb fakenavn
label variable fakenavn "fb-navn dummy"
label define fakenavnvl 0 "falsk" 1 "sandt"
label values fakenavn fakenavnvl

* Formatering af f¿dselsdag
generate f¿dt = yofd(f¿dselsdag)
label variable f¿dt "Date of birth (year)"

* Formatering af kommune
encode name, gen(kommune)
replace kommune = L.kommune if kommune >= .
label variable kommune "Municipality"
drop name

* Formatering af region
generate region = kommune
recode region (35 20 11 44 2 84 57 91 66 55 54 = .a)
recode region (51 80 75 37 92 65 58 82 15 73 33 67 42 3 74 62 38 72 = .b) // %Hedensted
recode region (88 8 90 13 89 47 29 18 86 1 83 94 81 14 60 63 46 59 56 6 = .c) // %Langeland %Fan¿
recode region (52 28 93 79 16 61 76 78 68 49 45 64 36 50 69 77 26 = .d)
recode region (27 31 17 34 30 21 5 41 70 22 12 7 32 24 23 53 40 4 25 71 10 39 87 43 85 48 19 9 = .e) // %Drag¿r
recode region (.a = 1) (.b = 2) (.c = 3) (.d = 4) (.e = 5)
label define regionvl 1 "Nordjylland" 2 "Midtjylland" 3 "Syddanmark" 4 "Sj¾lland" 5 "Hovedstaden"
label values region regionvl
label variable region "Region"

* Formatering af urban
generate urban = kommune
label variable urban "Urban (1=urban)"
recode urban (2 3 63 48 19 = 1) (1/1000 = 0) // Aalborg, Aarhus, Frederiksberg, KBH, Odense = 1
label define urbanvl 0 "rural" 1 "urban"
label values urban urbanvl

* Formatering af posts-dummy
generate dummyposts = posts
recode dummyposts (0 = 0) (1/99999 = 1)
label variable dummyposts "wallposts dummy"

* Formatering af alder og aldersgrupper
generate alder = yofd(dofm(tid))-f¿dt
label variable alder "Age (years)"

generate alder_kat = alder
label variable alder_kat "Age group (1= <15)"
recode alder_kat (60/999 = 4) (45/60 = 3) (30/45 = 2) (15/30 = 1) (0/15 = .)
label define alder_kat1vl 0 "<15" 1 "15-29" 2 "30-44" 3 "45-59" 4 "60+"
label values alder_kat alder_kat1vl

list id f¿dt tid alder if alder<15 // OBS: flere respondenter under 15!

drop if f¿dselsdag == . // fjerner tomme id
// tsappend - tilf¿jer data til tidsseriedatas¾t



**********************
*** DATAINSPEKTION ***
**********************
/*
* Overblik
*xtdescribe
xtsum

* Univariat fordelingsanalyse af kontrolvariable
xttab k¿n
xttab alder_kat
xttab udd
xttab region
xttab urban

* Migreringer mellem grupper
xttrans alder_kat

//grafisk fordeling per Œr??

* Tjek for outliers pŒ afh¾ngige variabel
by id: egen meanposts = mean(posts)
list id tid posts meanposts if meanposts>300 // OBS: flere id har over 300 posts/md i gennemsnit!
*/


**************************
*** FORUDS®TNINGSTESTS ***
**************************
*estat ovtest
*???
/*
graph hbar k¿n, over(alder, label(alt labsize(vsmall))) name(sk¾v_alderk¿n) // sk¾vhed i data: mange unge kvinder, mange ¾ldre m¾nd
*graph hbar urban, over(udd) //sk¾vhed i data: langtuddannede har st¿rre tendens til at bo i byer


xtline posts, overlay legend(off)
graph twoway (scatter posts tid) (lowess posts tid)
lowess posts tid

* Linearitet
preserve
xtdata posts tid, fe clear // see p. 249
graph twoway (scatter posts tid, msymbol(smcircle_hollow) msize(small) mlwidth(thin)) (lowess posts tid)
restore
*/

***************
*** ANALYSE ***
***************

scatter posts tid, msymbol(smcircle_hollow) msize(small) mlwidth(thin) ///
xlabel(, format(%tmCCYY) labsize(small)) ylabel(, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") ///
scheme(s2color) name(scatter_posts)

scatter posts tid if tid >= 636, msymbol(smcircle_hollow) msize(small) mlwidth(thin) ///
xlabel(, format(%tmm-Y) labsize(small)) ylabel(, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") ///
scheme(s2color) name(scatter_posts_last)

//H1: Danish FB-users post less wallposts over time 
xtpoisson posts tid, robust fe
margins, at(tid=(566(1)649))
marginsplot, ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) ///
name(xtpoisson_posts_re)

xtpoisson posts tid if tid >= 636, robust fe
margins, at(tid=(636(1)649))
marginsplot, ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) ///
name(xtpoisson_posts_re_last)

// Random vs fixed effects
xtpoisson posts tid i.alder_kat k¿n i.udd urban, fe
estimates store fixedpois
xtpoisson posts tid i.alder_kat k¿n i.udd urban, re
estimates store randompois
hausman fixedpois randompois

// overdispersion
summarize posts, detail // variance >> mean



*** Poisson regression
// all
xtpoisson posts tid k¿n i.alder_kat i.udd urban, robust re
margins, at(tid=(566(1)649)) predict(nu0)
marginsplot, ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_posts_re)

xtpoisson posts tid k¿n i.alder_kat i.udd urban if tid >= 636, robust re
margins, at(tid=(636(1)649)) predict(nu0)
marginsplot, ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_posts_re_last)

// k¿n
xtpoisson posts c.tid##k¿n i.alder_kat i.udd urban, re
margins i.k¿n, at(tid=(566(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_k¿n)

xtpoisson posts c.tid##k¿n i.alder_kat i.udd urban if tid >= 636, re
margins i.k¿n, at(tid=(636(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_k¿n_last)

// alder
xtpoisson posts c.tid##alder_kat k¿n i.udd urban, re
margins i.alder_kat, at(tid=(566(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_alder)

xtpoisson posts c.tid##alder_kat k¿n i.udd urban if tid >=636, re
margins i.alder_kat, at(tid=(636(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_alder_last)
// selection bias for 60+?

// udd
xtpoisson posts c.tid##udd i.alder_kat k¿n urban, re
margins i.udd, at(tid=(566(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_udd)

xtpoisson posts c.tid##udd i.alder_kat k¿n urban if tid >= 636, re
margins i.udd, at(tid=(636(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_udd_last)

// urban
xtpoisson posts c.tid##urban i.alder_kat k¿n i.udd, re
margins i.urban, at(tid=(566(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmCCYY) labsize(small)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_urban)

xtpoisson posts c.tid##urban i.alder_kat k¿n i.udd if tid >= 636, re
margins i.urban, at(tid=(636(1)649)) predict(nu0)
marginsplot, noci ///
recast(line) recastci(rline) ///
ciopts(lpattern(dash)) ///
xlabel(, format(%tmm-Y) labsize(small) angle(45)) ///
ylabel(0(1)10, labsize(small) angle(0)) ///
xtitle("") ytitle("Monthly wallposts") title("") ///
scheme(s2color) name(xtpoisson_urban_last)

*** Combined graphs
graph combine xtpoisson_k¿n xtpoisson_alder xtpoisson_udd xtpoisson_urban, name(combine_poisson_overall)
graph combine xtpoisson_k¿n_last xtpoisson_alder_last xtpoisson_udd_last xtpoisson_urban_last, name(combine_poisson_last)


*** Output overall regression table, robust
eststo m1, title("Model 1"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban, re irr robust
eststo m2, title("Model 2"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#k¿n, re irr robust
eststo m3, title("Model 3"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#alder_kat, re irr robust
eststo m4, title("Model 4"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#udd, re irr robust
eststo m5, title("Model 5"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#urban, re irr robust
estout m1 m2 m3 m4 m5 using regtable5.tex, eform ///
   cells(b (star fmt(3) label(Coef.)) se(par label(Std. err.))) ///
   stats(N, labels("N. of obs.")) ///
   mlabels("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
   label ///
   varlabels(_cons Constant) ///
   legend ///
   style(tex)

*** Output last year regression table, robust
eststo m1a, title("Model 1"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban if tid >= 636, re irr robust
eststo m2a, title("Model 2"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#k¿n if tid >= 636, re irr robust
eststo m3a, title("Model 3"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#alder_kat if tid >=636, re irr robust
eststo m4a, title("Model 4"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#udd if tid >= 636, re irr robust
eststo m5a, title("Model 5"): quietly xtpoisson posts tid k¿n i.alder_kat i.udd urban c.tid#urban if tid >= 636, re irr robust
estout m1a m2a m3a m4a m5a using regtable6.tex, eform ///
   cells(b (star fmt(3) label(Coef.)) se(par label(Std. err.))) ///
   stats(N, labels("N. of obs.")) ///
   mlabels("Model 1" "Model 2" "Model 3" "Model 4" "Model 5") ///
   label ///
   varlabels(_cons Constant) ///
   legend ///
   style(tex)
