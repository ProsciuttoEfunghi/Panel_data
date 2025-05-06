use "SOEP.dta", clear

//******************************************************************************
//***********  RECODING VARIABLES NAMES ***************
//******************************************************************************

//SUBJECTIVE HEALTH
tab m11126 
gen m11126_inv = 6 - m11126 
gen Subjective_Health=m11126_inv 
drop m11126_inv
label define health_labels ///
    1 "Bad" ///
    2 "Poor" ///
    3 "Satisfactory" ///
    4 "Good" ///
    5 "Very Good", replace
label value Subjective_Health health_labels 
tab Subjective_Health, missing
//MARRIAGE VARIABLE
gen marriage = (d11104==1)
// CONTROL VARIABLE FOR DISABILITY (1 DISABLED 0 NOT)
gen condition= m11124

xtset pid syear 
bys pid (syear): gen wave = _n   //count the wages for each individual
bys pid: gen nwaves = _N  //count the years number for each individual
*keep if syear >= 2000 & syear <= 2019 
*xtdes // there are no individuals with gaps
keep if nwaves>1 // let's remove individuals who appear only once (one interview)
*xtdes // Check Results
xtsum l11101
	display r(sd_b)^2 / (r(sd_b)^2 + r(sd_w)^2) // ICC=0.99:  We can assume l11101 to be time-constant 
bys pid: egen region=min(l11101) // We assign the minimum person-year value to make the variable time-constant
	label define region ///
		1  "[1] Schleswig-Holstein" ///
		2  "[2] Hamburg" ///
		3  "[3] Lower Saxony" ///
		4  "[4] Bremen" ///
		5  "[5] North-Rhine-Westfalia" ///
		6  "[6] Hessen" ///
		7  "[7] Rheinland-Pfalz" ///
		8  "[8] Baden-Wuerttemberg" ///
		9  "[9] Bavaria" ///
		10 "[10] Saarland" ///
		11 "[11] Berlin" ///
		12 "[12] Brandenburg" ///
		13 "[13] Mecklenburg-Vorpommern" ///
		14 "[14] Saxony" ///
		15 "[15] Saxony-Anhalt" ///
		16 "[16] Thuringia", replace
		label value region region
		fre region
		xtsum region
			display r(sd_b)^2 / (r(sd_b)^2 + r(sd_w)^2) // Time-constant 
	
	*Unemployment variable (individual-level)
		gen unemp=.
			replace unemp=0 if pglfs==11 // Employed
			replace unemp=1 if pglfs==6 //  Registered unemployed 

		tabstat unemp [aw=phrf], by(region) // From this variable, we can create three groups of low-medium-high unemployment regions
		bys region: sum unemp [aw=phrf]
		
		tab unemp,m
		
		bys syear(l11101): egen unemp_mean_region_tv=mean(unemp)
		sort pid syear
		bys l11101: egen unemp_mean_region_tc=mean(unemp)
		sort pid syear
		
	*Average unemployment by region between 2000 and 2019
		*bys region(syear): egen unemp_mean_region_tc=mean(unemp)
		
		*xtsum unemp_region // Time-constant
		*bys region: sum unemp_region [aw=phrf]
		*list pid syear region unemp unemp_region unemp_region2 in 1/300, sepby(pid)
		*tabstat unemp_region [aw=phrf], by(region)
		sort pid syear
		
		list pid syear l11101 e11102 unemp_mean_region_tc unemp_mean_region_tv in 1/100, sepby(pid)

********************************************************************************
*** IDENTIFICATION OF MISSING DATA ON ONE OF THE TWO VARIABLES Y, X
********************************************************************************		
gen help=0
replace help=1 if missing(unemp, d11104)
tab help // person-years (rows) with missing info on at least 1 out of 2 variables
keep if help==0 // keep if all 2 variables are observed
sort pid syear
drop help

********************************************************************************
*** IDENTIFICATION OF UNEMPLOYED INDIVIDUALS AT THE FIRST INTERVIEW
********************************************************************************
bysort pid (syear): gen pynr = _n            
sort pid syear
gen help=0
replace help=1 if unemp==1 & pynr==1      
bysort pid (syear): replace help = sum(help)  
tab help
*list pid syear unemp pynr help in 1/100, sepby(pid) // CHECKING RESULTS
keep if help==0                               // all observations of those unemployed from 1st observation are dropped
drop help pynr
bysort pid (syear): gen pynr = _n             // calculate anew after each case exclusion

*list pid syear unemp pynr in 1/104, sepby(pid) //CHECKING RESULTS


********************************************************************************
*** IDENTIFICATION OF UNEMPLOYED INDIVIDUALS WHO RETURN TO EMPLOYMENT 
********************************************************************************
gen help=0 
replace help=1 if unemp==1            
bysort pid (syear): replace help=sum(help)   // for each person, flag following person-years
gen help_1 = (help>0 & unemp==0)          // help_1==1 if a person becomes employed again 
tab help_1                                    
bysort pid (syear): replace help_1=sum(help_1) // we flag all following person-years
tab help_1
sort pid syear
*list pid syear unemp help help_1 in 1/100, sepby(pid) //CHECKING RESULTS
keep if help_1==0                             // and drop them
drop help help_1 pynr
bysort pid (syear): gen pynr = _n          // calculate anew after each case exclusion
*list pid syear unemp in 1/100, sepby(pid) //CHECKING RESULTS

********************************************************************************
*** COUNT THE NUMBER OF GAPS IN THE INTERVIEWS
********************************************************************************
gen gap1 = .  
bys pid (syear): replace gap1 = (syear - syear[_n-1] > 1 & pid == pid[_n-1]) if _n > 1  // 
replace gap1 = 0 if gap1 == . 
bys pid: egen gap1_obs = max(gap1)
tab gap1_obs
drop if gap1_obs==1 
drop gap1_obs gap1
bysort pid (syear): gen pycount = _N 

********************************************************************************
*** REGRESSION FIXED EFFECT (FE)
********************************************************************************
// In regions with higher unemployment rates, does the unemplyment affect the subjective health in a less severe way? 
//HP : include an interaction H0: moderation of a macro factor (region where you live) HP of the project, test it and see if the coefficients are significant.

xtreg Subjective_Health i.unemp i.marriage i.condition , fe vce(cluster pid)
estimate store mod1

********************************************************************************
*** DUMMY IMPACT FUNCTION, PRE-POST TRANSICTION EFFECT
********************************************************************************
// verify the heterogeneity over time by applying it to the first mode
*	Description of health satisfaction pre-post event 
		//generate a dummy variable indicating the treatment (1=ever unemployed) and control (0=always employed) groups and compare it with the dummy on unemp/employed
	bysort pid: egen treat = max(unemp)
	sort pid syear
	label define treat 0 "Control" 1 "Treatment", replace
	label value treat treat
	tab unemp treat
	*list pid syear d11104 unemp treat in 1/57, sepby(pid) //CHECKING RESULTS
	
	gen unemployment=1 if unemp==1 & l.unemp==0
	tab unemployment

* Let's calculate the average response for the dependent variable across groups: non-treated (unemployed) vs treated (those who transition to unemployment)."
tabstat Subjective_Health, by(treat)

* Compute unemployment duration from employed to unemployed (t0)
		
		bysort pid: egen year_of_unemployment= min(syear) if unemp==1   // define year in employment for first time
		gen yrs_unemp = syear - year_of_unemployment if unemp==1         // duration is time since
		replace yrs_unemp = 0 if unemp==0    // duration is 0 if person is never became unemployed (control group)
		sort pid syear
		
		
		gen year_current_unmp = syear - yrs_unemp                           // current year or unemp year
		bysort pid: egen event_year = max(year_current_unmp) if treat==1  // unemp year among treated (ever unemployed)
		sort pid syear
		list pid syear unemp treat yrs_unemp year_current_unmp event_year in 1/1000, sepby(pid) 
		
		
	// Create step-function variable: event-centered time scale
gen t_unemployment = syear - event_year if treat == 1  // Calculate relative years for treatment group
recode t_unemployment min/-2 = -2  // Recode pre-unemployment years to reference group
recode t_unemployment 5/max=5   // Recode post-unemployment years >5 to category 5 (due to small sample size)
tab t_unemployment


gen t_unemployment_abs = t_unemployment + 2  // Convert to positive values
recode t_unemployment_abs . = 0  // Ref group: always employed
label define t_unemployment_abs ///
    0 "t-1" 1 "t0" 2 "t+1" 3 "t+2" 4 "t+3" 5 "t+4" 6 "t+5" 7 "t+6", replace
label value t_unemployment_abs t_unemployment_abs

xtreg Subjective_Health i.t_unemployment_abs i.marriage i.condition, fe vce(cluster pid)

margins t_unemployment_abs

marginsplot, recast(line) ciopts(color(blue))

*version 16: table unemp, content (mean Subjective_Health n Subjective_Health)
*					egen av_SH=mean(Subjective_Health) if unemp~=., by(unemp)
*					twoway scatter av_SH t_unemployment if t_unemployment>=-5 & t_unemployment<=10, ///
*						xline(0) xlabel(-5(1)10) ytitle("Subjective_Health") xtitle("Unemployment Timing")


// Summary statistics for health satisfaction by the time relative to unemployment
*tabstat Subjective_Health if t_unemployment_abs >= 0, by(t_unemployment_abs)

* Generate average health satisfaction for each relative time period
bys t_unemployment_abs (Subjective_Health): egen av_Subjective_Health = mean(Subjective_Health) 

* Plot the trajectory of subjective health around unemployment timing
// DESCRIPTIVE
twoway (scatter av_Subjective_Health t_unemployment if t_unemployment >= -1 & t_unemployment <= 5, ///
        msymbol(O) mcolor(blue)) ///
       (line av_Subjective_Health t_unemployment if t_unemployment >= -1 & t_unemployment <= 5 & sex==1, ///
        lcolor(blue) lwidth(medium)), ///
       xline(0, lpattern(dash) lcolor(black)) ///
       xlabel(-1(1)5, angle(0)) ///
       ytitle("Average Health Satisfaction") ///
       xtitle("Time Relative to Unemployment") ///
       title("Trajectory") ///
       legend(off)
	   
// SIF (Step Impact Function)
xtreg Subjective_Health i.t_unemployment_abs i.condition i.marriage if treat==1, fe vce(cl pid)
xtreg Subjective_Health i.t_unemployment_abs i.condition i.marriage if t_unemployment_abs <= 7, fe cl(pid) 

coefplot, keep(*.t_unemployment_abs1) vertical yline(0) recast(line) lwidth(thick) lcolor(blue) ///
    ciopts(recast(rline) lpattern(dash) lwidth(medthick) lcolor(green)) ///
    coeflabels(1.t_unemployment_abs = "-1" 2.t_unemployment_abs = "0" 3.t_unemployment_abs = "1" ///
               4.t_unemployment_abs = "2" 5.t_unemployment_abs = "3" 6.t_unemployment_abs = "4" ///
               7.t_unemployment_abs = "5") ///
    ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f)) ///
    xtitle("Years since unemployment", size(medlarge) margin(0 0 0 2)) ///
    ytitle("Change in Subjective_Health vs t-1", size(medlarge)) ///
    xline(2, lcolor(red))

    
********************************************************************************
*** REGRESSION FIXED EFFECT (FE) WITH INTERACTION 
********************************************************************************
xtreg Subjective_Health i.unemp##c.unemp_mean_region_tv i.marriage i.condition i.syear if syear>=1998 & syear<=2008 , fe vce(cluster pid)
estimate store mod2
// linearity assumption
