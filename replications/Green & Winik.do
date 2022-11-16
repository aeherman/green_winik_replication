cd "C:\Users\David\Dropbox\Documents\Work\Clients & prospects\GiveWell\Criminal justice\Replications"
set scheme s1color

use "C:\Users\David\Dropbox\Documents\Work\Library\Sentencing reform\Green & Winik 2010\GreenWinik_Criminology_2010_LimitedAnonymizedDataset.2.dta", clear

gen dispdatecode = date(dispdate,"MDY")
gen laterarrdatecode = date(laterarrdate,"MDY")
gen timetorecid = laterarrdatecode - dispdatecode
replace laterarr = 0 if timetorecid > 1461

est clear

* Table 7, col 6
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
	calendar9 (toserve probat = calendar?), cluster(clusterid) liml small
est store originalarr

weakiv, graph(ar) contouronly cuepoint gridmin(-.02 -.025) gridmax(.1 .04) gridpoints(40 40) contouropt(ccut(0(.05)1) plotregion(margin(zero)) title("") ///
	xlabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%" .06 "+6%" .08 "+8%" .1 "+10%") ///
	ylabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%") ///
	xtitle(Per month of incarceration sentenced) ytitle(Per month of probation sentenced))
graph rename ARRearrest, replace
graph export "Green & Winik rearrest AR.png", replace width(1000)

* drop exclusion of calendar9
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
	calendar9 (toserve probat = calendar?), cluster(clusterid) liml small
est store replicationarr

weakiv, graph(ar) contouronly cuepoint gridmin(-.02 -.025) gridmax(.1 .04) gridpoints(40 40) contouropt(ccut(0(.05)1) plotregion(margin(zero)) title("") ///
	xlabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%" .06 "+6%" .08 "+8%" .1 "+10%") ///
	ylabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%") ///
	xtitle(Per month of incarceration sentenced) ytitle(Per month of probation sentenced))
graph rename ARRearrest, replace
graph export "Green & Winik rearrest AR.png", replace width(1000)

* drop exclusion of calendar5 as well as calendar9
ivreg2 laterarr age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
	calendar5 calendar9 (toserve probat = calendar?), cluster(clusterid) liml
est store replicationarr

weakiv, graph(ar) contouronly cuepoint gridmin(-.02 -.025) gridmax(.1 .04) gridpoints(40 40) contouropt(ccut(0(.05)1) plotregion(margin(zero)) title("") ///
	xlabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%" .06 "+6%" .08 "+8%" .1 "+10%") ///
	ylabel(-.02 "-2%" 0 "0%" .02 "+2%" .04 "+4%") ///
	xtitle(Per month of incarceration sentenced) ytitle(Per month of probation sentenced))
graph rename ARRearrest, replace
graph export "Green & Winik rearrest AR.png", replace width(1000)


* like Table 7, col 6, but felony reconviction
ivreg2 laterfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
	(toserve probat = calendar?), cluster(clusterid) liml
est store originalfelcon

* drop exclusion of calendar5, calendar9
ivreg2 laterfelcon age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
	calendar5 calendar9 (toserve probat = calendar?), cluster(clusterid) liml
est store replicationfelcon

weakiv, graph(ar) contouronly cuepoint gridmin(-.05 -.01) gridmax(.05 .05) gridpoints(40 40) contouropt(ccut(0(.05)1) plotregion(margin(zero)) title("") ///
	xlabel(-.05 "-5%" -.04 "-4%" -.03 "-3%" -.02 "-2%" -.01 "-1%" 0 "0%" .01 "+1%" .02 "+2%" .03 "+3%" .04 "+4%" .05 "+5%") ///
	ylabel(-.01 "-1%" 0 "0%" .01 "+1%" .02 "+2%" .03 "+3%" .04 "+4%" .05 "+5%") ///
	xtitle(Per month of incarceration sentenced) ytitle(Per month of probation sentenced))
graph rename ARFelConv, replace
graph export "Green & Winik felony reconv AR.png", replace width(1000)

est tab *, keep(toserve probat) b(%5.4f) se(%5.4f) stat(jp idp rkf)

* graph of % free and impact as function of time horizon
set obs 1461
foreach recidvar in arr felcon {
	preserve
	gen b = .
	gen se = .
	gen free = 0
	gen t = _n
	local recidname = cond("`recidvar'"=="arr", "rearrest", "felony reconviction")
	qui forvalues t=1461(-1)1 {
		replace later`recidvar' = 0 if timetorecid > `t'
		cap ivreg2 later`recidvar' age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
			/*calendar5*/ calendar9 (toserve probat = calendar?), cluster(clusterid) liml small // uncomment calendar5 for GiveWell report version
		if _rc continue, break
		replace b = _b[toserve] in `t'
		replace se = _se[toserve] in `t'
		count if toserve & toserve * 365.25 / 12 < 0.85 * `t' // assume 85% rule always binds
		replace free = r(N) in `t'
	}
	replace free = free / 584 // 584 incarcerated
	forvalues i=1/20 {
		gen CIBounds`i' = invt(e(df_r), .025+(`i'-1)*.05) * se + b
	}
	twoway rarea CIBounds1  CIBounds20 t, color(blue*.1) fintensity(100) || ///
				 rarea CIBounds2  CIBounds19 t, color(blue*.2) fintensity(100) || ///
				 rarea CIBounds3  CIBounds18 t, color(blue*.3) fintensity(100) || ///
				 rarea CIBounds4  CIBounds17 t, color(blue*.4) fintensity(100) || ///
				 rarea CIBounds5  CIBounds16 t, color(blue*.5) fintensity(100) || ///
				 rarea CIBounds6  CIBounds15 t, color(blue*.6) fintensity(100) || ///
				 rarea CIBounds7  CIBounds14 t, color(blue*.7) fintensity(100) || ///
				 rarea CIBounds8  CIBounds13 t, color(blue*.8) fintensity(100) || ///
				 rarea CIBounds9  CIBounds12 t, color(blue*.9) fintensity(100) || ///
				 rarea CIBounds10 CIBounds11 t, color(blue*1 ) fintensity(100) || ///
				 line b t, lcolor(black) || line free t, yaxis(2) lpattern(dash) lcolor(red) ///
	  legend(off) xtitle("Days since sentencing") ///
		ytitle("Share of incarcerated defendents freed", axis(2) orient(rvert) color(red)) ytitle("Impact on cumulative `recidname' rate", color(blue)) ///
		ylabel(-.02 "-2%" -.01 "-1%" 0 "0%" .01 "+1%" .02 "+2%" .03 "+3%", angle(hor) axis(1) labcolor(blue) tlcolor(blue)) ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%", angle(hor) axis(2) labcolor(red) tlcolor(red)) ///
		yscale(lcolor(blue)) yscale(lcolor(red) axis(2)) ///
		plotregion(lwidth(none)) graphregion(margin(none)) name("`recidvar'", replace)
	graph export "Green & Winik impact vs. follow-up time `recidname'.png", width(666) replace
	restore
}


***
*** Simulation-based estimates of criminogenesis
***
use "C:\Users\David\Dropbox\Documents\Work\Library\Sentencing reform\Green & Winik 2010\GreenWinik_Criminology_2010_LimitedAnonymizedDataset.2.dta", clear

gen failtime=fullreleasetorecid 
replace failtime=1600 if failtime==.
replace failtime=1 if failtime==0.

/* generate censoring indicator */
gen byte fail = fullreleasetorecid < .

/* define survival data with topcode=1600 */
stset failtime, failure(fail)

/* predicts cumulative survival given _t = failtime */
streg age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug, distribution(weibull)

/* replaces _t with 1461 instead of failtime */
replace _t = 1461
predict cs, csurv

* run new LIML regressions
gen laterarrsim = laterarr

cap program drop sim
program define sim, rclass
	replace laterarrsim = runiform() > cs if incarcerate & !laterarr // simulate arrest only for those incarcerated and not already arrested within their shortened windows

	ivregress liml laterarrsim age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
		(toserve = calendar?), cluster(clusterid)
	return scalar original_b_toserve = _b[toserve]

	* Now do it with probat included as endog var and calendar9 included in 2nd stage
	ivregress liml laterarrsim age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
		calendar9 (toserve probat = calendar?)
	return scalar Roodman_b_toserve = _b[toserve]
	return scalar Roodman_b_toprobat = _b[probat]
	
	eret clear
end

cap program drop boot
program define boot, rclass
	streg age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug, ///
		distribution(weibull) iter(100)

	if e(converged) {
		/* replaces _t with 1461 instead of failtime */
		replace _t = 1461
		cap drop cs
		predict cs, csurv

		replace laterarrsim = runiform() > cs if incarcerate & !laterarr // simulate arrest only for those incarcerated and not already arrested within their shortened windows

		preserve
		simulate, reps(100): sim
		
		foreach var in original_b_toserve Roodman_b_toserve Roodman_b_toprobat {
			sum `var', meanonly
			return scalar `var' = r(mean)
		}
		restore
	}
end

bootstrap original_b_toserve=r(original_b_toserve) Roodman_b_toserve=r(Roodman_b_toserve) Roodman_b_toprobat=r(Roodman_b_toprobat), ///
	reps(100) seed(987654321) saving(GreenWinikMC, replace) strata(calendar) cluster(clusterid): boot
use GreenWinikMC, clear
sum original_b_toserve, detail // distribution of criminogenesis impact estimates

---
* journal graph
use "C:\Users\David\Dropbox\Documents\Work\Library\Sentencing reform\Green & Winik 2010\GreenWinik_Criminology_2010_LimitedAnonymizedDataset.2.dta", clear

gen dispdatecode = date(dispdate,"MDY")
gen laterarrdatecode = date(laterarrdate,"MDY")
gen timetorecid = laterarrdatecode - dispdatecode

set obs 1461
foreach recidvar in arr felcon {
	preserve
	gen b = .
	gen se = .
	gen free = 0
	gen t = _n
	local recidname = cond("`recidvar'"=="arr", "rearrest", "felony reconviction")
	qui forvalues t=1461(-1)1 {
		replace later`recidvar' = 0 if timetorecid > `t'
		cap ivreg2 later`recidvar' age agesq female nonblack priorarr priordrugarr priorfelarr priorfeldrugarr priorcon priordrugcon priorfelcon priorfeldrugcon pwid dist marijuana cocaine crack heroin pcp otherdrug nondrug ///
			calendar5 calendar9 (toserve probat = calendar?), cluster(clusterid) liml small
		if _rc continue, break
		replace b = _b[toserve] in `t'
		replace se = _se[toserve] in `t'
		count if toserve & toserve * 365.25 / 12 < 0.85 * `t' // assume 85% rule always binds
		replace free = r(N) in `t'
	}
	replace free = free / 584 // 584 incarcerated
	forvalues i=1/20 {
		gen CIBounds`i' = invt(e(df_r), .025+(`i'-1)*.05) * se + b
	}
	twoway rarea CIBounds1  CIBounds20 t, color(blue*.1) fintensity(100) || ///
				 rarea CIBounds2  CIBounds19 t, color(blue*.2) fintensity(100) || ///
				 rarea CIBounds3  CIBounds18 t, color(blue*.3) fintensity(100) || ///
				 rarea CIBounds4  CIBounds17 t, color(blue*.4) fintensity(100) || ///
				 rarea CIBounds5  CIBounds16 t, color(blue*.5) fintensity(100) || ///
				 rarea CIBounds6  CIBounds15 t, color(blue*.6) fintensity(100) || ///
				 rarea CIBounds7  CIBounds14 t, color(blue*.7) fintensity(100) || ///
				 rarea CIBounds8  CIBounds13 t, color(blue*.8) fintensity(100) || ///
				 rarea CIBounds9  CIBounds12 t, color(blue*.9) fintensity(100) || ///
				 rarea CIBounds10 CIBounds11 t, color(blue*1 ) fintensity(100) || ///
				 line b t, lcolor(black) || line free t, yaxis(2) lpattern(dash) lcolor(red) ///
	  legend(off) ///
		`=cond("`recidvar'"=="felcon", `"xtitle("Days since sentencing")"', `"xtitle("") xlabel("")"')' ///
		text(`=.03+.002*("`recidvar'"=="arr")' 0 `"{char `=65+("`recidvar'"=="felcon")'}"', place(e) margin(zero)) ///
		ytitle("Incarcerated defendents freed", axis(2) orient(rvert) color(red)) ytitle("Cumulative `recidname' rate", color(blue)) ///
		ylabel(-.02 "-2%" -.01 "-1%" 0 "0%" .01 "+1%" .02 "+2%" .03 "+3%", angle(hor) axis(1) labcolor(blue) tlcolor(blue)) ylabel(0 "0%" .2 "20%" .4 "40%" .6 "60%" `=cond("`recidvar'"=="arr",`".8 "80%""',"")', angle(hor) axis(2) labcolor(red) tlcolor(red)) ///
		yscale(lcolor(blue)) yscale(lcolor(red) axis(2)) ///
		plotregion(lwidth(none)) graphregion(margin(none)) name("`recidvar'", replace)
	restore
}
graph combine arr felcon, cols(1) graphregion(margin(none))
graph export "Green & Winik journal.png", width(1000) replace

