/* Example of a simple prospective cohort study 

 X = exposure
 Y = outcome 
 
 P(X) = .2
 P(Y=1|X=0) = .1 
 P(Y=1|X=1) = .3
 OR = [.3/(1-.3)]/[.1/(1-.1)]
 
*/

// create a program to simualte the study

capture program drop sim_study
program define sim_study, rclass
	drop _all 
	local n = 1000
	local py_1 = .3
	local py_0 = .1
	local x_p = .3
	
	qui set obs `n'
	qui gen x = rbinomial(1, `x_p')
	qui gen y = .
	qui replace y = rbinomial(1, `py_1') if x == 1
	qui replace y = rbinomial(1, `py_0') if x == 0

	logit y x ,or
	return scalar est_b = _b[x]
	return scalar est_se_b = _se[x]
end

// simulate the study
sim_study 

// simulate the study 1,000 times
simulate est_b = r(est_b)  est_se_b = r(est_se_b) , seed(23016) reps(1000) : sim_study 

// summary statistics
summarize  est_b
di exp(r(mean))
gen or = exp(est_b)

* Expected mean and std deviation
di (60/140)/(80/720)
di sqrt(1/60+1/140+1/80+1/720)

scalar t_b = ln( (60/140)/(80/720) )
scalar t_se_b = sqrt(1/60+1/140+1/80+1/720)
di t_b+invnormal(.005)*t_se_b
di t_b+invnormal(.995)*t_se_b
di exp(t_b+invnormal(.005)*t_se_b)
di exp(t_b+invnormal(.995)*t_se_b)
su 

// figure
twoway ///
   (function t_b = normalden(x, t_b, t_se_b), range(`=t_b-4*t_se_b' `=t_b+4*t_se_b') recast(area) color(blue%20)) ///
   (hist est_b, bin(60) color(red%20)) ///
	, ytitle("Sampling Distribution") ylab(, nogrid) /// 
	xtitle("Estimated Odds Ratio") ///
	legend(label(1 "Theoretical") label(2 "Simulated") ring(0) pos(1) col(1) region(style(none))) ///
	xlabel(`=ln(3.86)' "3.86" .85 "2.33" 1.85 "6.36", nogrid) plotregion(style(none))
