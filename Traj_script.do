*** Clear space and set path
clear all

*** Import data
use "RS_MCS_analysis.dta", clear

*** Summarise data
summarize

*** Help documentation for traj
*help traj

*** Function: APP and OCC in one table
program summary_table_procTraj
	preserve
	** remove any cases where traj group classification missing
	drop if missing(_traj_Group)
	* APP
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
		replace Mp = `i' if `i' > Mp
	}
	sort _traj_Group
	* OCC
	by _traj_Group: gen countG = _N
	by _traj_Group: egen groupAPP = mean(Mp)
	by _traj_Group: gen counter = _n
	gen n = groupAPP / (1 - groupAPP)
	gen p = countG / _N
	gen d = p / (1-p)
	gen occ = n / d
	* Estimated proportion for each group
	scalar idx = 0
	gen TotProb = 0
	foreach i of varlist _traj_ProbG* {
		scalar idx = idx + 1
		quietly summarize `i'
		replace TotProb = r(sum) / _N if _traj_Group == idx
	}
	gen GROUP_APP = round(groupAPP*100, .1)
	gen OCC = round(occ, .1)
	* Count of people whose highest probability was for the specific group, divided by total sample size
	gen Probab = round(p*100, .1)
	* Sum of all individuals' probability of belonging to the specific group 
	gen Prob_post = round(TotProb*100, .1)
	list _traj_Group countG GROUP_APP OCC Probab Prob_post if counter == 1
	restore
end

*** Create variables for time
gen wave4 = 0
gen wave5 = 1
gen wave6 = 2

*** Single trajectories

* Peer aggression victimisation

** Slope: linear
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(6) order1(1) 

** Slope: quadratic
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(5) order1(2)

* Peer aggression perpetration

** Slope: linear
traj, multgroups(1) var1(Perpetration_s4-Perpetration_s6) indep1(wave4-wave6) model1(cnorm) min1(-1) max1(11) order1(1)

** Slope: quadratic
traj, multgroups(1) var1(Perpetration_s4-Perpetration_s6) indep1(wave4-wave6) model1(cnorm) min1(-1) max1(11) order1(2)

* Plot model

** Peer aggression victimisation
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ylabel1(-1(1)1) xlabel(0(1)2)

** Peer aggression perpetration
multtrajplot, xtitle("Wave") ytitle1("Perpetration") ylabel1(-1(1)1) xlabel(0(1)2)

*** Joint-trajectories

* 1-traj model

** Slope L: Victimisation, Perpetration
** Slope Q: none
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(6) order1(1) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1)
summary_table_procTraj

** Slope L: Perpetration
** Slope Q: Victimisation
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(6) order1(2) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1)
summary_table_procTraj

** Slope L: Victimisation
** Slope Q: Perpetration
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(6) order1(1) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2)
summary_table_procTraj

** Slope: none
** Slope Q: Perpetration, Victimisation
traj, multgroups(1) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) min1(-2) max1(6) order1(2) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 2-traj model

** Slope L: Victimisation G1, G2; Perpetration G1, G2
** Slope Q: none
traj, multgroups(2) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order1(1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1)
summary_table_procTraj

** Slope L: Perpetration Q1, Q2
** Slope Q: Victimisation G1, G2
traj, multgroups(2) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order1(2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2
** Slope Q: Perpetration G1, G2
traj, multgroups(2) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order1(1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2; Perpetration G1, G2
traj, multgroups(2) var1(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order1(2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 3-traj model

** Slope L: Victimisation G1, G2, G3; Perpetration G1, G2, G3
** Slope Q: none
traj, multgroups(3) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1)
summary_table_procTraj

** Slope L: Perpetration G1, G2, G3
** Slope Q: Victimisation G1, G2, G3
traj, multgroups(3) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2, G3
** Slope Q: Perpetration G1, G2, G3
traj, multgroups(3) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2, G3; Perpetration G1, G2, G3
traj, multgroups(3) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 4-traj model

** Slope L: Victimisation G1, G2, G3, G4; Perpetration G1, G2, G3, G4
** Slope Q: none
traj, multgroups(4) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1)
summary_table_procTraj

** Slope L: Perpetration G1, G2, G3, G4
** Slope Q: Victimisation G1, G2, G3, G4
****** 19-06-25 Pin: The number of groups decrease to 3?
traj, multgroups(4) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2, G3, G4
** Slope Q: Perpetration G1, G2, G3, G4
****** 19-06-25 Pin: The number of groups decrease to 3?
traj, multgroups(4) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2, G3, G4; Perpetration G1, G2, G3, G4
traj, multgroups(4) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 5-traj model

** Slope L: Victimisation G1, G2, G3, G4, G5; Perpetration G1, G2, G3, G4, G5
** Slope Q: none
traj, multgroups(5) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1)
summary_table_procTraj

** Slope L: Perpetration G1, G2, G3, G4, G5
** Slope Q: Victimisation G1, G2, G3, G4, G5
****** Fit improvement with APP and OCC both met
traj, multgroups(5) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2, G3, G4, G5
** Slope Q: Perpetration G1, G2, G3, G4, G5
****** Fit improvement with APP and OCC both met
traj, multgroups(5) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2, G3, G4, G5; Perpetration G1, G2, G3, G4, G5
traj, multgroups(5) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 6-traj model

** Slope L: Victimisation G1, G2, G3, G4, G5, G6; Perpetration G1, G2, G3, G4, G5, G6
** Slope Q: none
traj, multgroups(6) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1 1)
summary_table_procTraj

** Slope L: Perpetration G1, G2, G3, G4, G5, G6
** Slope Q: Victimisation G1, G2, G3, G4, G5, G6
****** Fit improvement with APP and OCC both met
traj, multgroups(6) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2, G3, G4, G5, G6
** Slope Q: Perpetration G1, G2, G3, G4, G5, G6
traj, multgroups(6) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2, G3, G4, G5, G6; Perpetration G1, G2, G3, G4, G5, G6
traj, multgroups(6) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* 7-traj model

** Slope L: Victimisation G1, G2, G3, G4, G5, G6, G7; Perpetration G1, G2, G3, G4, G5, G6, G7
** Slope Q: none
traj, multgroups(7) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1 1 1)
summary_table_procTraj

** Slope L: Perpetration G1, G2, G3, G4, G5, G6, G7
** Slope Q: Victimisation G1, G2, G3, G4, G5, G6, G7
****** Fit improvement with APP and OCC both met
traj, multgroups(7) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(1 1 1 1 1 1 1)
summary_table_procTraj

** Slope L: Victimisation G1, G2, G3, G4, G5, G6, G7
** Slope Q: Perpetration G1, G2, G3, G4, G5, G6, G7
****** Fit improvement with APP and OCC both met
traj, multgroups(7) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(1 1 1 1 1 1 1) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2 2 2)
summary_table_procTraj

** Slope L: none
** Slope Q: Victimisation G1, G2, G3, G4, G5, G6, G7; Perpetration G1, G2, G3, G4, G5, G6, G7
****** Fit decrease with APP and OCC both met
traj, multgroups(7) var(Victimisation_s4-Victimisation_s6) indep1(wave4-wave6) model1(cnorm) order(2 2 2 2 2 2 2) min1(-2) max1(6) var2(Perpetration_s4-Perpetration_s6) indep2(wave4-wave6) model2(cnorm) min2(-1) max2(11) order2(2 2 2 2 2 2 2)
summary_table_procTraj

* Plot model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* Plot final model
multtrajplot, xtitle("Wave") ytitle1("Victimisation") ytitle2("Perpetration") ylabel1(0(0.5)1.5) ylabel2(0(0.5)1.5) xlabel(0(1)2)

* Export data with four-trajectory (all quadratic slopes) class assignment to R
save RS_MCS_analysis_class_assignment.dta, replace

*** Plot trajectories in a graph

** Manually compute group means per wave

* Generate unique id for each participant
gen id = _n

* Reshape data into long format
rename (Victimisation_s4-Victimisation_s6) (Victimisation_4 Victimisation_5 Victimisation_6)
rename (Perpetration_s4-Perpetration_s6) (Perpetration_4 Perpetration_5 Perpetration_6)

reshape long Victimisation_ Perpetration_, i(id) j(wave)

* Collapse by group and wave
collapse(mean) Victimisation_ Perpetration_, by(_traj_Group wave)

* Plot final model

** Generate age for x-axis
gen age = .
replace age = 7 if wave==4
replace age = 11 if wave==5
replace age = 14 if wave==6

** Group 1
twoway ///
    (line Victimisation_ age if _traj_Group==1, lcolor(blue) lpattern(solid)) ///
    (line Perpetration_ age if _traj_Group==1, lcolor(orange) lpattern(dash)) ///
    (scatter Victimisation_ age if _traj_Group==1, mcolor(blue) msymbol(O)) ///
    (scatter Perpetration_ age if _traj_Group==1, mcolor(orange) msymbol(T)), ///
    legend(pos(11) order(1 "Victimisation" 2 "Perpetration")) ///
    xtitle("Age (years)") ytitle("Predictive Score") ///
    title("Uninvolved (69.8%, n = 11,471)") ///
    xlabel(7 11 14) ylabel(0(0.5)1.5, angle(horizontal))
graph save group1.gph, replace

** Group 2
twoway ///
	(line Victimisation_ age if _traj_Group==2, lcolor(blue) lpattern(solid)) ///
	(line Perpetration_ age if _traj_Group==2, lcolor(orange) lpattern(dash)) ///
	(scatter Victimisation_ age if _traj_Group==2, mcolor(blue) msymbol(O)) ///
    (scatter Perpetration_ age if _traj_Group==2, mcolor(orange) msymbol(T)), ///
	legend(pos(11) order(1 "Victimisation" 2 "Perpetration")) ///
	xtitle("Age (years)") ytitle("Predictive Score") ///
	title("Childhood Victims (14.4%, n = 2,370)") ///
	xlabel(7 11 14) ylabel(0(0.5)1.5, angle(horizontal))
graph save group2.gph, replace

** Group 3
twoway ///
	(line Victimisation_ age if _traj_Group==3, lcolor(blue) lpattern(solid)) ///
	(line Perpetration_ age if _traj_Group==3, lcolor(orange) lpattern(dash)) ///
	(scatter Victimisation_ age if _traj_Group==3, mcolor(blue) msymbol(O)) ///
    (scatter Perpetration_ age if _traj_Group==3, mcolor(orange) msymbol(T)), ///
	legend(pos(11) order(1 "Victimisation" 2 "Perpetration")) ///
	xtitle("Age (years)") ytitle("Predictive Score") ///
	title("Mid-Adolescence Victims (10.1%, n = 1,651)") ///
	xlabel(7 11 14) ylabel(0(0.5)1.5, angle(horizontal))
graph save group3.gph, replace

** Group 4
twoway ///
	(line Victimisation_ age if _traj_Group==4, lcolor(blue) lpattern(solid)) ///
	(line Perpetration_ age if _traj_Group==4, lcolor(orange) lpattern(dash)) ///
	(scatter Victimisation_ age if _traj_Group==4, mcolor(blue) msymbol(O)) ///
    (scatter Perpetration_ age if _traj_Group==4, mcolor(orange) msymbol(T)), ///
	legend(pos(11) order(1 "Victimisation" 2 "Perpetration")) ///
	xtitle("Age (years)") ytitle("Predictive Score") ///
	title("Victim-Perpetrators  (5.7%, n = 933)") ///
	xlabel(7 11 14) ylabel(0(0.5)1.5, angle(horizontal))
graph save group4.gph, replace

** Combine graphs
graph combine group1.gph group2.gph group3.gph group4.gph, ///
    rows(2) cols(2) ycommon xcommon ///
    title("Victimisation and Perpetration Trajectories by Group")
graph save TrajGroup.gph, replace
