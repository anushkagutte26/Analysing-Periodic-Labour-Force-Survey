// Stata test for Research Assistant position at CEDA
// By Anushka Gutte

// Working with the Periodic labour force data of 2017

clear all
set more off
capture log close 
ssc install listtex
ssc install texsave
ssc install estout

cd "/Users/apple/Desktop/Professional /2026 Applications/Ashoka University/CEDA/data"

log using "plfs_analysis_anushka_gutte.log", replace

use "level01_FV.dta", clear

describe


*******************************************************************


// Q1(i)
gen weight =.
replace weight = multiplier/100 if nss == nsc
replace weight = multiplier/200 if nss != nsc

gen sampling_wt = weight/4

preserve

decode state, gen(state_name)
replace state_name = proper(state_name)
replace state_name = subinstr(state_name, "&", "\&", .)

collapse (mean) avg_cons_exp = household_cons_expenditure_month [pweight = sampling_wt], by(state_name)
    
format avg_cons_exp %12.2fc
	
gsort -avg_cons_exp
    
gen rank = _n
	
keep if rank <= 10

label variable state_name "State"
label variable rank "Rank"
label variable avg_cons_exp "Average Household Consumption"

list
	
 listtex rank state_name avg_cons_exp using "table_q1i.tex", ///
    replace rstyle(tabular) ///
    head("\begin{table}[H]" ///
         "\centering" ///
         "\caption{Average household consumption across states}" ///
         "\begin{tabular}{clc}" ///
         "\toprule" ///
         "Rank & State & Average Household Consumption (Rs.) \\" ///
         "\midrule") ///
    foot("\bottomrule" "\end{tabular}" "\end{table}")
	
restore


*******************************************************************


// Q1(ii)

sum household_cons_expenditure_month , detail

xtile cons_decile = household_cons_expenditure_month ///
    [pweight=sampling_wt], nq(10)
	
_pctile household_cons_expenditure_month [pweight = sampling_wt], nq(10)

keep common_id household_cons_expenditure_month cons_decile sampling_wt
save hh_deciles_clean.dta, replace

collapse (min) min_exp=household_cons_expenditure_month ///
         (max) max_exp=household_cons_expenditure_month ///
         [pweight=sampling_wt], by(cons_decile)

format min_exp max_exp %12.2fc		
sort cons_decile
label variable cons_decile "Decile"
label variable max_exp "Maximum Household Consumption"
label variable min_exp "Minimum Household Consumption"

list

listtex cons_decile min_exp max_exp using "table_q1ii.tex", ///
        replace rstyle(tabular) ///
        head("\vspace{1em}" ///
		"\begin{table}[H]" ///
         "\centering" ///
         "\caption{Household consumption expenditure (Rs) in Deciles}" ///
		"\begin{tabular}{clc}" ///
             "\toprule" ///
             "Decile & Min HH Consumption & Max HH Consumption  \\" ///
             "\midrule") ///
         foot("\bottomrule" "\end{tabular}" "\end{table}")


*******************************************************************		 
		 
		 
// Q1(iii)

use "level02_FV.dta", clear

gen weight =.
replace weight = multiplier/100 if nss == nsc
replace weight = multiplier/200 if nss != nsc

gen sampling_wt = weight/4

keep if age >= 15 & age <= 59

label define prnc_status_code 11 "worked in h.h. enterprise (self-employed): own account worker" 12 "worked in h.h. enterprise (self-employed): employer" 21 "worked in h.h. enterprise (self-employed): worked as helper in h.h. enterprise (unpaid family worker)" 31 "worked as regular salaried/ wage employee" 41 "worked as casual wage labour: in public works" 51 "worked as casual wage labour: in other types of work" 81 "did not work but was seeking and/or available for work" 91 "attended educational institution" 92 "attended domestic duties only" 93 "attended domestic duties and was also engaged in fre collection of goods (vegetables, roots, firewood, cattle feed, etc.), sewing, tailoring, weaving, etc. for household use" 94 "rentiers, pensioners , remittance recipients, etc." 95 "not able to work due to disability" 97 "others (including begging, prostitution, etc.)" 99 "children aged 0-4 yrs"

gen employed = (pr_status_code >= 11 & pr_status_code <= 51 & !missing(pr_status_code))

preserve

collapse (mean) mean_emp=employed [pweight=sampling_wt], by(sex)

replace mean_emp = round(mean_emp,0.001)

listtex sex mean_emp using "table_q1iii.tex", ///
    replace ///
    rstyle(tabular) ///
    head("\begin{table}[htbp]" ///
         "\centering" ///
         "\caption{Weighted mean employment by sex}" ///
         "\begin{tabular}{lc}" ///
         "\toprule" ///
         "Sex & Mean Employment \\" ///
         "\midrule") ///
    foot("\bottomrule" "\end{tabular}" "\end{table}")
	
restore

gen female = (sex == 2)

label variable female "Female (1 = yes)"
label variable employed "Employed"


svyset fsu [pweight=sampling_wt], strata(stratum)

svy: reg employed female
estimates store model1_emp_gap


esttab model1_emp_gap using table_q1iii_2.tex, replace ///
    cells(b(fmt(3) star) se(fmt(3) par)) ///
	stats(N,fmt("%9.0fc")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Gender employment gap") ///
    label nogaps 
	
*******************************************************************

// Q1(iv)

preserve

merge m:1 common_id using "hh_deciles_clean.dta"
keep if _merge == 3


keep if sex == 2
keep if !missing(cons_decile)


gen emp_selfempl   = (pr_status_code >= 11 & pr_status_code <= 12) if !missing(pr_status_code)
gen emp_unpaid     = (pr_status_code == 21)                         if !missing(pr_status_code)
gen emp_regular    = (pr_status_code == 31)                         if !missing(pr_status_code)
gen emp_casual     = (pr_status_code >= 41 & pr_status_code <= 51)  if !missing(pr_status_code)


collapse (mean) rate_self    = emp_selfempl  ///
               rate_unpaid   = emp_unpaid    ///
               rate_regular  = emp_regular   ///
               rate_casual   = emp_casual    ///
         [pweight = sampling_wt], by(cons_decile)


gen x_self    = cons_decile - 0.30
gen x_unpaid  = cons_decile - 0.10
gen x_regular = cons_decile + 0.10
gen x_casual  = cons_decile + 0.30


twoway ///
    (bar rate_self    x_self    , color(navy%75)        barwidth(0.18)) ///
    (bar rate_unpaid  x_unpaid  , color(maroon%75)      barwidth(0.18)) ///
    (bar rate_regular x_regular , color(dkgreen%75)     barwidth(0.18)) ///
    (bar rate_casual  x_casual  , color(dkorange%75)    barwidth(0.18)), ///
    xlabel(1(1)10) ///
    ylabel(0(0.05)0.15, format(%4.2f)) ///
    xtitle("Consumption Decile") ///
    ytitle("Employment Rate (Females)") ///
    legend(order(1 "Self-employed (11-12)" ///
                 2 "Unpaid family worker (21)" ///
                 3 "Regular wage (31)" ///
                 4 "Casual wage (41-51)") ///
           position(6) rows(2) size(small)) ///
    graphregion(color(white)) 

graph export "female_emp_status_decile.png", replace

restore

*******************************************************************

// Q1(v)

gen weekly_earnings = ///
    wage_earning_act_1_1stday + wage_earning_act_2_1stday + ///
	wage_earning_act_1_2ndday + wage_earning_act_2_2ndday + ///
	wage_earning_act_1_3rdday + wage_earning_act_2_3rdday + ///
	wage_earning_act_1_4thday + wage_earning_act_2_4thday + ///
	wage_earning_act_1_5thday + wage_earning_act_2_5thday + ///
	wage_earning_act_1_6thday + wage_earning_act_2_6thday + ///
	wage_earning_act_1_7thday + wage_earning_act_2_7thday 

summarize weekly_earnings

gen days_worked = ///
    (tot_hrs_1stday>0) + ///
    (tot_hrs_2ndday>0) + ///
    (tot_hrs_3rdday>0) + ///
    (tot_hrs_4thday>0) + ///
    (tot_hrs_5thday>0) + ///
    (tot_hrs_6thday>0) + ///
    (tot_hrs_7thday>0)

summarize days_worked
gen daily_wage = weekly_earnings / days_worked


gen salaried_wage = (earning_regular_wage_1 + earning_regular_wage_2) / 30 ///
    if pr_status_code == 31
	
gen salaried_casual = daily_wage if inlist(pr_status_code,41,42,51)

gen salaried_self = daily_wage if inlist(pr_status_code,11,12,21)
summ salaried_wage salaried_casual salaried_self

label variable salaried_wage "Regular Salaried Wage"
label variable salaried_casual "Casual Worker Wage"
label variable salaried_self "Self-employed Wage"

estpost summarize salaried_wage salaried_casual salaried_self

esttab using table_q1v.tex, replace ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) count(fmt(0))") ///
    collabels("Mean" "SD" "Min" "Max" "N") ///
    label ///
	nonumber ///
    title("Daily wage rates by employment type") ///
	noobs 

	
*******************************************************************
	
	
// Q1(vi)

merge m:1 common_id using "level01_FV.dta", keepusing(social_group)

gen wage = salaried_wage
replace wage = salaried_casual if missing(wage)
replace wage = salaried_self if missing(wage)

graph bar (mean) wage [pw=sampling_wt], over(social_group, gap(10)) ///
    bar(1, color(navy%70))  ///
    ytitle("Average Daily Wage (Rs)") ///

graph export "daily_wage_by_caste.png", replace


*******************************************************************


//Q1(vii)

preserve
keep if wage>0
gen log_wage = log(wage)

encode quarter, gen(quarter_fe)      


reghdfe log_wage female age i.education_level [pweight = sampling_wt], ///
    absorb(district pr_status_code quarter_fe) ///
    vce(cluster district)
	
label variable age "Age"

estimates store q_vii

esttab q_vii using "table_q1vii.tex", replace ///
    keep(female age )  ///
	compress ///
	width(0.6\hsize) ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs noobs ///
    title("Linear Regression for gender wage gap") ///
	mtitles("Log Wage") ///
    note("FE: District, Occupation, Quarter. Clustered SE at district.") ///
	addnote("Education dummies included.") ///
    label 
	
restore

*******************************************************************

// Q1(viii)


gen post = 0
replace post = 1 if quarter == "Q3" | quarter == "Q4"


gen female_post = female * post


keep if age >= 15 & age <= 59
keep if wage>0
gen log_wage = log(wage)
keep if pr_status_code == 31
keep if log_wage != .
gen age2 = age^2


label variable female "Female"
label variable post "Post-policy (Jan 2018 onwards)"
label variable female_post "Female x Post (DiD)"


reg log_wage female post female_post  age age2 i.education_level ///
    i.district [pw=sampling_wt], cluster(district)
estimates store did_reg2
	

nlcom (pct_effect: exp(_b[female_post]) - 1)


esttab did_reg2 using "table_q1viii.tex", replace ///
    keep(female post female_post) ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs ///
	label ///
	title("DiD estimates for Wage Transparency Policy") ///
    mtitles("Log Wage") ///
    addnote("\parbox{0.9\linewidth}{District fixed effects; SE clustered at district level.}") ///
	noobs

	
preserve

encode quarter, gen(q_num)
collapse (mean) log_wage [pw=sampling_wt], by(q_num female)

twoway ///
(line log_wage q_num if female==1) ///
(line log_wage q_num if female==0), ///
xlabel(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4") ///
xtitle("Quarter") ytitle("Mean Log Wages") ///
legend(label(1 "Female") label(2 "Male") ) ///
xline(3, lpattern(dash)) ///
text(6.18 3 "Wage Transparency Policy", place(e))

graph export "did_trends.png", replace


restore

	