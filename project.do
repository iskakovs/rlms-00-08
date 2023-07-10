// Data enter
use "C:/Users/777/Desktop/rlms_data/rlms_2000-16_food.dta", clear

drop if year < 2009

*Let's rename variables we use
rename e1_6a potatos_yn
rename e1_6b potatoes_weight
rename e1_6c potatoes_cost

*Let's set labels for variables
label var potatos_yn "Have you bought in the last 7 days (Y/N)?"
label var potatoes_weight "How much was bought, kg"
label var potatoes_cost "How much was spent, RUR"

///////////////////////////////////////////////////

// Graphs
* Relation between number of family members and costs spent on potatoes (see Graph 2)
twoway (scatter potatoes_cost nfm if potatos_yn == 1)(lfit potatoes_cost nfm if potatos_yn == 1)

* Relation between income and costs spent on potatoes (without transformations) (see Graph 4)
twoway (scatter potatoes_cost income) (lfit potatoes_cost income)

/*We see that there are some outliers in the income data, so we cannot clearly see the relation between costs and income,
however the trendline slope shows that there is positive raltion between these two variables*/ 

* Let's check what kind of tranformation we can make to normalize the costs variable (see Graph 5)
gladder potatoes_cost, fraction 

* We see that log transformation is more sutiable

* Let's generate the log transformation variables
gen logpt_cost = log(potatoes_cost)
gen log_income = log(income)
gen log_crops = log(crops_cost)

* Hypothesis that cropgrowers spend less than non-cropgrowers. The hypothesis was stated in the paper for our homework (see Graph)
twoway (scatter logpt_cost log_income if crops==1, mcolor(blue)) (lfit logpt_cost log_income if crops==1), name (scatter_1)
twoway (scatter logpt_cost log_income if crops==0, mcolor(red)) (lfit logpt_cost log_income if crops==0), name (scatter_2)
graph combine scatter_1 scatter_2, col(2) row(1)

/* We see that trendlines have different slopes. So, crop non-cropgrowers spend more on potatoea as their income increases, 
while cropgrowers spend less*/

* Relation between income and costs spent on potatoes with log transformation (see Graph 3)
twoway (scatter logpt_cost log_income) (lfit logpt_cost log_income)

/* We see that there is a trend between the income and money spent on potatoes. 
So, as the income rises the costs (money spent) on potatoes rises too.*/


* On the previous graphs we do not see the center on mass 
* i.e. we do not know wehere people's spending on potatoes according to their income is concentrated.
* Let's check it.
sunflower logpt_cost log_income

/* We see that the concentration of the population on the log(income) ~ 10 
and log(potatoes_cost) ~ 4 */

//////////////////////////////////////////////////////////////////////////////////////

/// Let's analyze data with models and tests!
// Panel declaration
global id id
global t year

* We'll analyze how place (city-rural), number of family members, number of the children, and income affect on costs for buying potatoes
global ylist logpt_cost
global xlist urban nfm log_income children children_older crops log_crops

// Panel description
describe $id $t $ylist $xlist
summarize $id $t $ylist $xlist

* If we need more description
codebook $id $t $ylist $xlist
inspect $id $t $ylist $xlist

* Set data as panel data
sort $id $t
xtset $id $t
xtdescribe
xtsum $id $t $ylist $xlist

* Heterogenity test
sdtest logpt_cost == 4.46

* Other method - works with v. 14.0 Stata 
ssc install xthst
xthst $ylist $xlist 
 
* Pool regression (OLS)
reg $ylist $xlist 

* Population-averaged (PA) estimator
xtreg $ylist $xlist, pa

* Between estimator
xtreg $ylist $xlist, be

* Fixed effects (FE) aka within estimator
xtreg $ylist $xlist, fe

* first difference estimator
reg D.($ylist $xlist), noconstant

* Random effects estimator
xtreg $ylist $xlist, re theta

* Hausman test for FE vs RE effects models
quietly xtreg $ylist $xlist, fe
estimates store fixed
quietly xtreg $ylist $xlist, re
estimates store random
hausman fixed random

* Breusch-Pagan LM test for RE vs OLS 
quietly xtreg $ylist $xlist, re
xttest0

* Heteroskedastisity test
reg $ylist $xlist
estat hettest

* VIF test with intercept
reg $ylist $xlist
estat vif, uncentered

* VIF test without intercept
reg $ylist $xlist, noconstant
estat vif, uncentered

* Time autocorrelation test ----------- in Webs (doesn't work)
quietly xtreg $ylist $xlist, re
xttest1

//* Time autocorrelation test ++++++ Alternative
gen trend = _n
tsset trend

* Create model
reg $ylist $xlist

predict ehat, r

* Visualize error terms
tsline ehat

* Visualize error terms to check the autocorrelation
tsline ehat if e(sample) == 1, yline(0)

* Check for autocorrelation of the residuals-one lag model
reg ehat l.ehat

* Durbin Watson autocorrelationa test
reg $ylist $xlist
estat dwatson

* Install package for next test (spatial AC)
ssc install xtcsd

* Spatial autocorrelation test (Pesaran test)------ Doesn't work-needs too much memory
xtset $id $t
xtreg $ylist $xlist, fe
xtcsd, pesaran

* Tried to clean the data and missing values
keep year id logpt_cost urban nfm log_income children children_older crops potatos_yn potatoes_weight potatoes_cost income  _est_fixed _est_random trend ehat
drop if mi(logpt_cost)
drop if mi(log_income)
* But it doesn't help

* Spatial autocorrelation test
ssc install xttest2
xtset $id $t
xtreg $ylist $xlist, fe
xttest2

* Spatial autocorrelation correction
xtregar $ylist $xlist, fe

xtregar $ylist $xlist, re

//* Heteroscedastisity test ++++
* Installation of the package for test 
ssc install xttest3

* Heteroscedastisity test ++++
quietly xtreg $ylist $xlist, fe
xttest3

*Regression with Driscoll-Kraay standard errors
ssc install xtscc
xtscc $ylist $xlist, fe

////////////////// PART 2 //////////////////////////

ssc install estout

* Check for endogeneity using 2SLS WITHOUT instruments
ivregress 2sls $ylist $xlist

* Check for endogeneity using 2SLS WITH instruments
** When instrument is lag of income
ivregress 2sls logpt_cost urban nfm log_income children children_older (log_crops = l.log_income l.nfm l.urban l.children l.children_older)
** We assume that the lag of the log_income nfm urban and children as the instruments that affect log_crops

* Check the instruments
estat endog
estat firststage
estat overid

*Save the results of 2SLS
eststo two_sls

* Check for endogeneity using IV WITH instruments
ivreg logpt_cost urban nfm log_income children children_older (log_crops = l.log_income l.nfm l.urban l.children)

*Save the results of IV
eststo IV

* Now let's check the results of simple OLS and compare it with 2SLS and IV
reg logpt_cost urban nfm log_income children children_older log_crops

*Save the results of OLS
eststo OLS

*Compare the results:

estout

*** The results with instrumental methods are close to each other, while the results with OLS differ much

* Now, let's check the instruments with 2SLS
ivregress 2sls logpt_cost urban nfm log_income children children_older (log_crops = l.log_income l.nfm l.urban l.children l.children_older)
** We check the following instruments: lag of the log_income nfm urban and children 

* Check the instruments
estat endog
estat firststage
estat overid

* Let's choose time invariant regressor. Let this vartiable be a place where respondent live (urban)
** Check for FE regression
xtreg logpt_cost log_income log_crops nfm urban children children_older, fe 
** Save results
est store FE

** Check for RE regression
xtreg logpt_cost log_income log_crops nfm urban children children_older, re 
** Save results
eststo RE

** Apply Hausmann-Taylor regression
xthtaylor logpt_cost log_income log_crops nfm urban children children_older, endog(log_crops) 
** Save results
eststo HT

estout

** Check for kernel density
kdensity logpt_cost 
kdensity urban 
kdensity nfm 
kdensity log_income 
kdensity children 
kdensity children_older 
kdensity log_crops

*Make the panel data balanced
xtset $id $t
tsfill, full

* Clear all the missing data and replace them with 0 values
mvencode nfm logpt_cost log_income log_crops urban children children_older, mv(0) override

** Check for unit roots and stationarity
xtunitroot ips logpt_cost, demean
xtunitroot ips log_income, demean
xtunitroot ips log_crops, demean
xtunitroot ips nfm, demean
xtunitroot ips urban, demean
xtunitroot ips children, demean
xtunitroot ips children_older, demean

** Check for cointegration (works with Stata 15 and later)
xtcointtest kao logpt_cost log_income log_crops nfm urban children children_older 

//////////////////////////////////

* Now let's check for dynamic model

** Perform the Arelano-Bond model
ssc install xtabond2

*********
xtabond2 logpt_cost log_income log_crops nfm urban children children_older, gmm(l.log_crops) nocons twostep small
est store ab2c
 
estout
