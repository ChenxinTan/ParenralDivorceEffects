* =========================================================================
* Heterogeneous Effects of Parental Divorce on Child's Academic Performance in China
* 
* Author: Chenxin Tan
*	Master Student in Applied Quantitative Research, New York University
* 	Personal URL: https://github.com/ChenxinTan
*	Email: chenxin.tan@nyu.edu
*
* Data: China Educational Panel Survey (CEPS)
* 	The CEPS is a nationally representative, longitudinal social survey launched
*	by Renmin University of China and is avaliable at http://www.cnsda.org/index.php?r=projects/index
*
* Version: SE 15.1
*
* Last Modified: June 26, 2020
* =========================================================================

/* <! -- Note: This Stata .do file is modified on the basis of my course paper for
			   Applied Qantitative Analysis II course, taught by Prof Michael Hout
			   at NYU's Department of Sociology, and is avaiable at Chenxin Tan's GitHub. */

* === < Programmming Setting and Load Data > ===

clear all
clear matrix
capture log off
set more off

set seed 2020

global path "/Users/chenxintan/Desktop/New York University/AQA II/finalpaper"
global datadir "${path}/data_CEPS/"

cd "${path}"

use "${datadir}cepsw1studentEN.dta", clear										  //2013-14 Baseline Student Data
merge 1:1 ids using "${datadir}cepsw2studentEN.dta", keep(match using) nogenerate //2014-15 Follow-up Student Data
merge 1:1 ids using "${datadir}cepsw1parentEN.dta", keep(match master) nogenerate //2013-14 Baseline Parent Data
merge 1:1 ids using "${datadir}cepsw2parentEN.dta", keep(match) nogenerate		  //2014-15 Follow-up Parent Data

compress

/* NOTE: Constructed variables were prefixed by data source and wave, where S stands
		 for student survey and P stands for parent survey; 1 and 2 stand for baseline
		 and 1st follow-up, respectivrly. For example, "s2padivorce" refered to 
		 parental divorce obtained from student survey at 1st follow-up. */

* === < Parental Divorce at CEPS Wave 2 (2014-15 Follow-up) > ===
gen s2padivorce = w2a0802

lab var s2padivorce "S2 Bio-parents Divorced"
lab def s2padivorce 0 "0 Not-divorced" 1 "1 Divorced"
lab val s2padivorce s2padivorce

tab s2padivorce
tab s2padivorce [aw = w2sweight] /* In CEPS sample, around 7% and 6% of students
	whose biological parents were divorced, results were unweighted and weight, 
	respectively. */

* === < Child Outcome at CEPS Wave 2 (2014-15 Follow-up) > ===
	
	* Chinese Exam Percentile within Sampling School
	gen s2percchn = w2chn/w2upchn * 100 //exam score / maximum score
	
	bys schids: egen s2meanchn = mean(s2percchn)
	bys schids: egen s2sdchn = sd(s2percchn)
	bys schids: gen  s2stdchn = (s2percchn - s2meanchn)/s2sdchn
	
	bys schids: egen Nchn = count(s2percchn)
	bys schids: egen Ichn = rank(s2percch)
	gen s2rankchn = (Ichn - 1)/(Nchn - 1) * 100

	* Math Exam Percentile within Sampling School
	gen s2percmath = w2mat/w2upmat * 100
	replace s2percmath = . if s2percmath > 100 //the percentage should NOT exceed 100!
	
	bys schids: egen s2meanmath = mean(s2percmath)
	bys schids: egen s2sdmath = sd(s2percmath)
	bys schids: gen  s2stdmath = (s2percmath - s2meanmath)/s2sdmath

	bys schids: egen Nmath = count(s2percmath)
	bys schids: egen Imath = rank(s2percmath)
	gen s2rankmath = (Imath - 1)/(Nmath - 1) * 100

	* English Exam Percentile within Sampling School
	gen s2perceng = w2eng/w2upeng * 100
	replace s2perceng = . if s2perceng > 100
	
	bys schids: egen s2meaneng = mean(s2perceng)
	bys schids: egen s2sdeng = sd(s2perceng)
	bys schids: gen  s2stdeng = (s2perceng - s2meaneng)/s2sdeng

	bys schids: egen Neng = count(s2perceng)
	bys schids: egen Ieng = rank(s2perceng)
	gen s2rankeng = (Ieng - 1)/(Neng - 1) * 100
	
	lab var s2stdchn  "S2 STD Score Chinese"
	lab var s2stdmath "S2 STD Score Math"
	lab var s2stdeng "S2 STD Score English"
	
	lab var s2rankchn  "S2 percentage rank Chinese"
	lab var s2rankmath "S2 percentage rank Math"
	lab var s2rankeng  "S2 percentage rank English"
	
	drop s2mean* s2sd* N* I* s2perc*
	
	lab def binary 0 "0 no" 1 " 1 yes"

* === < Propensities for Divorce (2014-14 Baseline and 2014-15 Follow-up) > ===

/* For elminating the edogeneity issue as much as possible, I mainly used the baseline
   survey for construct the propensities for parental divorce. The 1st follow-up
   survey was also used for missing value imputation (Also there were near 500 students
   who were enrolled firstly at 1st follow-up. */

** - Family Background -

/* Note: 1. Since the student-reported might be inaccurate in some ways (they aren't
		 familiar with their family process or don't fully understand what a question 
		 refer to), I mainly used parent data to measure family backgrounds and 
		 used the student data for missing value imputation. */
	
	* Hukou (Household Registration Status) Type
	gen s1hktype = 1 - sthktype
	/* there were 471 new-entries and I used their answers at wave 2 for imputation 
	   and assumed there hukou status did not change */
	replace s1hktype = 1 if missing(s1hktype) & (w2bd02 == 2 | w2bd02 == 3)
	replace s1hktype = 0 if missing(s1hktype) & (w2bd02 == 1)
	replace s1hktype = 1 if missing(s1hktype) & (w2a02 == 2 | w2a02 == 3)
	replace s1hktype = 0 if missing(s1hktype) & (w2a02 == 1)
	
	lab var s1hktype "S1 hukou type (urban)"
	lab def s1hktype 0 "0 agri hukou" 1 "non-agri hukou"
	lab val s1hktype s1hktype
	
	* Migration Status
	recode stmigrant (1/2 = 0 "0 local/within prov")(3 = 1 "1 cross-prov migrate"), gen(s1migrate)
	
	lab var s1migrate "S1 migration status"
	
	* Only-child Status
	gen s1onech = 2 - stonly
	replace s1onech = 1 if missing(s1onech) & w2a05 == 1
	replace s1onech = 0 if missing(s1onech) & w2a05 == 2
	
	lab var s1onech "S1 only child"
	lab val s1onech binary
	
	* Sibling Size
	egen s1sibsize = rowtotal(b020*), missing
	egen s2sibsize = rowtotal(w2a0601*), missing
	replace s1sibsize = s2sibsize if missing(s1sibsize)
	
	quietly sum s1sibsize
	replace s1sibsize = r(mean) if missing(s1sibsize) & s1onech == 0 //take the mean sibsize as imputation value
	replace s1sibsize = 0 if s1onech == 1
	
	lab var s1sibsize "S1 sib size"
	
	replace s1onech = 1 if missing(s1onech) & (s1sibsize == 0)
	replace s1onech = 0 if missing(s1onech) & (s1sibsize > 0 & !missing(s1sibsize))
	
	* Grandparent Coresidence
	gen s1grandpa = be1205
	replace s1grandpa = b0404 if missing(s1grandpa)
	replace s1grandpa = 1 if missing(s1grandpa) & w2ba0206 == 1
	replace s1grandpa = 0 if missing(s1grandpa) & w2ba0206 == 0
	replace s1grandpa = w2a0706 if missing(s1grandpa)
	
	lab var s1grandpa "S1 coresidential grandparent"
	lab val s1grandpa binary
		
	* Parent's Ethnic Nationality (Han Ethnicity or Not)
	gen p2dadeth = 1 if w2be02 == 1
	replace p2dadeth = 0 if w2be02 != 1 & !missing(w2be02)
	gen p2mometh = 1 if w2be11 == 1
	replace p2mometh = 0 if w2be11 != 1 & !missing(w2be11)
	
	lab var p2dadeth "P2 dad eth (Han eth)"
	lab var p2mometh "P2 mom eht (Han ehh)"
	lab val p2dadeth p2mometh binay
	
	* Family Needing Long-term Care
	gen p1hhfamcare = 2 - be22
	replace p1hhfamcare = 2 - w2be26 if missing(p1hhfamcare)
	
	lab var p1hhfamcare "P1 HH having family needing care"
	lab val p1hhfamcare binary
	
** - Socioeconomic Status -
	
	* Parent's Political Memembership (CCP or Democratic Party)
	gen p1dadparty = 1 if ba01 == 1 & (be06 == 1 | be06 == 2)
	replace p1dadparty = 0 if ba01 == 1 & be06 == 3
	replace p1dadparty = 1 if missing(p1dadparty) & (w2be07 == 1 | w2be07 == 2)
	replace p1dadparty = 1 if missing(p1dadparty) & (w2be07 == 3)

	gen p1momparty = 1 if ba01 == 2 & (be06 == 1 | be06 == 2)
	replace p1momparty = 0 if ba01 == 2 & be06 == 3
	replace p1momparty = 1 if missing(p1momparty) & (w2be16 == 1 | w2be16 == 2)
	replace p1momparty = 1 if missing(p1momparty) & (w2be16 == 3)
	
	lab var p1dadparty "P1 dad party member"
	lab var p1momparty "P1 mom party member"
	lab val p1dadparty p1momparty binary
	
	* Parent's Educational Attainments
	/* NOTE: Since it was very likely that students didn't know their parents' 
			 educational attainments, we mainly used either baseline or 1st follow-up 
			 to construct these variables. */
	gen p1dadedu_r = be07 if ba01 == 1
	replace p1dadedu_r = w2be08 if !missing(w2be08)
	replace p1dadedu_r = w2be21 if missing(p1dadedu_r) & w2ba01 == 1
	replace p1dadedu_r = b07 if missing(p1dadedu_r) //student survey
	
	gen p1momedu_r = be07 if ba01 == 2
	replace p1momedu_r = w2be17 if !missing(w2be17)
	replace p1momedu_r = w2be21 if missing(p1momedu_r) & w2ba01 == 2
	replace p1dadedu_r = b06 if missing(p1momedu_r) //student survey
	
	recode p1dadedu_r (1/3 = 1 "1 less than HS") ///
					  (4/7 = 2 "2 HS or equiv") ///
					  (8/9 = 3 "3 collger or higher"), gen(p1dadedu)
	recode p1momedu_r (1/3 = 1 "1 less than HS") ///
					  (4/7 = 2 "2 HS or equiv") ///
					  (8/9 = 3 "3 collger or higher"), gen(p1momedu)
	
	lab var p1dadedu "P1 dad edu"
	lab var p1momedu "P1 mom edu"
	
	* Parents' Occupational Status (4-Class)
	recode be08 (9 = 1)(8 = 2)(4/7 10 = 3)(1/3 = 4) if ba01 == 1, gen(p1dadocc4c) //wave1 parent survey - RSP report (RSP == father)
	recode b08b (9 = 1)(8 = 2)(4/7 10 = 3)(1/3 = 4), gen(s1dadocc4c) //wave1 student survey
	recode w2be22 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4) if w2ba01 == 1, gen(p2dadocc4c) //wave2 parent survey - RSP report (RSP == father)
	recode w2be09 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4), gen(p2dadocc4c_dad) //wave2 parent survey - RSP report father's occupation
	recode w2a13 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4), gen(s2dadocc4c) //wave2 student survey
	
	replace p1dadocc4c = s1dadocc4c if missing(p1dadocc4c)
	replace p1dadocc4c = p2dadocc4c if missing(p1dadocc4c)
	replace p1dadocc4c = p2dadocc4c_dad if missing(p1dadocc4c)
	replace p1dadocc4c = s2dadocc4c if missing(p1dadocc4c)

	recode be08 (9 = 1)(8 = 2)(4/7 10 = 3)(1/3 = 4) if ba01 == 2, gen(p1momocc4c) //baseline RSP report (RSP == mother)
	recode b08a (9 = 1)(8 = 2)(4/7 10 = 3)(1/3 = 4), gen(s1momocc4c) //wave1 student survey
	recode w2be22 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4) if w2ba01 == 2, gen(p2momocc4c) //wave2 RSP report (RSP == mother)
	recode w2be18 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4), gen(p2momocc4c_mom) //wave2 RSP report mother's occupation
	recode w2a14 (13 = 1)(10/11 = 2)(5/9 12 14 = 3)(1/4 = 4), gen(s2momocc4c) //wave2 student survey
	
	replace p1momocc4c = s1momocc4c if missing(p1momocc4c)
	replace p1momocc4c = p2momocc4c if missing(p1momocc4c)
	replace p1momocc4c = p2momocc4c_mom if missing(p1momocc4c)
	replace p1momocc4c = s2momocc4c if missing(p1momocc4c)
	
	lab var p1dadocc4c "P1 dad occulational status (4C)"
	lab var p1momocc4c "P1 mom occulational status (4C)"
	lab def occ4c 1 "1 unemployed" 2 "2 elementary worker" 3 "3 tech worker or ord staff" 4 "4 high-rep" //"high-rep" includes governmental officials, company managers, teachers, lawyers, etc.
	lab val p1dadocc4c p1momocc4c occ4c
	
	* [BEFORE PRIMARY SCHOOL] Household Financial Hardships
	recode bd09 (4/5 = 1 "1 rich")(3 = 2 "2 fair")(1/2 = 3 "3 poor"), gen(p1hhfinhard_cld)
	replace p1hhfinhard_cld = 1 if missing(p1hhfinhard_cld) & (a11 == 4 | a11 == 5)
	replace p1hhfinhard_cld = 1 if missing(p1hhfinhard_cld) & (a11 == 3)
	replace p1hhfinhard_cld = 1 if missing(p1hhfinhard_cld) & (a11 == 1 | a11 == 2)

	lab var p1hhfinhard_cld "P1 HH financial hardships (age<6)"
	
	* [CURRENT] Household Financial Hardships
	recode be19 (4/5 = 1 "1 rich")(3 = 2 "2 fair")(1/2 = 3 "3 poor"), gen(p1hhfinhard)
	/* wave1 - student survey */
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (b09 == 4 | b09 == 5)
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (b09 == 3)
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (b09 == 1 | b09 == 2)
	/* wave2 - parent survey */
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (w2be23 == 4 | w2be23 == 5)
	replace p1hhfinhard = 2 if missing(p1hhfinhard) & (w2be23 == 3)
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (w2be23 == 1 | w2be23 == 1)
	/*wave2 - student survey */
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (w2a09 == 4 | w2a09 == 5)
	replace p1hhfinhard = 2 if missing(p1hhfinhard) & (w2a09 == 3)
	replace p1hhfinhard = 1 if missing(p1hhfinhard) & (w2a09 == 1 | w2a09 == 1)
	
	lab var p1hhfinhard "P1 HH financial hardships"
	
	* Household Poverty (Received Subsistance Allowance)
	gen p1hhpoverty = 2 - be20
	replace p1hhpoverty = 2 - w2be24 if missing(p1hhpoverty)
	
	lab var p1hhpoverty "P1 HH received subsistance allowance"
	lab val p1hhpoverty binary
	
	* Household Income Level (Local)
	recode be21 (1/2 = 1 "1 low")(3 = 2 "2 middel")(4/5 = 3 "3 high"), gen(p1hhinc)
	replace p1hhinc = 1 if missing(p1hhinc) & (w2be25 == 1 | w2be25 == 2)
	replace p1hhinc = 1 if missing(p1hhinc) & (w2be25 == 3)
	replace p1hhinc = 1 if missing(p1hhinc) & (w2be25 == 4 | w2be25 == 5)
	
	lab var p1hhinc "P1 HH income level (local)"
	
** - Student Characteristics -
	
	* Gender
	gen s1male = stsex
	
	lab var s1male "S1 male student"
	lab def s1male 1 "1 male" 0 "0 female"
	lab val s1male s1male
	
	* Cognitive Functioning (STD. Score)
	gen s1stdcog = cog3pl
	replace s1stdcog = w2cog3pl if missing(s1stdcog)
	
	lab var s1stdcog "S1 STD cognitive functioning"
	
	* Overall Health Level
	gen s1hth = 1 if (a17 == 4 | a17 == 5)
	replace s1hth = 0 if a17 <= 3
	replace s1hth = 1 if missing(s1hth) & (w2c04 == 4 & w2c04 == 5)
	replace s1hth = 0 if missing(s1hth) & (w2c04 <= 3)
	
	lab var s1hth "S1 good health"
		
	* Serious Illness [before Primary School]
	gen p1cldill = 2 - bd10
	replace p1cldill = 2 - a10 if missing(p1cldill) & a10 != 3
	
	lab var p1cldill "P1 child serious illness (age<6)"
	
** - Family Functioning and Well-being
	
	* Primary Caregiver (PCG) before primary school
	gen p1paPCG = 1 if bd08 == 1
	replace p1paPCG = 0 if bd08 != 1 & !missing(bd08)
	
	lab var p1paPCG "P1 parent as PCG (age<6)"
	lab val p1paPCG binary
	
	* Parent-Child Communication
	alpha ba1401 ba1402 ba1403 ba1405, item //alpha = 0.8116
	egen p1talk = rowtotal(ba1401 ba1402 ba1403 ba1405) if (ba01==1 | ba01==2), missing
	alpha w2ba260*, item //alpha = 0.8604
	egen p2talk = rowtotal(w2ba260*) if (w2ba01==1 | w2ba01==2), missing
	replace p1talk = p2talk if missing(p1talk)
	
	lab var p1talk "P1 pa-cld talk scale"
	
	* Parent-Child Relationship (binary, close == 1)
	recode b2502 (1/2 = 0)(3 = 1), gen(s1dadrel)
	replace s1dadrel = 1 if missing(s1dadrel) & (w2a22 == 3)
	replace s1dadrel = 1 if missing(s1dadrel) & (w2a22 == 1 | w2a22 == 2)
	
	recode b2501 (1/2 = 0)(3 = 1), gen(s1momrel)
	replace s1momrel = 1 if missing(s1momrel) & (w2a23 == 3)
	replace s1momrel = 1 if missing(s1momrel) & (w2a23 == 1 | w2a23 == 2)
	
	* Family Cohesion
	alpha ba170*, item //alpha = 0.7599 - parent survey
	egen p1famcohe = rowtotal(ba170*), missing
	
	alpha b280*, item //alpha = 0.7734 - student survey
	egen s1famcohe = rowtotal(b280*), missing
	
	replace p1famcohe = s1famcohe if missing(p1famcohe)
	
	lab var p1famcohe "P1 family cohesion"
	
	* Harsh Parenting (Questions were same in both student and parent survey)
	alpha ba0801 ba0802 ba0805 ba0806 ba0807 ba0808, item //alpha = 0.7562 - wave1 parent survey
	egen p1harshprt = rowtotal(ba0801 ba0801 ba0801 ba0801 ba0801 ba0801) if (ba01==1 | ba01==2), missing
	
	alpha b2301 b2302 b2305 b2306 b2307 b2308, item //alpha = 0.7102 - wave1 student survey
	egen s1harshprt = rowtotal(b2301 b2302 b2305 b2306 b2307 b2308), missing
	
	alpha w2ba170*, item //alpha = 0.7823 - wave2 parent survey
	egen p2harshprt = rowtotal(w2ba170*) if (w2ba01==1 | w2ba01==2), missing
	
	alpha w2a200*, item //alpha = 0.7461 - wave2 student survey
	egen s2harshprt = rowtotal(w2a200*), missing
	
	replace p1harshprt = s1harshprt if missing(p1harshprt)
	replace p1harshprt = p2harshprt if missing(p1harshprt)
	replace p1harshprt = s2harshprt if missing(p1harshprt)
	
	lab var p1harshprt "P1 harsh parenting"
	
	* Parental Discipline //a binary indicator of did the parent value parental discipline
	gen p1padiscip = bb1201 if (ba01==1 | ba01==2)
	replace p1padiscip = w2bb1401 if missing(p1padiscip) & (w2ba01==1 | w2ba01==2)
	
	lab var p1padiscip "P1 parental discipline: child performance"
	
	* Parent Relationsip - Quarrel (binary indicator from student survey)
	gen s1paquarrel = b1002 - 1
	replace s1paquarrel = 2 - w2a16 if missing(s1paquarrel)
	
	lab var s1paquarrel "S1 parent often quarrel"
	lab val s1paquarrel binary
	
	* Parent Relationship - Get Along Well (binary indicator from student survey)
	gen s1parel = b1003 - 1
	replace s1parel = 2 - w2a17 if missing(s1parel)
	
	lab var s1parel "S1 parent get along well"
	lab val s1parel binary
	
	* Parent's Educational Expectation
	recode ba18 (1 = 7)(2 = 9)(3 = 11)(4  = 11)(5 = 12)(6 = 14)(7 = 16)(8 = 18) ///
				(9 = 22), gen(p1eduexp)
	recode w2ba29 (1 = 8)(2 = 9)(3 = 11)(4  = 11)(5 = 12)(6 = 14)(7 = 16)(8 = 18) ///
				  (9 = 22), gen(p2eduexp)
	replace p1eduexp = p2eduexp if missing(p2eduexp)
	
	lab var p1eduexp "P1 edu expectation"
	
	* Household Member Alcohol Use (binary - did any HH member drink alcohol almost everyweek?)
	gen p1hhalcohol = 1 if be1101 >= 3 & be1101 <= 5
	replace p1hhalcohol = 0 if be1101 <= 2
	replace p1hhalcohol = 2 - w2ba09 if missing(p1hhalcohol)
	
	lab var p1hhalcohol "P1 HH with alcohol use"
	lab val p1hhalcohol binary
	
	* Father with Alcohol Use (often getting drunk)
	gen s1daddrunk = 2 - b1001
	replace s1daddrunk = 2 - w2a15 if missing(s1daddrunk)
	
	lab var s1daddrunk "S1 dad often drunk"
	lab val s1daddrunk binary
	
	* Household Member Smoking or Tobacco Use (binary - did ana HH member smoke almost everyweek?)
	gen p1hhsmoke = 1 if be1102 >= 3 & be1102 <= 5
	replace p1hhsmoke = 0 if be1102 <= 2
	replace p1hhsmoke = 2 - w2ba10 if missing(p1hhsmoke)
	replace p1hhsmoke = 1 if missing(p1hhsmoke) & w2c17 == 1
	replace p1hhsmoke = 0 if missing(p1hhsmoke) & w2c17 == 2
	
	lab var p1hhsmoke "P1 HH smoking"
	lab val p1hhsmoke binary
	
	* Weights Adjustment: For new-entries, use their wave2 cross-sectional weight
	gen stuweight = sweight
	replace stuweight = w2sweight if missing(stuweight)
	
	lab var stuweight "cross-sectional student weight"
	
** - Multiple Imputation -
	
	global varlist s2stdchn s2stdmath s2stdeng /* Student Outcome */ ///
				   s2padivorce s1hktype s1migrate s1grandpa p2dadeth p2mometh p1hhfamcare /* family BCG */ ///
				   p1dadparty p1momparty p1dadedu p1momedu p1dadocc4c p1momocc4c p1hhfinhard_cld p1hhfinhard p1hhpoverty p1hhinc /* SES */ ///
				   s1male s1onech s1sibsize s1stdcog s1hth p1cldill /* student characteristics */ ///
				   p1paPCG p1talk p1famcohe p1harshprt p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke /* family functioning */

	* Missing Value Table	
	misstable sum $varlist if !missing(s2padivorce) == 1, all
	
	global imvar s2stdchn s2stdmath s2stdeng ///
				 s1hktype s1migrate p2dadeth p2mometh p1hhfamcare ///
				 p1dadparty p1momparty p1dadedu p1momedu p1dadocc4c p1momocc4c p1hhfinhard_cld p1hhfinhard p1hhpoverty p1hhinc ///
				 s1male s1stdcog s1hth p1cldill ///
				 p1paPCG p1talk p1famcohe p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke
		
	* Imputation
	mi set flong
	mi register imputed $imvar
	mi register regular s1onech s1sibsize s1grandpa p1harshprt
		
	#delimit ;
	mi impute chained (logit) s1hktype 
					  (logit) s1migrate
					  (logit) p2dadeth
					  (logit) p2mometh
					  (logit) p1hhfamcare
					  (logit) p1dadparty
					  (logit) p1momparty
					  (ologit) p1dadedu
					  (ologit) p1momedu
					  (ologit) p1dadocc4c
					  (ologit) p1momocc4c
					  (ologit) p1hhfinhard_cld
					  (ologit) p1hhfinhard
					  (ologit) p1hhpoverty
					  (ologit) p1hhinc
					  (logit) s1male
					  (regress) s1stdcog
					  (logit) s1hth
					  (logit) p1cldill
					  (logit) p1paPCG
					  (truncreg, ll(1) ul(12)) p1talk
					  (truncreg, ll(1) ul(36)) p1famcohe
					  (logit) p1padiscip
					  (logit) s1paquarrel
					  (logit) s1parel
					  (truncreg, ll(7) ul(22)) p1eduexp
					  (logit) p1hhalcohol
					  (logit) s1daddrunk
					  (logit) p1hhsmoke = s1onech s1sibsize s1grandpa p1harshprt
		[pw = stuweight] if !missing(s2padivorce),
		add(10) replace dots force augment
	;
	
	* MI results: distribution of imputed varaibles
	foreach var in $varlist {
		tab _mi_m if !missing(s2padivorce), sum(`var')
	}
		
** - Descriptive Statistics -
	/* NOTE: Imputed values at the 5th imputation were used. */
	
	* Sample Selection since the Outcomes were NOT imputed
	egen scoresample = rowmiss(s2std*) if _mi_m == 4
	replace scoresample = . if scoresample >= 1
	replace scoresample = 1 if scoresample == 0
	
	lab var scoresample "SCORE sample tag=1" //outcome - exam percentage rank
	
	* Proportion of Divorced Families
	prop s2padivorce [pw = stuweight] if _mi_m == 4
	
	* Descriptives of Selection Factors
	quietly tab p1dadedu, gen(p1dadedu_dum)
	quietly tab p1momedu, gen(p1momedu_dum)
	quietly tab p1dadocc4c, gen(p1dadocc4c_dum)
	quietly tab p1momocc4c, gen(p1momocc4c_dum)
	quietly tab p1hhfinhard_cld, gen(p1hhfinhard_cld_dum)
	quietly tab p1hhfinhard, gen(p1hhfinhard_dum)
	quietly tab p1hhinc, gen(p1hhinc_dum)
	
	global descvar s1hktype s1migrate s1grandpa p2dadeth p2mometh p1hhfamcare ///
				   p1dadparty p1momparty p1dadedu_dum1 p1dadedu_dum2 p1dadedu_dum3 p1momedu_dum1 p1momedu_dum2 p1momedu_dum3 p1dadocc4c_dum1 p1dadocc4c_dum2 p1dadocc4c_dum3 p1dadocc4c_dum4 p1momocc4c_dum1 p1momocc4c_dum2 p1momocc4c_dum3 p1momocc4c_dum4 p1hhfinhard_cld_dum1 p1hhfinhard_cld_dum2 p1hhfinhard_cld_dum3 p1hhfinhard_dum1 p1hhfinhard_dum2 p1hhfinhard_dum3 p1hhpoverty p1hhinc_dum1 p1hhinc_dum2 p1hhinc_dum3 ///
				   s1male s1onech s1sibsize s1stdcog s1hth p1cldill ///
				   p1paPCG p1talk p1famcohe p1harshprt p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke
	
		* Full Sample
		sum $descvar if _mi_m == 4 & !missing(s2padivorce) [aw = stuweight], separator(50)
		
		* Divorced Sample
		sum $descvar if s2padivorce == 1 & _mi_m == 4 [aw = stuweight], separator(50)
		
		* Descriptives - Married Sample
		sum $descvar if s2padivorce == 0 & _mi_m == 4 [aw = stuweight], separator(50)
	
		* T-tests (unweighted)
		foreach var in $descvar {
			dis "When the selection factor is `var'"
			dis " "
			ttest `var' if _mi_m == 4, by(s2padivorce)
			dis " "
			dis "----------------------- S P A C E -----------------------"
			dis " "
		}
		
	* Descriptives of Outcomes
	
		* Full Sample 
		sum s2std* if scoresample == 1 & !missing(s2padivorce) [aw = stuweight]
		
		* Divorced Sample
		sum s2std* if scoresample == 1 & s2padivorce == 1 [aw = stuweight]
		
		* Married Sample
		sum s2std* if scoresample == 1 & s2padivorce == 0 [aw = stuweight]
		
		* T-tests (unweighted)
		foreach var in s2stdchn s2stdmath s2stdeng {
			dis "When the outcome is `var'"
			dis " "
			ttest `var' if scoresample == 1, by(s2padivorce)
			dis " "
			dis "----------------------- S P A C E -----------------------"
			dis " "
		}
		
** - Regular and MI Estimations -
	
	* Regualar Logit Estimation
	global indepvar s1hktype s1migrate s1grandpa p2dadeth p2mometh p1hhfamcare ///
					p1dadparty p1momparty i.p1dadedu i.p1momedu i.p1dadocc4c i.p1momocc4c i.p1hhfinhard_cld i.p1hhfinhard p1hhpoverty i.p1hhinc ///
					s1male s1onech s1sibsize s1stdcog s1hth p1cldill ///
					p1paPCG p1talk p1famcohe p1harshprt p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke
	
	logit s2padivorce $indepvar [pw = stuweight] if _mi_m == 0, vce(cluster ctyids)
	
	matrix regcoef = e(b)
	
	* MI Estimation
	mi estimate, saving(miest, replace): logit s2padivorce $indepvar [pw = stuweight], vce(cluster ctyids)
	
	matrix impcoef = e(b_mi)
	
	* Regular Logit Estimation BUT Use the MI values at 5th Imputation
	logit s2padivorce $indepvar [pw = stuweight] if _mi_m == 4, vce(cluster ctyids)
	
	matrix mivcoef = e(b)
	
		/* Export Logistic Regression Results */
		est store PsLogit
		
		outreg2 PsLogit using "${path}/table_and_chart/PsLogit.xls", replace excel ///
			sideway stats(coef se) dec(3) alpha(0.001, 0.01, 0.05)
	
	* Comparison
	matrix jointcoef = regcoef \ impcoef \ mivcoef
	matrix rownames jointcoef = RegularEst ImputedEst MI5Est
	matrix list jointcoef
	
	* MI Predicted Prppensities
	mi predict xb_mi using miest
	mi xeq: generate prDivorce = invlogit(xb_mi) * 100
	
	lab var prDivorce "Pr S2 divorce"
	
* === < Average Treatment Effect (Total Effects) > ===
	
	* Compute the Within P-score (within each MI set)

	quietly tab p1dadedu, gen(p1dadedu_dum)
	quietly tab p1momedu, gen(p1momedu_dum)
	quietly tab p1dadocc4c, gen(p1dadocc4c_dum)
	quietly tab p1momocc4c, gen(p1momocc4c_dum)
	quietly tab p1hhfinhard_cld, gen(p1hhfinhard_cld_dum)
	quietly tab p1hhfinhard, gen(p1hhfinhard_dum)
	quietly tab p1hhinc, gen(p1hhinc_dum)

	global pscorevar s1hktype s1migrate s1grandpa p2dadeth p2mometh p1hhfamcare ///
					 p1dadparty p1momparty p1dadedu_dum2 p1dadedu_dum3 p1momedu_dum2 p1momedu_dum3 p1dadocc4c_dum2 p1dadocc4c_dum3 p1dadocc4c_dum4 p1momocc4c_dum2 p1momocc4c_dum3 p1momocc4c_dum4 p1hhfinhard_cld_dum2 p1hhfinhard_cld_dum3 p1hhfinhard_dum2 p1hhfinhard_dum3 p1hhpoverty p1hhinc_dum2 p1hhinc_dum3 ///
					 s1male s1onech s1sibsize s1stdcog s1hth p1cldill ///
					 p1paPCG p1talk p1famcohe p1harshprt p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke

	mi xeq: pscore s2padivorce $pscorevar [pw = stuweight] if !missing(s2padivorce), pscore(MIpsDivorce) logit
	
	* Compute the across P-score for averaging the P-score (across 10 MI sets)
	drop if _mi_m == 0
	drop if missing(MIpsDivorce)
	
	collapse MIpsDivorce, by(ids)
	
	save MIpscore.dta, replace
	
	snapshot restore 1
	
	merge 1:1 ids using "${datadir}MIpscore.dta", keep(match master) nogenerate
	
	* [CHINESE EXAM] ATT and ATE using K-NN by looping from k = 1 to k = 3
	forval k = 1/3 {
		gen att`k' = .
		gen attse`k' = .
		gen ate`k' = .
		gen atese`k' = .
		
		forval i = 1/10 {
			mi xeq `i': psmatch2 s2padivorce ${indepvar}, out(s2stdchn) logit common n(`k') ate ai(`k') quietly
			
			replace att`k' = r(att) if _mi_m == `i'
			replace attse`k' = r(seatt) if _mi_m == `i'
			replace ate`k' = r(ate) if _mi_m == `i'
			replace atese`k' = r(seate) if _mi_m == `i'
		}
	}
	
	forval k = 1/3 {
		gen tatt`k' = att`k'/attse`k'
		gen tate`k' = ate`k'/atese`k'
	}
	
	/* Compare ATT and ATE by K-NN from k = 1 to k = 3 */
	forval k = 1/3 {
		table _mi_m if _mi_m >= 1, c(m att`k' m attse`k' m ate`k' m atese`k')
	}
	
	gen att_chn = att1
	gen attse_chn = attse1
	gen ate_chn = ate1
	gen atese_chn = atese2
	gen tatt_chn = att_chn/attse_chn
	gen tate_chn = ate_chn/atese_chn
	
	* [MATH EXAM] ATT and ATE using 1-NN
	gen att_math = .
	gen attse_math = .
	gen ate_math = .
	gen atese_math = .
	
	forval i = 1/10 {
		mi xeq `i': psmatch2 s2padivorce ${indepvar}, out(s2stdmath) logit common n(1) ate ai(1) quietly
			
		replace att_math = r(att) if _mi_m == `i'
		replace attse_math = r(seatt) if _mi_m == `i'
		replace ate_math = r(ate) if _mi_m == `i'
		replace atese_math = r(seate) if _mi_m == `i'
	}
	
	gen tatt_math = att_math/attse_math
	gen tate_math = ate_math/atese_math
		
	* [ENGLISH EXAM] ATT and ATE using 1-NN
	gen att_eng = .
	gen attse_eng = .
	gen ate_eng = .
	gen atese_eng = .
	
	forval i = 1/10 {
		mi xeq `i': psmatch2 s2padivorce ${indepvar}, out(s2stdeng) logit common n(1) ate ai(1) quietly
			
		replace att_eng = r(att) if _mi_m == `i'
		replace attse_eng = r(seatt) if _mi_m == `i'
		replace ate_eng = r(ate) if _mi_m == `i'
		replace atese_eng = r(seate) if _mi_m == `i'
	}
	
	gen tatt_eng = att_eng/attse_eng
	gen tate_eng = ate_eng/atese_eng
	
	lab var att_chn "ATT Chinese by Imputaion"
	lab var ate_chn "ATE Chinese by Imputaion"
	lab var attse_chn "S.E. ATT Chinese by Imputaion"
	lab var atese_chn "S.E. ATE Chinese by Imputaion"
	lab var tatt_chn "Ttest ATT Chinese by Imputaion"
	lab var tate_chn "Ttest ATE Chinese by Imputaion"
	
	lab var att_math "ATT Math by Imputaion"
	lab var ate_math "ATE Math by Imputaion"
	lab var attse_math "S.E. ATT Math by Imputaion"
	lab var atese_math "S.E. ATE Math by Imputaion"
	lab var tatt_math "Ttest ATT Math by Imputaion"
	lab var tate_math "Ttest ATE Math by Imputaion"
	
	lab var att_eng "ATT English by Imputaion"
	lab var ate_eng "ATE English by Imputaion"
	lab var attse_eng "S.E. ATT English by Imputaion"
	lab var atese_eng "S.E. ATE English by Imputaion"
	lab var tatt_eng "Ttest ATT English by Imputaion"
	lab var tate_eng "Ttest ATE English by Imputaion"	
	
	table _mi_m if _mi_m >= 1, c(m att_chn  m tatt_chn  m ate_chn  m tate_chn) row
	table _mi_m if _mi_m >= 1, c(m att_math m tatt_math m ate_math m tate_math) row
	table _mi_m if _mi_m >= 1, c(m att_eng  m tatt_eng  m ate_eng  m tate_eng) row
	
	table _mi_m if _mi_m >= 1, c(m att_chn  m attse_chn  m ate_chn  m atese_chn) row
	table _mi_m if _mi_m >= 1, c(m att_math m attse_math m ate_math m atese_math) row
	table _mi_m if _mi_m >= 1, c(m att_eng  m attse_eng  m ate_eng  m atese_eng) row
	
	* Plot the ATT and ATE with 95% CI		
	snapshot save, label("orginal data")
	
	collapse att_* attse_* ate_* atese_*, by(_mi_m)
	
	drop if _mi_m == 0
	
	foreach sub in chn math eng {
		gen attcilow_`sub' = att_`sub' - 1.96 * attse_`sub'
		gen attcihig_`sub' = att_`sub' + 1.96 * attse_`sub'
		gen atecilow_`sub' = ate_`sub' - 1.96 * atese_`sub'
		gen atecihig_`sub' = ate_`sub' + 1.96 * atese_`sub'
	}
	
	sum att_chn ate_chn att_math ate_math att_eng ate_eng
	
	/* calculate absolute differences */
	foreach sub in chn math eng {
		quiet sum att_`sub'
		gen diffatt_`sub' = abs(att_`sub' - r(mean))/r(sd)
		
		quiet sum ate_`sub'
		gen diffate_`sub' = abs(ate_`sub' - r(mean))/r(sd)
	}
	
	egen difftotal = rowtotal(diff*)
	egen diffatt   = rowtotal(diffatt*)
	egen diffate   = rowtotal(diffate*)
	
	sort difftotal
	list _mi_m difftotal diffatt diffate //imputation set 4 is closest to both mean ATT and mean ATE
	
	gen pseudomi1 = _mi_m - 0.15
	gen pseudomi2 = _mi_m + 0.15
	gen pseudomi3 = _mi_m
	replace pseudomi1 = . if pseudomi1 < 0
	replace pseudomi2 = . if pseudomi2 < 1
	replace pseudomi3 = . if pseudomi3 < 0.5
		
	set scheme cleanplots
	
	/* Chinese Exam */
	#delimit;
	tw (sca att_chn pseudomi1, mcol(maroon%90))
	   (rcap attcilow_chn attcihig_chn pseudomi1, lcol(maroon%60))
	   (sca ate_chn pseudomi2, mcol(navy%90))
	   (rcap atecilow_chn atecihig_chn pseudomi2, lcol(navy%60)),
	   yline(0, lcol(black))
	   ylab(#5)
	   xsca(range(1 10) extend)
	   xlab(#10)
	   xti(" ")
	   ysca(range(-0.4 0.2) extend)
	   yti("Treatment Effect (95% CI)")
	   legend(order(1 "ATT" 3 "ATE") pos(6) rows(1) ring(0))
	   subti("Chinese Exam")
	   text(0.15 5.5 "Mean ATT: -0.152 Mean ATE: - 0.205", place(c))
	   name(rankchn, replace)
	;

	/* Math Exam */
	#delimit;
	tw (sca att_math pseudomi1, mcol(maroon%90))
	   (rcap attcilow_math attcihig_math pseudomi1, lcol(maroon%60))
	   (sca ate_math pseudomi2, mcol(navy%90))
	   (rcap atecilow_math atecihig_math pseudomi2, lcol(navy%60)),
	   yline(0, lcol(black))
	   ylab(#5)
	   xsca(range(1 10) extend)
	   xlab(#10)
	   xti("Imputation")
	   ysca(range(-0.4 0.2) extend)
	   yti(" ")
	   legend(order(1 "ATT" 3 "ATE") pos(6) rows(1) ring(0))
	   subti("Math Exam")
	   text(0.15 5.5 "Mean ATT: -0.197 Mean ATE: - 0.203", place(c))
	   name(rankmath, replace)
	;

	/* English Exam */
	#delimit;
	tw (sca att_eng pseudomi1, mcol(maroon%90))
	   (rcap attcilow_eng attcihig_eng pseudomi1, lcol(maroon%60))
	   (sca ate_eng pseudomi2, mcol(navy%90))
	   (rcap atecilow_eng atecihig_eng pseudomi2, lcol(navy%60)),
	   yline(0, lcol(black))
	   ylab(#5)
	   xsca(range(1 10))
	   xlab(#10)
	   xti(" ")
	   ysca(range(-0.4 0.2) extend)
	   yti(" ")
	   legend(order(1 "ATT" 3 "ATE") pos(6) rows(1) ring(0))
	   subti("English Exam")
	   text(0.15 5.5 "Mean ATT: -0.110 Mean ATE: - 0.189", place(c))
	   name(rankeng, replace)
	;

	gr combine rankchn rankmath rankeng, rows(1) ycommon xsize(20) ysize(6) ///
	   note("{it:Source}: CEPS, Matched Sample based on 1-Nearest Neighbor Matching", size(small)) ///
	   name(totaleff, replace)
	
	gr export "${path}/table_and_chart/ATEmatching.png", replace
	
	snapshot restore 1

* === < Heterogeous Treatment Effect > ===
/* NOTE: The 5th imputation was used for estimating HTE */

global pscorevar s1hktype s1migrate s1grandpa p2dadeth p2mometh p1hhfamcare ///
				 p1dadparty p1momparty p1dadedu_dum2 p1dadedu_dum3 p1momedu_dum2 p1momedu_dum3 p1dadocc4c_dum2 p1dadocc4c_dum3 p1dadocc4c_dum4 p1momocc4c_dum2 p1momocc4c_dum3 p1momocc4c_dum4 p1hhfinhard_cld_dum2 p1hhfinhard_cld_dum3 p1hhfinhard_dum2 p1hhfinhard_dum3 p1hhpoverty p1hhinc_dum2 p1hhinc_dum3 ///
				 s1male s1onech s1sibsize s1stdcog s1hth p1cldill ///
				 p1paPCG p1talk p1famcohe p1harshprt p1padiscip s1paquarrel s1parel p1eduexp p1hhalcohol s1daddrunk p1hhsmoke

	** HTE: Matching Smoothing Method
	#delimit ;
	hte ms s2stdchn = s2padivorce $pscorevar if _mi_m == 4 & (s2stdchn >= -5 & s2stdchn <= 5),
		logit common n(1) quietly
		ttopts(msym(Oh) mcolor(maroon%90) msize(vsmall) mlwid(vthin) jitter(10))
		tcopts(msym(Oh) mcolor(midblue%60) msize(vsmall) mlwid(vvthin) jitter(10))
		lpoly(lcol(black))
		lpolyci(acol(gs8%50))
		yline(0)
		ysca(range(-5 5))
		legend(order(1 "Married" 2 "Divorced") bplace(s) rows(1) ring(0) fcol(white%50) lstyle(plotregion))
		ti(" ")
		xti(" ")
		yti("Treatment Effect (95% CI)")
		subti("Chinese Exam")
		name(ploychn, replace)
	;

	#delimit ;
	hte ms s2stdmath = s2padivorce $pscorevar if _mi_m == 4 & (s2stdmath >= -5 & s2stdmath <= 5),
		logit common n(1) quietly
		ttopts(msym(Oh) mcolor(maroon%90) msize(vsmall) mlwid(vthin) jitter(10))
		tcopts(msym(Oh) mcolor(midblue%60) msize(vsmall) mlwid(vvthin) jitter(10))
		lpoly(lcol(black))
		lpolyci(acol(gs8%50))
		yline(0)
		ysca(range(-5 5))
		legend(order(1 "Married" 2 "Divorced") bplace(s) rows(1) ring(0) fcol(white%50) lstyle(plotregion))
		ti(" ")
		yti(" ")
		subti("Math Exam")
		name(ploymath, replace)
	;

	#delimit ;
	hte ms s2stdeng = s2padivorce $pscorevar if _mi_m == 4 & (s2stdeng >= -5 & s2stdeng <= 5),
		logit common n(1) quietly
		ttopts(msym(Oh) mcolor(maroon%90) msize(vsmall) mlwid(vthin) jitter(10))
		tcopts(msym(Oh) mcolor(midblue%60) msize(vsmall) mlwid(vvthin) jitter(10))
		lpoly(lcol(black))
		lpolyci(acol(gs8%50))
		yline(0)
		ysca(range(-5 5))
		legend(order(1 "Married" 2 "Divorced") bplace(s) rows(1) ring(0) fcol(white%50) lstyle(plotregion))
		ti(" ")
		xti(" ")
		yti(" ")
		subti("English Exam")
		name(ployeng, replace)
	;
	
	gr combine ploychn ploymath ployeng, rows(1) ycommon xsize(12) ysize(4) ///
	   note("{it:Source}: CEPS, HTE using Matching-Smoothing Method", size(small)) ///
	   name(ployeff, replace)
	
	gr export "${path}/table_and_chart/HTEsmoothing.png", replace

	** HTE: Stratification Multilevel Method 
	hte sm s2stdchn  = s2padivorce $pscorevar if _mi_m == 4 & !missing(s2padivorce) [pw = stuweight], logit comsup join(1 2 3, 4 5 6, 7 8 9) nograph
	hte sm s2stdmath = s2padivorce $pscorevar if _mi_m == 4 & !missing(s2padivorce) [pw = stuweight], numblo(3) logit comsup join(1 2 3, 4 5 6, 7 8 9) nograph
	hte sm s2stdeng  = s2padivorce $pscorevar if _mi_m == 4 & !missing(s2padivorce) [pw = stuweight], numblo(3) logit comsup join(1 2 3, 4 5 6, 7 8 9) nograph
	
	** HTE: Local Polynomial Matching Smoothing by Propensity Score Strata
	pscore s2padivorce $pscorevar [pw = stuweight] if _mi_m == 4 & !missing(s2padivorce), pscore(psDivorce) blockid(psBlock) numblo(3) logit comsup
	
	/* Balancing Test */
	pstest $pscorevar if _mi_m == 4 & !missing(s2padivorce), treated(s2padivorce) mweight(psDivorce) support(comsup) comsup
	
	/* Stratum Construction */
	tab psBlock, sum(psDivorce)
	
	recode psBlock (1/2= 1 "1 least likely") ///
				   (3/6 = 2 "2 moderately likely") ///
				   (7/9 = 3 "3 most likely") if scoresample == 1 & comsup == 1, ///
				   gen(straDivorce)
	
	lab var straDivorce "divorce propensity strata"
	
	tab straDivorce, sum(psDivorce)
			
		/* Strata Specific ATE and ATT using 1-NN Matching */
		foreach sub in chn math eng {
			forval i = 1/3 {
				dis "while the outcome is `sub' rank and the strata is `i'"
				dis " "
				psmatch2 s2padivorce if _mi_m == 4 & straDivorce == `i', out(s2std`sub') pscore(psDivorce) n(1) ate ai(1) quietly logit
				dis " "
				dis "----------------- S P A C E -----------------"
				dis " "
				
				quietly scalar att`sub'`i' = r(att)
				quietly scalar attse`sub'`i' = r(seatt)
				quietly scalar tatt`sub'`i' = att`sub'`i'/attse`sub'`i'
				quietly scalar ate`sub'`i' = r(ate)
				quietly scalar atese`sub'`i' = r(seate)
				quietly scalar tate`sub'`i' = ate`sub'`i'/atese`sub'`i'
			}
		}

		foreach sub in chn math eng {
			matrix straTE_`sub' = (1,1,1,1,1,1,1)
			
			forval i = 1/3 {
				matrix define temp = (`i', att`sub'`i', attse`sub'`i', tatt`sub'`i', ate`sub'`i', atese`sub'`i', tate`sub'`i')
				matrix straTE_`sub' = straTE_`sub' \ temp
			}
			
			matrix colnames straTE_`sub' = PStrata ATT SE-ATT t-test ATE SE-ATE t-test
			matrix rownames straTE_`sub' = Null Stra1 Stra2 Stra3
		}
		
		matrix list straTE_chn
		matrix list straTE_math
		matrix list straTE_eng
		
		matrix straTE_sub = straTE_chn \ straTE_math \ straTE_eng
		matrix list straTE_sub
		
		/* Store ATE and S.E. */
		foreach sub in chn math eng {
			gen hte_`sub' = .
			gen htese_`sub' = .
			
			forval i = 1/3{
				replace hte_`sub' = ate`sub'`i' if straDivorce == `i'
				replace htese_`sub' = atese`sub'`i' if straDivorce == `i'
			}
		}

	** OLS Regression Clustered within Schools
	
	* Unadjusted Effects
	foreach sub in chn math eng {
		reg s2std`sub' s2padivorce if !missing(straDivorce) & scoresample == 1, vce(cluster schids)
		est store unmod_`sub'
		
		matrix unmat_`sub' = r(table)
		gen unate_`sub' = unmat_`sub'[1,1] if !missing(straDivorce) & scoresample == 1
		gen unse_`sub'  = unmat_`sub'[2,1] if !missing(straDivorce) & scoresample == 1
	}
	
	* Adjusted Effetcs
	foreach sub in chn math eng {
		reg s2std`sub' s2padivorce psDivorce p1paPCG s1paquarrel s1parel if !missing(straDivorce) & scoresample == 1, vce(cluster schids)
		est store ajmod_`sub'
		
		matrix ajmat_`sub' = r(table)
		gen ajate_`sub' = ajmat_`sub'[1,1] if !missing(straDivorce) & scoresample == 1
		gen ajse_`sub'  = ajmat_`sub'[2,1] if !missing(straDivorce) & scoresample == 1
	}
		
	* Comparison
	est table unmod_chn ajmod_chn unmod_math ajmod_math unmod_eng ajmod_eng, star(0.05 0.01 0.001) b(%8.3f)
	
	outreg2 [unmod_chn ajmod_chn unmod_math ajmod_math unmod_eng ajmod_eng] ///
		using "${path}/table_and_chart/ATERegression.xls", replace excel ///
		stats(coef se) dec(3) alpha(0.001, 0.01, 0.05)
		
	* Adjuested Effects within Propensity Strata
	foreach sub in chn math eng {
		reg s2std`sub' i.s2padivorce psDivorce p1paPCG s1paquarrel s1parel i.straDivorce#i.s2padivorce if !missing(straDivorce) & scoresample == 1, vce(cluster schids)
		est store stramod_`sub'
	}
	
	outreg2 [stramod_chn stramod_math stramod_eng] ///
		using "${path}/table_and_chart/HTERegression.xls", replace excel ///
		stats(coef se) dec(3) alpha(0.001, 0.01, 0.05)
	
		/* Marginal Effects over Propensity Strata */
		foreach sub in chn math eng {
			est restore stramod_`sub'
			margins, dydx(s2padivorce) over(straDivorce)
			
			matrix hte_`sub' = r(table)
			matrix htesoft_`sub' = hte_`sub'[1..6, 4..6]
			matrix colnames htesoft_`sub' = Strata1 Strata2 Strata3
		}
				
		matrix htemat = htesoft_chn \ htesoft_math \ htesoft_eng
		matrix list htemat
		
		foreach sub in chn math eng {
			gen reghte_`sub' = .
			gen regse_`sub' = .
			
			forval i = 1/3 {
				replace reghte_`sub' = htesoft_`sub'[1, `i'] if straDivorce == `i'
				replace regse_`sub'  = htesoft_`sub'[2, `i'] if straDivorce == `i'
			}
		}
		
		foreach sub in chn math eng {
			table straDivorce, c(m reghte_`sub' m regse_`sub' N regse_`sub') row
		}
	
	/* OLS Regression: UnAdj-ATE, Adj-ATE, Strata-HTE */
	foreach sub in chn math eng {
		table straDivorce, c(m unate_`sub' m ajate_`sub' m reghte_`sub' N reghte_`sub') row
	}
	
	lab var hte_chn    "HTE Chinese Matching by Strata"
	lab var htese_chn  "S.E. HTE Chinese Matching by Strata"
	lab var unate_chn  "ATE Chinese Unadj. Regression"
	lab var unse_chn   "S.E. ATE Chinese Unadj. Regression"
	lab var ajate_chn  "ATE Chinese Adj. Regression"
	lab var ajse_chn   "S.E. ATE Chinese Adj. Regression"
	lab var reghte_chn "HTE Chinese Regression by Strata"
	lab var regse_chn  "S.E. HTE Chinese Regression by Strata"
	
	lab var hte_math    "HTE Math Matching by Strata"
	lab var htese_math  "S.E. HTE Math Matching by Strata"
	lab var unate_math  "ATE Math Unadj. Regression"
	lab var unse_math   "S.E. ATE Math Unadj. Regression"
	lab var ajate_math  "ATE Math Adj. Regression"
	lab var ajse_math   "S.E. ATE Math Adj. Regression"
	lab var reghte_math "HTE Math Regression by Strata"
	lab var regse_math  "S.E. HTE Math Regression by Strata"

	lab var hte_eng    "HTE English Matching by Strata"
	lab var htese_eng  "S.E. HTE English Matching by Strata"
	lab var unate_eng  "ATE English Unadj. Regression"
	lab var unse_eng   "S.E. ATE English Unadj. Regression"
	lab var ajate_eng  "ATE English Adj. Regression"
	lab var ajse_eng   "S.E. ATE English Adj. Regression"
	lab var reghte_eng "HTE English Regression by Strata"
	lab var regse_eng  "S.E. HTE English Regression by Strata"
	
	sum hte_* htese_* unate_* unse_* ajate_* ajse_* reghte_* regse_*, separator(3)
	
	/* Plot HTE (both Matching and Regression Methods) */
	snapshot save, label("final analysis")
	
	collapse hte_* htese_* unate_* unse_* ajate_* ajse_* reghte_* regse_*, by(straDivorce)
	drop if straDivorce == .
	
		* 1. Calculate 95% Bounding
		foreach sub in chn math eng {
			/* HTE: Matching */
			gen htelow_`sub' = hte_`sub' - 1.96 * htese_`sub'
			gen htehig_`sub' = hte_`sub' + 1.96 * htese_`sub'
			
			/* ATE (unadjuested): Regression */
			gen unatelow_`sub' = unate_`sub' - 1.96 * unse_`sub'
			gen unatehig_`sub' = unate_`sub' + 1.96 * unse_`sub'
			
			/* ATE (adjuested): Regression */
			gen ajatelow_`sub' = ajate_`sub' - 1.96 * ajse_`sub'
			gen ajatehig_`sub' = ajate_`sub' + 1.96 * ajse_`sub'
			
			/* HTE: Regression */
			gen reghtelow_`sub' = reghte_`sub' - 1.96 * regse_`sub'
			gen reghtehig_`sub' = reghte_`sub' + 1.96 * regse_`sub'
		}
		
		* 2. HTE by Propensity Stratum (Matching)
		gen pseudoX1 = straDivorce - 0.1
		gen pseudoX2 = straDivorce + 0.1
		
		#delimit; 
		tw (sca hte_chn pseudoX1, mcol(purple%75))
		   (rcap htelow_chn htehig_chn pseudoX1, lcol(purple%50))
		   (sca hte_math straDivorce, mcol(ebblue%75))
		   (rcap htelow_math htehig_math straDivorce, lcol(ebblue%50))
		   (sca hte_eng pseudoX2, mcol(midgreen%75))
		   (rcap htelow_eng htehig_eng pseudoX2, lcol(midgreen%50)),
		   yline(0, lcol(black))
		   ysca(range(-0.4 0.2) extend)
		   ylab(#4)
		   yti("Treatment Effect (95% CI)")
		   xsca(range(0.7 3.3))
		   xlab(1 "Less Likely" 2 "Moderately Likely" 3 "Most Likely") 
		   xti("Propensity Stratum")
		   legend(order(1 "Chinese Exam" 3 "Math Exam" 5 "English Exam") pos(6) rows(1) ring(1))
		   subti("1-NN Matching for Matched Sample")
		   /* note("{it:Source}: CEPS, Matched Sample based on 1-Nearest Neighbor Matching", size(small) span) */
		   name(psHTE, replace)
		   ;
		   
		gr export "${path}/table_and_chart/HTEmatching.png", replace
		
		* 3. ATE (Unadjusted and Adjusted) (Regression)
		expand 2
		
		gen adjusted = 0 in 1/3
		replace adjusted = 1 in 4/6
		
		foreach sub in chn math eng {
			gen regate_`sub' = unate_`sub' in 1/3
			replace regate_`sub' = ajate_`sub' in 4/6
			
			gen regatelow_`sub' = unatelow_`sub' in 1/3
			replace regatelow_`sub'= ajatelow_`sub' in 4/6
			
			gen regatehig_`sub' = unatehig_`sub' in 1/3
			replace regatehig_`sub'= ajatehig_`sub' in 4/6
		}
		
		gen fakeX1 = adjusted - 0.1
		gen fakeX2 = adjusted + 0.1
		
		#delimit;
		tw (sca regate_chn fakeX1, mcol(purple%75))
		   (rcap regatelow_chn regatehig_chn fakeX1, lcol(purple%50))
		   (sca regate_math adjusted, mcol(ebblue%75))
		   (rcap regatelow_math regatehig_math adjusted, lcol(ebblue%50))
		   (sca regate_eng fakeX2, mcol(midgreen%75))
		   (rcap regatelow_eng regatehig_eng fakeX2, lcol(midgreen%50)),
		   yline(0, lcol(black))
		   ysca(range(-0.4 0.1) extend)
		   ylab(#6, ticks)
		   yti("Treatment Effect (95% CI)")
		   xsca(range(-0.5 1.5))
		   xlab(0 "Unadjusted Effects" 1 "Adjusted Effects") 
		   xti(" ")
		   legend(order(1 "Chinese Exam" 3 "Math Exam" 5 "English Exam") pos(6) rows(1) ring(1))
		   note("{it:Source}: CEPS, OLS Regressions", size(small) span)
		   name(regATE, replace)
		;
		
		gr export "${path}/table_and_chart/ATEregression.png", replace
		
		* 4. HTE by Propensity Stratum (Regression)
		#delimit; 
		tw (sca reghte_chn pseudoX1, mcol(purple%75))
		   (rcap reghtelow_chn reghtehig_chn pseudoX1, lcol(purple%50))
		   (sca reghte_math straDivorce, mcol(ebblue%75))
		   (rcap reghtelow_math reghtehig_math straDivorce, lcol(ebblue%50))
		   (sca reghte_eng pseudoX2, mcol(midgreen%75))
		   (rcap reghtelow_eng reghtehig_eng pseudoX2, lcol(midgreen%50)),
		   yline(0, lcol(black))
		   ysca(range(-0.6 0.2) extend)
		   ylab(#4)
		   yti("Treatment Effect (95% CI)")
		   xsca(range(0.7 3.3))
		   xlab(1 "Less Likely" 2 "Moderately Likely" 3 "Most Likely") 
		   xti("Propensity Stratum")
		   legend(order(1 "Chinese Exam" 3 "Math Exam" 5 "English Exam") pos(6) rows(1) ring(1))
		   subti("Linear Regression for Matched Sample")
		   /* note("{it:Source}: CEPS, OLS Regressions on Matched Sample ", size(small) span) */
		   name(regHTE, replace)
		   ;
		
		gr combine psHTE regHTE, rows(1) ycommon xsize(20) ysize(9) ///
			note("{it:Source}: China Educational Panel Survey: 2013 - 2015", size(small)) ///
			name(jointHTE, replace)
		
		gr export "${path}/table_and_chart/HTEjoint.png", replace

		snapshot restore 2

capture log close

* =========================================================================
* Author: Chenxin Tan
* Last Modified: June 26, 2020
* =========================================================================
