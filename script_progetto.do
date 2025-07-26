use  "C:\Users\Utente\Desktop\UNIVERSITA'\a.a. 2024-2025\II semestre\DEMOGRAFIA SOCIALE\progetto\data.dta", clear

*tab1 marital maritala maritalb
*fre marital maritala maritalb

*tab marital essround 
*tab maritala essround 
*tab maritalb essround

recode marital maritala maritalb (.a .b .c=.)

drop if marital==. & (essround>=1 & essround<=2)
drop if maritala==. & (essround>=3 & essround<=4)
drop if maritalb==. & (essround>=5 & essround<=11)


*fre lvgptne lvgptnea
*tab lvgptne essround 
*tab lvgptnea essround

recode lvgptne lvgptnea (.a .b .c .d=.)
drop if lvgptne==. & (essround>=1 & essround<=4)
drop if lvgptnea==. & (essround>=5 & essround<=11)


*----------CREO VARIABILE INDIPENDENTE---------------------------------------
*ETà
*fre agea
drop if agea==.a
drop if agea<30 | agea>65

gen cohab=.

replace cohab=1 if lvgptne==1 & marital<5 & (essround>=1 & essround<=2)
replace cohab=1 if lvgptne==1 & (maritala<9) & (essround>=3 & essround<=4)
replace cohab=1 if lvgptnea==1 & (maritalb<6) & (essround>=5 & essround<=11)

replace cohab=2 if lvgptne==1 & marital==5 & (essround>=1 & essround<=2)
replace cohab=2 if lvgptne==1 & maritala==9 & (essround>=3 & essround<=4)
replace cohab=2 if lvgptnea==1 & maritalb==6 & (essround>=5 & essround<=11)

replace cohab=3 if lvgptne==2 & marital<5 & (essround>=1 & essround<=2)
replace cohab=3 if lvgptne==2 & (maritala<9) & (essround>=3 & essround<=4)
replace cohab=3 if lvgptnea==2 & (maritalb<6) & (essround>=5 & essround<=11)

replace cohab=4 if lvgptne==2 & marital==5 & (essround>=1 & essround<=2)
replace cohab=4 if lvgptne==2 & maritala==9 & (essround>=3 & essround<=4)
replace cohab=4 if lvgptnea==2 & maritalb==6 & (essround>=5 & essround<=11)

label variable cohab "conv/spos?"
label define cohab 1 "Sposato e convivente" 2 "Non sposato ma convivente" 3 "Sposato senza pre-convivenza" 4 "Né sposato né convivente"
label value cohab cohab

drop if cohab==.
tab cohab, m

graph bar (count), over(cohab, label(angle(45))) ///
    bar(1, color(blue)) title("Distribuzione stato familiare") ///
    ylabel(, angle(0)) blabel(bar, format(%9.0g))

graph bar (percent), over(cohab, label(angle(45))) ///
    bar(1, color(green)) title("Distribuzione stato familiare (%)") ///
    ylabel(, angle(0)) blabel(bar, format(%9.1f))

graph export stato_familiare.png, replace




*----------VARIABILE DIPENDENTE---------------------------------------

*SODDISFAZIONE DI VITA
fre stflife
recode stflife (0/4=0 "bassa-soddisfazione") (5/7=1 "media soddisfazione") (8/10=2 "alta-soddisfazione") (.a .b .c=.), gen(stf3)
la var stf3 "stf-3 cat"
drop if stf3==.
fre stf3

/*preserve
collapse (mean) stflife, by(stflife agea)
separate cohab, by(stflife)
graph twoway line cohab1 cohab2 cohab3 agea if inrange(agea,30,65), ytitle("% ever cohabited") name(agestfcohab, replace)
restore
graph display agestfcohab
*/


*-----------VARIABILI DI CONTROLLO--------------------------------------

*ETà


preserve
contract agea cohab
bysort agea: gen tot_age = sum(_freq)
bysort agea (cohab): replace tot_age = tot_age[_N]
gen perc = 100 * _freq / tot_age
twoway ///
(line perc agea if cohab==1 & inrange(agea,15,95), lcolor(blue)) ///
(line perc agea if cohab==2 & inrange(agea,15,95), lcolor(red)) ///
(line perc agea if cohab==3 & inrange(agea,15,95), lcolor(green)) ///
(line perc agea if cohab==4 & inrange(agea,15,95), lcolor(orange)), ///
legend(order(1 "Sposato e convivente" 2 "Non sposato ma convivente" 3 "Sposato senza pre-convivenza" 4 "Né sposato né convivente")) ///
ytitle("Percentuale (%)") xtitle("Età") ///
xline(30) xline(65) ///
title("Distribuzione percentuale di cohab per età") ///
name(cohabline, replace)
restore

graph export distrib_cohab.png, replace



preserve
collapse (mean) stflife, by(agea cohab)
twoway (line stflife age if cohab==1, lcolor(blue) lpattern(solid)) ///
       (line stflife age if cohab==2, lcolor(red) lpattern(dash)) ///
       (line stflife age if cohab==3, lcolor(green) lpattern(dot)) ///
       (line stflife age if cohab==4, lcolor(orange) lpattern(shortdash)) ///
       , legend(order(1 "Sposato e convivente" 2 "Sposato non convivente" ///
                      3 "Non sposato ma convivente" 4 "Né sposato né convivente")) ///
         title("Soddisfazione di vita per età e stato relazionale") ///
         ytitle("Soddisfazione con la vita") xtitle("Età") ///
         ylabel(1(1)10)
restore
graph export distrib_cohab_soddisf.png, replace



*NAZIONALITà
tab cntry essround
describe cntry
encode cntry, gen(country)
fre country
tab country cohab, row nofreq

graph bar (percent), over(country, sort(1) label(angle(45))) ///
    title("Distribuzione per paese (%)") ylabel(, angle(0))
graph export distrib_country.png, replace

/*preserve
collapse (mean) cohab if inrange(agea,15,95), by(country)
graph bar cohab, over(country, sort(cohab) descending label(angle(vertical))) ytitle("% ever cohabited") name(countrycohab, replace)
restore
*/


*GENERE
tab gndr essround
fre gndr
drop if gndr==.a
recode gndr (1=0 "M") (2=1 "F"), gen(sex)
la var sex "sex"


*STATUS DI IMMIGRANTE
tab brncn essround
fre brncn
recode brncn (.a .b .c=.)
drop if brncn==.
recode brncn (1=0 "Native") (2=1 "Immigrant"), gen(immigrant)
la var immigrant "immigrant"


gen immig_cohab = immigrant*10 + cohab
label define immig_cohab_lbl ///
    1 "Nativo - Spos/conv" 11 "Immigrato - Spos/conv" ///
    2 "Nativo - Spos/nonconv" 12 "Immigrato - Spos/nonconv" ///
    3 "Nativo - Non sposato/conv" 13 "Immigrato - Non sposato/conv" ///
    4 "Nativo - Né spos. né conv." 14 "Immigrato - Né spos. né conv."

label values immig_cohab immig_cohab_lbl

preserve
collapse (mean) stflife, by(immig_cohab)

graph bar stflife, over(immig_cohab, label(angle(45))) ///
    title("Soddisfazione di vita per stato relazionale e immigrazione") ///
    ytitle("Soddisfazione media") bar(1, color(navy)) blabel(bar, format(%4.1f))
restore
graph export distrib_immigr_soddisf.png, replace


/* DIVORZIO
fre dvrcdev
tab dvrcdev essround 
fre dvrcdeva
tab dvrcdeva essround

gen divorce = .
replace divorce = 0 if dvrcdev==2 & inrange(essround, 1, 4)
replace divorce = 1 if dvrcdev==1 & inrange(essround, 1, 4)

replace divorce = 0 if dvrcdeva==2 & inrange(essround, 5, 11)
replace divorce = 1 if dvrcdeva==1 & inrange(essround, 5, 11)

drop if divorce==.

label variable divorce "ever divorced"
label define divorce 0 "no" 1 "yes" 
label value divorce divorce
fre divorce
*/

*REDDITO

fre hinctnt
tab hinctnt essround
fre hinctnta
tab hinctnta essround

gen income3 = .
replace income3 = 0 if inrange(hinctnt, 1, 4) & inrange(essround, 1, 3)
replace income3 = 1 if inrange(hinctnt, 5, 8) & inrange(essround, 1, 3)
replace income3 = 2 if inrange(hinctnt, 9, 12) & inrange(essround, 1, 3)

replace income3 = 0 if inrange(hinctnta, 1, 4) & inrange(essround, 4, 11)
replace income3 = 1 if inrange(hinctnta, 5, 7) & inrange(essround, 4, 11)
replace income3 = 2 if inrange(hinctnta, 8, 10) & inrange(essround, 4, 11)

drop if income3==.

label variable income3 "income-3 cat"
label define income3 0 "low-income" 1 "middle-income" 2 "hight-income"
label value income3 income3
fre income3


*AVERE UN LAVORO
fre pdwrk
tab pdwrk essround


*EDUCAZIONE
fre edulvla
tab edulvla essround
fre edulvlb
tab edulvlb essround

gen edu3 = .

replace edu3 = 0  if inrange(edulvla, 1, 2) & inrange(essround, 1, 4)
replace edu3 = 1  if inrange(edulvla, 3, 4) & inrange(essround, 1, 4)
replace edu3 = 2  if edulvla==5 & inrange(essround, 1, 4)

replace edu3 = 0 if inlist(edulvlb, 113, 129, 212, 213, 221, 222, 223) & inrange(essround, 5, 11)
replace edu3 = 1  if inlist(edulvlb, 229, 311, 312, 313, 321, 322, 323, 412, 413, 421, 422, 423) & inrange(essround, 5, 11)
replace edu3 = 2  if inlist(edulvlb, 510, 520, 610, 620, 710, 720, 800) & inrange(essround, 5, 11)

drop if edu3==.

label variable edu3 "education- 3 cat"
label define edu3 0 "low-sec" 1 "upp-sec" 2 "tertiary"
label value edu3 edu3
fre edu3


*FASCE DIFFUSIONE CONVIVENZA

gen nfm = .

replace nfm = 0.5   if country == 1   // AL - Albania
replace nfm = 41.7  if country == 2   // AT - Austria
replace nfm = 52.4  if country == 3   // BE - Belgio
replace nfm = 59.9  if country == 4   // BG - Bulgaria
replace nfm = 27.7  if country == 5   // CH - Svizzera
replace nfm = 21.2  if country == 6   // CY - Cipro
replace nfm = 48.5  if country == 7   // CZ - Repubblica Ceca
replace nfm = 33.1  if country == 8   // DE - Germania
replace nfm = 54.1  if country == 9   // DK - Danimarca
replace nfm = 53.8  if country == 10  // EE - Estonia
replace nfm = 50.1  if country == 11  // ES - Spagna
replace nfm = 48.9  if country == 12  // FI - Finlandia
replace nfm = 65.2  if country == 13  // FR - Francia
replace nfm = 51.3  if country == 14  // GB - Regno Unito
replace nfm = 19.2  if country == 15  // GR - Grecia
replace nfm = 24.8  if country == 16  // HR - Croazia
replace nfm = 46.7  if country == 17  // HU - Ungheria
replace nfm = 36.5  if country == 18  // IE - Irlanda
replace nfm = 5.0   if country == 19  // IL - Israele
replace nfm = 71.0  if country == 20  // IS - Islanda
replace nfm = 39.6  if country == 21  // IT - Italia
replace nfm = 27.7  if country == 22  // LT - Lituania
replace nfm = 40.7  if country == 23  // LU - Lussemburgo
replace nfm = 40.9  if country == 24  // LV - Lettonia
replace nfm = .     if country == 25  // ME - Montenegro
replace nfm = 12.7  if country == 26  // MK - Macedonia del Nord
replace nfm = 52.4  if country == 27  // NL - Paesi Bassi
replace nfm = 56	if country == 28  // NO - Norvegia
replace nfm = 26.4  if country == 29  // PL - Polonia
replace nfm = 60.2  if country == 30  // PT - Portogallo
replace nfm = 42.7  if country == 31  // RO - Romania
replace nfm = 12.0  if country == 32  // RS - Serbia
replace nfm = 30.0  if country == 33  // RU - Russia
replace nfm = 57.4  if country == 34  // SE - Svezia
replace nfm = 56.5  if country == 35  // SI - Slovenia
replace nfm = 40.2  if country == 36  // SK - Slovacchia
replace nfm = 5.0   if country == 37  // TR - Turchia
replace nfm = 20.0  if country == 38  // UA - Ucraina
replace nfm = .     if country == 39  // XK - Kosovo (dato mancante)

gen convivenza_fascia = .

replace convivenza_fascia = 1 if nfm > 55
replace convivenza_fascia = 2 if nfm > 45 & nfm <= 55
replace convivenza_fascia = 3 if nfm >= 35 & nfm <= 45
replace convivenza_fascia = 4 if nfm < 35 & nfm != .

label define fascia_lbl 1 "Alta (>55%)" 2 "Medio-alta (45-55%)" 3 "Media (35-44.9%)" 4 "Bassa (<35%)"
label values convivenza_fascia fascia_lbl


*AREE GEOGRAFICHE
gen area_europa = .

replace area_europa = 1 if inlist(country, 9, 12, 20, 28, 34)
replace area_europa = 2 if inlist(country, 1, 5, 6, 11, 15, 21, 25, 26, 30, 37)
replace area_europa = 3 if inlist(country, 4, 7, 16, 17, 22, 24, 29, 31, 32, 33, 36, 38, 39)
replace area_europa = 4 if inlist(country, 2, 3, 8, 10, 13, 14, 18, 23, 27)

drop if area_europa==.

label define area_lbl 1 "Scandinavia" 2 "Europa Meridionale" 3 "Europa Orientale" 4 "Europa Occidentale"
label values area_europa area_lbl


***********************
* Multivariate models *
***********************

reg stflife ib1.cohab, r

reg stflife ib1.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if agea>=30 & agea<=65, r


reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if convivenza_fascia==1 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if convivenza_fascia==2 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if convivenza_fascia==3 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if convivenza_fascia==4 & agea>=30 & agea<=65, r


reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if area_europa==1 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if area_europa==2 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if area_europa==3 & agea>=30 & agea<=65, r

reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if area_europa==4 & agea>=30 & agea<=65, r




reg stflife ib3.cohab c.agea##c.agea i.immigrant i.income3 i.pdwrk i.country i.edu3 i.essround i.sex if convivenza_fascia==1 & agea>=30 & agea<=65, r
