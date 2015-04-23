#the following lines help load the data into data frames 

fert_rates <- read.csv("adoFertility/sp.ado.tfrt_Indicator_en_csv_v2.csv", header=TRUE)
lit_rates <- read.csv("litRate/se.adt.1524.lt.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
girlboy_ratios <- read.csv("ratioGirlsBoys/se.enr.prsc.fm.zs_Indicator_en_csv_v2.csv", header=TRUE)
hiv_prev <- read.csv("hivPrev/sh.hiv.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_secprog <- read.csv("femaleSecProg/se.sec.prog.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
unempl_ado <- read.csv("unemplAdoFemales/sl.uem.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_primpers <- read.csv("femalePrimPers/se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
country_GDP <- read.csv("countryGDP/ny.gdp.mktp.cd_Indicator_en_csv_v2.csv", header=TRUE)
