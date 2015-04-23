# Kevin Tian, Naveen Yarlagadda, Taylor Perz

# load data into R
fert_rates <- read.csv("adoFertility/sp.ado.tfrt_Indicator_en_csv_v2.csv", header=TRUE)
lit_rates <- read.csv("litRate/se.adt.1524.lt.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
girlboy_ratios <- read.csv("ratioGirlsBoys/se.enr.prsc.fm.zs_Indicator_en_csv_v2.csv", header=TRUE)
hiv_prev <- read.csv("hivPrev/sh.hiv.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_secprog <- read.csv("femaleSecProg/se.sec.prog.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
unempl_ado <- read.csv("unemplAdoFemales/sl.uem.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_primpers <- read.csv("femalePrimPers/se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
country_GDP <- read.csv("countryGDP/ny.gdp.mktp.cd_Indicator_en_csv_v2.csv", header=TRUE)

# truncate data to look at 2009-2013 (most recent 5-year period)
fr_09_13 <- fert_rates[,c(1:4,54:58)]
lr_09_13 <- lit_rates[,c(1:4,54:58)]
gb_09_13 <- girlboy_ratios[,c(1:4,54:58)]
hiv_09_13 <- hiv_prev[,c(1:4,54:58)]
fsp_09_13 <- female_secprog[,c(1:4,54:58)]
ua_09_13 <- unempl_ado[,c(1:4,54:58)]
fpp_09_13 <- female_primpers[,c(1:4,54:58)]
gdp_09_13 <- country_GDP[,c(1:4,54:58)]

# remove NA's in dataset
# http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
fr_09_13.clean <- fr_09_13[complete.cases(fr_09_13),]
lr_09_13.clean <- lr_09_13[complete.cases(lr_09_13),]
gb_09_13.clean <- gb_09_13[complete.cases(gb_09_13),]
hiv_09_13.clean <- hiv_09_13[complete.cases(hiv_09_13),]
fsp_09_13.clean <- fsp_09_13[complete.cases(fsp_09_13),]
ua_09_13.clean <- ua_09_13[complete.cases(ua_09_13),]
fpp_09_13.clean <- fpp_09_13[complete.cases(fpp_09_13),]
gdp_09_13.clean <- gdp_09_13[complete.cases(gdp_09_13),]

# demo line on how to make a continuous variable into a categorical one
low_fr <- ifelse(fr_09_13.clean$X2013<=50,1,0)
fr_09_13.class <- cbind(fr_09_13.clean,low_fr)

# demo line on how to merge tables on a certain variable
fr_ua_13 <- merge(fr_09_13.class[,c('Country.Name','low_fr')],ua_09_13.clean[,c('Country.Name','X2013')],by='Country.Name')

