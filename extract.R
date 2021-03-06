# File-Name:       extract.R           
# Date:            2015-05-01                                
# Author:          Kevin Tian, Taylor Perz, Naveen Yarlagadda                             
# Purpose:         Extraction base code for experiments

# load data into R
fert_rates <- read.csv("adoFertility/sp.ado.tfrt_Indicator_en_csv_v2.csv", header=TRUE)
lit_rates <- read.csv("litRate/se.adt.1524.lt.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
girlboy_ratios <- read.csv("ratioGirlsBoys/se.enr.prsc.fm.zs_Indicator_en_csv_v2.csv", header=TRUE)
hiv_prev <- read.csv("hivPrev/sh.hiv.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_secprog <- read.csv("femaleSecProg/se.sec.prog.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
unempl_ado_female <- read.csv("unemplAdoFemales/sl.uem.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
unempl_ado_male <- read.csv("unemplAdoMales/sl.uem.1524.ma.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_primpers <- read.csv("femalePrimPers/se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
country_GDP <- read.csv("countryGDP/ny.gdp.mktp.cd_Indicator_en_csv_v2.csv", header=TRUE)

# truncate data to look at 2013 (most recent 1-year period)
lr_13 <- lit_rates[,c(1:4,58)]
fr_13 <- fert_rates[,c(1:4,58)]
gb_13 <- girlboy_ratios[,c(1:4,58)]
hiv_13 <- hiv_prev[,c(1:4,58)]
fsp_13 <- female_secprog[,c(1:4,58)]
uaf_13 <- unempl_ado_female[,c(1:4,58)]
uam_13 <- unempl_ado_male[,c(1:4,58)]
fpp_13 <- female_primpers[,c(1:4,58)]
gdp_13 <- country_GDP[,c(1:4,58)]

# remove NA's in dataset
# http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
fr_13.clean <- fr_13[complete.cases(fr_13),]
lr_13.clean <- lr_13[complete.cases(lr_13),]
gb_13.clean <- gb_13[complete.cases(gb_13),]
hiv_13.clean <- hiv_13[complete.cases(hiv_13),]
fsp_13.clean <- fsp_13[complete.cases(fsp_13),]
uaf_13.clean <- uaf_13[complete.cases(uaf_13),]
uam_13.clean <- uam_13[complete.cases(uam_13),]
fpp_13.clean <- fpp_13[complete.cases(fpp_13),]
gdp_13.clean <- gdp_13[complete.cases(gdp_13),]

# transform fertility rate into a categorical variable based off histogram
hist(fr_13.clean$X2013)
low_fr_13 <- ifelse(fr_13.clean$X2013<=60,1,0)
fr_13.class <- cbind(fr_13.clean,low_fr_13)

# clean counts: uaf/uam - 206, gdp - 218, fr - 227

# merge uaf, uam, gdp, and fr into one table
ua_13 <- merge(uaf_13.clean,uam_13.clean[,c(1,5)],by='Country.Name')
names(ua_13)[5] <- "uaf_2013"
names(ua_13)[6] <- "uam_2013"
ua_13$dua_2013 <- ua_13$uaf_2013 - ua_13$uam_2013
ua_gdp_13 <- merge(ua_13,gdp_13.clean[,c(1,5)],by='Country.Name')
names(ua_gdp_13)[8] <- "gdp_2013"
final_13 <- merge(ua_gdp_13,fr_13.class[,c(1,5:6)],by='Country.Name')
names(final_13)[9] <- "fr_2013"

# demo how to add region to data
country_metadata <- read.csv("femalePrimPers/Metadata_Country_se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
names(country_metadata)[1] <- "Country.Name"
fr_ua_13.region <- merge(fr_ua_13,country_metadata[,c(1,3)],by='Country.Name')
