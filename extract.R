# Kevin Tian, Naveen Yarlagadda, Taylor Perz

#loading necessary libraries and packages
library(MASS)

# load data into R
fert_rates <- read.csv("adoFertility/sp.ado.tfrt_Indicator_en_csv_v2.csv", header=TRUE)
lit_rates <- read.csv("litRate/se.adt.1524.lt.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
girlboy_ratios <- read.csv("ratioGirlsBoys/se.enr.prsc.fm.zs_Indicator_en_csv_v2.csv", header=TRUE)
hiv_prev <- read.csv("hivPrev/sh.hiv.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_secprog <- read.csv("femaleSecProg/se.sec.prog.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
unempl_ado <- read.csv("unemplAdoFemales/sl.uem.1524.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
female_primpers <- read.csv("femalePrimPers/se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
country_GDP <- read.csv("countryGDP/ny.gdp.mktp.cd_Indicator_en_csv_v2.csv", header=TRUE)

# truncate data to look at 2013 (most recent 1-year period)
lr_09_13 <- lit_rates[,c(1:4,58)]
fr_09_13 <- fert_rates[,c(1:4,58)]
gb_09_13 <- girlboy_ratios[,c(1:4,58)]
hiv_09_13 <- hiv_prev[,c(1:4,58)]
fsp_09_13 <- female_secprog[,c(1:4,58)]
ua_09_13 <- unempl_ado[,c(1:4,58)]
fpp_09_13 <- female_primpers[,c(1:4,58)]
gdp_09_13 <- country_GDP[,c(1:4,58)]

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

# ua - 206, hiv - 128, gdp - 218, fr - 227

#histrogram of the distribution of the fr_09_13 in order to determine a high 
#and low fertility rate 
hist(fr_09_13.clean$X2013)
summary(fr_09_13.clean$X2013)


#demo how to make a continuous variable into a categorical one
low_fr_13 <- ifelse(fr_09_13.clean$X2013<=60,1,0)
fr_09_13.class <- cbind(fr_09_13.clean,low_fr_13)

# merge
fr_ua_13.region.hiv.ua <- merge(fr_ua_13,hiv_09_13.clean[,c(1,9)],by='Country.Name')
fr_ua_13.region.hiv.ua.fert <-merge(fr_ua_13.region.hiv.ua, fr_09_13.clean[,c(1,9)], by='Country.Name' )


#creating a categorical variable from the fertility numbers
low_fr_13_NAV<- ifelse(fr_ua_13.region.hiv.ua.fert$X2013<=60,1,0)
fr_09_13.region.hiv.ua.class <- cbind(fr_ua_13.region.hiv.ua.fert,low_fr_13_NAV)

#dividing into training and testing 
tr<-sample(seq(1,150),50,replace=FALSE)
fr_ua_13.region.hiv.ua.tr<-fr_09_13.region.hiv.ua.class[tr,]
fr_ua_13.region.hiv.ua.ts<-fr_09_13.region.hiv.ua.class[-tr,]

#doing the classification analysis
fr_09_13.lda<-lda(low_fr_13_NAV~fr_ua_13.region.hiv.ua.tr$X2013.x + fr_ua_13.region.hiv.ua.tr$X2013.y ,data=fr_ua_13.region.hiv.ua.tr)

#results of the classification analysis with variables UA and HIV
fr_09_13.lda

#doing plots
plot(fr_09_13.lda)
plot(fr_09_13.lda, dimen=1,type="density")

#doing the predictions (for some reason this part does not work) 
fr_09_13.pred<-predict(fr_09_13.lda, fr_ua_13.region.hiv.ua.ts) 



# demo how to merge tables on a certain variable
fr_ua_13 <- merge(fr_09_13.class[,c('Country.Name','low_fr')],ua_09_13.clean[,c('Country.Name','X2013')],by='Country.Name')

# demo how to add region to data
country_metadata <- read.csv("femalePrimPers/Metadata_Country_se.prm.prsl.fe.zs_Indicator_en_csv_v2.csv", header=TRUE)
names(country_metadata)[1] <- "Country.Name"
fr_ua_13.region <- merge(fr_ua_13,country_metadata[,c(1,3)],by='Country.Name')


