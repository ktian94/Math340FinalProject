# File-Name:       classification.R           
# Date:            2015-05-01                                
# Author:          Kevin Tian, Naveen Yarlagadda, Taylor Perz                             
# Purpose:         Classification experiment for 
# Packages Used:   MASS, tree

# loading necessary libraries and packages
library(MASS)
library(tree)

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

# merge uaf, uam, gdp, and fr into one table
ua_13 <- merge(uaf_13.clean,uam_13.clean[,c(1,5)],by='Country.Name')
names(ua_13)[5] <- "uaf_2013"
names(ua_13)[6] <- "uam_2013"
ua_13$dua_2013 <- ua_13$uaf_2013 - ua_13$uam_2013
ua_gdp_13 <- merge(ua_13,gdp_13.clean[,c(1,5)],by='Country.Name')
names(ua_gdp_13)[8] <- "gdp_2013"
final_13 <- merge(ua_gdp_13,fr_13.class[,c(1,5:6)],by='Country.Name')
names(final_13)[9] <- "fr_2013"

# divide into training and testing for model evaluation
tr<-sample(seq(1,nrow(final_13)),98,replace=FALSE)
final_13.tr<-final_13[tr,]
final_13.ts<-final_13[-tr,]

# lda with dua variable
fr_13.lda<-lda(low_fr_13~dua_2013 + gdp_2013 ,data=final_13.tr)

# results of lda with predictors dua and gdp 
fr_13.lda

# doing plots
plot(fr_13.lda)
plot(fr_13.lda, dimen=1,type="density")

# doing the predictions 
fr_13.pred<-predict(fr_13.lda, final_13.ts) 

# get predicted class
fr_13.pclass<-fr_13.pred$class

# create confusion matrix
table(fr_13.pclass, final_13.ts$low_fr_13)

# lda with uam and uaf variables
fr_13.lda2 <- lda(low_fr_13~uam_2013 + uaf_2013 + gdp_2013 ,data=final_13.tr)

# results of lda with predictors uam, uaf, and gdp 
fr_13.lda2

# doing plots
plot(fr_13.lda2)
plot(fr_13.lda2, dimen=1,type="density")

# doing the predictions 
fr_13.pred2 <- predict(fr_13.lda2, final_13.ts) 

# get predicted class
fr_13.pclass2<-fr_13.pred2$class

# create confusion matrix
table(fr_13.pclass2, final_13.ts$low_fr_13)

# qda with dua variable
fr_13.qda<-qda(low_fr_13~dua_2013 + gdp_2013 ,data=final_13.tr)

# getting the results QDA
fr_13.qda

# getting predictions for QDA
pred.qda<-predict(fr_13.qda, final_13.ts)
pqda.c<-pred.qda$class

# create confusion matrix
table(pqda.c, final_13.ts$low_fr_13 )

# qda with uam and uaf variables
fr_13.qda2<-qda(low_fr_13~uam_2013 + uaf_2013 + gdp_2013 ,data=final_13.tr)

# getting the results QDA
fr_13.qda2

# getting predictions for QDA
pred.qda2<-predict(fr_13.qda2, final_13.ts)
pqda.c2<-pred.qda2$class

# create confusion matrix
table(pqda.c2, final_13.ts$low_fr_13 )

# tree analysis 

# make variables factors to invoke classification tree methods
final_13.tr$low_fr_13 <- as.factor(final_13.tr$low_fr_13)
final_13.ts$low_fr_13 <- as.factor(final_13.ts$low_fr_13)

# tree analysis with dua variable 
tm<-tree(low_fr_13~dua_2013 + gdp_2013 ,data=final_13.tr)
plot(tm)
text(tm)
summary(tm)

tree.pred<-predict(tm,final_13.ts,type="class")
table(tree.pred, final_13.ts$low_fr_13)

# cross-validate to determine optimal pruning factor
cv.mod<-cv.tree(tm,FUN=prune.misclass)
plot(cv.mod$size,cv.mod$dev,type='b')
plot(cv.mod$k,cv.mod$dev,type='b')

# prune tree
prune.mod<-prune.misclass(tm,k=2)
plot(prune.mod)
text(prune.mod)
summary(prune.mod)

tree.pred<-predict(prune.mod,final_13.ts,type="class")
table(tree.pred, final_13.ts$low_fr_13)

# tree with uam and uaf variables
tm2<-tree(low_fr_13~uaf_2013 + uam_2013 + gdp_2013 ,data=final_13.tr)
plot(tm2)
text(tm2)
summary(tm2)
tree.pred2<-predict(tm2,final_13.ts,type="class")
table(tree.pred2, final_13.ts$low_fr_13)

# cross-validate to determine optimal pruning factor
cv.mod2<-cv.tree(tm2,FUN=prune.misclass)
plot(cv.mod2$size,cv.mod2$dev,type='b')
plot(cv.mod2$k,cv.mod2$dev,type='b')

# prune tree
prune.mod2<-prune.misclass(tm2,k=3)
plot(prune.mod2)
text(prune.mod2)
summary(prune.mod2)

tree.pred2<-predict(prune.mod2,final_13.ts,type="class")
table(tree.pred2, final_13.ts$low_fr_13)

