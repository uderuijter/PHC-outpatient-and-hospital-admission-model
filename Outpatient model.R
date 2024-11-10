install.packages("dplyr")
install.packages("xlsx")
install.packages("readr")
install.packages("readxl")
install.packages("tidyverse")
install.packages("writexl")
install.packages("comorbidity")
install.packages("lubridate")
install.packages("stringr")
install.packages("caret")
install.packages("glue")
install.packages("bindrcpp")
install.packages("DEoptimR")
install.packages("pROC")
install.packages("rms")
install.packages("boot")

#### Libraries ####
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(writexl)
library(comorbidity)
library(lubridate)
library(stringr)
library(munsell) 
library(ggplot2) 
library(ModelMetrics) 
library(recipes) 
library(assertthat) 
library(bindrcpp) 
library(glue) 
library(pkgconfig) 
library(DEoptimR) 
library(caret)
library(glue)
library(pROC)
library(Rcpp)
library(rms)
library(boot)

setwd("~/2. Data/1. Bronbestanden")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

####-----------------------------------------------------------------------####
####----------------------- OUTCOME OUTPATIENT ----------------------------------####
####-----------------------------------------------------------------------####
#### Total costs per patient per calendar year ####

Total_costs15 <- aggregate(costs ~ patientID, data = DATA15, sum)
n_distinct(DATA15$patientID)

Total_costs16 <- aggregate(costs ~ patientID, data = DATA16, sum)
n_distinct(DATA16$patientID)

Total_costs17 <- aggregate(costs ~ patientID, data = DATA17, sum)
n_distinct(DATA17$patientID)

Total_costs18 <- aggregate(costs ~ patientID, data = DATA18, sum)
n_distinct(DATA18$patientID)

Total_costs19 <- aggregate(costs ~ patientID, data = DATA19, sum)
n_distinct(DATA19$patientID)

#### Scaling costs ####
median15 <- median(Total_costs15$costs)
median16 <- median(Total_costs16$costs)
median17 <- median(Total_costs17$costs)
median18 <- median(Total_costs18$costs)
median19 <- median(Total_costs19$costs)

threshold15 <- quantile(Total_costs15$costs, probs = c(0.90))
threshold15_95 <- quantile(Total_costs15$costs, probs = c(0.95))
threshold15_99 <- quantile(Total_costs15$costs, probs = c(0.99))
  
rm(Total_costs15)
rm(Total_costs16)
rm(Total_costs17)
rm(Total_costs18)
rm(Total_costs19)

schaling16 <- median15/median16
schaling17 <- median15/median17
schaling18 <- median15/median18
schaling19 <- median15/median19

# Scaling costs 
DATA15 <- DATA15 %>% mutate(
  scaled_costs = costs)

DATA16 <- DATA16 %>% mutate(
  scaled_costs = costs*schaling16)

DATA17 <- DATA17 %>% mutate(
  scaled_costs = costs*schaling17)

DATA18 <- DATA18 %>% mutate(
  scaled_costs = costs*schaling18)

DATA19 <- DATA19 %>% mutate(
  scaled_costs = costs*schaling19)

#### Join outpatient data with cost data ####
DATA_ALL <- rbind(DATA15, DATA16, DATA17, DATA18, DATA19)
rm(DATA15)
rm(DATA16)
rm(DATA17)
rm(DATA18)
rm(DATA19)

outpatient_ALL <- left_join(outpatient, DATA_ALL, by = c("patientID"))
n_distinct(outpatient_ALL$patientID)

outpatient_ALL <-  subset(outpatient_ALL,
                              select = c(patientID, Y0, Y1, Y2, Y3, index_date, scaled_costs))

outpatient_ALL$index_date <- as.Date(outpatient_ALL$index_date, format = '%d%B%Y:00:00:00')

#### costs Y0 ####
outpatient_ALL <- outpatient_ALL %>% mutate(
  keepY0 = case_when(index_date %within% Y0 ~ 1, 
                     TRUE ~ 0)
)

outpatient_Y0 <- outpatient_ALL %>%
  filter(keepY0 == 1)

OUTCOME_Y0 <- aggregate(scaled_costs ~ patientID, data = outpatient_Y0, sum)
OUTCOME_Y0 <- rename(OUTCOME_Y0, Total_costsY0 = scaled_costs)

summary(OUTCOME_Y0$Total_costsY0)

## Join costs Y0 en outpatient ##

outpatient <- left_join(outpatient, OUTCOME_Y0, by = c("patientID"))

rm(outpatient_Y0)
rm(OUTCOME_Y0)
summary(outpatient$Total_costsY0)

## Replace NA with 0 ##
outpatient["Total_costsY0"][is.na(outpatient["Total_costsY0"])] <- 0
summary(outpatient$Total_costsY0)

#### costs Y1 ####
outpatient_ALL <- outpatient_ALL %>% mutate(
  keepY1 = case_when(index_date %within% Y1 ~ 1, 
                     TRUE ~ 0)
)

outpatient_Y1 <- outpatient_ALL %>%
  filter(keepY1 == 1)

OUTCOME_Y1 <- aggregate(scaled_costs ~ patientID, data = outpatient_Y1, sum)
OUTCOME_Y1 <- rename(OUTCOME_Y1, Total_costsY1 = scaled_costs)

summary(OUTCOME_Y1$Total_costsY1)

## Join costs Y1 en outpatient ##
outpatient <- left_join(outpatient, OUTCOME_Y1, by = c("patientID"))

rm(outpatient_Y1)
rm(OUTCOME_Y1)

summary(outpatient$Total_costsY1)

#### costs Y2 ####
outpatient_ALL <- outpatient_ALL %>% mutate(
  keepY2 = case_when(index_date %within% Y2 ~ 1, 
                     TRUE ~ 0)
)

outpatient_Y2 <- outpatient_ALL %>%
  filter(keepY2 == 1)

OUTCOME_Y2 <- aggregate(scaled_costs ~ patientID, data = outpatient_Y2, sum)
OUTCOME_Y2 <- rename(OUTCOME_Y2, Total_costsY2 = scaled_costs)

summary(OUTCOME_Y2$Total_costsY2)

## Join costs Y2 en outpatient ##
outpatient <- left_join(outpatient, OUTCOME_Y2, by = c("patientID"))

summary(outpatient$Total_costsY2)

rm(outpatient_Y2)
rm(OUTCOME_Y2)

## Replace NA with 0 ##
outpatient["Total_costsY2"][is.na(outpatient["Total_costsY2"])] <- 0
summary(outpatient$Total_costsY2)

#### costs Y3 ####
outpatient_ALL <- outpatient_ALL %>% mutate(
  keepY3 = case_when(index_date %within% Y3 ~ 1, 
                     TRUE ~ 0)
)

outpatient_Y3 <- outpatient_ALL %>%
  filter(keepY3 == 1)

OUTCOME_Y3 <- aggregate(scaled_costs ~ patientID, data = outpatient_Y3, sum)
OUTCOME_Y3 <- rename(OUTCOME_Y3, Total_costsY3 = scaled_costs)

summary(OUTCOME_Y3$Total_costsY3)

## Join costs Y3 en outpatient ##
outpatient <- left_join(outpatient, OUTCOME_Y3, by = c("patientID"))

summary(outpatient$Total_costsY3)
rm(outpatient_Y3)
rm(OUTCOME_Y3)

## Replace NA with 0 ##
outpatient["Total_costsY3"][is.na(outpatient["Total_costsY3"])] <- 0
summary(outpatient$Total_costsY3)

rm(outpatient_ALL)

#### NHC en PHC outpatient model ####
# use threshold of 2015, all costs are scaled to that year
rm(DATA_ALL)

outpatient <- outpatient %>% mutate(
  HUY0 = case_when(Total_costsY0 > threshold15 ~ 1, 
                   TRUE ~ 0)
)

outpatient <- outpatient %>% mutate(
  HUY1 = case_when(Total_costsY1 > threshold15 ~ 1, 
                     TRUE ~ 0)
)

outpatient <- outpatient %>% mutate(
  HUY2 = case_when(Total_costsY2 > threshold15 ~ 1, 
                   TRUE ~ 0)
)

outpatient <- outpatient %>% mutate(
  HUY3 = case_when(Total_costsY3 > threshold15 ~ 1, 
                   TRUE ~ 0)
)

outpatient <- outpatient %>% mutate(
  HUY1Y2Y3 = case_when((HUY1 == 1 & HUY2 == 1 & HUY3 == 1) ~ 1, 
                     TRUE ~ 0)
)

outpatient <- outpatient %>% mutate(
  HU_status = case_when((HUY1 == 1 & HUY2 == 1 & HUY3 == 1) ~ 1,
                        (HUY1 == 1 & HUY2 == 0 & HUY3 == 0) |
                        (HUY1 == 0 & HUY2 == 1 & HUY3 == 0) | 
                        (HUY1 == 0 & HUY2 == 0 & HUY3 == 1) |
                        (HUY1 == 1 & HUY2 == 1 & HUY3 == 0) |
                        (HUY1 == 0 & HUY2 == 1 & HUY3 == 1) |
                        (HUY1 == 1 & HUY2 == 0 & HUY3 == 1) | 
                        (HUY1 == 0 & HUY2 == 0 & HUY3 == 0) ~ 2, 
                         TRUE ~ 0)
)

outpatient$HUY0 <- as.factor(outpatient$HUY0)
outpatient$HUY1 <- as.factor(outpatient$HUY1)
outpatient$HUY2 <- as.factor(outpatient$HUY2)
outpatient$HUY3 <- as.factor(outpatient$HUY3)
outpatient$HUY1Y2Y3 <- as.factor(outpatient$HUY1Y2Y3)
outpatient$HU_status <- as.factor(outpatient$HU_status)
summary(outpatient$HU_status)

####-----------------------------------------------------------------------####
####-------------------------- SPLIT INTO COHORTS -------------------------####
####-----------------------------------------------------------------------####
#### Split on July 2nd ####
outpatient_A <- outpatient[outpatient$prediction_date < "2016-07-02", ] 
outpatient_B <- outpatient[outpatient$prediction_date > "2016-07-01", ] 

outpatient_A <-  subset(outpatient_A,
                          select = -c(patientID, DATE_STARTY0, DATE_ENDY0, DATE_ENDY1,
                                      DATE_STARTY2, DATE_ENDY2, DATE_STARTY3, DATE_ENDY3, Y0, 
                                      Y1, Y2, Y3, HUY1, HUY2, HUY3))

outpatient_B <-  subset(outpatient_B,
                        select = -c(patientID, DATE_STARTY0, DATE_ENDY0, DATE_ENDY1,
                                    DATE_STARTY2, DATE_ENDY2, DATE_STARTY3, DATE_ENDY3, Y0, 
                                    Y1, Y2, Y3, HUY1, HUY2, HUY3))

####-----------------------------------------------------------------------####
####----------------------- CHARACTERISTICS DEVELOPMENT SET --------------####
####-----------------------------------------------------------------------####
summary(outpatient_A$HU_status)
summary(outpatient_B$HU_status)

PHU <- subset(outpatient_A,
              HU_status == 1,
              select = -c(HU_status))

NPHU <- subset(outpatient_A,
              HU_status == 2,
              select = -c(HU_status))


summary(outpatient_A)
summary(PHU)
summary(NPHU)

####-----------------------------------------------------------------------####
####--------------- REGRESSION ANALYSIS DEVELOPMENT WITH BOOTSTRAP --------####
####-----------------------------------------------------------------------####
dd <- datadist(outpatient_A) 
options(datadist = 'dd')

mod_poli_A <- lrm(HUY1Y2Y3 ~ AGE + SEX + s_cardio + s_ctc + s_colo + s_endo + s_genmed + s_gi + s_onco +
             s_nefro + s_neuro + s_eye + s_rad + s_uro + d_ckd + d_chf + d_ami + d_cva + d_ht + d_preg + d_dm + 
               d_lrd + d_copd + no_ER + no_vpdag + no_OKs + CCI, 
           data = outpatient_A, se.fit=TRUE, x=TRUE, y=TRUE)

mod_poli_A

cbind(OR=exp(coef(mod_poli_A)), 'CI 2,5%' = exp(coef(mod_poli_A)-1.96*sqrt(diag(vcov(mod_poli_A)))),
      'CI 97,5%' = exp(coef(mod_poli_A)+1.96*sqrt(diag(vcov(mod_poli_A)))))

#### Bootstrap validatiom ####
# C-statistic is (Dxy+1)/2
val_poli_A <- validate(mod_poli_A)
val_poli_A

lp_poli_A <- predict(mod_poli_A, type="lp")
rc_poli_A <- rcorr.cens(lp_poli_A, outpatient_A$HUY1Y2Y3)
optimism_poli_A <- val_poli_A["Dxy", "optimism"]

## Optimism from validate(mod_poli) subbtract from C lower and upper 
cindex_poli_A <- rc_poli_A["C Index"]
cindex.se_poli_A <- rc_poli_A["S.D."]/2
cindex_corr_poli_A <- cindex_poli_A - optimism_poli_A
cindex_corr_poli_A

####-----------------------------------------------------------------------####
####---------------------------- TEMPORAL VALIDATION ----------------------####
####-----------------------------------------------------------------------####
#### Linear predictor and predictions ####
summary(outpatient_A$HUY1Y2Y3)
summary(outpatient_B$HUY1Y2Y3)

lp_poli_B <- predict(mod_poli_A, newdata = outpatient_B, type = 'lp')
p_poli_B <- plogis(lp_poli_B)

y_outpatient_B <- outpatient_B$HUY1Y2Y3
y_outpatient_B = as.numeric(levels(y_outpatient_B)[as.integer(y_outpatient_B)])

rcorr.cens(lp_poli_B, y_outpatient_B) 

#### Predictive probabilites ####
measures <- function(predictions=p_poli_B, labels=outpatient_B$HUY1Y2Y3){
  
  #initialize cutoff values
  cutoffs <- seq(0, 1, by = 0.1) 
  
  # calculate confusion matrix
  TP <- c()
  FP <- c()
  TN <- c()
  FN <- c()
  classified.positive <- c()
  classified.negative <- c()
  
  for (cutoff in cutoffs){
    TP <- c(TP, sum(predictions>=cutoff & labels==1)) # TODO: what to do with ties?
    FP <- c(FP, sum(predictions>=cutoff & labels==0))
    TN <- c(TN, sum(predictions<cutoff & labels==0))
    FN <- c(FN, sum(predictions<cutoff & labels==1))
    classified.positive <- c(classified.positive, sum(predictions>=cutoff)/length(predictions))
    classified.negative <- c(classified.negative, sum(predictions<cutoff)/length(predictions))
  }
  
  # calculate measures
  SENS <- TP/(TP+FN)
  SPEC <- TN/(TN+FP)
  PPV <- TP/(TP+FP)
  NPV <- TN/(TN+FN)
  
  return(list(cutoffs=cutoffs, SENS=SENS, SPEC=SPEC, PPV=PPV, NPV=NPV,
              classified.positive=classified.positive,
              classified.negative=classified.negative))
}

measures(predictions=p_poli_B, labels=outpatient_B$HUY1Y2Y3)

#### Calibration ####
library(PredictionTools)
val.prob(p_poli_B, outpatient_B$HUY1Y2Y3, statloc = FALSE, smooth = TRUE)

val.prob.mi(lp_poli_B, outpatient_B$HUY1Y2Y3, show.metrics = FALSE, CI.metrics = TRUE)

####-----------------------------------------------------------------------####
####------------------------ PARSIMONIOUS MODELS --------------------------####
####-----------------------------------------------------------------------####
#### Logistische regressie (lrm) ####

dd <- datadist(outpatient_A) 
options(datadist = 'dd')

parsimonious_poliA <- lrm(HUY1Y2Y3 ~ AGE + s_cardio + s_colo + s_genmed + s_gi + s_eye + s_rad + d_ckd + d_copd + d_cva + d_preg + no_vpdag,
                    data = outpatient_A, x=TRUE, y=TRUE)

parsimonious_poliA

cbind(OR=exp(coef(parsimonious_poliA)), 'CI 2,5%' = exp(coef(parsimonious_poliA)-1.96*sqrt(diag(vcov(parsimonious_poliA)))),
      'CI 97,5%' = exp(coef(parsimonious_poliA)+1.96*sqrt(diag(vcov(parsimonious_poliA)))))

#### Bootstrap validation ####
# C-statistic is (Dxy+1)/2
val_subpoliA <- validate(parsimonious_poliA)
val_subpoliA

lp_subpoliA <- predict(parsimonious_poliA, type="lp")
rc_subpoliA <- rcorr.cens(lp_subpoliA, outpatient_A$HUY1Y2Y3)
optimism_subpoliA <- val_subpoliA["Dxy", "optimism"]

cindex_subpoliA <- rc_subpoliA["C Index"]
cindex_corr_subpoliA <- cindex_subpoliA - optimism_subpoliA
cindex_corr_subpoliA

#### DeLong test submodel en full model ####
pred_modfull = predict(mod_poli_A, type = 'lp')
pred_modsub = predict(parsimonious_poliA, type = 'lp')

ROCfull <- roc(outpatient_A$HUY1Y2Y3, pred_modfull)
ROCsub <- roc(outpatient_A$HUY1Y2Y3, pred_modsub)

roc.test(ROCfull,ROCsub,method = 'delong')

#### Temporal validation ####
lp_subpoli_B <- predict(parsimonious_poliA, newdata = outpatient_B ,type = 'lp')
p_subpoli_B <- plogis(lp_subpoli_B)
y_outpatient_B <- outpatient_B$HUY1Y2Y3
y_outpatient_B = as.numeric(levels(y_outpatient_B)[as.integer(y_outpatient_B)])

rcorr.cens(lp_subpoli_B, y_outpatient_B) 

#### Calibration ####
val.prob(p_subpoli_B, y_outpatient_B, statloc = FALSE, smooth = TRUE)

####-----------------------------------------------------------------------####
####------------------------ RELATIVE PREDICTOR IMPORTANCE ----------------####
####-----------------------------------------------------------------------####

#### Define RSQ4 ####
rsq4 <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- ols(formula, data=d)
  return(plot(anova(fit, test="Chisq"),sort="ascending", what="proportion chisq",margin="proportion chisq",pl=FALSE))
}

#### Bootstrapping with 1000 replications ####
outpatient_A$HUY1Y2Y3 <- as.numeric(outpatient_A$HUY1Y2Y3)-1
set.seed(99)
POLIBOOT <- boot(data=outpatient_A, 
                 statistic=rsq4,
                 R=2000, 
                 formula= HUY1Y2Y3 ~ AGE + s_cardio + s_ctc + s_colo + s_endo + s_genmed + s_gi + s_onco +
                   s_nefro + s_neuro + s_eye + s_rad + s_uro + d_ht + d_preg + d_dm + 
                   d_ckd + d_chf + d_ami + d_cva + d_lrd + d_copd + 
                   no_ER + no_vpdag + no_OKs + CCI)

#### 95% Confidence Intervals ####
e1p<-boot.ci(POLIBOOT, type="perc", index=1)
e2p<-boot.ci(POLIBOOT, type="perc", index=2)
e3p<-boot.ci(POLIBOOT, type="perc", index=3)
e4p<-boot.ci(POLIBOOT, type="perc", index=4)
e5p<-boot.ci(POLIBOOT, type="perc", index=5)
e6p<-boot.ci(POLIBOOT, type="perc", index=6)
e7p<-boot.ci(POLIBOOT, type="perc", index=7)
e8p<-boot.ci(POLIBOOT, type="perc", index=8)
e9p<-boot.ci(POLIBOOT, type="perc", index=9)
e10p<-boot.ci(POLIBOOT, type="perc", index=10)
e11p<-boot.ci(POLIBOOT, type="perc", index=11)
e12p<-boot.ci(POLIBOOT, type="perc", index=12)
e13p<-boot.ci(POLIBOOT, type="perc", index=13)
e14p<-boot.ci(POLIBOOT, type="perc", index=14)
e15p<-boot.ci(POLIBOOT, type="perc", index=15)
e16p<-boot.ci(POLIBOOT, type="perc", index=16)
e17p<-boot.ci(POLIBOOT, type="perc", index=17)
e18p<-boot.ci(POLIBOOT, type="perc", index=18)
e19p<-boot.ci(POLIBOOT, type="perc", index=19)
e20p<-boot.ci(POLIBOOT, type="perc", index=20)
e21p<-boot.ci(POLIBOOT, type="perc", index=21)
e22p<-boot.ci(POLIBOOT, type="perc", index=22)
e23p<-boot.ci(POLIBOOT, type="perc", index=23)
e24p<-boot.ci(POLIBOOT, type="perc", index=24)
e25p<-boot.ci(POLIBOOT, type="perc", index=25)
e26p<-boot.ci(POLIBOOT, type="perc", index=26)

#### Model maken ####
newnamesP<-names(plot(anova(mod_poli_A,test="Chisq"),what="proportion chisq"))

ElementsToChangeP<-c("s_ctc",
                    "d_chf",
                    "s_rad",
                    "d_ht",
                    "s_onco",
                    "s_nefro", 
                    "s_endo",
                    "s_colo", 
                    "d_dm", 
                    "s_neuro", 
                    "s_oog", 
                    "d_cva", 
                    "d_copd",
                    "d_ami",
                    "d_preg",
                    "s_cardio",
                    "s_uro",
                    "d_lrd",
                    "no_OKs",
                    "no_ER",
                    "AGE",
                    "s_genmed",
                    "d_ckd",
                    "s_gi",
                    "no_vpdag",
                    "CCI")

ChangeToP<-c("Cardiothoracic and vascular surgery",
            "Congestive heart failure",
            "Radiation oncology",
            "Hypertension",
            "Oncology",
            "Nephrology", 
            "Endocrinology",
            "Colorectal surgery", 
            "Diabetes Mellitus with complications", 
            "Neurology", 
            "Ophtalmology", 
            "Acute cerebrovascular disease", 
            "COPD",
            "Acute myocardial infarction",
            "Pregnancy and delivery complications",
            "Cardiology",
            "Urology",
            "Lower respiratory disease",
            "Number of surgeries per year",
            "Number of ER visits per year",
            "Age",
            "General medicine",
            "Chronic kidney disease",
            "Gastroenterology",
            "Number of inpatient days per year",
            "Charlson Comorbidity index")

x_p<-ifelse(newnamesP%in%ElementsToChangeP,ChangeToP[match(newnamesP,ElementsToChangeP)],newnamesP)

f4_p<-plot(anova(mod_poli_A, test="Chisq"), sort="ascending",newnames=x_p, what="proportion chisq", margin="proportion chisq",pl=FALSE)

#### Dataframe plot ####

t4_p<-data.frame(Name=names(f4_p),value1_p=round(f4_p,digits=3))

t4b_p<-cbind(rbind(e1p$perc[4:5],e2p$perc[4:5],e3p$perc[4:5],e4p$perc[4:5],e5p$perc[4:5],e6p$perc[4:5],e7p$perc[4:5],e8p$perc[4:5],e9p$perc[4:5],e10p$perc[4:5],e11p$perc[4:5],e12p$perc[4:5],e13p$perc[4:5],e14p$perc[4:5],e15p$perc[4:5],e16p$perc[4:5],e17p$perc[4:5],e18p$perc[4:5],e19p$perc[4:5],e20p$perc[4:5],e21p$perc[4:5],e22p$perc[4:5],e23p$perc[4:5],e24p$perc[4:5], e25p$perc[4:5], e26p$perc[4:5]),
           rbind(names(e1p$t0), names(e2p$t0), names(e3p$t0), names(e4p$t0), names(e5p$t0), names(e6p$t0), names(e7p$t0), names(e8p$t0), names(e9p$t0), names(e10p$t0), names(e11p$t0), names(e12p$t0), names(e13p$t0), names(e14p$t0), names(e15p$t0), names(e16p$t0), names(e17p$t0), names(e18p$t0), names(e19p$t0), names(e20p$t0), names(e21p$t0), names(e22p$t0), names(e23p$t0),names(e24p$t0),names(e25p$t0),names(e26p$t0)))

t4b_p[,3] <- c("Diabetes Mellitus with complications",
             "Congestive heart failure",
             "Colorectal surgery",
             "Cardiothoracic and vascular surgery",
             "Endocrinology",
             "Nephrology", 
             "Hypertension",
             "Pregnancy and delivery complications", 
             "Oncology", 
             "Radiation oncology", 
             "Ophtalmology", 
             "Acute myocardial infarction", 
             "General medicine",
             "Age",
             "Urology",
             "Cardiology",
             "COPD",
             "Neurology",
             "Acute cerebrovascular disease",
             "Lower respiratory disease",
             "Number of ER visits",
             "Number of surgeries",
             "Number of inpatient days per year",
             "Chronic kidney disease",
             "Gastroenterology",
             "Charlson Comorbidity index")

t4b_p<-data.frame(Name=t4b_p[,3],LL=round(as.numeric(t4b_p[,1]),digits=3),UL=round(as.numeric(t4b_p[,2]),digits=3))
t4_p<-merge(t4_p,t4b_p,by="Name")
t4_p$dist<-t4_p$UL-t4_p$LL
t4_p$LL1<-t4_p$value-0.5*t4_p$dist
t4_p$LL1[t4_p$LL1<0]<-0
t4_p$UL1<-t4_p$value+0.5*t4_p$dist
t4_p<-t4_p[order(-t4_p$value1_p,-t4_p$UL1),]
rownames(t4_p)<-NULL


#### Plot maken ####
#### !! Maar 24 predictors worden weergegeven 
poliplot <-ggplot(t4_p, aes(x = value1_p, y = factor(fct_reorder(t4_p$Name,rownames(t4_p)),levels=rev(fct_reorder(t4_p$Name,rownames(t4_p)))))) + 
            geom_point(shape = 16, colour="#5E888C") + 
            geom_errorbarh(aes(xmin=LL1,xmax=UL1, height=0.3),size=1,colour = "#5E888C40")+
            scale_x_continuous(breaks=c(0,0.05,0.10,0.15,0.20), limits = c(0,0.28))+
            geom_rect(mapping = aes(xmin = 0.20, xmax = 0.28 , ymin = -Inf, ymax = Inf),fill = "white",color = "black") +
            # Add rectangle with correct banground color for the differences
            geom_rect(mapping = aes(xmin = 0.20, xmax = Inf , ymin = -Inf, ymax = Inf), fill = "grey90",color = "black") +
            geom_text(aes(x=0.20, label = ifelse((LL1+UL1)>0.0006,paste0(round(value1_p,digits=3)," [",round(LL1,digits=3),"-",round(UL1,digits=3),"]"),paste0("0 [-]"))), colour = "black", size = 3, position=position_nudge(x=0.003),hjust=0) + 
            theme_bw() + 
            theme(panel.grid = element_blank(),
                  panel.grid.major.y = element_line(size=.1, color="grey90"),
                  legend.position = "none",
                  panel.border=element_rect(colour = "black", fill=NA, size=1),
                  plot.background=element_rect(fill="transparent",color="black")) +
            xlab(expression(paste("Proportion of overall"~chi^2,sep="")))+
            ylab("")
            dpi=600

show(poliplot)
