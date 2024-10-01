
##%######################################################%##
#                                                          #
####               06 ADDITIONAL ANALYSES               ####
#                                                          #
##%######################################################%##

# Analyses for:
# - sensitivity to change (pre-post)
# - RCIs

source("/Users/tim/NextcloudHGW/MEDI-Validierung/MEDI_German_Validation/MEDI_German_Validation_Study/medi_to_sedi.R")
# source("medi_to_sedi.R")
library(psych)
library(dplyr)
library(rciplot)
library(tidyr)
library(pROC)
library(gam)

alldata <- readRDS("/Users/tim/Documents/GitHub/new-explorer/caches/allData.RDS")
# alldata <- readRDS("C:/Users/steph/OneDrive/Desktop/Psychologie/Doktorat eHelp-MV-Projekt/GPNS/new-explorer/caches/allData.RDS")
alldata_medi_lv_scores <- readRDS("Data/alldata_medi_lv_scores.RDS") # validation sample (including only first session)

alldata_medi_lv_scores$sedi <- medi_to_sedi(alldata_medi_lv_scores)
alldata_medi_lv_scores$is.patient <- alldata_medi_lv_scores$sample == "patient"

#sedi.patient.roc <- pROC::roc(alldata_medi_lv_scores$is.patient ~ predict(glm(is.patient ~ sedi, data = alldata_medi_lv_scores, family = "binomial")))

#coords(sedi.patient.roc, "best", ret = c("threshold","ppv","npv"), 
#       best.method = "closest.topleft", best.weights = c(1, .25))

alldata$neurotic <- alldata$neurotic * 5
alldata$positive <- alldata$positive * 5
alldata$depression <- alldata$depression * 5
alldata$arousal <- alldata$arousal * 5
alldata$somatic <- alldata$somatic * 5
alldata$social <- alldata$social * 5
alldata$intrusive <- alldata$intrusive * 6
alldata$traumatic <- alldata$traumatic * 5
alldata$avoidance <- alldata$avoidance * 8

#-------- Sensitivity to Change in Pre-Post Data --------

# SEDI 

#alldata$sedi <- medi_to_sedi(alldata)

# must be done with the data from the study, not with the current ZPP data:
#group_by(alldata, sample) %>% summarise(mSEDI = mean(sedi), sdSEDI = sd(sedi))

#criterion_c <- ((2.34*0.472) + (1.87*0.472)) / (.56 + .472) 
#criterion_c

# select patient code, date, type of measurement, MEDI scales, and KODAP "problem" item
dat.pre.post <- alldata %>% 
  filter(type == "pre" | type == "post") %>% 
  dplyr::select(kkcode, type, ended, neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance, KPPost_problem, KPPost_change) %>%
  group_by(kkcode, type) %>% 
  arrange(ended) %>% 
  # filter(row_number() == 1) %>% 
  ungroup()

dat.pre.post <- pivot_wider(dat.pre.post, id_cols = "kkcode", names_from = "type", values_from = colnames(dat.pre.post)[3:14])

dat.pre.post$neurotic_change <- 1 - dat.pre.post$neurotic_post / dat.pre.post$neurotic_pre
dat.pre.post$positive_change <- 1 - dat.pre.post$positive_post / dat.pre.post$positive_pre
dat.pre.post$depression_change <- 1 - dat.pre.post$depression_post / dat.pre.post$depression_pre
dat.pre.post$arousal_change <- 1 - dat.pre.post$arousal_post / dat.pre.post$arousal_pre
dat.pre.post$somatic_change <- 1 - dat.pre.post$somatic_post / dat.pre.post$somatic_pre
dat.pre.post$intrusive_change <- 1 - dat.pre.post$intrusive_post / dat.pre.post$intrusive_pre
dat.pre.post$social_change <- 1 - dat.pre.post$social_post / dat.pre.post$social_pre
dat.pre.post$traumatic_change <- 1 - dat.pre.post$traumatic_post / dat.pre.post$traumatic_pre
dat.pre.post$avoidance_change <- 1 - dat.pre.post$avoidance_post / dat.pre.post$avoidance_pre

dat.pre.post$neurotic_change[which(is.infinite(dat.pre.post$neurotic_change))] <- NA
dat.pre.post$positive_change[which(is.infinite(dat.pre.post$positive_change))] <- NA
dat.pre.post$depression_change[which(is.infinite(dat.pre.post$depression_change))] <- NA
dat.pre.post$arousal_change[which(is.infinite(dat.pre.post$arousal_change))] <- NA
dat.pre.post$somatic_change[which(is.infinite(dat.pre.post$somatic_change))] <- NA
dat.pre.post$intrusive_change[which(is.infinite(dat.pre.post$intrusive_change))] <- NA
dat.pre.post$social_change[which(is.infinite(dat.pre.post$social_change))] <- NA
dat.pre.post$traumatic_change[which(is.infinite(dat.pre.post$traumatic_change))] <- NA
dat.pre.post$avoidance_change[which(is.infinite(dat.pre.post$avoidance_change))] <- NA

dat.pre.post$neurotic_reduction <- dat.pre.post$neurotic_post - dat.pre.post$neurotic_pre
dat.pre.post$positive_reduction <- dat.pre.post$positive_post - dat.pre.post$positive_pre
dat.pre.post$depression_reduction <- dat.pre.post$depression_post - dat.pre.post$depression_pre
dat.pre.post$arousal_reduction <- dat.pre.post$arousal_post - dat.pre.post$arousal_pre
dat.pre.post$somatic_reduction <- dat.pre.post$somatic_post - dat.pre.post$somatic_pre
dat.pre.post$intrusive_reduction <- dat.pre.post$intrusive_post - dat.pre.post$intrusive_pre
dat.pre.post$social_reduction <- dat.pre.post$social_post - dat.pre.post$social_pre
dat.pre.post$traumatic_reduction <- dat.pre.post$traumatic_post - dat.pre.post$traumatic_pre
dat.pre.post$avoidance_reduction <- dat.pre.post$avoidance_post - dat.pre.post$avoidance_pre
  
# correlation of anchors with points reduction:
cor(dat.pre.post$neurotic_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$depression_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$arousal_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$somatic_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$intrusive_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$traumatic_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$avoidance_reduction, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")

cor(dat.pre.post$neurotic_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$depression_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$arousal_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$somatic_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$intrusive_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$traumatic_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$avoidance_reduction, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")

# correlation of anchors with percentage reduction:
cor(dat.pre.post$neurotic_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$depression_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$arousal_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$somatic_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$intrusive_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$traumatic_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")
cor(dat.pre.post$avoidance_change, dat.pre.post$KPPost_problem_post, use = "complete", method = "spearman")

cor(dat.pre.post$neurotic_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$depression_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$arousal_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$somatic_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$intrusive_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$traumatic_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")
cor(dat.pre.post$avoidance_change, dat.pre.post$KPPost_change_post, use = "complete", method = "spearman")

# simple change score analysis
library(mgcv)

dat.pre.post$KPPost_problem.simple <- ifelse(dat.pre.post$KPPost_problem_post <= 2, 1, 0)

neurotic.gam <- gam(KPPost_problem.simple ~ s(neurotic_change) + s(neurotic_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
positive.gam <- gam(KPPost_problem.simple ~ s(positive_change) + s(positive_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
depression.gam <- gam(KPPost_problem.simple ~ s(depression_change) + s(depression_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
arousal.gam <- gam(KPPost_problem.simple ~ s(arousal_change) + s(arousal_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
somatic.gam <- gam(KPPost_problem.simple ~ s(somatic_change) + s(somatic_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
intrusive.gam <- gam(KPPost_problem.simple ~ s(intrusive_change) + s(intrusive_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
social.gam <- gam(KPPost_problem.simple ~ s(social_change) + s(social_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
traumatic.gam <- gam(KPPost_problem.simple ~ s(traumatic_change) + s(traumatic_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))
avoidance.gam <- gam(KPPost_problem.simple ~ s(avoidance_change) + s(avoidance_pre), method = "REML", select = TRUE, data = dat.pre.post, family = binomial(link = "logit"))

return.criteria <- c("threshold","sensitivity","specificity","ppv","npv","auc")

coords(roc(KPPost_problem.simple ~ neurotic_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ positive_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ depression_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ arousal_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ somatic_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ intrusive_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ social_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ traumatic_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")
coords(roc(KPPost_problem.simple ~ avoidance_change, data = dat.pre.post), "best", ret = return.criteria, method = "closest.topleft")

coords(roc(KPPost_problem.simple ~ rowSums(dat.pre.post[,45:53]), data = dat.pre.post), "best")

roc(KPPost_problem.simple ~ neurotic_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ positive_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ depression_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ arousal_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ somatic_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ intrusive_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ social_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ traumatic_change, data = dat.pre.post)
roc(KPPost_problem.simple ~ avoidance_change, data = dat.pre.post)

dat.pre.post$neurotic.acc <- dat.pre.post$neurotic_change >= .366
dat.pre.post$positive.acc <- dat.pre.post$positive_change <= -.106
dat.pre.post$depression.acc <- dat.pre.post$depression_change >= .592
dat.pre.post$arousal.acc <- dat.pre.post$arousal_change >= .634
dat.pre.post$somatic.acc <- dat.pre.post$somatic_change >= .222
dat.pre.post$intrusive.acc <- dat.pre.post$intrusive_change >= .455
dat.pre.post$social.acc <- dat.pre.post$social_change >= .212
dat.pre.post$traumatic.acc <- dat.pre.post$traumatic_change >= .567
dat.pre.post$avoidance.acc <- dat.pre.post$avoidance_change >= .508

prop.table(table(dat.pre.post$neurotic.acc | dat.pre.post$positive.acc | dat.pre.post$depression.acc | dat.pre.post$arousal.acc | dat.pre.post$somatic.acc | dat.pre.post$intrusive.acc | dat.pre.post$social.acc | dat.pre.post$traumatic.acc | dat.pre.post$avoidance.acc))

# equipercentile linking
library(equate)
eq.neurotic <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$neurotic_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.positive <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$positive_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.depression <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$depression_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.arousal <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$arousal_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.somatic <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$somatic_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.intrusive <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$intrusive_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.social <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$social_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.traumatic <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$traumatic_change)), type = "equip", smoothmethod = "bump", jmin = 1)
eq.avoidance <- equate(as.freqtab(table(dat.pre.post$KPPost_problem_post, dat.pre.post$avoidance_change)), type = "equip", smoothmethod = "bump", jmin = 1)

# raw summary
group_by(dat.pre.post, KPPost_problem.simple) %>%
  summarise(mChange.neur = mean(neurotic_change, na.rm = T), 
            mChange.dep = mean(depression_change, na.rm = T),
            mChange.pos = mean(positive_change, na.rm = T),
            mChange.aro = mean(arousal_change, na.rm = T),
            mChange.som = mean(somatic_change, na.rm = T),
            mChange.int = mean(intrusive_change, na.rm = T),
            mChange.soc = mean(social_change, na.rm = T),
            mChange.tra = mean(traumatic_change, na.rm = T),
            mChange.avoid = mean(avoidance_change, na.rm = T),
            n = n())

# get concordance tables
eq.neurotic$concordance
eq.positive$concordance
eq.depression$concordance
eq.arousal$concordance
eq.somatic$concordance
eq.intrusive$concordance
eq.social$concordance
eq.traumatic$concordance
eq.avoidance$concordance

# get group-wise summary statistics
group_by(alldata_medi_lv_scores, sample) %>% 
  summarise(
    mNeurotic = mean(neurotic),
    sdNeurotic = sd(neurotic),
    mPositive = mean(positive),
    sdPositive = sd(positive),
    mDepression = mean(depression),
    sdDepression = sd(depression),
    mArousal = mean(arousal),
    sdArousal = sd(arousal),
    mSomatic = mean(somatic),
    sdSomatic = sd(somatic),
    mSocial = mean(social),
    sdSocial = sd(social),
    mIntrusive = mean(intrusive),
    sdIntrusive = sd(intrusive),
    mTraumatic = mean(traumatic),
    sdTraumatic = sd(traumatic),
    mAvoidance = mean(avoidance),
    sdAvoidance = sd(avoidance)
  ) -> group.scores

# manual computation of Criterion C (midpoint of clinical and community sample)
round((group.scores$sdNeurotic[1]*group.scores$mNeurotic[2] + group.scores$sdNeurotic[2]*group.scores$mNeurotic[1]) / sum(group.scores$sdNeurotic)) # 18
round((group.scores$sdPositive[1]*group.scores$mPositive[2] + group.scores$sdPositive[2]*group.scores$mPositive[1]) / sum(group.scores$sdPositive)) # 23
round((group.scores$sdDepression[1]*group.scores$mDepression[2] + group.scores$sdDepression[2]*group.scores$mDepression[1]) / sum(group.scores$sdDepression)) # 11
round((group.scores$sdSomatic[1]*group.scores$mSomatic[2] + group.scores$sdSomatic[2]*group.scores$mSomatic[1]) / sum(group.scores$sdSomatic)) # 11
round((group.scores$sdArousal[1]*group.scores$mArousal[2] + group.scores$sdArousal[2]*group.scores$mArousal[1]) / sum(group.scores$sdArousal)) # 8
round((group.scores$sdSocial[1]*group.scores$mSocial[2] + group.scores$sdSocial[2]*group.scores$mSocial[1]) / sum(group.scores$sdSocial)) # 16
round((group.scores$sdIntrusive[1]*group.scores$mIntrusive[2] + group.scores$sdIntrusive[2]*group.scores$mIntrusive[1]) / sum(group.scores$sdIntrusive)) # 15
round((group.scores$sdTraumatic[1]*group.scores$mTraumatic[2] + group.scores$sdTraumatic[2]*group.scores$mTraumatic[1]) / sum(group.scores$sdTraumatic)) # 9
round((group.scores$sdAvoidance[1]*group.scores$mAvoidance[2] + group.scores$sdAvoidance[2]*group.scores$mAvoidance[1]) / sum(group.scores$sdAvoidance)) # 20


#-------- Jacobson-Truax Plots --------

# report only cutoff scores instead of RCI plots

# NEUROTIC
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "neurotic_pre", 
             post = "neurotic_post", 
             reliability = .692, 
             dysfM = 23.30, dysfSD = 8.91, 
             normM = 8.91, normSD = 8.45)

# POSITIVE 
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "positive_pre", 
             post = "positive_post", 
             reliability = .731, higherIsBetter = T,
             dysfM = 20.06, dysfSD = 7.26, 
             normM = 25.56, normSD = 6.45)

# DEPRESSION
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "depression_pre", 
             post = "depression_post", 
             reliability = .648,
             dysfM = 18.57, dysfSD = 9.59, 
             normM = 6.56, normSD = 6.54)

# AROUSAL
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "arousal_pre", 
             post = "arousal_post", 
             reliability = .632,
             dysfM = 13.88, dysfSD = 10.21, 
             normM = 4.19, normSD = 5.32)

# SOMATIC
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "somatic_pre", 
             post = "somatic_post", 
             reliability = .786,
             dysfM = 14.96, dysfSD = 9.02, 
             normM = 8.98, normSD = 6.46, JTcrit = "C")

# SOCIAL
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "social_pre", 
             post = "social_post", 
             reliability = .770,
             dysfM = 18.49, dysfSD = 11.74, 
             normM = 13.29, normSD = 10.14, JTcrit = "C")

# INTRUSIVE
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "intrusive_pre", 
             post = "intrusive_post", 
             reliability = .635,
             dysfM = 19.96, dysfSD = 11.02, 
             normM = 10.44, normSD = 8.94, JTcrit = "C")

# TRAUMATIC
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "intrusive_pre", 
             post = "intrusive_post", 
             reliability = .754,
             dysfM = 12.91, dysfSD = 6.15, 
             normM = 10.80, normSD = 8.94, JTcrit = "C")

# AVOIDANCE
JTRCI::JTRCI(data = data.frame(dat.pre.post), 
             pre = "avoidance_pre", 
             post = "avoidance_post", 
             reliability = .683,
             dysfM = 25.64, dysfSD = 15.18, 
             normM = 11.81, normSD = 9.17, JTcrit = "C")

