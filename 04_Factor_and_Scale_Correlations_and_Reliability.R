
##%######################################################%##
#                                                          #
####  04 FACTOR AND SCALE CORRELATIONS AND RELIABILITY  ####
#                                                          #
##%######################################################%##

rci_threshold_diff <- function(sd = NULL, rtt = NULL) {
  se <- sd * sqrt(1 - rtt)
  sdiff <- sqrt(2 * (se^2))
  min_difference <- 1.96 * sdiff
  return(min_difference)
}


pacman::p_load(dplyr, tidyr, ggplot2, lavaan, forcats, psych, summarytools, corrplot, papaja, report, flextable, apaTables, 
               labelled, broom, ltm, descr, DescTools, tableone, frequency, skimr, gam)
options(max.print = 2500)

alldata <- readRDS("Data/alldata.RDS")
alldata_medi_lv_scores <- readRDS("Data/alldata_medi_lv_scores.RDS") # Validation sample (including only the first assessment per individual)


#-------- .. Creating MEDI Item-Level Data Set (Sorted by Factor Association) --------

neur = c("medi1","medi10","medi16","medi32","medi35")
pos = c("medi2","medi17","medi24","medi33","medi36")
depr = c("medi3","medi11","medi25","medi37","medi43")
arou = c("medi4","medi13","medi18","medi26","medi44")
som = c("medi6","medi19","medi28","medi38","medi45")
intr = c("medi5","medi12","medi21","medi30","medi40","medi46")
soc = c("medi7","medi14","medi22","medi41","medi47")
traum = c("medi8","medi20","medi29","medi39","medi48")
avoid = c("medi9","medi15","medi23","medi27","medi31","medi34","medi42","medi49")

medi_items <- alldata %>% 
  dplyr::select(medi1:medi49) %>%
  relocate(neur, pos, depr, arou, som, intr, soc, traum, avoid)

medi_items_val <- alldata_medi_lv_scores %>% 
  dplyr::select(medi1:medi49) %>% 
  relocate(neur, pos, depr, arou, som, intr, soc, traum, avoid)

#-------- .. Summary of the Validation Sample --------

alldata_medi_lv_scores %>% 
  dplyr::select(
    sex,Alter,Altersgruppe,sample,location,sessno,Diagnose_primär,
    GAD_yn,PDA_yn,SAD_yn,SPEC_yn,OCD_yn,PTSD_yn,DEP_yn,SSD_yn) %>% 
  summarytools::dfSummary(varnumbers = FALSE, max.distinct.values = 100) %>% 
  view()

#-------- .. Item, Factor, and Scale Intercorrelations --------

### Item correlation matrix
medi_items_val %>% 
  apaTables::apa.cor.table(filename = "MEDI_Item_Correlations.doc")
psych::corPlot(medi_items_val)

### Correlation matrix for the 9 latent factors (ESEM)
fit1 <- readRDS("Data/fit1.RDS")
latcormat <- lavaan::lavInspect(fit1, what = "cor.lv") %>% round(2)
colnames(latcormat) <- rownames(latcormat) <- c("soc", "traum", "arou", "intr", "depr", "som", "pos", "avoid", "neur")
latcormat
# soc   traum arou  intr  depr  som   pos   avoid neur 
# soc    1.00                                                
# traum  0.26  1.00                                          
# arou   0.36  0.48  1.00                                    
# intr   0.31  0.36  0.41  1.00                              
# depr   0.33  0.32  0.38  0.32  1.00                        
# som    0.35  0.45  0.46  0.44  0.42  1.00                  
# pos    0.26  0.39  0.36  0.48  0.17  0.42  1.00            
# avoid  0.42  0.36  0.44  0.35  0.41  0.48  0.29  1.00      
# neur  -0.33 -0.17 -0.21 -0.12 -0.51 -0.12  0.00 -0.28  1.00

### Correlation matrix for the 49 observed variables
lavaan::lavInspect(fit1, what = "cor.ov")

### Saving latent factor correlation matrix in Word document
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic_lv, positive_lv, depression_lv, arousal_lv, somatic_lv, intrusive_lv, social_lv, traumatic_lv, avoidance_lv) %>% 
  apaTables::apa.cor.table(filename = "MEDI_Latent_Factor_Correlations.doc")

### Correlation matric for the 9 MEDI scales
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance) %>% 
  apaTables::apa.cor.table(filename = "MEDI_Scale_Correlations.doc")



##%######################################################%##
#                                                          #
####       . Convergent and Discriminant Validity       ####
#                                                          #
##%######################################################%##

### Validity Measures 

# PID5BF+M
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                negaff, detachment, intAvoid) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_PID5BF+M.doc")

# PAS
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                pas) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_PAS.doc")

# OCI-R
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                oci_mean) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_OCI-R.doc")

# BDI-II
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                bdi_score) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_BDI-2.doc")

# BSI
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                gsi_score, dep_score, som_score, anx_score, int_score, obs_score) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_BSI.doc")

# PDS
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                pds_total) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_PDS.doc")

# BFIK
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                bfik_extraversion, bfik_agreeableness, bfik_conscientiousness, bfik_neuroticism, bfik_openness) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_BFIK.doc")

# PSWQ
alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                pswqd_total) %>% 
  # apaTables::apa.cor.table(show.conf.interval = FALSE)
  apaTables::apa.cor.table(filename = "Cor_MEDI_PSWQ.doc")


### Correlation between raw scale values and latent scale values

lv_raw_cor <- alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                neurotic_lv, positive_lv, depression_lv, arousal_lv, somatic_lv, intrusive_lv, social_lv, traumatic_lv, avoidance_lv) %>% 
  cor()

dsm.cor <- lv_raw_cor[1:9,10:18]


### Calculation of the convergent validity with diagnoses using factor values instead of scale values:
library(parameters)

medi_diag <- alldata_medi_lv_scores %>% 
  dplyr::select(neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                GAD_yn, PDA_yn, SAD_yn, SPEC_yn, OCD_yn, PTSD_yn, DEP_yn, SSD_yn) 

write.csv(round(cor_auto(medi_diag)[1:9,10:17],2), "lavCor_DSM5.csv")

dsm.cor <- cor_auto(medi_diag)[1:9,10:17]
dsm.cor 

ros.cor <- openxlsx::read.xlsx("diag_cors_Rosellini.xlsx", rowNames = T)
ros.cor[,1:8] <- sapply(ros.cor, as.numeric)

mean(as.matrix(abs(dsm.cor - ros.cor)))

medi_diag %>% 
  apaTables::apa.cor.table(filename = "Cor_MEDI_Diagnoses.doc")

round(cor(medi_diag, use = "complete", method = "pearson")[1:9, 10:17], 2)



##%######################################################%##
#                                                          #
####                   . Reliability                    ####
#                                                          #
##%######################################################%##

### McDonald´s Hierarchical Omega_h and Omega_t
psych::omegaFromSem(fit1)

### Cronbach´s Alpha [.73,.92], Omega_h [.51,.84], Omega_tot [.80,.94], min.split [.61,.85], max.split [.74,.90]
alldata_medi_lv_scores %>% dplyr::select(neur) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.66  0.82      0.85 0.99     1      1       0.8      0.78   0.48  0.47       5

alldata_medi_lv_scores %>% dplyr::select(pos) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.51  0.73      0.81 0.87  0.87   0.99      0.74      0.61   0.35  0.34       5

alldata_medi_lv_scores %>% dplyr::select(depr) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.74  0.86      0.89 0.98  0.99   0.99      0.86       0.8   0.55  0.56       5

alldata_medi_lv_scores %>% dplyr::select(arou) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.69  0.86      0.89 0.98  0.99   0.99      0.85       0.8   0.55  0.52       5

alldata_medi_lv_scores %>% dplyr::select(som) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.52  0.76      0.82 0.93  0.95   0.98      0.79      0.67   0.38  0.39       5

alldata_medi_lv_scores %>% dplyr::select(intr) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.64  0.84      0.87 0.98  0.98      1      0.86      0.82   0.46  0.46       6

alldata_medi_lv_scores %>% dplyr::select(soc) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.84  0.92      0.94 0.99  0.99      1       0.9      0.85    0.7   0.7       5

alldata_medi_lv_scores %>% dplyr::select(traum) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.69  0.87       0.9 0.99  0.99      1      0.85      0.81   0.57  0.56       5

alldata_medi_lv_scores %>% dplyr::select(avoid) %>% psych::reliability()
#           omega_h alpha omega.tot  Uni r.fit fa.fit max.split min.split mean.r med.r n.items
# All_items    0.54  0.76       0.8 0.84  0.89   0.94      0.81      0.65   0.29  0.29       8


### Test-Retest Reliability of MEDI Scales Using GAMs With Cubic Spline Smoothing

IDs_with_multiple_sessions <- alldata %>% 
  filter(duplicated(MEDI_Fallzahl)) %>% 
  pull(MEDI_Fallzahl) %>% 
  as.character() %>% 
  unique()

retest_df <- alldata %>% 
  filter(MEDI_Fallzahl %in% IDs_with_multiple_sessions) %>% 
  filter(if_all(.cols = medi1:medi49, .fns = ~ !is.na(.x))) %>% 
  group_by(MEDI_Fallzahl) %>%
  arrange(sessno) %>%
  slice_head(n = 2) %>%
  mutate(sessno = c(1,2)) %>% 
  ungroup() %>% 
  dplyr::select(MEDI_Fallzahl, sessno, ended, medi1:medi49,
                neurotic, positive, depression, arousal, somatic, intrusive, social, traumatic, avoidance,
                negaff, detachment, intAvoid, pas, oci_mean, bdi_score, gsi_score, dep_score, som_score, anx_score, int_score, obs_score, pds_total) %>% 
                # no test-retest data for bfik_extraversion, bfik_agreeableness, bfik_conscientiousness, bfik_neuroticism, bfik_openness, pswqd_total
    pivot_wider(., id_cols = MEDI_Fallzahl, 
              names_from = sessno, 
              values_from = ended:pds_total) %>% 
  mutate(diff_days = as.numeric(abs(ended_2 - ended_1)), .after = ended_2)

saveRDS(retest_df, "Data/retest_df.RDS")

retest_df <- retest_df[order(retest_df$diff_days),]

retest_df$diff_days %>% summary() # time difference in days
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   111.0   209.0   205.5   283.0   755.0

# GAM including a cubic spline smoothing function to correct for unequal time differences
coef(gam(scale(neurotic_2) ~ scale(neurotic_1) + s(scale(diff_days), 3), data = retest_df))[2]     # .66
coef(gam(scale(positive_2) ~ scale(positive_1) + s(scale(diff_days), 3), data = retest_df))[2]     # .70
coef(gam(scale(depression_2) ~ scale(depression_1) + s(scale(diff_days), 3), data = retest_df))[2] # .61
coef(gam(scale(arousal_2) ~ scale(arousal_1) + s(scale(diff_days), 3), data = retest_df))[2]       # .63
coef(gam(scale(somatic_2) ~ scale(somatic_1) + s(scale(diff_days), 3), data = retest_df))[2]       # .74
coef(gam(scale(intrusive_2) ~ scale(intrusive_1) + s(scale(diff_days), 3), data = retest_df))[2]   # .69
coef(gam(scale(social_2) ~ scale(social_1) + s(scale(diff_days), 3), data = retest_df))[2]         # .78
coef(gam(scale(traumatic_2) ~ scale(traumatic_1) + s(scale(diff_days), 3), data = retest_df))[2]   # .70
coef(gam(scale(avoidance_2) ~ scale(avoidance_1) + s(scale(diff_days), 3), data = retest_df))[2]   # .58


### Test-Retest Reliability of Validity Measures Calculated as Pre-Post Correlations

# lower25 <- quantile(retest_df$diff_days)[[2]] # 111
# upper25 <- quantile(retest_df$diff_days)[[4]] # 283

retest_df_IQR <- retest_df %>% 
  # filter(diff_days >= lower25 & diff_days <= upper25) # results in n = 138
  filter(diff_days <= 365) # results in n = 138

# PID5BF+M
cor.test(retest_df_IQR$negaff_1, retest_df_IQR$negaff_2) # .63
cor.test(retest_df_IQR$detachment_1, retest_df_IQR$detachment_2) # .67
cor.test(retest_df_IQR$intAvoid_1, retest_df_IQR$intAvoid_2) # .67
# PAS
cor.test(retest_df_IQR$pas_1, retest_df_IQR$pas_2) # .51
# OCI-R
cor.test(retest_df_IQR$oci_mean_1, retest_df_IQR$oci_mean_2) # .78
# BDI-II
cor.test(retest_df_IQR$bdi_score_1, retest_df_IQR$bdi_score_2) # .41
# BSI4
cor.test(retest_df_IQR$gsi_score_1, retest_df_IQR$gsi_score_2) # .51
cor.test(retest_df_IQR$dep_score_1, retest_df_IQR$dep_score_2) # .48
cor.test(retest_df_IQR$som_score_1, retest_df_IQR$som_score_2) # .56
cor.test(retest_df_IQR$anx_score_1, retest_df_IQR$anx_score_2) # .50
cor.test(retest_df_IQR$int_score_1, retest_df_IQR$int_score_2) # .57
cor.test(retest_df_IQR$obs_score_1, retest_df_IQR$obs_score_2) # .50
# PDS
cor.test(retest_df_IQR$pds_total_1, retest_df_IQR$pds_total_2) # .41



##%######################################################%##
#                                                          #
####              . Measurement Invariance              ####
#                                                          #
##%######################################################%##

# devtools::install_github("maksimrudnev/MIE.package", dependencies = TRUE)
pacman::p_load(dplyr, lavaan, MIE)

fit1 <- readRDS("Data/fit1.RDS")

# only validation sample:
medi_val_items <- alldata_medi_lv_scores %>% 
  dplyr::select(medi1:medi49, sample, sex, location)

colnames(medi_val_items)[1:49] <- paste0("MEDI_", 1:49)

model <- 
"
  F1=~ 0.01*MEDI_1+-0.03*MEDI_2+0.06*MEDI_3+0.01*MEDI_4+0.11*MEDI_5+0.04*MEDI_6+0.75*MEDI_7+-0.07*MEDI_8+-0.14*MEDI_9+-0.05*MEDI_10+-0.11*MEDI_11+-0.1*MEDI_12+0*MEDI_13+0.59*MEDI_14+0.32*MEDI_15+0.19*MEDI_16+0*MEDI_17+0*MEDI_18+-0.15*MEDI_19+-0.06*MEDI_20+-0.08*MEDI_21+0.74*MEDI_22+0.22*MEDI_23+-0.02*MEDI_24+0*MEDI_25+-0.03*MEDI_26+-0.04*MEDI_27+-0.05*MEDI_28+-0.09*MEDI_29+0.08*MEDI_30+0*MEDI_31+0.23*MEDI_32+-0.14*MEDI_33+0.26*MEDI_34+0.14*MEDI_35+-0.06*MEDI_36+0.01*MEDI_37+-0.06*MEDI_38+-0.02*MEDI_39+-0.04*MEDI_40+0.78*MEDI_41+0.16*MEDI_42+0.17*MEDI_43+0.09*MEDI_44+0.04*MEDI_45+0.08*MEDI_46+0.85*MEDI_47+0.11*MEDI_48+0.28*MEDI_49
  F2=~ -0.06*MEDI_1+-0.04*MEDI_2+-0.01*MEDI_3+0.06*MEDI_4+-0.14*MEDI_5+-0.07*MEDI_6+-0.05*MEDI_7+0.72*MEDI_8+-0.12*MEDI_9+-0.02*MEDI_10+0.07*MEDI_11+0.2*MEDI_12+0.1*MEDI_13+0.01*MEDI_14+0.19*MEDI_15+0.06*MEDI_16+-0.06*MEDI_17+0.1*MEDI_18+-0.03*MEDI_19+0.67*MEDI_20+0.07*MEDI_21+0.01*MEDI_22+0.16*MEDI_23+-0.08*MEDI_24+-0.07*MEDI_25+0.08*MEDI_26+0.08*MEDI_27+-0.03*MEDI_28+0.79*MEDI_29+0.17*MEDI_30+0.02*MEDI_31+0.01*MEDI_32+0.11*MEDI_33+0.16*MEDI_34+0.2*MEDI_35+0.02*MEDI_36+0.01*MEDI_37+0.07*MEDI_38+0.7*MEDI_39+0.05*MEDI_40+-0.02*MEDI_41+0.07*MEDI_42+0.07*MEDI_43+-0.01*MEDI_44+-0.13*MEDI_45+0*MEDI_46+-0.01*MEDI_47+0.61*MEDI_48+0.13*MEDI_49
  F3=~ 0.15*MEDI_1+-0.01*MEDI_2+-0.02*MEDI_3+0.61*MEDI_4+-0.03*MEDI_5+0.36*MEDI_6+-0.03*MEDI_7+0.04*MEDI_8+-0.09*MEDI_9+-0.02*MEDI_10+0.09*MEDI_11+0.08*MEDI_12+0.64*MEDI_13+0.05*MEDI_14+0.02*MEDI_15+0.09*MEDI_16+0.1*MEDI_17+0.58*MEDI_18+0.06*MEDI_19+0.1*MEDI_20+-0.04*MEDI_21+0*MEDI_22+0.1*MEDI_23+0.01*MEDI_24+0.1*MEDI_25+0.57*MEDI_26+0.11*MEDI_27+0.07*MEDI_28+0.09*MEDI_29+-0.06*MEDI_30+-0.06*MEDI_31+0.31*MEDI_32+0.05*MEDI_33+-0.11*MEDI_34+-0.18*MEDI_35+-0.06*MEDI_36+-0.02*MEDI_37+-0.19*MEDI_38+-0.01*MEDI_39+-0.04*MEDI_40+0.06*MEDI_41+-0.05*MEDI_42+0*MEDI_43+0.65*MEDI_44+0.1*MEDI_45+0.15*MEDI_46+0.03*MEDI_47+-0.02*MEDI_48+0.18*MEDI_49
  F4=~ 0.27*MEDI_1+0.06*MEDI_2+0.16*MEDI_3+-0.03*MEDI_4+0.56*MEDI_5+0.13*MEDI_6+0.03*MEDI_7+0*MEDI_8+-0.01*MEDI_9+0.11*MEDI_10+-0.01*MEDI_11+0.32*MEDI_12+0.08*MEDI_13+-0.11*MEDI_14+-0.04*MEDI_15+-0.09*MEDI_16+-0.05*MEDI_17+0.05*MEDI_18+-0.11*MEDI_19+0.06*MEDI_20+0.73*MEDI_21+0.1*MEDI_22+0.15*MEDI_23+-0.02*MEDI_24+-0.03*MEDI_25+-0.06*MEDI_26+0.09*MEDI_27+0.13*MEDI_28+0.06*MEDI_29+0.49*MEDI_30+-0.03*MEDI_31+0.05*MEDI_32+0.09*MEDI_33+0.03*MEDI_34+0.07*MEDI_35+-0.07*MEDI_36+0.01*MEDI_37+-0.08*MEDI_38+0.05*MEDI_39+0.66*MEDI_40+0.05*MEDI_41+0.12*MEDI_42+0.17*MEDI_43+0.02*MEDI_44+0.22*MEDI_45+0.43*MEDI_46+0.04*MEDI_47+0.05*MEDI_48+0.01*MEDI_49
  F5=~ 0.12*MEDI_1+0*MEDI_2+0.46*MEDI_3+-0.08*MEDI_4+0.06*MEDI_5+-0.14*MEDI_6+0.07*MEDI_7+-0.01*MEDI_8+0.06*MEDI_9+-0.01*MEDI_10+0.65*MEDI_11+0.13*MEDI_12+0.14*MEDI_13+-0.01*MEDI_14+0.02*MEDI_15+0.18*MEDI_16+0.04*MEDI_17+-0.05*MEDI_18+0.06*MEDI_19+-0.01*MEDI_20+0.03*MEDI_21+0.04*MEDI_22+0.03*MEDI_23+-0.11*MEDI_24+0.74*MEDI_25+0.29*MEDI_26+0.08*MEDI_27+-0.04*MEDI_28+-0.03*MEDI_29+0.07*MEDI_30+-0.05*MEDI_31+-0.06*MEDI_32+-0.21*MEDI_33+0*MEDI_34+0.17*MEDI_35+-0.08*MEDI_36+0.63*MEDI_37+-0.08*MEDI_38+0.08*MEDI_39+-0.02*MEDI_40+0*MEDI_41+-0.09*MEDI_42+0.42*MEDI_43+-0.06*MEDI_44+0.12*MEDI_45+0.01*MEDI_46+0.05*MEDI_47+0.01*MEDI_48+0.14*MEDI_49
  F6=~ -0.05*MEDI_1+0.01*MEDI_2+-0.15*MEDI_3+0.09*MEDI_4+-0.02*MEDI_5+0.37*MEDI_6+0.03*MEDI_7+0*MEDI_8+-0.05*MEDI_9+0.09*MEDI_10+-0.05*MEDI_11+-0.06*MEDI_12+0.02*MEDI_13+-0.06*MEDI_14+0.12*MEDI_15+0.11*MEDI_16+-0.1*MEDI_17+0.11*MEDI_18+0.69*MEDI_19+-0.03*MEDI_20+0.05*MEDI_21+-0.02*MEDI_22+0.06*MEDI_23+0.01*MEDI_24+0.06*MEDI_25+0.17*MEDI_26+0.06*MEDI_27+0.67*MEDI_28+-0.03*MEDI_29+0.1*MEDI_30+-0.01*MEDI_31+0.05*MEDI_32+-0.03*MEDI_33+0.25*MEDI_34+0.1*MEDI_35+0.06*MEDI_36+0.06*MEDI_37+0.7*MEDI_38+0*MEDI_39+0.07*MEDI_40+-0.02*MEDI_41+0.11*MEDI_42+-0.04*MEDI_43+0.02*MEDI_44+0.39*MEDI_45+0.06*MEDI_46+-0.03*MEDI_47+0.02*MEDI_48+0.11*MEDI_49
  F7=~ 0.03*MEDI_1+0.48*MEDI_2+-0.02*MEDI_3+-0.02*MEDI_4+-0.04*MEDI_5+-0.06*MEDI_6+-0.11*MEDI_7+-0.09*MEDI_8+0.02*MEDI_9+-0.1*MEDI_10+-0.11*MEDI_11+-0.08*MEDI_12+0.05*MEDI_13+-0.15*MEDI_14+-0.02*MEDI_15+-0.01*MEDI_16+0.75*MEDI_17+0.03*MEDI_18+0.02*MEDI_19+-0.05*MEDI_20+-0.03*MEDI_21+-0.04*MEDI_22+0.04*MEDI_23+0.78*MEDI_24+0.02*MEDI_25+0.04*MEDI_26+0.08*MEDI_27+-0.09*MEDI_28+-0.07*MEDI_29+0.06*MEDI_30+-0.02*MEDI_31+-0.07*MEDI_32+0.39*MEDI_33+0.05*MEDI_34+0.14*MEDI_35+0.4*MEDI_36+-0.17*MEDI_37+0.03*MEDI_38+0.03*MEDI_39+-0.04*MEDI_40+-0.02*MEDI_41+0.1*MEDI_42+-0.17*MEDI_43+0*MEDI_44+0*MEDI_45+0.03*MEDI_46+-0.02*MEDI_47+0.06*MEDI_48+0.05*MEDI_49
  F8=~ -0.01*MEDI_1+0.04*MEDI_2+0.08*MEDI_3+0.06*MEDI_4+0.09*MEDI_5+0.02*MEDI_6+0.03*MEDI_7+0.04*MEDI_8+0.79*MEDI_9+0.06*MEDI_10+0.06*MEDI_11+0.17*MEDI_12+0.03*MEDI_13+0.1*MEDI_14+0.26*MEDI_15+0.05*MEDI_16+0.05*MEDI_17+0.04*MEDI_18+0.07*MEDI_19+0.02*MEDI_20+0.01*MEDI_21+0.1*MEDI_22+0.04*MEDI_23+-0.03*MEDI_24+0.05*MEDI_25+-0.04*MEDI_26+0.52*MEDI_27+0*MEDI_28+0.06*MEDI_29+-0.04*MEDI_30+0.72*MEDI_31+0.1*MEDI_32+0.01*MEDI_33+0.03*MEDI_34+-0.1*MEDI_35+0.11*MEDI_36+0.03*MEDI_37+0.07*MEDI_38+0.02*MEDI_39+0.02*MEDI_40+0*MEDI_41+0.28*MEDI_42+0*MEDI_43+0.07*MEDI_44+0.02*MEDI_45+0.09*MEDI_46+0.04*MEDI_47+0.07*MEDI_48+0.15*MEDI_49
  F9=~ 0.47*MEDI_1+0.06*MEDI_2+0.26*MEDI_3+0.02*MEDI_4+0.12*MEDI_5+0.15*MEDI_6+0.01*MEDI_7+0.12*MEDI_8+0.02*MEDI_9+0.56*MEDI_10+0.24*MEDI_11+0.24*MEDI_12+-0.01*MEDI_13+0.17*MEDI_14+0.07*MEDI_15+0.4*MEDI_16+-0.13*MEDI_17+0.11*MEDI_18+0.09*MEDI_19+-0.04*MEDI_20+0.01*MEDI_21+0.03*MEDI_22+-0.15*MEDI_23+0.02*MEDI_24+-0.08*MEDI_25+-0.06*MEDI_26+-0.15*MEDI_27+-0.05*MEDI_28+-0.01*MEDI_29+0.04*MEDI_30+-0.07*MEDI_31+0.34*MEDI_32+0.03*MEDI_33+0.08*MEDI_34+0.53*MEDI_35+0.23*MEDI_36+-0.01*MEDI_37+0.04*MEDI_38+-0.02*MEDI_39+0.03*MEDI_40+0.1*MEDI_41+-0.01*MEDI_42+-0.1*MEDI_43+0.12*MEDI_44+-0.04*MEDI_45+-0.05*MEDI_46+0.05*MEDI_47+0.03*MEDI_48+0.04*MEDI_49
"


#-------- .. Invariance Between Locations (Giessen vs. Greifswald) --------

### With {lavaan}: No equality constraints (configural invariance)
fitmeasures(lavaan::cfa(
  model = model,
  data = medi_val_items,
  group = "location",
  estimator = "MLR"), 
  c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.946        0.944        0.034        0.041

### With {lavaan}: Force equal loadings (metric invariance)
fitmeasures(lavaan::cfa(
  model = model,
  data = medi_val_items,
  group = "location",
  group.equal = c("loadings"),
  estimator = "MLR"), 
  c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.946        0.944        0.034        0.041

### With {lavaan}: Force equal loadings and intercepts (scalar invariance)
fitmeasures(lavaan::cfa(
  model = model,
  data = medi_val_items,
  group = "location",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR"), 
  c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.944        0.942        0.034        0.041


#-------- .. Invariance Between Genders (Male vs. Female) --------

# Removing Sex == Diverse (n = 10)
dat <- medi_val_items %>% 
  filter(sex %in% c("Männlich", "Weiblich"))

### With {lavaan}: No equality constraints (configural invariance)
fitmeasures(cfa(
  model = model,
  data = dat, # medi_val_items
  group = "sex",
  estimator = "MLR"), 
  c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.950        0.948        0.033        0.043

### With {lavaan}: Force equal loadings (metric invariance)
fitmeasures(cfa(
  model = model,
  data = dat, # medi_val_items
  group = "sex",
  group.equal = c("loadings"),
  estimator = "MLR"), 
  c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.950        0.948        0.033        0.043

### With {lavaan}: Force equal loadings and intercepts (scalar invariance)
gender.scalar <- cfa(
  model = model,
  data = dat, # medi_val_items
  group = "sex",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR")

omegaFromSem(gender.scalar)

fitmeasures(gender.scalar)
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.948        0.946        0.034        0.044

colMeans(predict(gender.scalar)$Männlich)
matrixStats::colSds(predict(gender.scalar)$Männlich)


#-------- .. Invariance Between Samples (Patients vs. Non-Patients) --------

### With {lavaan}: No equality constraints (configural invariance)
sample.configural <- cfa(
  model = model,
  data = medi_val_items,
  group = "sample",
  estimator = "MLR")
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.933        0.930        0.036        0.046

### With {lavaan}: Force equal loadings (metric invariance)
sample.metric <- cfa(
  model = model,
  data = medi_val_items,
  group = "sample",
  group.equal = c("loadings"),
  estimator = "MLR")
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.933        0.930        0.036        0.046

### With {lavaan}: Force equal loadings and intercepts (scalar invariance)
sample.scalar <- cfa(
  model = model,
  data = medi_val_items,
  group = "sample",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR")
# cfi.robust   tli.robust rmsea.robust         srmr 
#      0.926        0.925        0.037        0.047

fitmeasures(sample.configural ,c("cfi.robust","tli.robust","rmsea.robust","srmr"))
fitmeasures(sample.metric ,c("cfi.robust","tli.robust","rmsea.robust","srmr"))
fitmeasures(sample.scalar ,c("cfi.robust","tli.robust","rmsea.robust","srmr"))

