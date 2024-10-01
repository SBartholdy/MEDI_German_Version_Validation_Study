
##%######################################################%##
#                                                          #
####                      05 NORMS                      ####
#                                                          #
##%######################################################%##

pacman::p_load(dplyr, tidyr, ggplot2, forcats, gtools, psych, summarytools, overlapping, corrplot, papaja, report, flextable, apaTables, labelled, broom, ltm, descr, DescTools, tableone, frequency, skimr)
# options(max.print = 2500)

alldata <- readRDS("Data/alldata.RDS")
alldata_medi_lv_scores <- readRDS("Data/alldata_medi_lv_scores.RDS") # validation sample (including only the first assessment per individual)

### MEDI Item-Scale Association:
# Neurotic Temperament: 1, 10, 16, 32, 35
neur = c("medi1","medi10","medi16","medi32","medi35")
# Positive Temperament: 2, 17, 24, 33, 36
pos = c("medi2","medi17","medi24","medi33","medi36")
# Depression: 3, 11, 25, 37, 43
depr = c("medi3","medi11","medi25","medi37","medi43")
# Autonomic Arousal: 4, 13, 18, 26, 44
arou = c("medi4","medi13","medi18","medi26","medi44")
# Somatic Anxiety: 6, 19, 28, 38, 45
som = c("medi6","medi19","medi28","medi38","medi45")
# Intrusive Cognition: 5, 12, 21, 30, 40, 46
intr = c("medi5","medi12","medi21","medi30","medi40","medi46")
# Social Concerns: 7, 14, 22, 41, 47
soc = c("medi7","medi14","medi22","medi41","medi47")
# Traumatic Re-experiencing: 8, 20, 29, 39, 48
traum = c("medi8","medi20","medi29","medi39","medi48")
# Avoidance: 9, 15, 23, 27, 31, 34, 42, 49
avoid = c("medi9","medi15","medi23","medi27","medi31","medi34","medi42","medi49")

### Recoding MEDI Scales as Mean Scores for the Calculation of Norm Values:
alldata_medi_lv_scores <- alldata_medi_lv_scores %>% 
  mutate(neurotic = rowMeans(dplyr::select(alldata_medi_lv_scores, neur), na.rm = TRUE),
         positive = rowMeans(dplyr::select(alldata_medi_lv_scores, pos), na.rm = TRUE),
         depression = rowMeans(dplyr::select(alldata_medi_lv_scores, depr), na.rm = TRUE),
         arousal = rowMeans(dplyr::select(alldata_medi_lv_scores, arou), na.rm = TRUE),
         somatic = rowMeans(dplyr::select(alldata_medi_lv_scores, som), na.rm = TRUE),
         intrusive = rowMeans(dplyr::select(alldata_medi_lv_scores, intr), na.rm = TRUE),
         social = rowMeans(dplyr::select(alldata_medi_lv_scores, soc), na.rm = TRUE),
         traumatic = rowMeans(dplyr::select(alldata_medi_lv_scores, traum), na.rm = TRUE),
         avoidance = rowMeans(dplyr::select(alldata_medi_lv_scores, avoid), na.rm = TRUE))


#-------- . Tests for Normal Distribution --------

# Tests of Normality for Deciding on Z- or T-Scores for Norm Values
shapiro.test(alldata_medi_lv_scores$arousal) # sig.
shapiro.test(alldata_medi_lv_scores$avoidance) # sig.
shapiro.test(alldata_medi_lv_scores$depression) # sig.
shapiro.test(alldata_medi_lv_scores$intrusive) # sig.
shapiro.test(alldata_medi_lv_scores$neurotic) # sig.
shapiro.test(alldata_medi_lv_scores$positive) # sig.
shapiro.test(alldata_medi_lv_scores$social) # sig.
shapiro.test(alldata_medi_lv_scores$somatic) # sig.
shapiro.test(alldata_medi_lv_scores$traumatic) # sig.
# Z-scores not appropriate for norm values -> calculation of T-scores and percentiles


#-------- . Complete Sample --------

tab_alldata_Ges_Norms <- 
  alldata %>% # CAUTION: complete sample with multiple MEDI assessment per person
  dplyr::select(neurotic:avoidance) %>% 
  mutate(neurotic = neurotic / 5,
         positive = positive / 5,
         depression = depression / 5,
         arousal = arousal / 5,
         somatic = somatic / 5,
         social = social / 5,
         intrusive = intrusive / 6,
         traumatic = traumatic / 5,
         avoidance = avoidance / 8) %>%
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
#   variable    mean    sd   min    q1   med    q3   max n.valid
#   <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1 neurotic    4.2   1.92     0  2.8   4.4   5.8    8      1661
# 2 positive    4.25  1.51     0  3.2   4.2   5.4    8      1661
# 3 depression  3.03  2.03     0  1.2   2.8   4.6    8      1661
# 4 arousal     2.25  1.98     0  0.6   1.8   3.6    8      1661
# 5 somatic     2.58  1.77     0  1.2   2.4   3.8    8      1661
# 6 intrusive   2.79  1.91     0  1.33  2.5   4.17   8      1661
# 7 social      3.47  2.33     0  1.4   3.4   5.4    8      1661
# 8 traumatic   2.13  2.02     0  0.4   1.6   3.4    8      1661
# 9 avoidance   2.87  1.5      0  1.75  2.75  3.88   7.5    1661
openxlsx::write.xlsx(tab_alldata_Ges_Norms, "Normwerttabellen/Descriptives_alldata.xlsx")


#-------- . Validation Sample --------

tab_Validation_Ges_Norms <- alldata_medi_lv_scores %>% # validation sample (including only the first assessment per individual)
  dplyr::select(neurotic:avoidance) %>% 
  # mutate(neurotic = neurotic * 5,
  #        positive = positive * 5,
  #        depression = depression * 5,
  #        arousal = arousal * 5,
  #        somatic = somatic * 5,
  #        social = social * 5,
  #        intrusive = intrusive * 6,
  #        traumatic = traumatic * 5,
  #        avoidance = avoidance * 8) %>% 
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
#   variable    mean    sd   min    q1   med    q3   max n.valid
#   <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
# 1 neurotic    4.16  1.96     0  2.8   4.4   5.8    8      1129
# 2 positive    4.28  1.49     0  3.2   4.4   5.4    8      1129
# 3 depression  3.13  2.06     0  1.4   3     4.8    8      1129
# 4 arousal     2.3   2.03     0  0.6   1.8   3.8    8      1129
# 5 somatic     2.7   1.77     0  1.4   2.4   4      8      1129
# 6 intrusive   2.94  1.88     0  1.33  2.83  4.33   8      1129
# 7 social      3.44  2.32     0  1.4   3.2   5.2    8      1129
# 8 traumatic   2.25  2.06     0  0.4   1.6   3.6    8      1129
# 9 avoidance   2.89  1.51     0  1.75  2.75  3.88   7.5    1129
openxlsx::write.xlsx(tab_Validation_Ges_Norms, "Normwerttabellen/Descriptives_Validation.xlsx")

### Scale Value, T-Value, and Percentile Rank Distribution Tables Through Linear Models
# neurotic #
tab_Ges_neur_t <- predict(lm(neurotic_t ~ neurotic, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1)))
tab_Ges_neur_pr <- predict(lm(neurotic_pr ~ neurotic, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1)))
tab_Ges_neur <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Ges_neur_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Ges_neur_pr), digits = 2))
tab_Ges_neur[which(tab_Ges_neur$Percentile < 0 | tab_Ges_neur$Percentile > 100), "Percentile"] <- NA
# positive #
tab_Ges_pos_t <- predict(lm(positive_t ~ positive, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1)))
tab_Ges_pos_pr <- predict(lm(positive_pr ~ positive, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1)))
tab_Ges_pos <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                          "T" = round(as.numeric(tab_Ges_pos_t), digits = 2),
                          "Percentile" = round(as.numeric(tab_Ges_pos_pr), digits = 2))
tab_Ges_pos[which(tab_Ges_pos$Percentile < 0 | tab_Ges_pos$Percentile > 100), "Percentile"] <- NA
# depression #
tab_Ges_depr_t <- predict(lm(depression_t ~ depression, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1)))
tab_Ges_depr_pr <- predict(lm(depression_pr ~ depression, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1)))
tab_Ges_depr <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Ges_depr_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Ges_depr_pr), digits = 2))
tab_Ges_depr[which(tab_Ges_depr$Percentile < 0 | tab_Ges_depr$Percentile > 100), "Percentile"] <- NA
# arousal #
tab_Ges_arou_t <- predict(lm(arousal_t ~ arousal, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1)))
tab_Ges_arou_pr <- predict(lm(arousal_pr ~ arousal, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1)))
tab_Ges_arou <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Ges_arou_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Ges_arou_pr), digits = 2))
tab_Ges_arou[which(tab_Ges_arou$Percentile < 0 | tab_Ges_arou$Percentile > 100), "Percentile"] <- NA
# somatic #
tab_Ges_som_t <- predict(lm(somatic_t ~ somatic, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1)))
tab_Ges_som_pr <- predict(lm(somatic_pr ~ somatic, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1)))
tab_Ges_som <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                          "T" = round(as.numeric(tab_Ges_som_t), digits = 2),
                          "Percentile" = round(as.numeric(tab_Ges_som_pr), digits = 2))
tab_Ges_som[which(tab_Ges_som$Percentile < 0 | tab_Ges_som$Percentile > 100), "Percentile"] <- NA
# social #
tab_Ges_soc_t <- predict(lm(social_t ~ social, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1)))
tab_Ges_soc_pr <- predict(lm(social_pr ~ social, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1)))
tab_Ges_soc <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                          "T" = round(as.numeric(tab_Ges_soc_t), digits = 2),
                          "Percentile" = round(as.numeric(tab_Ges_soc_pr), digits = 2))
tab_Ges_soc[which(tab_Ges_soc$Percentile < 0 | tab_Ges_soc$Percentile > 100), "Percentile"] <- NA
# intrusive #
tab_Ges_intr_t <- predict(lm(intrusive_t ~ intrusive, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1)))
tab_Ges_intr_pr <- predict(lm(intrusive_pr ~ intrusive, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1)))
tab_Ges_intr <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Ges_intr_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Ges_intr_pr), digits = 2))
tab_Ges_intr[which(tab_Ges_intr$Percentile < 0 | tab_Ges_intr$Percentile > 100), "Percentile"] <- NA
# traumatic #
tab_Ges_traum_t <- predict(lm(traumatic_t ~ traumatic, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1)))
tab_Ges_traum_pr <- predict(lm(traumatic_pr ~ traumatic, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1)))
tab_Ges_traum <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Ges_traum_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Ges_traum_pr), digits = 2))
tab_Ges_traum[which(tab_Ges_traum$Percentile < 0 | tab_Ges_traum$Percentile > 100), "Percentile"] <- NA
# avoidance #
tab_Ges_avoid_t <- predict(lm(avoidance_t ~ avoidance, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1)))
tab_Ges_avoid_pr <- predict(lm(avoidance_pr ~ avoidance, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1)))
tab_Ges_avoid <- data.frame("Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Ges_avoid_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Ges_avoid_pr), digits = 2))
tab_Ges_avoid[which(tab_Ges_avoid$Percentile < 0 | tab_Ges_avoid$Percentile > 100), "Percentile"] <- NA

tab_Ges <- cbind(tab_Ges_neur, tab_Ges_pos, tab_Ges_depr,
                 tab_Ges_arou, tab_Ges_som, tab_Ges_soc,
                 tab_Ges_intr, tab_Ges_traum, tab_Ges_avoid)
openxlsx::write.xlsx(tab_Ges, "Normwerttabellen/Norm_Values_Gesamt.xlsx")


#-------- . Sex --------

tab_Sex_Norms <- alldata_medi_lv_scores %>% 
  group_by(sex) %>% 
  dplyr::select(neurotic:avoidance) %>% 
  # mutate(neurotic = neurotic * 5,
  #        positive = positive * 5,
  #        depression = depression * 5,
  #        arousal = arousal * 5,
  #        somatic = somatic * 5,
  #        social = social * 5,
  #        intrusive = intrusive * 6,
  #        traumatic = traumatic * 5,
  #        avoidance = avoidance * 8) %>% 
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
openxlsx::write.xlsx(tab_Sex_Norms, "Normwerttabellen/Descriptives_Sex.xlsx")


aov(arousal ~ sex, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(avoidance ~ sex, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(depression ~ sex, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(intrusive ~ sex, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(neurotic ~ sex, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(positive ~ sex, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(social ~ sex, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(somatic ~ sex, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(traumatic ~ sex, data = alldata_medi_lv_scores) %>% summary() # n.s. differences


### Scale Value, T-Value, and Percentile Rank Distribution Tables Through Linear Models
# neurotic #
# male n = 317
tab_Sex_M_neur_t <- predict(lm(neurotic_t ~ neurotic + sex, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_neur_pr <- predict(lm(neurotic_pr ~ neurotic + sex, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_neur <- data.frame("Sex" = "Male", 
                              "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                              "T" = round(as.numeric(tab_Sex_M_neur_t), digits = 2),
                              "Percentile" = round(as.numeric(tab_Sex_M_neur_pr), digits = 2))
tab_Sex_M_neur[which(tab_Sex_M_neur$Percentile < 0 | tab_Sex_M_neur$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_neur_t <- predict(lm(neurotic_t ~ neurotic + sex, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_neur_pr <- predict(lm(neurotic_pr ~ neurotic + sex, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_neur <- data.frame("Sex" = "Female", 
                              "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                              "T" = round(as.numeric(tab_Sex_F_neur_t), digits = 2),
                              "Percentile" = round(as.numeric(tab_Sex_F_neur_pr), digits = 2))
tab_Sex_F_neur[which(tab_Sex_F_neur$Percentile < 0 | tab_Sex_F_neur$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# positive #
# male n = 317
tab_Sex_M_pos_t <- predict(lm(positive_t ~ positive + sex, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_pos_pr <- predict(lm(positive_pr ~ positive + sex, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_pos <- data.frame("Sex" = "Male", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_M_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_M_pos_pr), digits = 2))
tab_Sex_M_pos[which(tab_Sex_M_pos$Percentile < 0 | tab_Sex_M_pos$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_pos_t <- predict(lm(positive_t ~ positive + sex, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_pos_pr <- predict(lm(positive_pr ~ positive + sex, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_pos <- data.frame("Sex" = "Female", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_F_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_F_pos_pr), digits = 2))
tab_Sex_F_pos[which(tab_Sex_F_pos$Percentile < 0 | tab_Sex_F_pos$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# depression #
# male n = 317
tab_Sex_M_depr_t <- predict(lm(depression_t ~ depression + sex, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_depr_pr <- predict(lm(depression_pr ~ depression + sex, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_depr <- data.frame("Sex" = "Male", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_M_depr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_M_depr_pr), digits = 2))
tab_Sex_M_depr[which(tab_Sex_M_depr$Percentile < 0 | tab_Sex_M_depr$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_depr_t <- predict(lm(depression_t ~ depression + sex, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_depr_pr <- predict(lm(depression_pr ~ depression + sex, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_depr <- data.frame("Sex" = "Female", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_F_depr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_F_depr_pr), digits = 2))
tab_Sex_F_depr[which(tab_Sex_F_depr$Percentile < 0 | tab_Sex_F_depr$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# arousal #
# male n = 317
tab_Sex_M_arou_t <- predict(lm(arousal_t ~ arousal + sex, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_arou_pr <- predict(lm(arousal_pr ~ arousal + sex, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_arou <- data.frame("Sex" = "Male", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_M_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_M_arou_pr), digits = 2))
tab_Sex_M_arou[which(tab_Sex_M_arou$Percentile < 0 | tab_Sex_M_arou$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_arou_t <- predict(lm(arousal_t ~ arousal + sex, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_arou_pr <- predict(lm(arousal_pr ~ arousal + sex, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_arou <- data.frame("Sex" = "Female", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_F_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_F_arou_pr), digits = 2))
tab_Sex_F_arou[which(tab_Sex_F_arou$Percentile < 0 | tab_Sex_F_arou$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# somatic #
# male n = 317
tab_Sex_M_som_t <- predict(lm(somatic_t ~ somatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_som_pr <- predict(lm(somatic_pr ~ somatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_som <- data.frame("Sex" = "Male", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_M_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_M_som_pr), digits = 2))
tab_Sex_M_som[which(tab_Sex_M_som$Percentile < 0 | tab_Sex_M_som$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_som_t <- predict(lm(somatic_t ~ somatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_som_pr <- predict(lm(somatic_pr ~ somatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_som <- data.frame("Sex" = "Female", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_F_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_F_som_pr), digits = 2))
tab_Sex_F_som[which(tab_Sex_F_som$Percentile < 0 | tab_Sex_F_som$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# social #
# male n = 317
tab_Sex_M_soc_t <- predict(lm(social_t ~ social + sex, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_soc_pr <- predict(lm(social_pr ~ social + sex, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_soc <- data.frame("Sex" = "Male", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_M_soc_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_M_soc_pr), digits = 2))
tab_Sex_M_soc[which(tab_Sex_M_soc$Percentile < 0 | tab_Sex_M_soc$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_soc_t <- predict(lm(social_t ~ social + sex, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_soc_pr <- predict(lm(social_pr ~ social + sex, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_soc <- data.frame("Sex" = "Female", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_F_soc_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_F_soc_pr), digits = 2))
tab_Sex_F_soc[which(tab_Sex_F_soc$Percentile < 0 | tab_Sex_F_soc$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# intrusive #
# male n = 317
tab_Sex_M_intr_t <- predict(lm(intrusive_t ~ intrusive + sex, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_intr_pr <- predict(lm(intrusive_pr ~ intrusive + sex, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_intr <- data.frame("Sex" = "Male", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_M_intr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_M_intr_pr), digits = 2))
tab_Sex_M_intr[which(tab_Sex_M_intr$Percentile < 0 | tab_Sex_M_intr$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_intr_t <- predict(lm(intrusive_t ~ intrusive + sex, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_intr_pr <- predict(lm(intrusive_pr ~ intrusive + sex, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_intr <- data.frame("Sex" = "Female", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_F_intr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_F_intr_pr), digits = 2))
tab_Sex_F_intr[which(tab_Sex_F_intr$Percentile < 0 | tab_Sex_F_intr$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# traumatic #
# male n = 317
tab_Sex_M_traum_t <- predict(lm(traumatic_t ~ traumatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_traum_pr <- predict(lm(traumatic_pr ~ traumatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_traum <- data.frame("Sex" = "Male", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_M_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_M_traum_pr), digits = 2))
tab_Sex_M_traum[which(tab_Sex_M_traum$Percentile < 0 | tab_Sex_M_traum$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_traum_t <- predict(lm(traumatic_t ~ traumatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_traum_pr <- predict(lm(traumatic_pr ~ traumatic + sex, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_traum <- data.frame("Sex" = "Female", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Sex_F_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Sex_F_traum_pr), digits = 2))
tab_Sex_F_traum[which(tab_Sex_F_traum$Percentile < 0 | tab_Sex_F_traum$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

# avoidance #
# male n = 317
tab_Sex_M_avoid_t <- predict(lm(avoidance_t ~ avoidance + sex, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + sex, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sex = "Männlich"))
tab_Sex_M_avoid <- data.frame("Sex" = "Male", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Sex_M_avoid_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Sex_M_avoid_pr), digits = 2))
tab_Sex_M_avoid[which(tab_Sex_M_avoid$Percentile < 0 | tab_Sex_M_avoid$Percentile > 100), "Percentile"] <- NA
# female n = 802
tab_Sex_F_avoid_t <- predict(lm(avoidance_t ~ avoidance + sex, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + sex, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sex = "Weiblich"))
tab_Sex_F_avoid <- data.frame("Sex" = "Female", 
                              "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                              "T" = round(as.numeric(tab_Sex_F_avoid_t), digits = 2),
                              "Percentile" = round(as.numeric(tab_Sex_F_avoid_pr), digits = 2))
tab_Sex_F_avoid[which(tab_Sex_F_avoid$Percentile < 0 | tab_Sex_F_avoid$Percentile > 100), "Percentile"] <- NA
# diverse only n = 10 -> no norm values

tab_Sex <- cbind(tab_Sex_M_neur, tab_Sex_F_neur,
                 tab_Sex_M_pos, tab_Sex_F_pos,
                 tab_Sex_M_depr, tab_Sex_F_depr,
                 tab_Sex_M_arou, tab_Sex_F_arou,
                 tab_Sex_M_som, tab_Sex_F_som,
                 tab_Sex_M_soc, tab_Sex_F_soc,
                 tab_Sex_M_intr, tab_Sex_F_intr,
                 tab_Sex_M_traum, tab_Sex_F_traum,
                 tab_Sex_M_avoid, tab_Sex_F_avoid)
openxlsx::write.xlsx(tab_Sex, "Normwerttabellen/Norm_Values_Sex.xlsx")


#-------- . Age Groups --------

# Original Age Groups (Taken From the Subsamples From Giessen):
# value       label     n
# 1    18 bis 20 Jahre  142
# 2    21 - 25 Jahre    319
# 3    26 - 30 Jahre    146
# 4    31 - 35 Jahre    98
# 5    36 - 40 Jahre    51
# 6    41 - 45 Jahre    28
# 7    46 - 50 Jahre    26
# 8    51 - 55 Jahre    34
# 9    56 - 60 Jahre    27
# 10   61 - 65 Jahre    25

# Checking the appropriateness of Age Groups by forming quantiles with gtools::quantcut 
# (only in Greifswald data, because the numerical age is also available for this):
q4 <- alldata_medi_lv_scores %>% 
  filter(location == "Greifswald") %>% 
  dplyr::pull(Alter) %>% 
  gtools::quantcut(q = 4)
table(q4)
# [18,21] (21,25] (25,33] (33,79] 
#     252     286     213     223

q5 <- alldata_medi_lv_scores %>% 
  filter(location == "Greifswald") %>% 
  dplyr::pull(Alter) %>% 
  gtools::quantcut(q = 5)
table(q5)
# [18,21] (21,23] (23,27] (27,36] (36,79] 
#     252     174     182     179     187

q6 <- alldata_medi_lv_scores %>% 
  filter(location == "Greifswald") %>% 
  dplyr::pull(Alter) %>% 
  gtools::quantcut(q = 6)
table(q6)
# [18,20] (20,22] (22,25] (25,29] (29,38] (38,79] 
#     169     175     194     121     157     158

q7 <- alldata_medi_lv_scores %>% 
  filter(location == "Greifswald") %>% 
  dplyr::pull(Alter) %>% 
  gtools::quantcut(q = 7)
table(q7)
# [18,20] (20,22] (22,23] (23,26] (26,31] (31,40] (40,79] 
#     169     175      82     155     120     135     138

# (suboptimal) solution: combining Age Groups "31 - 40 Jahre" and "über 40 Jahre" (already done in 01_Pre-Processing.R)
alldata_medi_lv_scores <- alldata_medi_lv_scores %>% 
  mutate(Altersgruppe2 = case_when(
    Altersgruppe == 5 ~ 4,
    Altersgruppe >= 6 ~ 5,
    TRUE ~ Altersgruppe))
# saveRDS(alldata_medi_lv_scores, file = "Data/alldata_medi_lv_scores.RDS")

# Resulting Age Groups:
# value       label     n
# 1    18 - 20 Jahre    190
# 2    21 - 25 Jahre    414
# 3    26 - 30 Jahre    176
# 4    31 - 40 Jahre    172
# 5    > 40 Jahre       177


tab_Age_Norms <- alldata_medi_lv_scores %>% 
  group_by(Altersgruppe2) %>% 
  dplyr::select(Altersgruppe2, neurotic:avoidance) %>% 
  # mutate(neurotic = neurotic * 5,
  #        positive = positive * 5,
  #        depression = depression * 5,
  #        arousal = arousal * 5,
  #        somatic = somatic * 5,
  #        social = social * 5,
  #        intrusive = intrusive * 6,
  #        traumatic = traumatic * 5,
  #        avoidance = avoidance * 8) %>% 
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
openxlsx::write.xlsx(tab_Age_Norms, "Normwerttabellen/Descriptives_Age.xlsx")


aov(arousal ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(avoidance ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(depression ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(intrusive ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(neurotic ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(positive ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(social ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(somatic ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(traumatic ~ Altersgruppe2, data = alldata_medi_lv_scores) %>% summary() # n.s. differences


### Scale Value, T-Value, and Percentile Rank Distribution Tables Through Linear Models

# neurotic #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_neur_t <- predict(lm(neurotic_t ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_neur_pr <- predict(lm(neurotic_pr ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_neur <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_neur_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_neur_pr), digits = 2))
tab_Age_1_neur[which(tab_Age_1_neur$Percentile < 0 | tab_Age_1_neur$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_neur_t <- predict(lm(neurotic_t ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_neur_pr <- predict(lm(neurotic_pr ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_neur <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_neur_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_neur_pr), digits = 2))
tab_Age_2_neur[which(tab_Age_2_neur$Percentile < 0 | tab_Age_2_neur$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_neur_t <- predict(lm(neurotic_t ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_neur_pr <- predict(lm(neurotic_pr ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_neur <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_neur_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_neur_pr), digits = 2))
tab_Age_3_neur[which(tab_Age_3_neur$Percentile < 0 | tab_Age_3_neur$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_neur_t <- predict(lm(neurotic_t ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_neur_pr <- predict(lm(neurotic_pr ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_neur <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_neur_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_neur_pr), digits = 2))
tab_Age_4_neur[which(tab_Age_4_neur$Percentile < 0 | tab_Age_4_neur$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_neur_t <- predict(lm(neurotic_t ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_neur_pr <- predict(lm(neurotic_pr ~ neurotic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_neur <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_neur_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_neur_pr), digits = 2))
tab_Age_5_neur[which(tab_Age_5_neur$Percentile < 0 | tab_Age_5_neur$Percentile > 100), "Percentile"] <- NA

# positive #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_pos_t <- predict(lm(positive_t ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_pos_pr <- predict(lm(positive_pr ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_pos <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_pos_pr), digits = 2))
tab_Age_1_pos[which(tab_Age_1_pos$Percentile < 0 | tab_Age_1_pos$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_pos_t <- predict(lm(positive_t ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_pos_pr <- predict(lm(positive_pr ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_pos <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_pos_pr), digits = 2))
tab_Age_2_pos[which(tab_Age_2_pos$Percentile < 0 | tab_Age_2_pos$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_pos_t <- predict(lm(positive_t ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_pos_pr <- predict(lm(positive_pr ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_pos <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_pos_pr), digits = 2))
tab_Age_3_pos[which(tab_Age_3_pos$Percentile < 0 | tab_Age_3_pos$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_pos_t <- predict(lm(positive_t ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_pos_pr <- predict(lm(positive_pr ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_pos <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_pos_pr), digits = 2))
tab_Age_4_pos[which(tab_Age_4_pos$Percentile < 0 | tab_Age_4_pos$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_pos_t <- predict(lm(positive_t ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_pos_pr <- predict(lm(positive_pr ~ positive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_pos <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_pos_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_pos_pr), digits = 2))
tab_Age_5_pos[which(tab_Age_5_pos$Percentile < 0 | tab_Age_5_pos$Percentile > 100), "Percentile"] <- NA

# depression #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_depr_t <- predict(lm(depression_t ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_depr_pr <- predict(lm(depression_pr ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_depr <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_depr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_depr_pr), digits = 2))
tab_Age_1_depr[which(tab_Age_1_depr$Percentile < 0 | tab_Age_1_depr$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_depr_t <- predict(lm(depression_t ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_depr_pr <- predict(lm(depression_pr ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_depr <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_depr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_depr_pr), digits = 2))
tab_Age_2_depr[which(tab_Age_2_depr$Percentile < 0 | tab_Age_2_depr$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_depr_t <- predict(lm(depression_t ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_depr_pr <- predict(lm(depression_pr ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_depr <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_depr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_depr_pr), digits = 2))
tab_Age_3_depr[which(tab_Age_3_depr$Percentile < 0 | tab_Age_3_depr$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_depr_t <- predict(lm(depression_t ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_depr_pr <- predict(lm(depression_pr ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_depr <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_depr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_depr_pr), digits = 2))
tab_Age_4_depr[which(tab_Age_4_depr$Percentile < 0 | tab_Age_4_depr$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_depr_t <- predict(lm(depression_t ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_depr_pr <- predict(lm(depression_pr ~ depression + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_depr <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_depr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_depr_pr), digits = 2))
tab_Age_5_depr[which(tab_Age_5_depr$Percentile < 0 | tab_Age_5_depr$Percentile > 100), "Percentile"] <- NA

# arousal #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_arou_t <- predict(lm(arousal_t ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_arou_pr <- predict(lm(arousal_pr ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_arou <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_arou_pr), digits = 2))
tab_Age_1_arou[which(tab_Age_1_arou$Percentile < 0 | tab_Age_1_arou$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_arou_t <- predict(lm(arousal_t ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_arou_pr <- predict(lm(arousal_pr ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_arou <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_arou_pr), digits = 2))
tab_Age_2_arou[which(tab_Age_2_arou$Percentile < 0 | tab_Age_2_arou$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_arou_t <- predict(lm(arousal_t ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_arou_pr <- predict(lm(arousal_pr ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_arou <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_arou_pr), digits = 2))
tab_Age_3_arou[which(tab_Age_3_arou$Percentile < 0 | tab_Age_3_arou$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_arou_t <- predict(lm(arousal_t ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_arou_pr <- predict(lm(arousal_pr ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_arou <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_arou_pr), digits = 2))
tab_Age_4_arou[which(tab_Age_4_arou$Percentile < 0 | tab_Age_4_arou$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_arou_t <- predict(lm(arousal_t ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_arou_pr <- predict(lm(arousal_pr ~ arousal + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_arou <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_arou_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_arou_pr), digits = 2))
tab_Age_5_arou[which(tab_Age_5_arou$Percentile < 0 | tab_Age_5_arou$Percentile > 100), "Percentile"] <- NA

# somatic #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_som_t <- predict(lm(somatic_t ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_som_pr <- predict(lm(somatic_pr ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_som <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_som_pr), digits = 2))
tab_Age_1_som[which(tab_Age_1_som$Percentile < 0 | tab_Age_1_som$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_som_t <- predict(lm(somatic_t ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_som_pr <- predict(lm(somatic_pr ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_som <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_som_pr), digits = 2))
tab_Age_2_som[which(tab_Age_2_som$Percentile < 0 | tab_Age_2_som$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_som_t <- predict(lm(somatic_t ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_som_pr <- predict(lm(somatic_pr ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_som <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_som_pr), digits = 2))
tab_Age_3_som[which(tab_Age_3_som$Percentile < 0 | tab_Age_3_som$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_som_t <- predict(lm(somatic_t ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_som_pr <- predict(lm(somatic_pr ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_som <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_som_pr), digits = 2))
tab_Age_4_som[which(tab_Age_4_som$Percentile < 0 | tab_Age_4_som$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_som_t <- predict(lm(somatic_t ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_som_pr <- predict(lm(somatic_pr ~ somatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_som <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_som_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_som_pr), digits = 2))
tab_Age_5_som[which(tab_Age_5_som$Percentile < 0 | tab_Age_5_som$Percentile > 100), "Percentile"] <- NA

# social #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_soc_t <- predict(lm(social_t ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_soc_pr <- predict(lm(social_pr ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_soc <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_soc_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_soc_pr), digits = 2))
tab_Age_1_soc[which(tab_Age_1_soc$Percentile < 0 | tab_Age_1_soc$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_soc_t <- predict(lm(social_t ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_soc_pr <- predict(lm(social_pr ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_soc <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_soc_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_soc_pr), digits = 2))
tab_Age_2_soc[which(tab_Age_2_soc$Percentile < 0 | tab_Age_2_soc$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_soc_t <- predict(lm(social_t ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_soc_pr <- predict(lm(social_pr ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_soc <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_soc_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_soc_pr), digits = 2))
tab_Age_3_soc[which(tab_Age_3_soc$Percentile < 0 | tab_Age_3_soc$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_soc_t <- predict(lm(social_t ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_soc_pr <- predict(lm(social_pr ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_soc <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_soc_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_soc_pr), digits = 2))
tab_Age_4_soc[which(tab_Age_4_soc$Percentile < 0 | tab_Age_4_soc$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_soc_t <- predict(lm(social_t ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_soc_pr <- predict(lm(social_pr ~ social + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_soc <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_soc_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_soc_pr), digits = 2))
tab_Age_5_soc[which(tab_Age_5_soc$Percentile < 0 | tab_Age_5_soc$Percentile > 100), "Percentile"] <- NA

# intrusive #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_intr_t <- predict(lm(intrusive_t ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_intr_pr <- predict(lm(intrusive_pr ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_intr <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_intr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_intr_pr), digits = 2))
tab_Age_1_intr[which(tab_Age_1_intr$Percentile < 0 | tab_Age_1_intr$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_intr_t <- predict(lm(intrusive_t ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_intr_pr <- predict(lm(intrusive_pr ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_intr <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_intr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_intr_pr), digits = 2))
tab_Age_2_intr[which(tab_Age_2_intr$Percentile < 0 | tab_Age_2_intr$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_intr_t <- predict(lm(intrusive_t ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_intr_pr <- predict(lm(intrusive_pr ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_intr <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_intr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_intr_pr), digits = 2))
tab_Age_3_intr[which(tab_Age_3_intr$Percentile < 0 | tab_Age_3_intr$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_intr_t <- predict(lm(intrusive_t ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_intr_pr <- predict(lm(intrusive_pr ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_intr <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_intr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_intr_pr), digits = 2))
tab_Age_4_intr[which(tab_Age_4_intr$Percentile < 0 | tab_Age_4_intr$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_intr_t <- predict(lm(intrusive_t ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_intr_pr <- predict(lm(intrusive_pr ~ intrusive + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_intr <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_intr_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_intr_pr), digits = 2))
tab_Age_5_intr[which(tab_Age_5_intr$Percentile < 0 | tab_Age_5_intr$Percentile > 100), "Percentile"] <- NA

# traumatic #
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_traum_t <- predict(lm(traumatic_t ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_traum_pr <- predict(lm(traumatic_pr ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_traum <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_traum_pr), digits = 2))
tab_Age_1_traum[which(tab_Age_1_traum$Percentile < 0 | tab_Age_1_traum$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_traum_t <- predict(lm(traumatic_t ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_traum_pr <- predict(lm(traumatic_pr ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_traum <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_traum_pr), digits = 2))
tab_Age_2_traum[which(tab_Age_2_traum$Percentile < 0 | tab_Age_2_traum$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_traum_t <- predict(lm(traumatic_t ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_traum_pr <- predict(lm(traumatic_pr ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_traum <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_traum_pr), digits = 2))
tab_Age_3_traum[which(tab_Age_3_traum$Percentile < 0 | tab_Age_3_traum$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_traum_t <- predict(lm(traumatic_t ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_traum_pr <- predict(lm(traumatic_pr ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_traum <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_traum_pr), digits = 2))
tab_Age_4_traum[which(tab_Age_4_traum$Percentile < 0 | tab_Age_4_traum$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_traum_t <- predict(lm(traumatic_t ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_traum_pr <- predict(lm(traumatic_pr ~ traumatic + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_traum <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_traum_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_traum_pr), digits = 2))
tab_Age_5_traum[which(tab_Age_5_traum$Percentile < 0 | tab_Age_5_traum$Percentile > 100), "Percentile"] <- NA

# avoidance
# Age Group 1 (18 - 20 years), n = 190
tab_Age_1_avoid_t <- predict(lm(avoidance_t ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 1))
tab_Age_1_avoid <- data.frame(Altersgruppe = "1 (18 bis 20 Jahre, n = 190)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_1_avoid_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_1_avoid_pr), digits = 2))
tab_Age_1_avoid[which(tab_Age_1_avoid$Percentile < 0 | tab_Age_1_avoid$Percentile > 100), "Percentile"] <- NA
# Age Group 2 (21 - 25 years), n = 414
tab_Age_2_avoid_t <- predict(lm(avoidance_t ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 2))
tab_Age_2_avoid <- data.frame(Altersgruppe = "2 (21 - 25 Jahre, n = 414)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_2_avoid_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_2_avoid_pr), digits = 2))
tab_Age_2_avoid[which(tab_Age_2_avoid$Percentile < 0 | tab_Age_2_avoid$Percentile > 100), "Percentile"] <- NA
# Age Group 3 (26 - 30 years), n = 176
tab_Age_3_avoid_t <- predict(lm(avoidance_t ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 3))
tab_Age_3_avoid <- data.frame(Altersgruppe = "3 (26 - 30 Jahre, n = 176)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_3_avoid_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_3_avoid_pr), digits = 2))
tab_Age_3_avoid[which(tab_Age_3_avoid$Percentile < 0 | tab_Age_3_avoid$Percentile > 100), "Percentile"] <- NA
# Age Group 4 (31 - 40 years), n = 172
tab_Age_4_avoid_t <- predict(lm(avoidance_t ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 4))
tab_Age_4_avoid <- data.frame(Altersgruppe = "4 (31 - 40 Jahre, n = 172)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_4_avoid_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_4_avoid_pr), digits = 2))
tab_Age_4_avoid[which(tab_Age_4_avoid$Percentile < 0 | tab_Age_4_avoid$Percentile > 100), "Percentile"] <- NA
# Age Group 5 (> 40 years), n = 177
tab_Age_5_avoid_t <- predict(lm(avoidance_t ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + Altersgruppe2, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), Altersgruppe2 = 5))
tab_Age_5_avoid <- data.frame(Altersgruppe = "5 (> 40 Jahre, n = 177)", 
                             "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                             "T" = round(as.numeric(tab_Age_5_avoid_t), digits = 2),
                             "Percentile" = round(as.numeric(tab_Age_5_avoid_pr), digits = 2))
tab_Age_5_avoid[which(tab_Age_5_avoid$Percentile < 0 | tab_Age_5_avoid$Percentile > 100), "Percentile"] <- NA


tab_Age <- cbind(tab_Age_1_neur, tab_Age_2_neur, tab_Age_3_neur, tab_Age_4_neur, tab_Age_5_neur,
                 tab_Age_1_pos, tab_Age_2_pos, tab_Age_3_pos, tab_Age_4_pos, tab_Age_5_pos,
                 tab_Age_1_depr, tab_Age_2_depr, tab_Age_3_depr, tab_Age_4_depr, tab_Age_5_depr,
                 tab_Age_1_arou, tab_Age_2_arou, tab_Age_3_arou, tab_Age_4_arou, tab_Age_5_arou,
                 tab_Age_1_som, tab_Age_2_som, tab_Age_3_som, tab_Age_4_som, tab_Age_5_som,
                 tab_Age_1_soc, tab_Age_2_soc, tab_Age_3_soc, tab_Age_4_soc, tab_Age_5_soc,
                 tab_Age_1_intr, tab_Age_2_intr, tab_Age_3_intr, tab_Age_4_intr, tab_Age_5_intr,
                 tab_Age_1_traum, tab_Age_2_traum, tab_Age_3_traum, tab_Age_4_traum, tab_Age_5_traum,
                 tab_Age_1_avoid, tab_Age_2_avoid, tab_Age_3_avoid, tab_Age_4_avoid, tab_Age_5_avoid)
openxlsx::write.xlsx(tab_Age, "Normwerttabellen/Norm_Values_Age.xlsx")


#-------- . Patient vs. Community Sample --------

tab_PatComm_Norms <- alldata_medi_lv_scores %>% 
  group_by(sample) %>% 
  dplyr::select(sample, neurotic:avoidance) %>% 
  # mutate(neurotic = neurotic * 5,
  #        positive = positive * 5,
  #        depression = depression * 5,
  #        arousal = arousal * 5,
  #        somatic = somatic * 5,
  #        social = social * 5,
  #        intrusive = intrusive * 6,
  #        traumatic = traumatic * 5,
  #        avoidance = avoidance * 8) %>% 
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
openxlsx::write.xlsx(tab_PatComm_Norms, "Normwerttabellen/Descriptives_PatComm_Samples.xlsx")

overlapping::final.plot(
  list(Patient = alldata_medi_lv_scores[which(alldata_medi_lv_scores$sample == "patient"),"neurotic"], 
       Community = alldata_medi_lv_scores[which(alldata_medi_lv_scores$sample == "community"),"neurotic"]), 
  overlap(list(Patient = alldata_medi_lv_scores[which(alldata_medi_lv_scores$sample == "patient"),"neurotic"], 
               Community = alldata_medi_lv_scores[which(alldata_medi_lv_scores$sample == "community"),"neurotic"]))$OV)
# eta = 0.39 -> 39% overlap between patient and community samples

aov(arousal ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(avoidance ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(depression ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(intrusive ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(neurotic ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(positive ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(social ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(somatic ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(traumatic ~ sample, data = alldata_medi_lv_scores) %>% summary() # sig. differences


### Scale Value, T-Value, and Percentile Rank Distribution Tables Through Linear Models

# neurotic #
# Patient Sample, n = 854
tab_Pat_neur_t <- predict(lm(neurotic_t ~ neurotic + sample, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_neur_pr <- predict(lm(neurotic_pr ~ neurotic + sample, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_neur <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_neur_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_neur_pr), digits = 2))
tab_Pat_neur[which(tab_Pat_neur$Percentile < 0 | tab_Pat_neur$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_neur_t <- predict(lm(neurotic_t ~ neurotic + sample, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_neur_pr <- predict(lm(neurotic_pr ~ neurotic + sample, data = alldata_medi_lv_scores), newdata = data.frame(neurotic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_neur <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_neur_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_neur_pr), digits = 2))
tab_Comm_neur[which(tab_Comm_neur$Percentile < 0 | tab_Comm_neur$Percentile > 100), "Percentile"] <- NA

# positive #
# Patient Sample, n = 854
tab_Pat_pos_t <- predict(lm(positive_t ~ positive + sample, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_pos_pr <- predict(lm(positive_pr ~ positive + sample, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_pos <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_pos_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_pos_pr), digits = 2))
tab_Pat_pos[which(tab_Pat_pos$Percentile < 0 | tab_Pat_pos$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_pos_t <- predict(lm(positive_t ~ positive + sample, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_pos_pr <- predict(lm(positive_pr ~ positive + sample, data = alldata_medi_lv_scores), newdata = data.frame(positive = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_pos <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_pos_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_pos_pr), digits = 2))
tab_Comm_pos[which(tab_Comm_pos$Percentile < 0 | tab_Comm_pos$Percentile > 100), "Percentile"] <- NA

# depression #
# Patient Sample, n = 854
tab_Pat_depr_t <- predict(lm(depression_t ~ depression + sample, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_depr_pr <- predict(lm(depression_pr ~ depression + sample, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_depr <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_depr_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_depr_pr), digits = 2))
tab_Pat_depr[which(tab_Pat_depr$Percentile < 0 | tab_Pat_depr$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_depr_t <- predict(lm(depression_t ~ depression + sample, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_depr_pr <- predict(lm(depression_pr ~ depression + sample, data = alldata_medi_lv_scores), newdata = data.frame(depression = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_depr <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_depr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_depr_pr), digits = 2))
tab_Comm_depr[which(tab_Comm_depr$Percentile < 0 | tab_Comm_depr$Percentile > 100), "Percentile"] <- NA

# arousal #
# Patient Sample, n = 854
tab_Pat_arou_t <- predict(lm(arousal_t ~ arousal + sample, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_arou_pr <- predict(lm(arousal_pr ~ arousal + sample, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_arou <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_arou_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_arou_pr), digits = 2))
tab_Pat_arou[which(tab_Pat_arou$Percentile < 0 | tab_Pat_arou$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_arou_t <- predict(lm(arousal_t ~ arousal + sample, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_arou_pr <- predict(lm(arousal_pr ~ arousal + sample, data = alldata_medi_lv_scores), newdata = data.frame(arousal = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_arou <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_arou_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_arou_pr), digits = 2))
tab_Comm_arou[which(tab_Comm_arou$Percentile < 0 | tab_Comm_arou$Percentile > 100), "Percentile"] <- NA

# somatic #
# Patient Sample, n = 854
tab_Pat_som_t <- predict(lm(somatic_t ~ somatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_som_pr <- predict(lm(somatic_pr ~ somatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_som <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_som_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_som_pr), digits = 2))
tab_Pat_som[which(tab_Pat_som$Percentile < 0 | tab_Pat_som$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_som_t <- predict(lm(somatic_t ~ somatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_som_pr <- predict(lm(somatic_pr ~ somatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(somatic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_som <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_som_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_som_pr), digits = 2))
tab_Comm_som[which(tab_Comm_som$Percentile < 0 | tab_Comm_som$Percentile > 100), "Percentile"] <- NA

# social #
# Patient Sample, n = 854
tab_Pat_soc_t <- predict(lm(social_t ~ social + sample, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_soc_pr <- predict(lm(social_pr ~ social + sample, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_soc <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_soc_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_soc_pr), digits = 2))
tab_Pat_soc[which(tab_Pat_soc$Percentile < 0 | tab_Pat_soc$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_soc_t <- predict(lm(social_t ~ social + sample, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_soc_pr <- predict(lm(social_pr ~ social + sample, data = alldata_medi_lv_scores), newdata = data.frame(social = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_soc <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_soc_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_soc_pr), digits = 2))
tab_Comm_soc[which(tab_Comm_soc$Percentile < 0 | tab_Comm_soc$Percentile > 100), "Percentile"] <- NA

# intrusive #
# Patient Sample, n = 854
tab_Pat_intr_t <- predict(lm(intrusive_t ~ intrusive + sample, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_intr_pr <- predict(lm(intrusive_pr ~ intrusive + sample, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_intr <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_intr_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_intr_pr), digits = 2))
tab_Pat_intr[which(tab_Pat_intr$Percentile < 0 | tab_Pat_intr$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_intr_t <- predict(lm(intrusive_t ~ intrusive + sample, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_intr_pr <- predict(lm(intrusive_pr ~ intrusive + sample, data = alldata_medi_lv_scores), newdata = data.frame(intrusive = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_intr <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_intr_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_intr_pr), digits = 2))
tab_Comm_intr[which(tab_Comm_intr$Percentile < 0 | tab_Comm_intr$Percentile > 100), "Percentile"] <- NA

# traumatic #
# Patient Sample, n = 854
tab_Pat_traum_t <- predict(lm(traumatic_t ~ traumatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_traum_pr <- predict(lm(traumatic_pr ~ traumatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_traum <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_traum_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_traum_pr), digits = 2))
tab_Pat_traum[which(tab_Pat_traum$Percentile < 0 | tab_Pat_traum$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_traum_t <- predict(lm(traumatic_t ~ traumatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_traum_pr <- predict(lm(traumatic_pr ~ traumatic + sample, data = alldata_medi_lv_scores), newdata = data.frame(traumatic = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_traum <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_traum_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_traum_pr), digits = 2))
tab_Comm_traum[which(tab_Comm_traum$Percentile < 0 | tab_Comm_traum$Percentile > 100), "Percentile"] <- NA

# avoidance #
# Patient Sample, n = 854
tab_Pat_avoid_t <- predict(lm(avoidance_t ~ avoidance + sample, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + sample, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sample = "patient"))
tab_Pat_avoid <- data.frame(Sample = "Patient (n = 854)", 
                           "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                           "T" = round(as.numeric(tab_Pat_avoid_t), digits = 2),
                           "Percentile" = round(as.numeric(tab_Pat_avoid_pr), digits = 2))
tab_Pat_avoid[which(tab_Pat_avoid$Percentile < 0 | tab_Pat_avoid$Percentile > 100), "Percentile"] <- NA
# Community Sample, n = 275
tab_Comm_avoid_t <- predict(lm(avoidance_t ~ avoidance + sample, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_avoid_pr <- predict(lm(avoidance_pr ~ avoidance + sample, data = alldata_medi_lv_scores), newdata = data.frame(avoidance = seq(from = 0, to = 8, by = .1), sample = "community"))
tab_Comm_avoid <- data.frame(Sample = "Community (n = 275)", 
                            "Scale_Mean" = as.character(seq(from = 0, to = 8, by = .1)), 
                            "T" = round(as.numeric(tab_Comm_avoid_t), digits = 2),
                            "Percentile" = round(as.numeric(tab_Comm_avoid_pr), digits = 2))
tab_Comm_avoid[which(tab_Comm_avoid$Percentile < 0 | tab_Comm_avoid$Percentile > 100), "Percentile"] <- NA


tab_Sample <- cbind(tab_Pat_arou, tab_Comm_arou,
                    tab_Pat_pos, tab_Comm_pos,
                    tab_Pat_depr, tab_Comm_depr,
                    tab_Pat_arou, tab_Comm_arou,
                    tab_Pat_som, tab_Comm_som,
                    tab_Pat_soc, tab_Comm_soc,
                    tab_Pat_intr, tab_Comm_intr,
                    tab_Pat_traum, tab_Comm_traum,
                    tab_Pat_avoid, tab_Comm_avoid)
openxlsx::write.xlsx(tab_Sample, "Normwerttabellen/Norm_Values_Patient_Community_Samples.xlsx")


#-------- . Diagnoses --------

table(alldata_medi_lv_scores$GAD_yn) # 10 GAD
table(alldata_medi_lv_scores$PDA_yn) # 27 PDA
table(alldata_medi_lv_scores$SAD_yn) # 25 SAD
table(alldata_medi_lv_scores$SPEC_yn) # 12 SPEC
table(alldata_medi_lv_scores$OCD_yn) # 7 OCD
table(alldata_medi_lv_scores$PTSD_yn) # 14 PTSD
table(alldata_medi_lv_scores$DEP_yn) # 104 DEP
table(alldata_medi_lv_scores$SSD_yn) # 13 SSD

# already created in 01_Pre-Processing.R:
alldata_medi_lv_scores$Diagnosis <- NA
alldata_medi_lv_scores[which(alldata_medi_lv_scores$GAD_yn != 0),"Diagnosis"] <- "GAD"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$PDA_yn != 0),"Diagnosis"] <- "PDA"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$SAD_yn != 0),"Diagnosis"] <- "SAD"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$SPEC_yn != 0),"Diagnosis"] <- "SPEC"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$OCD_yn != 0),"Diagnosis"] <- "OCD"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$PTSD_yn != 0),"Diagnosis"] <- "PTSD"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$DEP_yn != 0),"Diagnosis"] <- "DEP"
alldata_medi_lv_scores[which(alldata_medi_lv_scores$SSD_yn != 0),"Diagnosis"] <- "SSD"
alldata_medi_lv_scores$Diagnosis <- as.factor(alldata_medi_lv_scores$Diagnosis)
# saveRDS(alldata_medi_lv_scores, file = "Data/alldata_medi_lv_scores.RDS")

forcats::fct_count(alldata_medi_lv_scores$Diagnosis) # discrepancy with the result of table() above because of multiple diagnoses per person
#   f         n
#   <fct> <int>
# 1 DEP      96
# 2 GAD       5
# 3 OCD       3
# 4 PDA      21
# 5 PTSD     10
# 6 SAD      13
# 7 SPEC      5
# 8 SSD      13
# 9 NA      963

tab_Diagnosis_Norms <- alldata_medi_lv_scores %>% 
  group_by(Diagnosis) %>% 
  dplyr::select(neurotic:avoidance) %>% 
  # mutate(neurotic = neurotic * 5,
  #        positive = positive * 5,
  #        depression = depression * 5,
  #        arousal = arousal * 5,
  #        somatic = somatic * 5,
  #        social = social * 5,
  #        intrusive = intrusive * 6,
  #        traumatic = traumatic * 5,
  #        avoidance = avoidance * 8) %>% 
  summarytools::descr(stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid"), order = "preserve") %>% 
  tb() %>% 
  within(., {mean = round(mean, digits = 2)
         sd = round(sd, digits = 2)
         min = round(min, digits = 2)
         q1 = round(q1, digits = 2)
         med = round(med, digits = 2)
         q3 = round(q3, digits = 2)
         max = round(max, digits = 2)})
openxlsx::write.xlsx(tab_Diagnosis_Norms, "Normwerttabellen/Descriptives_Diagnosis.xlsx")


aov(arousal ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(avoidance ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # n.s. differences
aov(depression ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(intrusive ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(neurotic ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(positive ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(social ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(somatic ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
aov(traumatic ~ Diagnosis, data = alldata_medi_lv_scores) %>% summary() # sig. differences
# significant differences between the selected diagnoses, but too small diagnosis subsamples to create norm value tables

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = neurotic) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = positive) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = depression) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = arousal) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = somatic) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = intrusive) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = social) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = traumatic) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

ggplot(alldata_medi_lv_scores) +
  aes(x = Diagnosis, y = avoidance) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()


