
##%######################################################%##
#                                                          #
####            02 EXPLORATORY DATA ANALYSES            ####
#                                                          #
##%######################################################%##

pacman::p_load(dplyr, forcats, descr, psych, DescTools, tableone, frequency, summarytools, 
               naniar, VIM, DataExplorer, report, naniar, skimr, gt, pander, apaTables)

alldata <- readRDS("Data/alldata.RDS")

### Validation sample that only contains pre-therapy assessments (combined from alldata_val_session 
### and latent variables, see 03_Factor_Structure_Validation.R):
alldata_medi_lv_scores <- readRDS("Data/alldata_medi_lv_scores.RDS")



##%######################################################%##
#                                                          #
####                   . Descriptives                   ####
#                                                          #
##%######################################################%##

#-------- .. Complete Sample --------

alldata %>% 
  dplyr::select(sample, location, type, ended,
                sex, Alter, Altersgruppe2,
                Diagnose_primär, Diagnosis) %>% 
  summarytools::dfSummary(varnumbers = FALSE, max.distinct.values = 100, graph.col = FALSE) # %>% view()

alldata %>% 
  dplyr::select(sample, location, type,
                sex, Altersgruppe2,
                Diagnosis, Diagnose_primär) %>% 
  summarytools::freq()

summary(alldata$ended)
summarytools::descr(alldata$Alter)

alldata_medi_lv_scores %>% 
  filter(location == "Greifswald") %>% 
  dplyr::select(sample) %>% 
  table()
# patient community 
#     764       210

alldata_medi_lv_scores %>% 
  filter(location == "Giessen") %>% 
  dplyr::select(sample) %>% 
  table()
# patient community 
#      90        65


#-------- .. Validation Sample --------

alldata_medi_lv_scores %>% 
  dplyr::select(sample, location, type,
                sex, Altersgruppe2,
                Diagnosis, Diagnose_primär) %>% 
  summarytools::freq()

summary(alldata_medi_lv_scores$ended)
summarytools::descr(alldata_medi_lv_scores$Alter)


#-------- .. Patient and Community Samples --------

### Patient Sample
# alldata_medi_lv_scores %>% 
#   filter(sample == "patient") %>% 
#   # summarytools::dfSummary() %>% view()
#   dfSummary(., varnumbers = FALSE, plain.ascii = FALSE, style = 'grid', graph.magnif = 0.75, valid.col = FALSE, na.col = FALSE, display.labels = FALSE, silent = FALSE, headers = FALSE, footnote = NA, tmp.img.dir = "/tmp") %>% 
#   print(method = 'render')

### Community Sample
# alldata_medi_lv_scores %>% 
#   filter(sample == "community") %>% 
#   # summarytools::dfSummary() %>% view()
#   dfSummary(., varnumbers = FALSE, plain.ascii = FALSE, style = 'grid', graph.magnif = 0.75, valid.col = FALSE, na.col = FALSE, display.labels = FALSE, silent = FALSE, headers = FALSE, footnote = NA, tmp.img.dir = "/tmp") %>% 
#   print(method = 'render')

### Descriptives by Sample
alldata_medi_lv_scores %>% 
  group_by(sample) %>%
  summarise(
    mNeurotic = mean(neurotic, na.rm = T),
    mPositive = mean(positive, na.rm = T),
    mDepression = mean(depression, na.rm = T),
    mArousal = mean(arousal, na.rm = T),
    mSomatic = mean(somatic, na.rm = T),
    mSocial = mean(social, na.rm = T),
    mIntrusive = mean(intrusive, na.rm = T),
    mTraumatic = mean(traumatic, na.rm = T),
    mAvoidance = mean(avoidance, na.rm = T)) %>% 
  as.data.frame()

alldata_medi_lv_scores %>% 
  group_by(sample) %>%
  summarise(
    mNeurotic = sd(neurotic, na.rm = T),
    mPositive = sd(positive, na.rm = T),
    mDepression = sd(depression, na.rm = T),
    mArousal = sd(arousal, na.rm = T),
    mSomatic = sd(somatic, na.rm = T),
    mSocial = sd(social, na.rm = T),
    mIntrusive = sd(intrusive, na.rm = T),
    mTraumatic = sd(traumatic, na.rm = T),
    mAvoidance = sd(avoidance, na.rm = T)) %>% 
  as.data.frame()

