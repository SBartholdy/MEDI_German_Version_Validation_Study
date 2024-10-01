
##%######################################################%##
#                                                          #
####                 01 PRE-PROCESSING                  ####
#                                                          #
##%######################################################%##

set.seed(42)
# if(!require(pacman)){install.packages("pacman")}
pacman::p_load(dplyr, tidyr, tibble, stringr, haven, labelled, summarytools, forcats, magrittr, missForest, psych, lubridate, openxlsx)



##%######################################################%##
#                                                          #
####               . Importing Data Sets                ####
#                                                          #
##%######################################################%##

#-------- Greifswald Patient Data (ZPP) --------#
# current patient dataset from Greifswald as of: 07.05.2022
GW_data_pat <- readRDS("Data/MEDI_GW_Patienten.RDS") %>% 
  as_tibble()

#-------- Gießen Patient Data --------#
# current patient dataset from Gießen as of: 26.01.2022
GI_data_pat <- read_sav("Data/MEDI_GI_Patienten.sav") %>% 
  as_tibble()
# saveRDS(GI_data_pat, "Data/MEDI_GI_Patienten.RDS")

#-------- Greifswald Community Data --------#
# current community dataset from Greifswald as of: 23.01.2023
GW_data_comm <- read.csv("Data/medi_gesunde.csv", encoding = "UTF-8") %>% 
  as_tibble()
# saveRDS(GW_data_comm, "Data/MEDI_GW_GesundeKontrollen.RDS")

#-------- Gießen Community Data --------#
# current community dataset from Gießen as of: 28.11.2022
GI_data_comm <- read_sav("Data/MEDI_GI_GesundeKontrollen_Nov22.sav") %>% 
  as_tibble()
# saveRDS(GI_data_comm, "Data/MEDI_GI_GesundeKontrollen.RDS")



##%######################################################%##
#                                                          #
####          . Creating MEDI Item Dictionary           ####
#                                                          #
##%######################################################%##

# saving a MEDI item dictionary in a handy format, just in case we need the item wording later
# (before removing MEDI item labels and saving the pre-processed data sets)

medi_dict <- GW_data_pat %>% 
  dplyr::select(medi1:medi49) %>% 
  generate_dictionary()

MEDI_Items <- medi_dict$label
head(MEDI_Items)
# medi1 
# "Belanglose Dinge wühlen mich auf." 
# medi2 
# "Es braucht nicht viel, um mich zum Lachen zu bringen." 
# medi3 
# "Ich bin enttäuscht von mir." 
# medi4 
# "Ich erlebe Kurzatmigkeit oder Atemnot." 
# medi5 
# "Andere Menschen würden einige meiner Gedanken merkwürdig finden." 
# medi6 
# "Unerwartete Körperempfindungen machen mir Angst."

write.csv(MEDI_Items, file = "MEDI_Item-Dictionary.csv")
openxlsx::write.xlsx(as.data.frame(MEDI_Items), file = "MEDI_Item-Dictionary.xlsx")
rm(medi_dict, MEDI_Items)


##%######################################################%##
#                                                          #
####                . Cleaning Data Sets                ####
#                                                          #
##%######################################################%##

#-------- .. Checking and Cleaning Data --------

### Greifswald Patient Data (ZPP) ###

dplyr::glimpse(GW_data_pat)

### MEDI_GW_Patienten.RDS (= allData.RDS from the Greifswald Psychotherapy Navigation System, GPNS) was intentionally 
### composed including missings and must therefore still be filtered for all variables to be used in the calculation. 
### Usually, patient data from different sources is joined using the variable "ended".

### session = unique random code for each individual session
### kkcode = ID variable for patients, with multiple sessions per patient

length(unique(GW_data_pat$session)) # N = 1638 unique session codes
sum(is.na(GW_data_pat$session)) # 0 missings in session
sum(duplicated(GW_data_pat$session)) # N = 211 duplicates, but session codes should be unique
# GW_data_pat$session[which(duplicated(GW_data_pat$session))]
# GW_data_pat$kkcode[which(duplicated(GW_data_pat$session))]

# filtering only unique sessions with "ended" timestamp and complete MEDI assessments:
summary(GW_data_pat$ended) # 0 missings (NAs)
sum(!is.na(GW_data_pat$ended)) # all N = 1849 sessions were ended correctly

GW_data_pat <- GW_data_pat %>% 
  group_by(kkcode) %>% 
  fill(c(sex, age, KTprae_diagtype, KTprae_primary, KTPost_diagtype, KTPost_Diag1, KTPost_Diag2, 
         KTPost_Diag3, KTPost_Diag4, KTPost_Diag5), .direction = "downup") %>% 
  ungroup() %>% 
  filter(if_all(.cols = medi1:medi49, .fns = ~ !is.na(.x))) %>% 
  mutate(sex = labelled::to_factor(sex)) %>% 
  arrange(kkcode)

GW_data_pat$age[which(is.na(GW_data_pat$age))] <- lubridate::as_date(substr(GW_data_pat$kkcode[which(is.na(GW_data_pat$age))], 2, 7), format = "%d%m%y")
year(GW_data_pat$age[which(GW_data_pat$age > as.Date("2022-01-01"))]) <- 
  year(GW_data_pat$age[which(GW_data_pat$age > as.Date("2022-01-01"))]) - 100


# <-------- manual replacement of missing values for sex (after fill(), see above), so that not all individuals 
# with missing sex (but complete MEDI) need to be removed:
length(which(is.na(GW_data_pat$sex))) # 331

GW_kkcodes_with_NAs <- GW_data_pat %>% 
  filter(is.na(sex)) %>% 
  pull(kkcode) %>% 
  unique()

### replacing missing values from the waiting list dataset ###
thanfr <- readRDS("Data/thanfr.RDS")
thanfr_infos <- thanfr %>% 
  filter(Chiffre %in% GW_kkcodes_with_NAs) %>% 
  dplyr::select(Chiffre, Sex) %>% 
  rename(kkcode = Chiffre) %>% 
  mutate(sex = as.factor(case_when(Sex == "Herr" ~ "Männlich",
                         Sex == "Frau" | Sex == "Frau " ~ "Weiblich",
                         TRUE ~ Sex)))

for (i in thanfr_infos$kkcode){
  GW_data_pat$sex[which(GW_data_pat$kkcode == i)] <- thanfr_infos$sex[which(thanfr_infos$kkcode == i)]
  }

### replacing missing values manually ###
missings_manuell <- openxlsx::read.xlsx("Data/sex_missings_filled.xlsx", sheet = 1)

missings_manuell_infos <- missings_manuell %>% 
  filter(kkcode %in% GW_kkcodes_with_NAs) %>% 
  filter(!is.na(sex))

for (i in missings_manuell_infos$kkcode){
  GW_data_pat$sex[which(GW_data_pat$kkcode == i)] <- missings_manuell_infos$sex[which(missings_manuell_infos$kkcode == i)]
  }

rm(GW_kkcodes_with_NAs, thanfr, thanfr_infos, missings_manuell, missings_manuell_infos, i)
# -------->

length(which(is.na(GW_data_pat$sex))) # 10 sessions from individuals with missing sex -> remove

GW_data_pat <- GW_data_pat %>% 
  group_by(kkcode) %>% 
  fill(sex, .direction = "downup") %>% 
  ungroup() %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(sex)) %>% 
  arrange(kkcode)

length(unique(GW_data_pat$session)) # N = 1312 complete sessions including MEDI assessments are left
length(unique(GW_data_pat$kkcode)) # N = 771 individuals remain in the dataset
sum(is.na(GW_data_pat$kkcode)) # 0 missings in kkcode
fct_count(as_factor(GW_data_pat$kkcode)) %>% dplyr::select(n) %>% freq() # up to 11 sessions per person
#               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
#           0      7      0.90           0.90      0.90           0.90
#           1    495     63.62          64.52     63.62          64.52
#           2    151     19.41          83.93     19.41          83.93
#           3     61      7.84          91.77      7.84          91.77
#           4     27      3.47          95.24      3.47          95.24
#           5     16      2.06          97.30      2.06          97.30
#           6     13      1.67          98.97      1.67          98.97
#           7      3      0.39          99.36      0.39          99.36
#           8      2      0.26          99.61      0.26          99.61
#           9      2      0.26          99.87      0.26          99.87
#          11      1      0.13         100.00      0.13         100.00
#        <NA>      0                               0.00         100.00
#       Total    778    100.00         100.00    100.00         100.00

str(GW_data_pat$sex)
# Factor w/ 3 levels "Männlich","Weiblich",..: 2 2 2 2 2 2 2 2 2 2 ...


### Greifswald Community Data ###

dplyr::glimpse(GW_data_comm)

### X = numeric ID variable (1 - 210)
length(unique(GW_data_comm$X)) # N = 210 unique values in X

GW_data_comm <- GW_data_comm %>% 
  filter(if_all(.cols = MEDI_1:MEDI_49, .fns = ~ !is.na(.x))) %>%  # N = 210 with complete MEDI ratings
  dplyr::rename(MEDI_Fallzahl = X) # MEDI_Fallzahl = ID variable for the final dataset
GW_data_comm$MEDI_Fallzahl <- 201:410 # to avoid overlaps with MEDI_Fallzahl from GI_data_pat and GI_data_comm

str(GW_data_comm$Geschlecht)
# Factor w/ 3 levels "divers","männlich",..: 2 3 3 3 3 2 3 3 3 2 ...


### Gießen Patient Data ###

dplyr::glimpse(GI_data_pat)

### MEDI_Fallzahl = numeric ID variable (1 - 95), with multiple assessments per person named "D1_..." etc.

length(unique(GI_data_pat$MEDI_Fallzahl)) # N = 95 unique values in MEDI_Fallzahl
sum(is.na(GI_data_pat$MEDI_Fallzahl)) # 0 missings in MEDI_Fallzahl
sum(duplicated(GI_data_pat$MEDI_Fallzahl)) # 0 duplicates in MEDI_Fallzahl

# lookfor_to_long_format(look_for(GI_data_pat, "MEDI")) %>% View() # MEDI only on assessment occasion D1
GI_data_pat <- GI_data_pat %>% 
  filter(if_all(.cols = D1_MEDI_01:D1_MEDI_49, .fns = ~ !is.na(.x))) # N = 90 with complete MEDI ratings

labelled::print_labels(GI_data_pat$Geschlecht)
#   value    label
#       1 weiblich
#       2 männlich
#       3   divers
labelled::print_labels(GI_data_pat$Altersgruppe)
# value         label
# 1    18 bis 20 Jahre
# 2    21 - 25 Jahre
# 3    26 - 30 Jahre
# 4    31 - 35 Jahre
# 5    36 - 40 Jahre
# 6    41 - 45 Jahre
# 7    46 - 50 Jahre
# 8    51 - 55 Jahre
# 9    56 - 60 Jahre
# 10   61 - 65 Jahre


### Gießen Community Data ###

dplyr::glimpse(GI_data_comm)

### MEDI_Fallzahl = numeric ID variable (101 - 146), with multiple assessments per person named "D1_..." etc.
### No overlap in IDs between patient and community samples from Giessen.
length(intersect(GI_data_pat$MEDI_Fallzahl, GI_data_comm$MEDI_Fallzahl))

length(unique(GI_data_comm$MEDI_Fallzahl)) # N = 46 unique values in MEDI_Fallzahl
sum(is.na(GI_data_comm$MEDI_Fallzahl)) # 0 missings in MEDI_Fallzahl
sum(duplicated(GI_data_comm$MEDI_Fallzahl)) # 0 duplicates in MEDI_Fallzahl

# lookfor_to_long_format(look_for(GI_data_comm, "MEDI")) %>% View() # MEDI only on assessment occasion D1
GI_data_comm <- GI_data_comm %>% 
  filter(if_all(.cols = D1_MEDI_01:D1_MEDI_49, .fns = ~ !is.na(.x))) # N = 46 with complete MEDI ratings

labelled::print_labels(GI_data_comm$D1_Geschlecht)
#   value    label
#       1 weiblich
#       2 männlich
#       3   divers



#-------- .. Checking MEDI-Item Likert Scales --------

# Checking if MEDI items are equally scaled from 0 to 8 in all data sets (and correcting if not)
# Problem is present in almost all items: likert scales in GW_data_pat start with 1, but need to start with 0

# in Greifswald patient data, MEDI responses are scaled from 1 to 9 -> need to be corrected before joining with Gießen data
labelled::print_labels(GW_data_pat$medi1)
# value                                  label
# 1                                      0
# 2                                      1
# 3                                      2
# 4                                      3
# 5                                      4
# 6                                      5
# 7                                      6
# 8                                      7
# 9                                      8
# NA(s) Item was never rendered for this user.

# in Greifswald community data, MEDI responses are correctly scaled from 0 to 8
summary(GW_data_comm$MEDI_1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   2.000   2.314   4.000   8.000

# in Gießen data, MEDI responses are correctly scaled from 0 to 8
labelled::print_labels(GI_data_pat$D1_MEDI_01)
# value                                                                label
#     0             nicht charakteristisch für mich/trifft nicht auf mich zu
#     1                                                                    1
#     2 geringfügig charakteristisch für mich/trifft geringfügig auf mich zu
#     3                                                                    3
#     4             etwas charakteristisch für mich/trifft etwas auf mich zu
#     5                                                                    5
#     6               sehr charakteristisch für mich/trifft sehr auf mich zu
#     7                                                                    7
#     8          extrem charakteristisch für mich/ trifft extrem auf mich zu
labelled::print_labels(GI_data_comm$D1_MEDI_01)
# value                                                                label
#     0             nicht charakteristisch für mich/trifft nicht auf mich zu
#     1                                                                    1
#     2 geringfügig charakteristisch für mich/trifft geringfügig auf mich zu
#     3                                                                    3
#     4             etwas charakteristisch für mich/trifft etwas auf mich zu
#     5                                                                    5
#     6               sehr charakteristisch für mich/trifft sehr auf mich zu
#     7                                                                    7
#     8          extrem charakteristisch für mich/ trifft extrem auf mich zu


#-------- .. Removing, Renaming, Reformatting, And Computing Variables --------

GW_data_pat <- GW_data_pat %>% 
  dplyr::rename(bdi_suicide = bdi_suidice) %>% # typo corrected
  dplyr::select(kkcode, sex, age, type, session, sessno, ended, 
         KTprae_diagtype, KTprae_primary, 
         KTPost_diagtype, KTPost_Diag1, KTPost_Diag2, KTPost_Diag3, KTPost_Diag4, KTPost_Diag5, 
         medi1:medi, bsi1:bsi, bdi1:bdi_suicide, pds1:pds_disab, pid5bf1:pid5bf, oci1:oci, pas1:pas14) %>% 
  mutate(session = as_factor(session),
         sessno = as.numeric(sessno),
         kkcode = as_factor(kkcode),
         sex = labelled::to_factor(sex),
         ended = as.Date(ended),
         type = factor(type, levels = c("wl", "pre", "inter", "post")),
         KTprae_diagtype = labelled::to_factor(KTprae_diagtype),
         KTPost_diagtype = labelled::to_factor(KTPost_diagtype)) %>% 
  mutate(Alter = trunc((age %--% ended) / years(1)), .after = age) %>% 
  filter(Alter >= 18) %>% 
  mutate(Altersgruppe = case_when(
    Alter >= 18 & Alter <= 20 ~ 1,
    Alter >= 21 & Alter <= 25 ~ 2,
    Alter >= 26 & Alter <= 30 ~ 3,
    Alter >= 31 & Alter <= 35 ~ 4,
    Alter >= 36 & Alter <= 40 ~ 5,
    Alter >= 41 & Alter <= 45 ~ 6,
    Alter >= 46 & Alter <= 50 ~ 7,
    Alter >= 51 & Alter <= 55 ~ 8,
    Alter >= 56 & Alter <= 60 ~ 9,
    Alter >= 61 ~ 10,
    TRUE ~ Alter), .after = Alter) %>% 
  mutate(Altersgruppe2 = case_when( # creating age groups, necessary for the calculation of norm values
    Altersgruppe == 5 ~ 4,
    Altersgruppe >= 6 ~ 5,
    TRUE ~ Altersgruppe)) %>% 
  mutate(MEDI_Fallzahl = kkcode) %>% 
  mutate(across(.cols = medi1:medi49, .fns = ~ .x - 1),  # subtract 1 from MEDI item responses in GW data set
         across(.cols = bsi1:bsi53, .fns = ~ .x - 1),  # subtract 1 from BSI item responses in GW data set
         across(.cols = oci1:oci24, .fns = ~ .x - 1),  # subtract 1 from OCI item responses in GW data set
         across(.cols = bdi1:bdi21, .fns = ~ .x - 1),  # subtract 1 from BDI item responses in GW data set
         across(.cols = pds_sever6:pds_sever7, .fns = ~ .x - 1),  # subtract 1 from PDS item responses in GW data set (only the 2 items we need, not all PDS items)
         across(.cols = pas1:pas5, .fns = ~ .x - 1),  # subtract 1 from PAS item responses in GW data set
         across(.cols = pas7:pas14, .fns = ~ .x - 1), # PDS item 6 is complicated (not numeric)
         across(.cols = pid5bf1:pid5bf36, .fns = ~ .x - 1)) %>%   # subtract 1 from PID5-BF item responses in GW data set
  remove_labels()

GW_data_comm <- GW_data_comm %>% 
  mutate(MEDI_Fallzahl = as.factor(MEDI_Fallzahl),
         sessno = 2,
         type = "pre") %>% 
  mutate(type = factor(type, levels = c("wl", "pre", "inter", "post"))) %>% 
  mutate(Geschlecht = forcats::fct_recode(Geschlecht, Männlich = "männlich", Weiblich = "weiblich", Divers = "divers")) %>% 
  mutate(Geschlecht = forcats::fct_relevel(Geschlecht, "Männlich", "Weiblich", "Divers")) %>% 
  mutate(
    Alter = as.numeric(Alter),
    Altersgruppe = case_when(
      Alter >= 18 & Alter <= 20 ~ 1,
      Alter >= 21 & Alter <= 25 ~ 2,
      Alter >= 26 & Alter <= 30 ~ 3,
      Alter >= 31 & Alter <= 35 ~ 4,
      Alter >= 36 & Alter <= 40 ~ 5,
      Alter >= 41 & Alter <= 45 ~ 6,
      Alter >= 46 & Alter <= 50 ~ 7,
      Alter >= 51 & Alter <= 55 ~ 8,
      Alter >= 56 & Alter <= 60 ~ 9,
      Alter >= 61 ~ 10,
      TRUE ~ Alter)) %>% 
  mutate(Altersgruppe2 = case_when( # creating age groups, necessary for the calculation of norm values
    Altersgruppe == 5 ~ 4,
    Altersgruppe >= 6 ~ 5,
    TRUE ~ Altersgruppe))

GI_data_pat <- GI_data_pat %>% 
  dplyr::rename(Diagnose_weitere = Diagose_weitere) %>%  # typo corrected
  dplyr::select(
    MEDI_Fallzahl, Diagnose_primär, Diagnose_weitere, Status, Altersgruppe, Geschlecht,
    D1_BSI_01:D1_BSI_53,
    D1_BDI_II_01:D1_BDI_II_21,
    D1_BAI_01:D1_BAI_21,
    D1_MEDI_01:D1_MEDI_49,
    D1_PSWQD_01:D1_PSWQD_16,
    D1_PDS_I_01:D1_PDS_IV_09_Leistungsfähigkeit,
    OCI001p6:OCISUMp6,
    D2_BFIK_01:D2_BFIK_21) %>% 
  mutate(Altersgruppe = as.numeric(Altersgruppe)) %>% 
  mutate(Altersgruppe2 = case_when( # creating age groups, necessary for the calculation of norm values
    Altersgruppe == 5 ~ 4,
    Altersgruppe >= 6 ~ 5,
    TRUE ~ Altersgruppe)) %>% 
  mutate(MEDI_Fallzahl = as.factor(MEDI_Fallzahl),
         sessno = 2,
         type = "pre") %>% 
  mutate(type = factor(type, levels = c("wl", "pre", "inter", "post"))) %>% 
  mutate(Geschlecht = labelled::to_factor(Geschlecht, levels = "labels")) %>% 
  mutate(Geschlecht = forcats::fct_recode(Geschlecht, Männlich = "männlich", Weiblich = "weiblich", Divers = "divers")) %>% 
  mutate(Geschlecht = forcats::fct_relevel(Geschlecht, "Männlich", "Weiblich", "Divers")) %>% 
  remove_labels()

GI_data_comm <- GI_data_comm %>% 
  dplyr::rename(Geschlecht = D1_Geschlecht,
         Alter = D1_Jahre,
         Ausbildungsjahre = D1_Ausbildungsjahre) %>% 
  dplyr::select(
    MEDI_Fallzahl, Status, Alter, Geschlecht, Ausbildungsjahre,
    D1_BSI_01:D1_BSI_53,
    D1_BDI_II_01:D1_BDI_II_21,
    D1_BAI_01:D1_BAI_21,
    D1_MEDI_01:D1_MEDI_49,
    D1_PSWQD_01:D1_PSWQD_16,
    D2_BFIK_01:D2_BFIK_21) %>% 
  mutate(MEDI_Fallzahl = as.factor(MEDI_Fallzahl),
         sessno = 2,
         type = "pre") %>% 
  mutate(type = factor(type, levels = c("wl", "pre", "inter", "post"))) %>% 
  mutate(Geschlecht = labelled::to_factor(Geschlecht, levels = "labels")) %>% 
  mutate(Geschlecht = forcats::fct_recode(Geschlecht, Männlich = "männlich", Weiblich = "weiblich", Divers = "divers")) %>% 
  mutate(Geschlecht = forcats::fct_relevel(Geschlecht, "Männlich", "Weiblich", "Divers")) %>% 
  mutate(
    Alter = as.numeric(Alter),
    Altersgruppe = case_when(
      Alter >= 18 & Alter <= 20 ~ 1,
      Alter >= 21 & Alter <= 25 ~ 2,
      Alter >= 26 & Alter <= 30 ~ 3,
      Alter >= 31 & Alter <= 35 ~ 4,
      Alter >= 36 & Alter <= 40 ~ 5,
      Alter >= 41 & Alter <= 45 ~ 6,
      Alter >= 46 & Alter <= 50 ~ 7,
      Alter >= 51 & Alter <= 55 ~ 8,
      Alter >= 56 & Alter <= 60 ~ 9,
      Alter >= 61 ~ 10,
      TRUE ~ Alter)) %>% 
  mutate(Altersgruppe2 = case_when( # creating age groups, necessary for the calculation of norm values
    Altersgruppe == 5 ~ 4,
    Altersgruppe >= 6 ~ 5,
    TRUE ~ Altersgruppe)) %>% 
  remove_labels()



##%######################################################%##
#                                                          #
####             . Exploring Missing Values             ####
#                                                          #
##%######################################################%##

# GW_data_pat %>% summarytools::dfSummary() %>% view()
# GW_data_comm %>% summarytools::dfSummary() %>% view()
# GI_data_pat %>% summarytools::dfSummary() %>% view()
# GI_data_comm %>% summarytools::dfSummary() %>% view()

# missing values:
# -99: allowed missing
# -88: not allowed missing
GI_data_pat[GI_data_pat == -88 | GI_data_pat == "-88" | GI_data_pat == -99 | GI_data_pat == "-99"] <- NA
GI_data_comm[GI_data_comm == -88 | GI_data_comm == "-88" | GI_data_comm == -99 | GI_data_comm == "-99"] <- NA



##%######################################################%##
#                                                          #
####                . Joining Data Sets                 ####
#                                                          #
##%######################################################%##

#-------- .. Renaming Items Before Joining --------

# item-name format taken from GW_data_pat for the final dataset (e.g., "medi1" instead of "D1_MEDI_01")

# MEDI
GW_data_comm <- GW_data_comm %>% 
  rename_with(.cols = MEDI_1:MEDI_49, .fn = ~ tolower(.x)) %>%
  rename_with(.cols = medi_1:medi_49, .fn = ~ gsub("_", "", .x))
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_MEDI_01:D1_MEDI_49, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = medi_01:medi_49, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = medi1:medi_49, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D1_MEDI_01:D1_MEDI_49, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = medi_01:medi_49, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = medi1:medi_49, .fn = ~ gsub("_", "", .x))

# BSI
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_BSI_01:D1_BSI_53, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bsi_01:bsi_53, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bsi1:bsi_53, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D1_BSI_01:D1_BSI_53, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bsi_01:bsi_53, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bsi1:bsi_53, .fn = ~ gsub("_", "", .x))

# BDI
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_BDI_II_01:D1_BDI_II_21, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bdi_ii_01:bdi_ii_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bdi_ii1:bdi_ii_21, .fn = ~ gsub("_ii", "", .x)) %>%
  rename_with(.cols = bdi1:bdi_21, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D1_BDI_II_01:D1_BDI_II_21, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bdi_ii_01:bdi_ii_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bdi_ii1:bdi_ii_21, .fn = ~ gsub("_ii", "", .x)) %>%
  rename_with(.cols = bdi1:bdi_21, .fn = ~ gsub("_", "", .x))

# BAI (only in GI_data)
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_BAI_01:D1_BAI_21, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bai_01:bai_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bai1:bai_21, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D1_BAI_01:D1_BAI_21, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = bai_01:bai_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bai1:bai_21, .fn = ~ gsub("_", "", .x))

# PSWQ (only in GI_data)
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_PSWQD_01:D1_PSWQD_16, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = pswqd_01:pswqd_16, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = pswqd1:pswqd_16, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D1_PSWQD_01:D1_PSWQD_16, .fn = ~ tolower(gsub("D1_", "", .x))) %>%
  rename_with(.cols = pswqd_01:pswqd_16, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = pswqd1:pswqd_16, .fn = ~ gsub("_", "", .x))

# PDS
# we only need the avoidance scale (pds_sever6 and pds_sever7)
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D1_PDS_I_01:D1_PDS_IV_09_Leistungsfähigkeit, .fn = ~ tolower(gsub("D1_", "", .x))) %>% 
  mutate(pds_sever6 = pds_iii_06,
         pds_sever7 = pds_iii_07)

# OCI
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = OCI001p6:OCI018p6, .fn = ~ tolower(gsub("p6", "", .x))) %>%
  rename_with(.cols = oci001:oci018, .fn = ~ gsub("00", "", .x)) %>%
  rename_with(.cols = oci1:oci018, .fn = ~ gsub("oci0", "oci", .x))

# BFIK
GI_data_pat <- GI_data_pat %>% 
  rename_with(.cols = D2_BFIK_01:D2_BFIK_21, .fn = ~ tolower(gsub("D2_", "", .x))) %>%
  rename_with(.cols = bfik_01:bfik_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bfik1:bfik_21, .fn = ~ gsub("_", "", .x))
GI_data_comm <- GI_data_comm %>% 
  rename_with(.cols = D2_BFIK_01:D2_BFIK_21, .fn = ~ tolower(gsub("D2_", "", .x))) %>%
  rename_with(.cols = bfik_01:bfik_21, .fn = ~ gsub("_0", "", .x)) %>%
  rename_with(.cols = bfik1:bfik_21, .fn = ~ gsub("_", "", .x))

# Diagnoses
GW_data_pat <- GW_data_pat %>% 
  rename(Diagnose_primär = KTprae_primary)


#-------- .. Creating a Complete Data Set Including Patients and Non-Patients --------

### Adding Grouping Variables for Patient vs. Non-Patient Status and Origin Location
GW_data_pat$sample <- as_factor(rep("patient", times = nrow(GW_data_pat)))
GW_data_comm$sample <- as_factor(rep("community", times = nrow(GW_data_comm)))
GI_data_pat$sample <- as_factor(rep("patient", times = nrow(GI_data_pat)))
GI_data_comm$sample <- as_factor(rep("community", times = nrow(GI_data_comm)))

GW_data_pat$location <- as_factor(rep("Greifswald", times = nrow(GW_data_pat)))
GW_data_comm$location <- as_factor(rep("Greifswald", times = nrow(GW_data_comm)))
GI_data_pat$location <- as_factor(rep("Giessen", times = nrow(GI_data_pat)))
GI_data_comm$location <- as_factor(rep("Giessen", times = nrow(GI_data_comm)))

alldata <- bind_rows(GW_data_pat, GW_data_comm, GI_data_pat, GI_data_comm)

### Filling NA Values in Variables "sex" and "Geschlecht"
alldata$sex[which(is.na(alldata$sex))] <- alldata$Geschlecht[which(is.na(alldata$sex))]
alldata$Geschlecht[which(is.na(alldata$Geschlecht))] <- alldata$sex[which(is.na(alldata$Geschlecht))]

saveRDS(alldata, file = "Data/alldata_prelim.RDS")



##%######################################################%##
#                                                          #
####          . Aggregating Scales From Items           ####
#                                                          #
##%######################################################%##

#-------- .. MEDI --------
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

# Either (1) Creating MEDI Scales as Mean Scores:
alldata <- alldata %>% 
  mutate(neurotic = rowMeans(dplyr::select(alldata, neur), na.rm = TRUE),
         positive = rowMeans(dplyr::select(alldata, pos), na.rm = TRUE),
         depression = rowMeans(dplyr::select(alldata, depr), na.rm = TRUE),
         arousal = rowMeans(dplyr::select(alldata, arou), na.rm = TRUE),
         somatic = rowMeans(dplyr::select(alldata, som), na.rm = TRUE),
         intrusive = rowMeans(dplyr::select(alldata, intr), na.rm = TRUE),
         social = rowMeans(dplyr::select(alldata, soc), na.rm = TRUE),
         traumatic = rowMeans(dplyr::select(alldata, traum), na.rm = TRUE),
         avoidance = rowMeans(dplyr::select(alldata, avoid), na.rm = TRUE))

# ... or (2) Recoding MEDI Scales From Means to Sum Scores [Done for the Paper]:
alldata$neurotic <- alldata$neurotic * 5
alldata$positive <- alldata$positive * 5
alldata$depression <- alldata$depression * 5
alldata$arousal <- alldata$arousal * 5
alldata$somatic <- alldata$somatic * 5
alldata$social <- alldata$social * 5
alldata$intrusive <- alldata$intrusive * 6
alldata$traumatic <- alldata$traumatic * 5
alldata$avoidance <- alldata$avoidance * 8

#-------- .. PID5BF+M --------
alldata$negaff <- ((alldata$pid5bf1 + alldata$pid5bf19) + 
                     (alldata$pid5bf2 + alldata$pid5bf20) + 
                     (alldata$pid5bf3 + alldata$pid5bf21)) / 3 # = (emoLab + anxiety + sepIns) / 3
# alldata$negaffNorm <- round(50 + 10 * (alldata$negaff - 2.39) / 1.44)
alldata$detachment <- ((alldata$pid5bf4 + alldata$pid5bf22) + 
                         (alldata$pid5bf5 + alldata$pid5bf23) + 
                         (alldata$pid5bf6 + alldata$pid5bf24)) / 3 # = (withd + anhed + intAvoid) / 3
# alldata$detachmentNorm <- round(50 + 10 * (alldata$detachment - 1.2) / 1.13)
alldata$intAvoid <- (alldata$pid5bf6 + alldata$pid5bf24)
# alldata$intAvoidNorm <- round(50 + 10 * (alldata$intAvoid - 1.11) / 1.40)

#-------- .. PAS --------
alldata <- alldata %>% 
  mutate(pas = rowSums(dplyr::select(alldata, .cols = 
      c("pas1","pas2","pas3","pas4","pas5","pas7","pas8","pas9","pas10","pas11","pas12","pas13","pas14")), na.rm = TRUE))

#-------- .. OCI-R --------
alldata <- alldata %>% 
  mutate(oci_mean = rowMeans(dplyr::select(alldata, oci1:oci24), na.rm = TRUE))

#-------- .. BDI --------
alldata <- alldata %>% 
  mutate(bdi_score = rowSums(dplyr::select(alldata, bdi1:bdi21), na.rm = TRUE))

#-------- .. BSI --------
alldata <- alldata %>% 
  mutate(gsi_score = rowMeans(dplyr::select(alldata, bsi1:bsi53), na.rm = TRUE),
         dep_score = rowMeans(dplyr::select(alldata, .cols = c("bsi9","bsi16","bsi17","bsi18","bsi35","bsi50")), na.rm = TRUE),
         som_score = rowMeans(dplyr::select(alldata, .cols = c("bsi2","bsi7","bsi23","bsi29","bsi30","bsi33","bsi37")), na.rm = TRUE),
         anx_score = rowMeans(dplyr::select(alldata, .cols = c("bsi1","bsi12","bsi19","bsi38","bsi45","bsi49")), na.rm = TRUE),
         int_score = rowMeans(dplyr::select(alldata, .cols = c("bsi20","bsi21","bsi22","bsi42")), na.rm = TRUE),
         obs_score = rowMeans(dplyr::select(alldata, .cols = c("bsi5","bsi15","bsi26","bsi27","bsi32","bsi36")), na.rm = TRUE))

#-------- .. PDS --------
alldata <- alldata %>% 
  mutate(pds_total = rowSums(dplyr::select(alldata, .cols = c("pds_sever1","pds_sever2","pds_sever3","pds_sever4","pds_sever5","pds_sever6","pds_sever7","pds_sever8","pds_sever9","pds_sever10","pds_sever11","pds_sever12","pds_sever13","pds_sever14","pds_sever15","pds_sever16","pds_sever17")), na.rm = TRUE))

#-------- .. BFIK --------
alldata <- alldata %>% 
  mutate(
    bfik1r = case_when(
      bfik1 == 1 ~ 5,
      bfik1 == 2 ~ 4,
      bfik1 == 4 ~ 2,
      bfik1 == 5 ~ 1,
      TRUE ~ bfik1),
    bfik2r = case_when(
      bfik2 == 1 ~ 5,
      bfik2 == 2 ~ 4,
      bfik2 == 4 ~ 2,
      bfik2 == 5 ~ 1,
      TRUE ~ bfik2),
    bfik8r = case_when(
      bfik8 == 1 ~ 5,
      bfik8 == 2 ~ 4,
      bfik8 == 4 ~ 2,
      bfik8 == 5 ~ 1,
      TRUE ~ bfik8),
    bfik9r = case_when(
      bfik9 == 1 ~ 5,
      bfik9 == 2 ~ 4,
      bfik9 == 4 ~ 2,
      bfik9 == 5 ~ 1,
      TRUE ~ bfik9),
    bfik11r = case_when(
      bfik11 == 1 ~ 5,
      bfik11 == 2 ~ 4,
      bfik11 == 4 ~ 2,
      bfik11 == 5 ~ 1,
      TRUE ~ bfik11),
    bfik12r = case_when(
      bfik12 == 1 ~ 5,
      bfik12 == 2 ~ 4,
      bfik12 == 4 ~ 2,
      bfik12 == 5 ~ 1,
      TRUE ~ bfik12),
    bfik17r = case_when(
      bfik17 == 1 ~ 5,
      bfik17 == 2 ~ 4,
      bfik17 == 4 ~ 2,
      bfik17 == 5 ~ 1,
      TRUE ~ bfik17),
    bfik21r = case_when(
      bfik21 == 1 ~ 5,
      bfik21 == 2 ~ 4,
      bfik21 == 4 ~ 2,
      bfik21 == 5 ~ 1,
      TRUE ~ bfik21))

alldata <- alldata %>% 
  mutate(
    bfik_extraversion = rowMeans(dplyr::select(alldata, .cols = c("bfik1r","bfik6","bfik11r","bfik16")), na.rm = TRUE),
    bfik_agreeableness = rowMeans(dplyr::select(alldata, .cols = c("bfik2r","bfik7","bfik12r","bfik17r")), na.rm = TRUE),
    bfik_conscientiousness = rowMeans(dplyr::select(alldata, .cols = c("bfik3","bfik8r","bfik13","bfik18")), na.rm = TRUE),
    bfik_neuroticism = rowMeans(dplyr::select(alldata, .cols = c("bfik4","bfik9r","bfik14","bfik19")), na.rm = TRUE),
    bfik_openness = rowMeans(dplyr::select(alldata, .cols = c("bfik5","bfik10","bfik15","bfik20","bfik21r")), na.rm = TRUE))

#-------- .. PSWQ --------
alldata <- alldata %>% 
  mutate(pswqd1r = case_when(
    pswqd1 == 1 ~ 5,
    pswqd1 == 2 ~ 4,
    pswqd1 == 4 ~ 2,
    pswqd1 == 5 ~ 1,
    TRUE ~ pswqd1),
  pswqd3r = case_when(
    pswqd3 == 1 ~ 5,
    pswqd3 == 2 ~ 4,
    pswqd3 == 4 ~ 2,
    pswqd3 == 5 ~ 1,
    TRUE ~ pswqd3),
  pswqd8r = case_when(
    pswqd8 == 1 ~ 5,
    pswqd8 == 2 ~ 4,
    pswqd8 == 4 ~ 2,
    pswqd8 == 5 ~ 1,
    TRUE ~ pswqd8),
  pswqd10r = case_when(
    pswqd10 == 1 ~ 5,
    pswqd10 == 2 ~ 4,
    pswqd10 == 4 ~ 2,
    pswqd10 == 5 ~ 1,
    TRUE ~ pswqd10),
  pswqd11r = case_when(
    pswqd11 == 1 ~ 5,
    pswqd11 == 2 ~ 4,
    pswqd11 == 4 ~ 2,
    pswqd11 == 5 ~ 1,
    TRUE ~ pswqd11))

alldata <- alldata %>% 
  mutate(pswqd_total = rowSums(dplyr::select(alldata, .cols = c("pswqd1r","pswqd2","pswqd3r","pswqd4","pswqd5","pswqd6",
                                                         "pswqd7","pswqd8r","pswqd9","pswqd10r","pswqd11r","pswqd12",
                                                         "pswqd13","pswqd14","pswqd15","pswqd16")), na.rm = TRUE))

#-------- .. ICD-10 Diagnoses --------

### Translation of diagnosis codes from ICD-10 to DSM-5 (only diagnoses as reported by Rosellini & Brown, 2019):
# GAD = "F41.1"
# PD/A = panic disorder or agoraphobia = "F40.0", "F40.00", "F40.01", "F41.0", "F40.01"
# SAD = social anxiety disorder = "F40.1"
# SPEC = "F40.2"
# OCD = "F42", "F42.0", "F42.1", "F42.2", "F42.8", "F42.9"
# PTSD = "F43.1"
# DEP = major depression or persistent depressive disorder = "F32", "F32.0", "F32.1", "F32.2", "F32.3", "F32.8", "F32.9",
#       "F33", "F33.0", "F33.1", "F33.2", "F33.3", "F33.4", "F33.8", "F33.9"
# SSD = somatic symptom or illness anxiety disorder = "F45", "F45.0", "F45.1", "F45.2", "F45.3", "F45.30", "F45.31", "F45.32", "F45.33", "F45.34", "F45.37", "F45.38", "F45.39", "F45.4", "F45.40", "F45.41", "F45.8", "F45.9"

# initializing the columns with NA:
alldata <- alldata %>% 
  mutate(GAD_yn = NA_integer_,
         PDA_yn = NA_integer_,
         SAD_yn = NA_integer_,
         SPEC_yn = NA_integer_,
         OCD_yn = NA_integer_,
         PTSD_yn = NA_integer_,
         DEP_yn = NA_integer_,
         SSD_yn = NA_integer_)

# defining a helper function to simplify the recoding process:
recode_diag <- function(data, codes, col_name, primary, secondary) {
  for (code in codes) {
    data[grep(code, data[[primary]]), col_name] <- 1
    data[grep(code, data[[secondary]]), col_name] <- 1
  }
  return(data)
}

# using the helper function for each diagnosis:
alldata$Diagnose_primär

alldata <- recode_diag(alldata, c("F41.1"), "GAD_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F40.0", "F40.00", "F40.01", "F41.0"), "PDA_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F40.1"), "SAD_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F40.2"), "SPEC_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F42", "F42.0", "F42.1", "F42.2", "F42.8", "F42.9"), "OCD_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F43.1"), "PTSD_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F32", "F32.0", "F32.1", "F32.2", "F32.3", "F32.8", "F32.9", "F33", "F33.0", "F33.1", "F33.2", "F33.3", "F33.4", "F33.8", "F33.9"), 
                       "DEP_yn", "Diagnose_primär", "Diagnose_weitere")
alldata <- recode_diag(alldata, c("F45", "F45.0", "F45.1", "F45.2", "F45.3", "F45.30", "F45.31", "F45.32", "F45.33", "F45.34", "F45.37", "F45.38", "F45.39", "F45.4", 
                                  "F45.40", "F45.41", "F45.8", "F45.9"),
                       "SSD_yn", "Diagnose_primär", "Diagnose_weitere")

alldata$GAD_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$GAD_yn))] <- 0
alldata$PDA_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$PDA_yn))] <- 0
alldata$SAD_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$SAD_yn))] <- 0
alldata$SPEC_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$SPEC_yn))] <- 0
alldata$OCD_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$OCD_yn))] <- 0
alldata$PTSD_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$PTSD_yn))] <- 0
alldata$DEP_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$DEP_yn))] <- 0
alldata$SSD_yn[which(!is.na(alldata$Diagnose_primär) & is.na(alldata$SSD_yn))] <- 0

alldata$Diagnosis <- NA
alldata[which(alldata$GAD_yn != 0),"Diagnosis"] <- "GAD"
alldata[which(alldata$PDA_yn != 0),"Diagnosis"] <- "PDA"
alldata[which(alldata$SAD_yn != 0),"Diagnosis"] <- "SAD"
alldata[which(alldata$SPEC_yn != 0),"Diagnosis"] <- "SPEC"
alldata[which(alldata$OCD_yn != 0),"Diagnosis"] <- "OCD"
alldata[which(alldata$PTSD_yn != 0),"Diagnosis"] <- "PTSD"
alldata[which(alldata$DEP_yn != 0),"Diagnosis"] <- "DEP"
alldata[which(alldata$SSD_yn != 0),"Diagnosis"] <- "SSD"
alldata$Diagnosis <- as.factor(alldata$Diagnosis)



##%######################################################%##
#                                                          #
####      . Saving Complete Pre-Processed Data Set      ####
#                                                          #
##%######################################################%##


saveRDS(alldata, file = "Data/alldata.RDS")

