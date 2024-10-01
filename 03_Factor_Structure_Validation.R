
##%######################################################%##
#                                                          #
####           03 FACTOR STRUCTURE VALIDATION           ####
#                                                          #
##%######################################################%##

pacman::p_load(dplyr, tibble, psych, psychTools, openxlsx, sem, lavaan, lavaanPlot, semPlot, broom, polycor, corrplot, report, DiagrammeR)

alldata <- readRDS("Data/alldata.RDS")


#-------- .. Creating MEDI Item-Level Data Set --------

### Validation sample only, but with all variables:
alldata_val_session <- alldata %>%
  group_by(MEDI_Fallzahl) %>%
  arrange(sessno) %>%
  slice_head(n = 1) %>%
  ungroup()

saveRDS(alldata_val_session, "Data/alldata_val_session.RDS")

### Validation sample with only MEDI items:
medi_items <- alldata %>% 
  group_by(MEDI_Fallzahl) %>% 
  arrange(sessno) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  dplyr::select(medi1:medi49)

colnames(medi_items) <- paste0("MEDI_", 1:49)

### Saving the kkcodes/case numbers of the persons in the validation sample dataset:
Validation_Sample_IDs <- alldata_val_session %>% dplyr::select(MEDI_Fallzahl)

saveRDS(Validation_Sample_IDs, file = "Data/Validation_Sample_IDs.RDS")


#-------- .. Checking Correlations Between Items --------

lowerCor(medi_items)
corPlot(medi_items)

fa.parallel(medi_items) # test for the number of factors in data using parallel analysis
# > Parallel analysis suggests that the number of factors = 9 and the number of components =  8



##%######################################################%##
#                                                          #
####           . Confirmatory Factor Analysis           ####
#                                                          #
##%######################################################%##

#-------- .. Fitting a Conventional CFA Based on the Assumed Factor Structure --------

### Testing the Assumed Structure Before Fitting ESEMs

model <- "
F1 =~ medi1 + medi10 + medi16 + medi32 + medi35
F2 =~ medi2 + medi17 + medi24 + medi33 + medi36
F3 =~ medi3 + medi11 + medi25 + medi37 + medi43
F4 =~ medi4 + medi13 + medi18 + medi26 + medi44
F5 =~ medi6 + medi19 + medi28 + medi38 + medi45
F6 =~ medi5 + medi12 + medi21 + medi30 + medi40 + medi46
F7 =~ medi7 + medi14 + medi22 + medi41 + medi47
F8 =~ medi8 + medi20 + medi29 + medi39 + medi48
F9 =~ medi9 + medi15 + medi23 + medi27 + medi31 + medi34 + medi42 + medi49
"

cfa_fit <- lavaan::cfa(model, data = alldata_val_session, estimator = "MLR")
summary(cfa_fit, fit.measures = TRUE)


### Factor Analysis using psych and Target Rotation (see ESEM)

tmat <- as.matrix(openxlsx::read.xlsx("target_matrix.xlsx", sheet = 1, rowNames = TRUE))
rownames(tmat) <- paste0("MEDI_", 1:49)
tmat[tmat == 1] <- NA
# View(tmat)

### TargetQ does an oblique target rotation with elements that can be missing (NA) or numeric (e.g., 0, 1).
fm.hake <- fa(scale(medi_items), nfactors = 9, rotate = "TargetQ", Target = list(tmat), maxit = 5000)
summary(fm.hake, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# Factor analysis with Call: fa(r = scale(medi_items), nfactors = 9, rotate = "TargetQ", 
#                               Target = list(tmat), maxit = 5000)
# 
# Test of the hypothesis that 9 factors are sufficient.
# The degrees of freedom for the model is 771  and the objective function was  1.85 
# The number of observations was  1129  with Chi Square =  2045.36  with prob <  5.1e-116 
# 
# The root mean square of the residuals (RMSA) is  0.02 
# The df corrected root mean square of the residuals is  0.02 
# 
# Tucker Lewis Index of factoring reliability =  0.929
# RMSEA index =  0.038  and the 10 % confidence intervals are  0.036 0.04
# BIC =  -3374.06
# With factor correlations of 
# MR6   MR8   MR7   MR4   MR3   MR9   MR5   MR1   MR2
# MR6  1.00  0.26  0.37  0.32  0.34  0.33  0.25  0.39 -0.34
# MR8  0.26  1.00  0.48  0.36  0.32  0.44  0.40  0.33 -0.18
# MR7  0.37  0.48  1.00  0.41  0.39  0.45  0.36  0.41 -0.23
# MR4  0.32  0.36  0.41  1.00  0.34  0.42  0.46  0.30 -0.14
# MR3  0.34  0.32  0.39  0.34  1.00  0.41  0.16  0.36 -0.52
# MR9  0.33  0.44  0.45  0.42  0.41  1.00  0.41  0.44 -0.12
# MR5  0.25  0.40  0.36  0.46  0.16  0.41  1.00  0.27 -0.01
# MR1  0.39  0.33  0.41  0.30  0.36  0.44  0.27  1.00 -0.24
# MR2 -0.34 -0.18 -0.23 -0.14 -0.52 -0.12 -0.01 -0.24  1.00


### TargetT is an orthogonal target rotation function which allows for missing NA values in the target.
fm.hake.orth <- fa(medi_items, nfactors = 9, rotate = "TargetT", Target = tmat, maxit = 5000)
summary(fm.hake.orth, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# Factor analysis with Call: fa(r = medi_items, nfactors = 9, rotate = "TargetT", Target = tmat, 
#                               maxit = 5000)
# 
# Test of the hypothesis that 9 factors are sufficient.
# The degrees of freedom for the model is 771  and the objective function was  1.85 
# The number of observations was  1129  with Chi Square =  2045.36  with prob <  5.1e-116 
# 
# The root mean square of the residuals (RMSA) is  0.02 
# The df corrected root mean square of the residuals is  0.02 
# 
# Tucker Lewis Index of factoring reliability =  0.929
# RMSEA index =  0.038  and the 10 % confidence intervals are  0.036 0.04
# BIC =  -3374.06

hake.loadmat <- zapsmall(matrix(round(fm.hake$loadings, 2), nrow = 49, ncol = 9))
hake.loadmat2 <- zapsmall(matrix(round(fm.hake.orth$loadings, 2), nrow = 49, ncol = 9))
rownames(hake.loadmat2) <- rownames(hake.loadmat) <- paste0("MEDI_", 1:49)

View(hake.loadmat)
View(hake.loadmat2)



##%######################################################%##
#                                                          #
####  . Exploratory Structural Equation Models (ESEMs)  ####
#                                                          #
##%######################################################%##

### Rosellini & Brown (2019):
# Model fit was examined using the root mean squared error of approximation (RMSEA) and its test 
# of close fit (C-Fit), Tucker Lewis Index (TLI), comparative fit index (CFI), and standardized
# root-mean-square residual (SRMR). Multiple goodness-of-fit indices were evaluated to examine 
# various aspects of model fit (i.e., absolute fit, parsimonious fit, fit relative to the null 
# model, Brown, 2015). Conventional guidelines for acceptable model fit include RMSEA near or 
# below 0.06, C-Fit above .05, TLI and CFI close to or above .95, and SRMR near or below .08 
# (Hu & Bentler, 1999).


#-------- .. ESEM Based on SEM Model Using {psych} and {lavaan} --------

### ESEM Estimation Using psych::fa() and {lavaan} Syntax for Model Specification, 
# Based on a Tutorial by Tim Kaiser (2018):
#   https://tkaiser.science/ESEM-in-R-a-tutorial-e83f33d905c249aba6cd3562d6826287

tmat <- as.matrix(openxlsx::read.xlsx("target_matrix.xlsx", sheet = 1, rowNames = TRUE))
rownames(tmat) <- paste0("MEDI_", 1:49)
tmat[tmat == 1] <- NA
View(tmat)

# TargetQ does an oblique target rotation with elements that can be missing (NA) or numeric (e.g., 0, 1).
fm.hake <- fa(scale(medi_items), nfactors = 9, rotate = "TargetQ", Target = list(tmat), maxit = 5000)

# TargetT is an orthogonal target rotation function which allows for missing NA values in the target.
fm.hake.orth <- fa(medi_items, nfactors = 9, rotate = "TargetT", Target = tmat, maxit = 5000)

hake.loadmat <- zapsmall(matrix(round(fm.hake$loadings, 2), nrow = 49, ncol = 9))
hake.loadmat2 <- zapsmall(matrix(round(fm.hake.orth$loadings, 2), nrow = 49, ncol = 9))
rownames(hake.loadmat2) <- rownames(hake.loadmat) <- paste0("MEDI_", 1:49)

View(hake.loadmat)
View(hake.loadmat2)

# openxlsx::write.xlsx(as.data.frame(hake.loadmat), file = "Data/hake_loadmat_oblique.xlsx")
# openxlsx::write.xlsx(as.data.frame(hake.loadmat2), file = "Data/hake_loadmat2_orthogonal.xlsx")

terms <- vector()
terms2 <- vector()

for(i in 1:9){
  terms[i] <- 
    paste0("F",i,"=~ ", paste0(c(hake.loadmat[,i]), "*", names(hake.loadmat[,1]), collapse = "+"))
  
  terms2[i] <- 
    paste0("F",i,"=~ ", paste0(c(hake.loadmat2[,i]), "*", names(hake.loadmat2[,1]), collapse = "+"))
}

terms
# [1] "F1=~ 0.01*MEDI_1+-0.01*MEDI_2+0.05*MEDI_3+0*MEDI_4+0.13*MEDI_5+0.04*MEDI_6+0.78*MEDI_7+-0.05*MEDI_8+-0.11*MEDI_9+-0.02*MEDI_10+-0.08*MEDI_11+-0.09*MEDI_12+-0.01*MEDI_13+0.63*MEDI_14+0.31*MEDI_15+0.15*MEDI_16+0*MEDI_17+-0.01*MEDI_18+-0.14*MEDI_19+-0.05*MEDI_20+-0.08*MEDI_21+0.73*MEDI_22+0.21*MEDI_23+-0.01*MEDI_24+0.02*MEDI_25+-0.01*MEDI_26+-0.06*MEDI_27+-0.03*MEDI_28+-0.06*MEDI_29+0.07*MEDI_30+-0.01*MEDI_31+0.2*MEDI_32+-0.16*MEDI_33+0.23*MEDI_34+0.11*MEDI_35+-0.03*MEDI_36+0.03*MEDI_37+-0.05*MEDI_38+-0.01*MEDI_39+-0.04*MEDI_40+0.79*MEDI_41+0.13*MEDI_42+0.14*MEDI_43+0.08*MEDI_44+0.04*MEDI_45+0.08*MEDI_46+0.85*MEDI_47+0.1*MEDI_48+0.24*MEDI_49"  
# [2] "F2=~ -0.06*MEDI_1+-0.03*MEDI_2+0*MEDI_3+0.05*MEDI_4+-0.11*MEDI_5+-0.07*MEDI_6+-0.03*MEDI_7+0.7*MEDI_8+-0.11*MEDI_9+-0.02*MEDI_10+0.07*MEDI_11+0.2*MEDI_12+0.1*MEDI_13+0.02*MEDI_14+0.19*MEDI_15+0.04*MEDI_16+-0.05*MEDI_17+0.1*MEDI_18+-0.03*MEDI_19+0.68*MEDI_20+0.07*MEDI_21+0.01*MEDI_22+0.18*MEDI_23+-0.07*MEDI_24+-0.04*MEDI_25+0.08*MEDI_26+0.06*MEDI_27+-0.02*MEDI_28+0.79*MEDI_29+0.15*MEDI_30+0*MEDI_31+0.01*MEDI_32+0.09*MEDI_33+0.15*MEDI_34+0.18*MEDI_35+0.02*MEDI_36+0.01*MEDI_37+0.07*MEDI_38+0.71*MEDI_39+0.05*MEDI_40+-0.01*MEDI_41+0.07*MEDI_42+0.07*MEDI_43+0*MEDI_44+-0.11*MEDI_45+0*MEDI_46+-0.01*MEDI_47+0.6*MEDI_48+0.12*MEDI_49"                  
# [3] "F3=~ 0.28*MEDI_1+0.06*MEDI_2+0.16*MEDI_3+-0.03*MEDI_4+0.58*MEDI_5+0.13*MEDI_6+0.03*MEDI_7+0*MEDI_8+-0.02*MEDI_9+0.1*MEDI_10+0*MEDI_11+0.33*MEDI_12+0.08*MEDI_13+-0.12*MEDI_14+-0.05*MEDI_15+-0.08*MEDI_16+-0.06*MEDI_17+0.04*MEDI_18+-0.1*MEDI_19+0.03*MEDI_20+0.73*MEDI_21+0.09*MEDI_22+0.16*MEDI_23+0*MEDI_24+-0.01*MEDI_25+-0.05*MEDI_26+0.08*MEDI_27+0.11*MEDI_28+0.06*MEDI_29+0.5*MEDI_30+-0.02*MEDI_31+0.07*MEDI_32+0.07*MEDI_33+0.02*MEDI_34+0.04*MEDI_35+-0.06*MEDI_36+0.02*MEDI_37+-0.08*MEDI_38+0.06*MEDI_39+0.66*MEDI_40+0.05*MEDI_41+0.17*MEDI_42+0.16*MEDI_43+0.02*MEDI_44+0.22*MEDI_45+0.45*MEDI_46+0.04*MEDI_47+0.06*MEDI_48+0.03*MEDI_49"                
# [4] "F4=~ 0.13*MEDI_1+-0.01*MEDI_2+-0.01*MEDI_3+0.62*MEDI_4+-0.05*MEDI_5+0.37*MEDI_6+-0.04*MEDI_7+0.04*MEDI_8+-0.07*MEDI_9+0*MEDI_10+0.1*MEDI_11+0.06*MEDI_12+0.65*MEDI_13+0.03*MEDI_14+0.03*MEDI_15+0.1*MEDI_16+0.1*MEDI_17+0.56*MEDI_18+0.03*MEDI_19+0.1*MEDI_20+-0.05*MEDI_21+0*MEDI_22+0.11*MEDI_23+0*MEDI_24+0.08*MEDI_25+0.56*MEDI_26+0.1*MEDI_27+0.06*MEDI_28+0.07*MEDI_29+-0.04*MEDI_30+-0.06*MEDI_31+0.3*MEDI_32+0.02*MEDI_33+-0.07*MEDI_34+-0.15*MEDI_35+-0.06*MEDI_36+-0.02*MEDI_37+-0.21*MEDI_38+-0.01*MEDI_39+-0.03*MEDI_40+0.04*MEDI_41+-0.01*MEDI_42+0.01*MEDI_43+0.65*MEDI_44+0.12*MEDI_45+0.16*MEDI_46+0.02*MEDI_47+-0.02*MEDI_48+0.2*MEDI_49"               
# [5] "F5=~ 0.12*MEDI_1+0.03*MEDI_2+0.45*MEDI_3+-0.06*MEDI_4+0.04*MEDI_5+-0.14*MEDI_6+0.04*MEDI_7+-0.03*MEDI_8+0.06*MEDI_9+-0.03*MEDI_10+0.61*MEDI_11+0.11*MEDI_12+0.13*MEDI_13+0*MEDI_14+0*MEDI_15+0.2*MEDI_16+0.06*MEDI_17+-0.02*MEDI_18+0.1*MEDI_19+0.02*MEDI_20+0.05*MEDI_21+0.04*MEDI_22+0*MEDI_23+-0.1*MEDI_24+0.73*MEDI_25+0.27*MEDI_26+0.1*MEDI_27+-0.03*MEDI_28+-0.03*MEDI_29+0.08*MEDI_30+-0.05*MEDI_31+-0.05*MEDI_32+-0.2*MEDI_33+0*MEDI_34+0.18*MEDI_35+-0.11*MEDI_36+0.66*MEDI_37+-0.09*MEDI_38+0.08*MEDI_39+-0.01*MEDI_40+0.02*MEDI_41+-0.11*MEDI_42+0.44*MEDI_43+-0.06*MEDI_44+0.11*MEDI_45+0.01*MEDI_46+0.05*MEDI_47+0.01*MEDI_48+0.13*MEDI_49"                 
# [6] "F6=~ -0.01*MEDI_1+0.03*MEDI_2+0.09*MEDI_3+0.06*MEDI_4+0.08*MEDI_5+-0.02*MEDI_6+0.01*MEDI_7+0.06*MEDI_8+0.8*MEDI_9+0.06*MEDI_10+0.07*MEDI_11+0.18*MEDI_12+0.04*MEDI_13+0.08*MEDI_14+0.28*MEDI_15+0.08*MEDI_16+0.04*MEDI_17+0.07*MEDI_18+0.08*MEDI_19+0.01*MEDI_20+0.03*MEDI_21+0.11*MEDI_22+0.05*MEDI_23+-0.05*MEDI_24+0.08*MEDI_25+-0.03*MEDI_26+0.53*MEDI_27+0*MEDI_28+0.04*MEDI_29+-0.06*MEDI_30+0.78*MEDI_31+0.09*MEDI_32+0.01*MEDI_33+0.03*MEDI_34+-0.06*MEDI_35+0.1*MEDI_36+0.02*MEDI_37+0.05*MEDI_38+0.01*MEDI_39+0.03*MEDI_40+0*MEDI_41+0.27*MEDI_42+-0.02*MEDI_43+0.08*MEDI_44+0.05*MEDI_45+0.09*MEDI_46+0.03*MEDI_47+0.05*MEDI_48+0.16*MEDI_49"                 
# [7] "F7=~ -0.02*MEDI_1+-0.02*MEDI_2+-0.13*MEDI_3+0.09*MEDI_4+-0.03*MEDI_5+0.37*MEDI_6+0.01*MEDI_7+-0.02*MEDI_8+-0.06*MEDI_9+0.08*MEDI_10+-0.05*MEDI_11+-0.07*MEDI_12+0.01*MEDI_13+-0.06*MEDI_14+0.1*MEDI_15+0.11*MEDI_16+-0.08*MEDI_17+0.1*MEDI_18+0.71*MEDI_19+-0.02*MEDI_20+0.06*MEDI_21+-0.01*MEDI_22+0.05*MEDI_23+0*MEDI_24+0.07*MEDI_25+0.16*MEDI_26+0.07*MEDI_27+0.66*MEDI_28+-0.02*MEDI_29+0.11*MEDI_30+0*MEDI_31+0.07*MEDI_32+0*MEDI_33+0.22*MEDI_34+0.1*MEDI_35+0.05*MEDI_36+0.07*MEDI_37+0.71*MEDI_38+0.01*MEDI_39+0.06*MEDI_40+0*MEDI_41+0.12*MEDI_42+-0.02*MEDI_43+0.01*MEDI_44+0.37*MEDI_45+0.05*MEDI_46+-0.01*MEDI_47+0.02*MEDI_48+0.11*MEDI_49"
# [8] "F8=~ 0.47*MEDI_1+0.08*MEDI_2+0.27*MEDI_3+0.01*MEDI_4+0.12*MEDI_5+0.15*MEDI_6+-0.01*MEDI_7+0.1*MEDI_8+0.02*MEDI_9+0.57*MEDI_10+0.25*MEDI_11+0.24*MEDI_12+0.01*MEDI_13+0.14*MEDI_14+0.08*MEDI_15+0.41*MEDI_16+-0.16*MEDI_17+0.12*MEDI_18+0.07*MEDI_19+-0.05*MEDI_20+-0.01*MEDI_21+0.02*MEDI_22+-0.14*MEDI_23+0.01*MEDI_24+-0.06*MEDI_25+-0.04*MEDI_26+-0.12*MEDI_27+-0.05*MEDI_28+-0.02*MEDI_29+0.05*MEDI_30+-0.07*MEDI_31+0.34*MEDI_32+0.02*MEDI_33+0.11*MEDI_34+0.55*MEDI_35+0.23*MEDI_36+-0.02*MEDI_37+0.04*MEDI_38+-0.03*MEDI_39+0.02*MEDI_40+0.08*MEDI_41+-0.01*MEDI_42+-0.09*MEDI_43+0.11*MEDI_44+-0.02*MEDI_45+-0.05*MEDI_46+0.05*MEDI_47+0.05*MEDI_48+0.06*MEDI_49"
# [9] "F9=~ 0.01*MEDI_1+0.5*MEDI_2+-0.04*MEDI_3+-0.01*MEDI_4+-0.04*MEDI_5+-0.04*MEDI_6+-0.11*MEDI_7+-0.1*MEDI_8+0.01*MEDI_9+-0.12*MEDI_10+-0.11*MEDI_11+-0.08*MEDI_12+0.02*MEDI_13+-0.11*MEDI_14+-0.03*MEDI_15+-0.03*MEDI_16+0.76*MEDI_17+0.01*MEDI_18+0.02*MEDI_19+-0.03*MEDI_20+-0.03*MEDI_21+-0.03*MEDI_22+0.03*MEDI_23+0.8*MEDI_24+0.02*MEDI_25+0.02*MEDI_26+0.09*MEDI_27+-0.1*MEDI_28+-0.07*MEDI_29+0.07*MEDI_30+-0.02*MEDI_31+-0.1*MEDI_32+0.38*MEDI_33+0.06*MEDI_34+0.12*MEDI_35+0.4*MEDI_36+-0.15*MEDI_37+0.02*MEDI_38+0.03*MEDI_39+-0.04*MEDI_40+0*MEDI_41+0.07*MEDI_42+-0.17*MEDI_43+-0.02*MEDI_44+-0.01*MEDI_45+0.03*MEDI_46+-0.01*MEDI_47+0.07*MEDI_48+0.01*MEDI_49"
terms2
# [1] "F1=~ 0.2*MEDI_1+-0.08*MEDI_2+0.22*MEDI_3+0.1*MEDI_4+0.27*MEDI_5+0.17*MEDI_6+0.78*MEDI_7+0.08*MEDI_8+0.01*MEDI_9+0.16*MEDI_10+0.13*MEDI_11+0.12*MEDI_12+0.14*MEDI_13+0.65*MEDI_14+0.4*MEDI_15+0.3*MEDI_16+-0.19*MEDI_17+0.13*MEDI_18+-0.02*MEDI_19+0.04*MEDI_20+0.08*MEDI_21+0.76*MEDI_22+0.24*MEDI_23+-0.21*MEDI_24+0.16*MEDI_25+0.12*MEDI_26+0.04*MEDI_27+0.09*MEDI_28+0.06*MEDI_29+0.19*MEDI_30+0.08*MEDI_31+0.36*MEDI_32+-0.23*MEDI_33+0.28*MEDI_34+0.24*MEDI_35+-0.08*MEDI_36+0.18*MEDI_37+0*MEDI_38+0.08*MEDI_39+0.11*MEDI_40+0.8*MEDI_41+0.19*MEDI_42+0.26*MEDI_43+0.21*MEDI_44+0.16*MEDI_45+0.19*MEDI_46+0.85*MEDI_47+0.18*MEDI_48+0.35*MEDI_49"                      
# [2] "F2=~ 0.08*MEDI_1+-0.03*MEDI_2+0.12*MEDI_3+0.14*MEDI_4+0.05*MEDI_5+0.08*MEDI_6+0.05*MEDI_7+0.68*MEDI_8+0.03*MEDI_9+0.09*MEDI_10+0.19*MEDI_11+0.32*MEDI_12+0.21*MEDI_13+0.07*MEDI_14+0.28*MEDI_15+0.16*MEDI_16+-0.11*MEDI_17+0.22*MEDI_18+0.11*MEDI_19+0.65*MEDI_20+0.23*MEDI_21+0.11*MEDI_22+0.23*MEDI_23+-0.13*MEDI_24+0.08*MEDI_25+0.18*MEDI_26+0.19*MEDI_27+0.13*MEDI_28+0.75*MEDI_29+0.27*MEDI_30+0.12*MEDI_31+0.14*MEDI_32+0.05*MEDI_33+0.21*MEDI_34+0.25*MEDI_35+0.01*MEDI_36+0.12*MEDI_37+0.16*MEDI_38+0.68*MEDI_39+0.2*MEDI_40+0.08*MEDI_41+0.16*MEDI_42+0.16*MEDI_43+0.11*MEDI_44+0.05*MEDI_45+0.14*MEDI_46+0.08*MEDI_47+0.59*MEDI_48+0.23*MEDI_49"                  
# [3] "F3=~ 0.22*MEDI_1+-0.01*MEDI_2+0.1*MEDI_3+0.61*MEDI_4+0.09*MEDI_5+0.45*MEDI_6+0.06*MEDI_7+0.16*MEDI_8+0.07*MEDI_9+0.1*MEDI_10+0.21*MEDI_11+0.2*MEDI_12+0.67*MEDI_13+0.1*MEDI_14+0.18*MEDI_15+0.22*MEDI_16+0.03*MEDI_17+0.6*MEDI_18+0.2*MEDI_19+0.19*MEDI_20+0.11*MEDI_21+0.12*MEDI_22+0.19*MEDI_23+-0.06*MEDI_24+0.21*MEDI_25+0.6*MEDI_26+0.24*MEDI_27+0.23*MEDI_28+0.2*MEDI_29+0.12*MEDI_30+0.09*MEDI_31+0.38*MEDI_32+-0.01*MEDI_33+0.06*MEDI_34+0*MEDI_35+-0.04*MEDI_36+0.11*MEDI_37+-0.03*MEDI_38+0.13*MEDI_39+0.12*MEDI_40+0.15*MEDI_41+0.11*MEDI_42+0.12*MEDI_43+0.65*MEDI_44+0.25*MEDI_45+0.27*MEDI_46+0.14*MEDI_47+0.12*MEDI_48+0.33*MEDI_49"                          
# [4] "F4=~ 0.35*MEDI_1+0.02*MEDI_2+0.26*MEDI_3+0.09*MEDI_4+0.55*MEDI_5+0.22*MEDI_6+0.12*MEDI_7+0.17*MEDI_8+0.1*MEDI_9+0.21*MEDI_10+0.16*MEDI_11+0.43*MEDI_12+0.21*MEDI_13+0.01*MEDI_14+0.13*MEDI_15+0.1*MEDI_16+-0.13*MEDI_17+0.19*MEDI_18+0.03*MEDI_19+0.16*MEDI_20+0.68*MEDI_21+0.2*MEDI_22+0.21*MEDI_23+-0.1*MEDI_24+0.12*MEDI_25+0.1*MEDI_26+0.2*MEDI_27+0.2*MEDI_28+0.22*MEDI_29+0.51*MEDI_30+0.11*MEDI_31+0.22*MEDI_32+0.01*MEDI_33+0.12*MEDI_34+0.18*MEDI_35+-0.04*MEDI_36+0.14*MEDI_37+0.01*MEDI_38+0.2*MEDI_39+0.62*MEDI_40+0.16*MEDI_41+0.23*MEDI_42+0.24*MEDI_43+0.15*MEDI_44+0.27*MEDI_45+0.46*MEDI_46+0.15*MEDI_47+0.2*MEDI_48+0.18*MEDI_49"                          
# [5] "F5=~ 0.23*MEDI_1+-0.1*MEDI_2+0.51*MEDI_3+0.04*MEDI_4+0.17*MEDI_5+-0.02*MEDI_6+0.15*MEDI_7+0.11*MEDI_8+0.18*MEDI_9+0.13*MEDI_10+0.66*MEDI_11+0.27*MEDI_12+0.22*MEDI_13+0.12*MEDI_14+0.14*MEDI_15+0.3*MEDI_16+-0.19*MEDI_17+0.1*MEDI_18+0.11*MEDI_19+0.11*MEDI_20+0.17*MEDI_21+0.16*MEDI_22+0.06*MEDI_23+-0.35*MEDI_24+0.69*MEDI_25+0.31*MEDI_26+0.18*MEDI_27+0.03*MEDI_28+0.1*MEDI_29+0.15*MEDI_30+0.09*MEDI_31+0.12*MEDI_32+-0.28*MEDI_33+0.04*MEDI_34+0.23*MEDI_35+-0.18*MEDI_36+0.66*MEDI_37+-0.09*MEDI_38+0.16*MEDI_39+0.12*MEDI_40+0.13*MEDI_41+-0.02*MEDI_42+0.49*MEDI_43+0.08*MEDI_44+0.16*MEDI_45+0.11*MEDI_46+0.16*MEDI_47+0.09*MEDI_48+0.23*MEDI_49"                
# [6] "F6=~ 0.49*MEDI_1+0.04*MEDI_2+0.34*MEDI_3+0.09*MEDI_4+0.21*MEDI_5+0.22*MEDI_6+0.1*MEDI_7+0.19*MEDI_8+0.15*MEDI_9+0.57*MEDI_10+0.33*MEDI_11+0.34*MEDI_12+0.12*MEDI_13+0.22*MEDI_14+0.21*MEDI_15+0.45*MEDI_16+-0.22*MEDI_17+0.21*MEDI_18+0.14*MEDI_19+0.04*MEDI_20+0.11*MEDI_21+0.14*MEDI_22+-0.04*MEDI_23+-0.1*MEDI_24+0.05*MEDI_25+0.06*MEDI_26+0.03*MEDI_27+0.05*MEDI_28+0.09*MEDI_29+0.15*MEDI_30+0.08*MEDI_31+0.42*MEDI_32+-0.04*MEDI_33+0.17*MEDI_34+0.54*MEDI_35+0.17*MEDI_36+0.09*MEDI_37+0.08*MEDI_38+0.07*MEDI_39+0.13*MEDI_40+0.18*MEDI_41+0.09*MEDI_42+0.03*MEDI_43+0.2*MEDI_44+0.08*MEDI_45+0.06*MEDI_46+0.17*MEDI_47+0.14*MEDI_48+0.18*MEDI_49"
# [7] "F7=~ 0.05*MEDI_1+0.01*MEDI_2+-0.07*MEDI_3+0.18*MEDI_4+0.04*MEDI_5+0.4*MEDI_6+0.04*MEDI_7+0.08*MEDI_8+0.02*MEDI_9+0.11*MEDI_10+0*MEDI_11+0.03*MEDI_12+0.12*MEDI_13+-0.01*MEDI_14+0.17*MEDI_15+0.15*MEDI_16+-0.05*MEDI_17+0.21*MEDI_18+0.64*MEDI_19+0.08*MEDI_20+0.12*MEDI_21+0.06*MEDI_22+0.11*MEDI_23+0.01*MEDI_24+0.09*MEDI_25+0.22*MEDI_26+0.16*MEDI_27+0.61*MEDI_28+0.09*MEDI_29+0.17*MEDI_30+0.08*MEDI_31+0.15*MEDI_32+0.02*MEDI_33+0.24*MEDI_34+0.13*MEDI_35+0.07*MEDI_36+0.07*MEDI_37+0.62*MEDI_38+0.09*MEDI_39+0.12*MEDI_40+0.06*MEDI_41+0.18*MEDI_42+0.01*MEDI_43+0.12*MEDI_44+0.37*MEDI_45+0.13*MEDI_46+0.05*MEDI_47+0.11*MEDI_48+0.18*MEDI_49"                     
# [8] "F8=~ -0.05*MEDI_1+0.45*MEDI_2+-0.15*MEDI_3+0*MEDI_4+-0.08*MEDI_5+-0.01*MEDI_6+-0.17*MEDI_7+-0.11*MEDI_8+0.01*MEDI_9+-0.13*MEDI_10+-0.23*MEDI_11+-0.13*MEDI_12+-0.02*MEDI_13+-0.15*MEDI_14+-0.06*MEDI_15+-0.09*MEDI_16+0.69*MEDI_17+0*MEDI_18+0.04*MEDI_19+-0.05*MEDI_20+-0.06*MEDI_21+-0.1*MEDI_22+0.01*MEDI_23+0.76*MEDI_24+-0.12*MEDI_25+-0.04*MEDI_26+0.07*MEDI_27+-0.06*MEDI_28+-0.09*MEDI_29+0.02*MEDI_30+0.01*MEDI_31+-0.11*MEDI_32+0.39*MEDI_33+0.04*MEDI_34+0.04*MEDI_35+0.39*MEDI_36+-0.27*MEDI_37+0.07*MEDI_38+-0.02*MEDI_39+-0.06*MEDI_40+-0.08*MEDI_41+0.07*MEDI_42+-0.26*MEDI_43+-0.02*MEDI_44+-0.03*MEDI_45+0*MEDI_46+-0.09*MEDI_47+0.03*MEDI_48+-0.04*MEDI_49"
# [9] "F9=~ 0.09*MEDI_1+0.04*MEDI_2+0.15*MEDI_3+0.12*MEDI_4+0.13*MEDI_5+0.09*MEDI_6+0.06*MEDI_7+0.15*MEDI_8+0.66*MEDI_9+0.13*MEDI_10+0.15*MEDI_11+0.24*MEDI_12+0.13*MEDI_13+0.11*MEDI_14+0.31*MEDI_15+0.17*MEDI_16+0.03*MEDI_17+0.16*MEDI_18+0.17*MEDI_19+0.1*MEDI_20+0.11*MEDI_21+0.16*MEDI_22+0.1*MEDI_23+-0.05*MEDI_24+0.15*MEDI_25+0.08*MEDI_26+0.49*MEDI_27+0.1*MEDI_28+0.14*MEDI_29+0.05*MEDI_30+0.65*MEDI_31+0.17*MEDI_32+0*MEDI_33+0.1*MEDI_34+0.07*MEDI_35+0.11*MEDI_36+0.09*MEDI_37+0.12*MEDI_38+0.11*MEDI_39+0.1*MEDI_40+0.08*MEDI_41+0.27*MEDI_42+0.05*MEDI_43+0.15*MEDI_44+0.13*MEDI_45+0.15*MEDI_46+0.1*MEDI_47+0.14*MEDI_48+0.23*MEDI_49"

fit1 <- (lavaan::cfa(model = paste0(terms, collapse = "\n"), data = medi_items)) # TargetQ (oblique)
fit2 <- (lavaan::cfa(model = paste0(terms2, collapse = "\n"), data = medi_items)) # TargetT (orthogonal)

fitMeasures(fit1, output = "text") # TargetQ (oblique)
#----
# Model Test User Model:
#   
#   Test statistic                              2294.875
# Degrees of freedom                              1131
# P-value                                        0.000
# 
# Model Test Baseline Model:
#   
#   Test statistic                             29097.424
# Degrees of freedom                              1176
# P-value                                        0.000
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.958
# Tucker-Lewis Index (TLI)                       0.957
# Bentler-Bonett Non-normed Fit Index (NNFI)     0.957
# Bentler-Bonett Normed Fit Index (NFI)          0.921
# Parsimony Normed Fit Index (PNFI)              0.886
# Bollen's Relative Fit Index (RFI)              0.918
#   Bollen's Incremental Fit Index (IFI)           0.958
# Relative Noncentrality Index (RNI)             0.958
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)            -115464.560
# Loglikelihood unrestricted model (H1)    -114317.123
# 
# Akaike (AIC)                              231117.121
# Bayesian (BIC)                            231589.855
# Sample-size adjusted Bayesian (BIC)       231291.284
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.030
# 90 Percent confidence interval - lower         0.028
# 90 Percent confidence interval - upper         0.032
# P-value RMSEA <= 0.05                          1.000
# 
# Standardized Root Mean Square Residual:
#   
#   RMR                                            0.215
# RMR (No Mean)                                  0.215
# SRMR                                           0.034
# 
# Other Fit Indices:
#   
#   Hoelter Critical N (CN) alpha = 0.05         596.451
# Hoelter Critical N (CN) alpha = 0.01         613.288
# 
# Goodness of Fit Index (GFI)                    0.918
# Adjusted Goodness of Fit Index (AGFI)          0.911
# Parsimony Goodness of Fit Index (PGFI)         0.848
# 
# McDonald Fit Index (MFI)                       0.597
# 
# Expected Cross-Validation Index (ECVI)         2.199
#----
fitMeasures(fit2, output = "text") # TargetT (orthogonal)
#----
# Model Test User Model:
#   
#   Test statistic                              2297.614
# Degrees of freedom                              1131
# P-value                                        0.000
# 
# Model Test Baseline Model:
#   
#   Test statistic                             29097.424
# Degrees of freedom                              1176
# P-value                                        0.000
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.958
# Tucker-Lewis Index (TLI)                       0.957
# Bentler-Bonett Non-normed Fit Index (NNFI)     0.957
# Bentler-Bonett Normed Fit Index (NFI)          0.921
# Parsimony Normed Fit Index (PNFI)              0.886
# Bollen's Relative Fit Index (RFI)              0.918
#   Bollen's Incremental Fit Index (IFI)           0.958
# Relative Noncentrality Index (RNI)             0.958
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)            -115465.929
# Loglikelihood unrestricted model (H1)    -114317.123
# 
# Akaike (AIC)                              231119.859
# Bayesian (BIC)                            231592.593
# Sample-size adjusted Bayesian (BIC)       231294.023
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.030
# 90 Percent confidence interval - lower         0.028
# 90 Percent confidence interval - upper         0.032
# P-value RMSEA <= 0.05                          1.000
# 
# Standardized Root Mean Square Residual:
#   
#   RMR                                            0.216
# RMR (No Mean)                                  0.216
# SRMR                                           0.034
# 
# Other Fit Indices:
#   
#   Hoelter Critical N (CN) alpha = 0.05         595.741
# Hoelter Critical N (CN) alpha = 0.01         612.558
# 
# Goodness of Fit Index (GFI)                    0.918
# Adjusted Goodness of Fit Index (AGFI)          0.911
# Parsimony Goodness of Fit Index (PGFI)         0.848
# 
# McDonald Fit Index (MFI)                       0.597
# 
# Expected Cross-Validation Index (ECVI)         2.202
#----

summary(fit1)
summary(fit2)

saveRDS(fit1, "Data/fit1.RDS")
saveRDS(fit2, "Data/fit2.RDS")

fitcomp <- semTools::compareFit(fit1, fit2)
summary(fitcomp)
# ################### Nested Model Comparison #########################
# Chi-Squared Difference Test
# 
# Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# fit1 1131 231117 231590 2294.9                              
# fit2 1131 231120 231593 2297.6     2.7382       0           
# 
# ####################### Model Fit Indices ###########################
# chisq   df pvalue rmsea   cfi   tli  srmr         aic         bic
# fit1 2294.875† 1131   .000 .030† .958† .957† .034† 231117.121† 231589.855†
# fit2 2297.614  1131   .000 .030  .958  .957  .034  231119.859  231592.593 
# 
# ################## Differences in Fit Indices #######################
# df rmsea cfi tli srmr   aic   bic
# fit2 - fit1  0     0   0   0    0 2.738 2.738



#---- Standardized Model ----

fit1_std <- cfa(paste0(terms, collapse = "\n"), data = medi_items, std.lv = TRUE)
fitMeasures(fit1_std, output = "text")
#----
# Model Test User Model:
#   
#   Test statistic                             10290.854
# Degrees of freedom                              1140
# P-value                                        0.000
# 
# Model Test Baseline Model:
#   
#   Test statistic                             29097.424
# Degrees of freedom                              1176
# P-value                                        0.000
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.672
# Tucker-Lewis Index (TLI)                       0.662
# Bentler-Bonett Non-normed Fit Index (NNFI)     0.662
# Bentler-Bonett Normed Fit Index (NFI)          0.646
# Parsimony Normed Fit Index (PNFI)              0.627
# Bollen's Relative Fit Index (RFI)              0.635
#   Bollen's Incremental Fit Index (IFI)           0.673
# Relative Noncentrality Index (RNI)             0.672
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)            -119462.549
# Loglikelihood unrestricted model (H1)    -114317.123
# 
# Akaike (AIC)                              239095.099
# Bayesian (BIC)                            239522.571
# Sample-size adjusted Bayesian (BIC)       239252.587
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.084
# 90 Percent confidence interval - lower         0.083
# 90 Percent confidence interval - upper         0.086
# P-value RMSEA <= 0.05                          0.000
# 
# Standardized Root Mean Square Residual:
#   
#   RMR                                            1.611
# RMR (No Mean)                                  1.611
# SRMR                                           0.245
# 
# Other Fit Indices:
#   
#   Hoelter Critical N (CN) alpha = 0.05         134.808
# Hoelter Critical N (CN) alpha = 0.01         138.577
# 
# Goodness of Fit Index (GFI)                    0.647
# Adjusted Goodness of Fit Index (AGFI)          0.621
# Parsimony Goodness of Fit Index (PGFI)         0.602
# 
# McDonald Fit Index (MFI)                       0.017
# 
# Expected Cross-Validation Index (ECVI)         9.266
#----

summary(fit1_std)
standardizedSolution(fit1)
saveRDS(fit1_std, "Data/fit1_std.RDS")


#---- Factor Loadings ----

### Factor loadings (lambda and other estimates) for the standardized model
std_loadings <- inspect(fit1, what = "std")$lambda
View(std_loadings)

item_order <- psych::cs(1, 10, 16, 32, 35, # neurotic
                        2, 17, 24, 33, 36, # positive
                        3, 11, 25, 37, 43, # depression
                        4, 13, 18, 26, 44, # arousal
                        6, 19, 28, 38, 45, # somatic
                        7, 14, 22, 41, 47, # social
                        5, 12, 21, 30, 40, 46, # intrusive
                        8, 20, 29, 39, 48, # traumatic
                        9, 15, 23, 27, 31, 34, 42, 49) # avoidance

std_loadings <- std_loadings %>% 
  as_tibble() %>% 
  mutate(Item = row.names(.)) %>% 
  dplyr::rename(neurotic = F8,
                positive = F9,
                depression = F5,
                arousal = F4,
                somatic = F7,
                social = F1,
                intrusive = F3,
                traumatic = F2,
                avoidance = F6) %>% 
  relocate(Item, neurotic, positive, depression, arousal, somatic, social, intrusive, traumatic, avoidance)

std_loadings <- std_loadings[order(factor(std_loadings$Item, levels = item_order)),]
std_loadings <- std_loadings %>% 
  mutate(neurotic = round(neurotic, digits = 2),
         positive = round(positive, digits = 2),
         depression = round(depression, digits = 2),
         arousal = round(arousal, digits = 2),
         somatic = round(somatic, digits = 2),
         social = round(social, digits = 2),
         intrusive = round(intrusive, digits = 2),
         traumatic = round(traumatic, digits = 2),
         avoidance = round(avoidance, digits = 2))

saveRDS(std_loadings, "Data/fit1_std_loadings.RDS")
openxlsx::write.xlsx(as.data.frame(std_loadings), file = "Data/fit1_std_loadings.xlsx")


#---- Predicted Latent Variable Scores ----

# fit1_pred <- fit1_pred %>% 
#   as_tibble() %>% 
#   rename_with(.cols = MEDI_1:MEDI_49, .fn = ~ tolower(.x)) %>%
#   rename_with(.cols = medi_1:medi_49, .fn = ~ gsub("_", "", .x)) %>% 
#   rename_with(.cols = medi1:medi49, .fn = ~ paste0(.x, "_fitted"))

fit1_lv_scores <- lavPredict(fit1, newdata = medi_items, type = "lv")

fit1_lv_scores <- fit1_lv_scores %>% 
  as_tibble() %>% 
  rename(neurotic = F8,
         positive = F9,
         depression = F5,
         arousal = F4,
         somatic = F7,
         social = F1,
         intrusive = F3,
         traumatic = F2,
         avoidance = F6)

alldata_medi_lv_scores <- cbind(alldata_val_session, fit1_lv_scores)


#---- Calculating T Scores and Percentile Ranks for MEDI Scales in the Validation Data Set ----

### T Scores
alldata_medi_lv_scores <- alldata_medi_lv_scores %>% 
  mutate(
    neurotic_t = 50 + (10 * (neurotic - mean(neurotic)) / sd(neurotic)),
    positive_t = 50 + (10 * (positive - mean(positive)) / sd(positive)),
    depression_t = 50 + (10 * (depression - mean(depression)) / sd(depression)),
    arousal_t = 50 + (10 * (arousal - mean(arousal)) / sd(arousal)),
    somatic_t = 50 + (10 * (somatic - mean(somatic)) / sd(somatic)),
    intrusive_t = 50 + (10 * (intrusive - mean(intrusive)) / sd(intrusive)),
    social_t = 50 + (10 * (social - mean(social)) / sd(social)),
    traumatic_t = 50 + (10 * (traumatic - mean(traumatic)) / sd(traumatic)),
    avoidance_t = 50 + (10 * (avoidance - mean(avoidance)) / sd(avoidance)))

### Percentile Ranks
alldata_medi_lv_scores <- alldata_medi_lv_scores %>% 
  mutate(neurotic_pr = rank(neurotic) / length(neurotic) * 100,
         positive_pr = rank(positive) / length(positive) * 100,
         depression_pr = rank(depression) / length(depression) * 100,
         arousal_pr = rank(arousal) / length(arousal) * 100,
         somatic_pr = rank(somatic) / length(somatic) * 100,
         intrusive_pr = rank(intrusive) / length(intrusive) * 100,
         social_pr = rank(social) / length(social) * 100,
         traumatic_pr = rank(traumatic) / length(traumatic) * 100,
         avoidance_pr = rank(avoidance) / length(avoidance) * 100)


#---- Saving Validation Data Set ----

saveRDS(alldata_medi_lv_scores, "Data/alldata_medi_lv_scores.RDS")

