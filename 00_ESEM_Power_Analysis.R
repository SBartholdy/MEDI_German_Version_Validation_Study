
##%######################################################%##
#                                                          #
####     Post-Hoc Power Analysis for the ESEM Model     ####
#                                                          #
##%######################################################%##

pacman::p_load(dplyr, lavaan, semPower)
fit1 <- readRDS("Data/fit1.RDS")


#---- Getting Model Parameters ----

# The parameters for the power analysis are taken from the model fit summary of the 
# ESEM validation model "fit1" (see 03_Factor_Structure_Validation.R):

fitMeasures(fit1, output = "text")
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



#---- Power Analysis Using {semPower} ----

power_result <- semPower.postHoc(
  effect = 0.03,
  effect.measure = "RMSEA",
  alpha = 0.05,
  N = 1129,
  df = 1131)

summary(power_result)
# semPower: Post hoc power analysis
# 
# F0                       1.017900    
# RMSEA                    0.030000    
# Mc                       0.601126    
# 
# df                       1131        
# Num Observations         1129        
# NCP                      1148.191    
# 
# Critical Chi-Square      1210.350    
# Alpha                    0.050000    
# Beta                     8.744928e-54
# Power (1 - Beta)         > 0.9999    
# Implied Alpha/Beta Ratio 5.717600e+51
