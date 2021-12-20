######## SCENARIOS ################## USING STANDARD DOSE TO FIND TRUE BETAS #######################
####################################### FROM probit_inv.Rmd calibrations ######################
pcov = rep(.5,5)# c(.5, .55, .5, 0.7, 0.3) - from paper 
pcov_5 = c(pcov, rep(.5, 5-5))
pcov_10 = c(pcov, rep(.5, 10-5))

########################################      MTDs 1 dose  APART      ###############################################
### beta scenario 1 (b): 1 covariate effect, MTDs close tg (3rd cov) 
############ ptox w X3: 0.089 *0.313* 0.644 0.890 0.981
############ ptox w/o X3: 0.012 0.079 *0.290* 0.619 0.877

betas = c(2.6935909,	-4.2822724,	-0.9211286) # intercept, dose, 3rd covar
beta_1b_5 = rep(0, 13)
beta_1b_5[c(1,3,6)] = betas

beta_1b_10 = rep(0, 23)
beta_1b_10[c(1,3,6)] = betas

########################################      MTDs 2 doses  APART      ###############################################

### beta scenario 2b: 1 covariate effect, MTDs 2 apart (3rd cov)
######## ptox w X3:  0.134 0.278 0.4739 0.676 0.836
######## ptox w/o X3:  0.017 0.055 0.140 0.288 0.486
betas = c(2.384779,	-2.609745,	-1.014550) 
beta_2_5 = rep(0, 13)
beta_2_5[c(1,3,6)] = betas

beta_2_10 = rep(0, 23)
beta_2_10[c(1,3,6)] = betas


########################################      MTDs 3 doses  APART      ###############################################
### beta scenario 3: 1 covariate effect, MTDs far apart (3rd cov)
######## ptox w X3:  0.012 0.034 0.082 0.169 0.298
######## ptox w X3:  0.170 0.300 0.463 0.632 0.779
betas = c(2.466380,	-2.151905,	-1.297255) 
beta_3_5 = rep(0, 13)
beta_3_5[c(1,3,6)] = betas

beta_3_10 = rep(0, 23)
beta_3_10[c(1,3,6)] = betas

### beta scenario 4: Zero covars matter
betas = c(1.849748,	-2.667395,	8.673617e-16, 8.014422e-16)
beta_4_5 = rep(0, 13)
beta_4_5[c(1,3,4, 6)] = betas

beta_4_10 = rep(0, 23)
beta_4_10[c(1,3,4,6)] = betas









