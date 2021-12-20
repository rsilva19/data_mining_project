library(tidyverse)
library(bindata)
library(dfcrm)
library(glmnet)
library(R2jags)
true_dose = c(.1,.3,.5,.7,.9)
p_0 = getprior(halfwidth = 0.09, target = 0.30, nu = 3, nlevel = 5, model = "logistic", intcpt = 3)
pcov_5 = rep(0.5, 5)
pcov_10 = rep(0.5, 10)
ntrials = 500
nnext = 3

# run_simulation_lasso = function(nnext, pcov, params, p_0, true_dose, ntrials){
#   seq_sim = vector("list", ntrials)
#   cohort_list = vector("list", 27)
#   for (h in 1:ntrials){
#     ## stage I
#     seed = h*10
#     crm_list = fit_crm(prior = p_0, target = 0.3, pcov = pcov, params = params, ncohort = 21, 
#                        start_dose = 3, dose = true_dose, seed = seed, cohort_size = 1, t=100, h = h)
#     
#     ## get dose levels
#     p_hat = crm_list$prior
#     model_dose = (log(p_hat/(1-p_hat)) - (-3))/1
#     
#     for (t in 1:27){  # 27*3 + 21 approx = 100 pts
#       ## stage II
#       seed = t*h +t
#       skip = FALSE
#       cohort_list[[t]] = tryCatch(
#         stage_II(true_dose, model_dose, params, nnext, pcov, crm_list$data, target = 0.30, seed, t, h), 
#         error = function(e) { skip <<- TRUE})
#       if(skip) { next } 
#       crm_list = cohort_list[[t]]
#     }
#     if(skip) { next } 
#     # get pcs for each additional ~10 subjects
#     # pcs_results_obs = vector("list", length(c(seq(1,27, 3), 27)))
#     # index = 0
#     # for (g in c(seq(1,27, 3), 27)){
#     #   index = index + 1
#     #   pcs_results_obs[[index]] = get_pcs_obsdat_lasso(cohort_list[[g]], ncov = length(pcov), params = params)
#     # }
#     # pcs_results_obs = do.call(rbind, pcs_results_obs)
#     seq_sim[[h]] = crm_list
#     #seq_sim[[h]]$pcs_results_obs = pcs_results_obs
#   }
#   return(seq_sim)
# }

# sim_lasso_scen1_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_1b_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen1_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen1_10.Rda")
# 
# sim_lasso_scen2_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_2_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen2_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen2_10.Rda")
# 
# sim_lasso_scen4_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_4_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen4_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen4_10.Rda")
# sim_lasso_scen2_5 = run_simulation_lasso(nnext, pcov = pcov_5, params = beta_2_5, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen2_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen2_5.Rda")
# sim_lasso_scen4_5 = run_simulation_lasso(nnext, pcov = pcov_5, params = beta_4_5, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen4_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen4_5.Rda")
# sim_lasso_scen3_5 = run_simulation_lasso(nnext, pcov = pcov_5, params = beta_3_5, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen3_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen3_5.Rda")

# sim_blasso_scen1_5 = run_simulation_blasso(nnext, pcov = pcov_5, params = beta_1b_5, p_0, true_dose, ntrials = 500)
# save(sim_blasso_scen1_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/blasso/sim_blasso_scen1_5.Rda")

# 
# sim_blasso_scen2_5 = run_simulation_blasso(nnext, pcov = pcov_5, params = beta_2_5, p_0, true_dose, ntrials = 500)
# save(sim_blasso_scen2_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/blasso/sim_blasso_scen2_5.Rda")
# 
# 
# sim_blasso_scen3_5 = run_simulation_blasso(nnext, pcov = pcov_5, params = beta_3_5, p_0, true_dose, ntrials = 500)
# save(sim_blasso_scen3_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/blasso/sim_blasso_scen3_5.Rda")
# 
# sim_blasso_scen4_5 = run_simulation_blasso(nnext, pcov = pcov_5, params = beta_4_5, p_0, true_dose, ntrials = 500)
# save(sim_blasso_scen4_5,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/blasso/sim_blasso_scen4_5.Rda")


sim_lasso_scen1_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_1b_10, p_0, true_dose, ntrials = 500)
save(sim_lasso_scen1_10,
     file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen1_10_wpcs.Rda")

# sim_lasso_scen2_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_2_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen2_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen2_10_wpcs.Rda")
# 
# sim_lasso_scen3_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_3_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen3_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen3_10_wpcs.Rda")
# 
# sim_lasso_scen4_10 = run_simulation_lasso(nnext, pcov = pcov_10, params = beta_4_10, p_0, true_dose, ntrials = 500)
# save(sim_lasso_scen4_10,
#      file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/lasso/sim_lasso_scen4_10_wpcs.Rda")

