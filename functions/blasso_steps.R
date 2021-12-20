params = beta_3_5
true_dose = c(.1,.3,.5,.7,.9)
p_0 = getprior(halfwidth = 0.09, target = 0.30, nu = 3, nlevel = 5, model = "logistic", intcpt = 3)
pcov_5 = rep(0.5, 5)
pcov_10 = rep(0.5, 10)
ntrials = 500
nnext = 3


seq_sim = vector("list", ntrials)
cohort_list = vector("list", 27)
for (h in 1:ntrials){
  ## stage I
  seed = h*10
  crm_list = fit_crm(prior = p_0, target = 0.3, pcov = pcov_5, params = params, ncohort = 21, 
                     start_dose = 3, dose = true_dose, seed = seed, cohort_size = 1, t=100, h = h)
  
  ## get dose levels
  p_hat = crm_list$prior
  model_dose = (log(p_hat/(1-p_hat)) - (-3))/1
  
  for (t in 1:27){  # 27*3 + 21 approx = 100 pts
    ## stage II
    seed = t*h +t
    skip = FALSE
    cohort_list[[t]] = tryCatch(
      stage_II_blasso(true_dose, model_dose, params, nnext, pcov = pcov_5, crm_list$data, target = 0.30, seed, t, h), 
      error = function(e) { skip <<- TRUE})
    if(skip) { next } 
    crm_list = cohort_list[[t]]
  }
  # get lasso params through shirnk fn
  # fit final model:
  if(skip) { next } 
  full_data = cohort_list[[27]]$data
  skip = FALSE
  final_params = tryCatch(
    fit_final_jags(full_data, model_dose, pcov_5), 
    error = function(e) { skip <<- TRUE})
  if(skip) { next } 
  cohort_list[[27]]$model_params = final_params
  
  # get pcs for each additional ~10 subjects
  pcs_results_obs = vector("list", length(c(seq(1,27, 3), 27)))
  index = 0
  for (g in c(seq(1,27, 3), 27)){
    index = index + 1
    if (is.list(cohort_list[[g]])){
      pcs_results_obs[[index]] = get_pcs_obsdat_blasso(cohort_list[[g]], ncov = length(pcov_5), params = params, target = 0.30) 
    } else {
      skip = FALSE
      pcs_results_obs[[index]] = tryCatch(get_pcs_obsdat_blasso(cohort_list[[g+1]], ncov = length(pcov_5), params = params, target = 0.30), 
                                          error = function(e) { skip <<- TRUE}) 
      if(skip) { next } 
    }
    # list = ifelse(is.list(cohort_list[[g]]), cohort_list[[g]], cohort_list[[g+1]])
    # list = ifelse(is.list(list), list, cohort_list[[g+1]])
    #pcs_results_obs[[index]] = get_pcs_obsdat_blasso(cohort_list[[g]], ncov = length(pcov), params = params, target = 0.30), 
  }
  pcs_results_obs = do.call(rbind, pcs_results_obs)
  seq_sim[[h]] = cohort_list[[27]]
  seq_sim[[h]]$pcs_results_obs = pcs_results_obs
}

sim_blasso_scen3_5 = seq_sim
save(sim_blasso_scen3_5,
     file = "/Users/rebeccasilva/projs_R/PK_data/sim_lasso/fall2021/lasso_proj/saved_data/blasso/sim_blasso_scen3_5_2.Rda")
# time: 4:10-6:02 (5 covariates)

