ntrials = 500
params = beta_1b_10
pcov = pcov_10

seq_sim = vector("list", ntrials)
cohort_list = vector("list", 27)
for (h in 1:ntrials){
  ## stage I
  seed = h*10
  crm_list = fit_crm(prior = p_0, target = 0.3, pcov = pcov, params = params, ncohort = 21, 
                     start_dose = 3, dose = true_dose, seed = seed, cohort_size = 1, t=100, h = h)
  
  ## get dose levels
  p_hat = crm_list$prior
  model_dose = (log(p_hat/(1-p_hat)) - (-3))/1
  
  for (t in 1:27){  # 27*3 + 21 approx = 100 pts
    ## stage II
    seed = t*h +t
    skip = FALSE
    cohort_list[[t]] = tryCatch(
      stage_II(true_dose, model_dose, params, nnext, pcov, crm_list$data, target = 0.30, seed, t, h), 
      error = function(e) { skip <<- TRUE})
    if(skip) { next } 
    crm_list = cohort_list[[t]]
  }
  if(skip) { next } 
  # get pcs for each additional ~10 subjects
  pcs_results_obs = vector("list", length(c(seq(1,27, 3), 27)))
  index = 0
  for (g in c(seq(1,27, 3), 27)){
    index = index + 1
    if (is.list(cohort_list[[g]])){
      pcs_results_obs[[index]] = get_pcs_obsdat_lasso(cohort_list[[g]], ncov = length(pcov), params = params) 
    } else {
      skip = FALSE
      pcs_results_obs[[index]] = tryCatch(get_pcs_obsdat_lasso(cohort_list[[g+1]], ncov = length(pcov), params = params), 
                                          error = function(e) { skip <<- TRUE}) 
      if(skip) { next } 
    }  
    }
  pcs_results_obs = do.call(rbind, pcs_results_obs)
  seq_sim[[h]] = crm_list
  seq_sim[[h]]$pcs_results_obs = pcs_results_obs
}


## PCS part: 
list = seq_sim[[1]]
data = list$data; model_dose = list$model_dose; lasso_mod = list$model
data$dose_x = model_dose[data$lev]
true_mtd = data$true_mtd
############ get new "pts": obs data covars ############
new_pts_df = data[,1:10]
############# if predictor is sig OR chosen covars is not NULL: fit logistic ############
##### get recommended dose from lasso
### want prediction for all doses - and choose dose which gives closest ptox to target
dose = vector("list", length = length(model_dose))
for( i in 1:length(model_dose)){
  dose[[i]] = rep(i, nrow(data))
}
df_list = vector("list", length(model_dose))
for (j in 1:length(model_dose)){
  df_list[[j]] = cbind(new_pts_df, dose[[j]])
  names(df_list[[j]])[(10 + 1)] = "dose"  
}
fit = vector("list", length(df_list))
for(k in 1:length(model_dose)){
  colnames(df_list[[k]]) = sub("X", "V", colnames(df_list[[k]]))
  fit[[k]] = apply_lasso(df_list[[k]], lasso_mod, length(pcov_10), model_dose)
}
# convert to probabilities 
fit_prob = map(fit, inv_logit)
# get rec dose from lasso
rec_dose = get_rec_dose(fit_prob)
# pcs measure 
n_correct = sum((rec_dose-true_mtd) == 0)
pcs = n_correct/(nrow(data)) 
