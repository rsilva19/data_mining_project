params = beta_1b_5

seq_sim = vector("list", ntrials)
cohort_list = vector("list", 327)
h = 1
  ## stage I
  seed = h*10
  crm_list = fit_crm(prior = p_0, target = 0.3, pcov = pcov_5, params = params, ncohort = 21, 
                     start_dose = 3, dose = true_dose, seed = seed, cohort_size = 1, t=100, h = h)
  
  ## get dose levels
  p_hat = crm_list$prior
  model_dose = (log(p_hat/(1-p_hat)) - (-3))/1
  
  for (t in 1:327){  # 327*3 + 21 approx = 4000 pts
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
  full_data = cohort_list[[327]]$data
  skip = FALSE
  final_params = tryCatch(
    fit_final_jags(full_data, model_dose, pcov_5), 
    error = function(e) { skip <<- TRUE})
  if(skip) { next } 
  cohort_list[[327]]$model_params = final_params
  
  # get pcs for each additional ~10 subjects
  pcs_results_obs = vector("list", length(c(seq(1,327, 10), 327)))
  index = 0
  for (g in c(seq(1,327, 10), 327)){
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

  
  
##########################################################
################      analyze results:   #################
##########################################################
  
# check lengths
length(cohort_list)
str(pcs_results_obs)

## convg of PCS
n_subj_convg = NULL
for (g in c(seq(1,327, 10), 327)){
  n_subj_convg = c(n_subj_convg, nrow(cohort_list[[g]]$data))
}
convg_scen = tibble(n_subj_convg, pcs_results_obs)
colnames(convg_scen) = c("n","pcs")
round(convg_scen, 3) %>% 
  kbl(caption = "") %>%
  kable_classic(full_width = F, html_font = "Cambria") 

data.frame(convg_scen) %>% 
  ggplot(aes(x =n, y = pcs)) +
  geom_point() +
  geom_line()
  
### convg of covar chosen  : does not just pick X3
cohort_list[[327]]$model_params
  
convg_scen_blasso_1 = convg_scen
