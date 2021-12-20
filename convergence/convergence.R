params = beta_1b_5

seq_sim = vector("list", ntrials)
cohort_list = vector("list", 27)
h = 1
  ## stage I
  seed = h*10
  crm_list = fit_crm(prior = p_0, target = 0.3, pcov = pcov, params = params, ncohort = 21, 
                     start_dose = 3, dose = true_dose, seed = seed, cohort_size = 1, t=100, h = h)
  
  ## get dose levels
  p_hat = crm_list$prior
  model_dose = (log(p_hat/(1-p_hat)) - (-3))/1
  
  for (t in 1:2490){  # 2490*3 + 21 approx = 5000 pts
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
  # get pcs for each additional ~20 subjects
  pcs_results_obs = vector("list", length(c(seq(1,2490, 10), 2490)))
  index = 0
  for (g in c(seq(1,2490, 10), 2490)){
    index = index + 1
    pcs_results_obs[[index]] = get_pcs_obsdat_lasso(cohort_list[[g]], ncov = length(pcov), params = params)
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
for (g in c(seq(1,2490, 10), 2490)){
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
cohort_list[[2000]]$model$beta
  
