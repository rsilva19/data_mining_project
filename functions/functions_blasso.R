###### FUNCTIONS in stageII.R #########################################################################

# apply bayesian lasso
apply_jags = function(covars_mat, jags_params, dose_vec, target){
  rec_dose= rep(0, nrow(covars_mat))
  for(c in 1:length(rec_dose)){
    covars = as.numeric(covars_mat[c,]) # unlist vector
    ptox = c()
    for (i in 1:length(dose_vec)){
      nu = t(matrix(jags_params)) %*% 
        matrix(c(1, dose_vec[i], covars))
      ptox[i] = exp(nu)/(1+exp(nu))
    }
    rec_dose[c] = which.min(rank(abs(target-ptox),ties.method = "last"))
    if (sum(ptox < target) == length(dose_vec)){ # all less than target
      rec_dose[c] = 5
    }
    if (sum( ptox > target)  == length(dose_vec)){ # all greater than target
      rec_dose[c] = 1
    }
  }
  return(rec_dose)
}


# jags function
fit_jags = function(data, model_dose, pcov){
  data = data %>% mutate(tdose = model_dose[lev]) #%>% select(c(X1:X5, tdose, dlt))
  data = data[,-c((ncol(data) - (length(model_dose) + 1)) : ncol(data) -1)]        
  data.list = with(data, list(X = data[,1:length(pcov)], tdose = tdose, y = dlt, 
                              N = length(dlt),
                              h = length(pcov)))
  
  # JAGs model
  model = function(){
    for(i in 1:N){
      y[i] ~ dbern(p[i])
      logit(p[i]) <- a0 + a1*tdose[i] + inprod(X[i,], b) #+ inprod(X[i,], g)*tdose[i]
    }
    a1 ~ dgamma(.5, .5)
    a0 ~ dt(0, 10, 1) # cauchy is t dist w 1 df
    for(j in 1:h){
      b[j] ~ dnorm(0, 1/(sigma*sigma))
      # g[j] ~ dnorm(0, 1/(sigma*sigma))
    }
    #sigma ~ dexp(lambda)
    sigma ~ dexp(0.5*(lambda*lambda))
    lambda ~ dgamma(0.1, 0.1)
  }
  # specify initial param values for MCMC sampler 
  init <- function(){
    list(a0 = rnorm(1),a1 = rgamma(1, .5, .5), b = rep(0, length(pcov)), 
         sigma = runif(1), lambda = dunif(1))  # u = rep(0, nrow(data)) random effect
  }
  jags_fit <- jags(data = data.list, inits = init, parameters.to.save = c("a0", "a1", "b"), 
                   model.file = model, n.chains = 3, n.iter = 1200, n.burnin = 200, n.thin = 1, DIC = F, progress.bar = "none")
  jags_fit_mcmc <- as.mcmc(jags_fit)
  # convert mcmc list into matrix then dataframe
  jags_fit_dat <- as.data.frame(as.matrix(jags_fit_mcmc))
  summary_dat = gather(jags_fit_dat, factor_key=TRUE) %>% 
    group_by(key) %>%
    summarise(mean= mean(value), sd= sd(value))
  #return(colMeans(jags_fit_dat))
  # if(nrow(data) > n_max){
  #         shrink_params = get_shrink(jags_fit_dat)
  #         summary_dat = cbind(summary_dat, shrink_params = t(shrink_params))
  # }
  return(summary_dat)
}

# get beta coeff
shrink = function(col){
  ifelse(sum(abs(col) > sd(col))/length(col) >.5, mean(col), 0)
}

get_shrink = function(jags_fit_dat){
  non_shrink = data.frame(
    a0 = mean(jags_fit_dat$a0),
    a1 = mean(jags_fit_dat$a1))
  lasso_params = jags_fit_dat %>% 
    dplyr::select(-a0, -a1) %>% 
    map_dfr(~shrink(.))
  shrink_params = cbind(non_shrink, lasso_params)
  return(shrink_params)
}

stage_II_blasso = function(true_dose, model_dose, params, nnext, pcov, df_current_pts, target, seed, t, h){
  s = seed
  #set.seed(s)
  mod_est = fit_jags(df_current_pts, model_dose, pcov) 
  jags_params = mod_est$mean 
  df_old = df_current_pts
  # get next 5 (or 10) patients covariate patterns
  cov_patterns = gen_ptcovars(pcov, nnext, seed = s, t, h)
  rec_dose = apply_jags(cov_patterns, jags_params, model_dose, target)
  
  # get tox of that specific dose to get dlt  *** need true equation
  prob_dose = rep(0, nnext)
  true_mtd = rep(0, nnext)
  DT_curve = vector("list", nnext)
  for (m in 1:nnext){
    DT = get_ptox(covars =as.numeric(cov_patterns[m,]), params, dose = true_dose,target)
    DT_curve[[m]] = round(DT$ptox, 3)
    true_mtd[m] = DT$true_mtd
    prob_dose[m] = DT_curve[[m]][rec_dose[m]]
  }
  dl_t = rbinom(nnext, 1, prob = prob_dose)
  DT_curve_df = do.call("rbind", DT_curve)
  new_cohort_df = cbind(cov_patterns, dl_t, rec_dose, true_mtd, DT_curve_df)
  names(new_cohort_df) = colnames(df_old)
  df_new = rbind(df_old, new_cohort_df)
  return(list(data = df_new, model_params = mod_est, model_dose = model_dose))
}

# fit final model:
fit_final_jags = function(data, model_dose, pcov){
  data = data %>% mutate(tdose = model_dose[lev]) 
  data = data[,-c((ncol(data) - (length(model_dose) + 1)) : ncol(data) -1)]        
  data.list = with(data, list(X = data[,1:length(pcov)], tdose = tdose, y = dlt, 
                              N = length(dlt), 
                              h = length(pcov)))
  # JAGs model
  model = function(){
    for(i in 1:N){
      y[i] ~ dbern(p[i]) # outcome in binary indication of DLT with param p 
      logit(p[i]) <- a0 + a1*tdose[i] + inprod(b, X[i,])  # logistic model 
    }
    a1 ~ dgamma(.5, .5) # specification of priors 
    a0 ~ dt(0, 10, 1) # cauchy is t dist w 1 df
    for(j in 1:h){
      b[j] ~ dnorm(0, 1/(sigma*sigma))
      # g[j] ~ dnorm(0, 1/(sigma*sigma)) # if include interaction
    }
    #sigma ~ dexp(lambda)
    sigma ~ dexp(0.5*(lambda*lambda))
    lambda ~ dgamma(0.1, 0.1)
  }
  # specify initial param values for MCMC sampler 
  init <- function(){
    list(a0 = rnorm(1),a1 = rgamma(1, .5, .5), b = rep(0, length(pcov)), 
         sigma = runif(1), lambda = dunif(1))  # u = rep(0, nrow(data)) random effect
  }
  # 3 chains, each with 1200 iterations (first 200 discarded)
  jags_fit <- jags(data = data.list, inits = init, parameters.to.save = c("a0", "a1", "b"), 
                   model.file = model, n.chains = 3, n.iter = 1200, n.burnin = 200, n.thin = 1, DIC = F, progress.bar = "none")
  jags_fit_mcmc <- as.mcmc(jags_fit)
  # convert mcmc list into matrix then dataframe
  jags_fit_dat <- as.data.frame(as.matrix(jags_fit_mcmc))
  summary_dat = gather(jags_fit_dat, factor_key=TRUE) %>% 
    group_by(key) %>%
    summarise(mean= mean(value), sd= sd(value))
  shrink_params = get_shrink(jags_fit_dat)
  summary_dat = cbind(summary_dat, shrink_params = t(shrink_params))
  return(summary_dat)
}
# nnext, pcov, params, p_0, true_dose, ntrials
run_simulation_blasso = function(nnext, pcov, params, p_0, true_dose, ntrials){
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
        stage_II_blasso(true_dose, model_dose, params, nnext, pcov, crm_list$data, target = 0.30, seed, t, h), 
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
      fit_final_jags(full_data, model_dose, pcov), 
      error = function(e) { skip <<- TRUE})
    if(skip) { next } 
    cohort_list[[27]]$model_params = final_params
    
    # get pcs for each additional ~10 subjects
    pcs_results_obs = vector("list", length(c(seq(1,27, 3), 27)))
    index = 0
    for (g in c(seq(1,27, 3), 27)){
      index = index + 1
      if (is.list(cohort_list[[g]])){
        pcs_results_obs[[index]] = get_pcs_obsdat_blasso(cohort_list[[g]], ncov = length(pcov), params = params, target = 0.30) 
      } else {
        skip = FALSE
        pcs_results_obs[[index]] = tryCatch(get_pcs_obsdat_blasso(cohort_list[[g+1]], ncov = length(pcov), params = params, target = 0.30), 
                                            error = function(e) { skip <<- TRUE}) 
        if(skip) { next } 
      }
    }
    pcs_results_obs = do.call(rbind, pcs_results_obs)
    seq_sim[[h]] = cohort_list[[27]]
    seq_sim[[h]]$pcs_results_obs = pcs_results_obs
  }
  return(seq_sim)
}

##################################################################################################################
##########                         pcs with observed data:                          ##############################
##################################################################################################################
get_pcs_obsdat_blasso = function(list, ncov, params, target){
  data= list$data; model_dose = list$model_dose; model_params = list$model_params
  jags_params = model_params$mean
  #print(jags_params)
  true_mtd = data$true_mtd
  ############ get new "pts": obs data covars ############
  new_pts_df = data[,1:ncov]
  rec_dose = apply_jags(new_pts_df, jags_params, model_dose, target)
  ############# if predictor is sig OR chosen covars is not NULL: fit logistic ############
  ##### get recommended dose from lasso
  ### want prediction for all doses - and choose dose which gives closest ptox to target
  #dose = vector("list", length = length(model_dose))
  # for( i in 1:length(model_dose)){
  #   dose[[i]] = rep(i, nrow(data))
  # }
  # df_list = vector("list", length(model_dose))
  # for (j in 1:length(model_dose)){
  #   df_list[[j]] = cbind(new_pts_df, dose[[j]])
  #   names(df_list[[j]])[(ncov + 1)] = "lev"  
  # }
  # fit = rep(0, length(df_list))
  # for(k in 1:length(model_dose)){
  #   rec_dose[k] = apply_jags(df_list[[k]], model_params$mean, model_dose, target = 0.30)
  # }
  # pcs measure 
  n_correct = sum((rec_dose-true_mtd) == 0)
  pcs = n_correct/(nrow(data)) 
  return(pcs)
}

