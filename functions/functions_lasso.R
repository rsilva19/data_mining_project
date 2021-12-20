# generate patient's covariate pattern 
gen_ptcovars = function(pcov, npts, seed = seed, t, h){ 
  # binary correlation matrix
  set.seed(seed*t*h)
  ncovs = length(pcov)
  rho = 0.7 # between x1 and x2
  m = matrix(0, nrow = ncovs, ncol = ncovs)
  diag(m) = rep(1, ncovs)
  m[1,2] = m[2,1] = rho
  covars = bindata::rmvbin(npts, margprob = pcov, bincorr = m) 
  return(as.data.frame(covars))
}

# based on fitted (predicted) probability, get recommended dose (dose level with predicted tox closest to target)
get_rec_dose = function(fit_prob, target = .30){
  prob = do.call("cbind", fit_prob)
  dose = c()
  for (m in 1:nrow(prob)){
    dose[m] = unname(which.min(rank(abs(target-prob[m,]),ties.method = "last")))
    if (sum(prob[m,]< target)== 5){  # deals with ties or all similar
      dose[m] = 5
    }
    if (sum(prob[m,]> target)== 5){
      dose[m] = 1
    }
  }
  return(dose)
}

# from data generating model - get true prob of tox and true mtd
get_ptox = function(covars, params, dose_vec, target){ 
  covars = as.numeric(covars) # unlist vector
  ptox = c()
  for (i in 1:length(dose_vec)){
    mat_B0 = matrix(c(1,0,dose_vec[i]), ncol = 3) # intercept, sd, dose
    mat_covars = matrix(covars, ncol = length(covars))
    matX = matrix(cbind(mat_B0, mat_covars, mat_covars*dose_vec[i]), ncol = 1) # mat of int, dose, cov, cov*dose
    matB = matrix(params, ncol = length(params))
    ptox[i] = 1 - pnorm(matB %*% matX)
  }
  true_mtd = which.min(rank(abs(target-ptox),ties.method = "last"))
  if (sum(ptox < target) == length(dose_vec)){ # all less than target?
    true_mtd = length(dose_vec) # max mtd
  }
  if (sum(ptox > target) == length(dose_vec)){ # all greater than target?
    true_mtd = 1 # min mtd
  }
  return(list(ptox = ptox, true_mtd = true_mtd))
}


# function gets STAGE I data - assign dose with CRM for each cohort - for N1 patients
fit_crm = function(prior, target, pcov, params, ncohort, start_dose, dose, seed, cohort_size = 3, t, h){
  # initialize
  df_list = vector("list", ncohort)
  rec_dose = start_dose
  tox = c()
  level = c()
  s = seed
  # for each cohort
  for (i in 1:ncohort){
    # get covariate pattern
    cov_data = vector("list", cohort_size)
    for(j in 1: length(cov_data)){
      #set.seed(i*j)
      cov_data[[j]] = gen_ptcovars(pcov, 1, seed = (s*j), t = t, h= h)  
    }
    cov_data = map(.x = cov_data, as.numeric) # unlist each vector
    
    # get prob tox for each covariate pattern 
    prob_tox = rep(0,cohort_size)
    true_mtd = rep(0, cohort_size)
    DT_curve = vector("list", cohort_size)
    for(k in 1:length(cov_data)){ # for each pt
      DT = get_ptox(cov_data[[k]], params, dose = dose, target) 
      DT_curve[[k]] = round(DT$ptox,3)
      true_mtd[k] = DT$true_mtd
      prob_tox[k] = DT_curve[[k]][rec_dose]
    }
    # get dlt from ptox
    tox = c(tox, rbinom(length(prob_tox), 1, prob = prob_tox))
    level = c(level, rep(rec_dose,cohort_size))
    mtd = crm(prior, target = 0.30, tox, level = level, model = "logistic", intcpt = 3)$mtd
    #prior_update = crm(prior, target = 0.30, tox, level = level, model = "logistic", intcpt = 3)$ptox # update prior
    # get next dose, no skipping 
    if(mtd > rec_dose){
      rec_dose = rec_dose + 1
    }else if (mtd == rec_dose){
      rec_dose = mtd
    } else {
      rec_dose = rec_dose - 1
    }
    # get data frame for cohort 
    df = data.frame(t(sapply(cov_data,c)))
    DT_curve_df = do.call("rbind", DT_curve)
    if (cohort_size == 1){
      df_list[[i]] = cbind(df, dlt = tox[i], lev = level[i], true_mtd = true_mtd, DT_curve_df)
    } else if (cohort_size == 3){
      df_list[[i]] = cbind(df, dlt = tox[(i+(i-1)*2):(i+((i-1)*2)+2)], lev = level[(i+(i-1)*2):(i+((i-1)*2)+2)], true_mtd = true_mtd, DT_curve_df)
    } else{
      stop("cohort size must be 1 or 3")
    }    
    s = s + 1
  }
  df_crm = do.call("rbind", df_list)
  crm_list = list(data = df_crm, prior = prior, mtd = rec_dose)
  return(crm_list)
}

inv_logit = function(nu){
  return(exp(nu)/(1+exp(nu)))
}

############################################    STAGE II    ###########################################################
########################################################################################################################


# apply lasso model
apply_lasso = function(df, model, ncov, model_dose){
  temp = df
  #colnames(df) <- sub("V", "X", colnames(temp))
  dose_x = model_dose[temp$dose]
  temp$dose_x = dose_x
  xnam <- c(paste("V", 1:ncov, sep=""), "0") 
  f =  as.formula(paste("~ dose_x +", paste(xnam, collapse= "+")))
  x = model.matrix(f, temp)
  #print(predict(model, x))
  return(predict(model, x))
}

# fit lasso function
fit_lasso = function(df, model_dose, ncov, seed, t, h){
  set.seed(seed*h*t)
  temp = df
  dose_x = model_dose[temp$lev] # get actual dose from index 
  temp$dose_x = dose_x
  xnam <- c(paste("X", 1:ncov, sep=""), "0")
  f =  as.formula(paste("dlt ~ dose_x +", paste(xnam, collapse= "+"))) # adds extra interc w/o "+ 0"
  x = model.matrix(f, temp)
  y = as.matrix(temp$dlt, ncol=1)
  p.fac = c(0, rep(1, ncov)) # does not penalize dose coeff (dose main effect)
  # cv of tuning param
  if(nrow(temp) <80){
    nfolds = nrow(temp)
  }else{
    nfolds = 10
  }
  cv.out <- cv.glmnet(x,y,
                     alpha=1, # lasso
                     family="binomial",type.measure = "mse", 
                     penalty.factor = p.fac,
                     nfolds= nfolds, 
                     path = TRUE) # bug fix with try()
  # if (cv.out == "try-error"){
  #   cv.out <- cv.glmnet(x,y,
  #                           alpha=1, # lasso
  #                           family="binomial",type.measure = "mse", 
  #                           #penalty.factor = p.fac,
  #                           nfolds= nfolds)
  # }
  #min value of lambda
  #print("error 161")
  lambda_min = cv.out$lambda.min
  #print("error 163")
  # fit
  lasso_mod <- glmnet(x,y, modeldardize = FALSE, alpha = 1, penalty.factor = p.fac,
                     family = "binomial", lambda = lambda_min, path = TRUE)
  # if (lasso_mod == "try-error"){
  #   lasso_mod <- glmnet(x,y, modeldardize = FALSE, alpha = 1, 
  #                    family = "binomial", lambda = lambda_min)
  # }
  return(lasso_mod)
}

stage_II = function(true_dose,model_dose, params, nnext, pcov, df_current_pts, target, seed, t, h){
  s = seed
  lasso_mod = fit_lasso(df_current_pts, model_dose, length(pcov), seed = s, t, h) 
  df_old = df_current_pts
  # get next (nnext) patients covariate patterns
  cov_patterns = gen_ptcovars(pcov, nnext, seed = s, t, h)
  
  
  ##### get recommended dose from lasso: want prediction for all doses - and choose dose which gives closest ptox to target
  dose = vector("list", length = length(model_dose))
  for( i in 1:length(model_dose)){
    dose[[i]] = rep(i, nnext)
  }
  df_list = vector("list", length(model_dose))
  for (j in 1:length(model_dose)){
    df_list[[j]] = cbind(cov_patterns, dose[[j]])
    names(df_list[[j]])[(length(pcov) + 1)] = "dose"   #**** check out 
  }
  
  fit = vector("list", length(df_list))
  for(k in 1:length(model_dose)){
    fit[[k]] = apply_lasso(df_list[[k]], lasso_mod, length(pcov), model_dose)
  }
  # convert to probabilities 
  fit_prob = map(fit, inv_logit)
  # get rec dose from lasso
  rec_dose = get_rec_dose(fit_prob)

  
  # get tox of that specific dose to get dlt: use truth
  prob_dose = rep(0, nnext)
  true_mtd = rep(0, nnext)
  DT_curve = vector("list", nnext)
  for (n in 1:nnext){
    DT = get_ptox(covars =as.numeric(cov_patterns[n,]), params, dose = true_dose,target)
    DT_curve[[n]] = round(DT$ptox, 3)
    true_mtd[n] = DT$true_mtd
    prob_dose[n] = DT_curve[[n]][rec_dose[n]]
  }
  dl_t = rbinom(nnext, 1, prob = prob_dose)
  DT_curve_df = do.call("rbind", DT_curve)
  new_cohort_df = cbind(cov_patterns, dl_t, rec_dose, true_mtd, DT_curve_df)
  names(new_cohort_df) = colnames(df_old)
  df_new = rbind(df_old, new_cohort_df)
  return(crm_list = list(data = df_new, model = lasso_mod, model_dose = model_dose))
}




##################################################################################################################
################################        full function to run simulation                   ########################
##################################################################################################################
# true_dose, stan_dose,  df_current_pts, target, seed

run_simulation_lasso = function(nnext, pcov, params, p_0, true_dose, ntrials){
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
    ##get pcs for each additional ~10 subjects
    pcs_results_obs = vector("list", length(c(seq(1,27, 3), 27)))
    index = 0
    for (g in c(seq(1,27, 3), 27)){
      index = index + 1
      if (is.list(cohort_list[[g]])){
        pcs_results_obs[[index]] = get_pcs_obsdat_lasso(cohort_list[[g]], ncov = length(pcov), params = params) 
      } else {
        skip = FALSE
        pcs_results_obs[[index]] = tryCatch(get_pcs_obsdat_llasso(cohort_list[[g+1]], ncov = length(pcov), params = params), 
                                            error = function(e) { skip <<- TRUE}) 
        if(skip) { next } 
      }
    }
    pcs_results_obs = do.call(rbind, pcs_results_obs)
    seq_sim[[h]] = crm_list
    seq_sim[[h]]$pcs_results_obs = pcs_results_obs
  }
  return(seq_sim)
}

##################################################################################################################
##########                         pcs with observed data:                          ##############################
##################################################################################################################
get_pcs_obsdat_lasso = function(list, ncov, params){
  data = list$data; model_dose = list$model_dose; lasso_mod = list$model
  data$dose_x = model_dose[data$lev]
  true_mtd = data$true_mtd
  ############ get new "pts": obs data covars ############
  new_pts_df = data[,1:ncov]
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
    names(df_list[[j]])[(ncov + 1)] = "dose"  
  }
  fit = vector("list", length(df_list))
  for(k in 1:length(model_dose)){
    colnames(df_list[[k]]) = sub("X", "V", colnames(df_list[[k]]))
    fit[[k]] = apply_lasso(df_list[[k]], lasso_mod, ncov, model_dose)
  }
  # convert to probabilities 
  fit_prob = map(fit, inv_logit)
  # get rec dose from lasso
  rec_dose = get_rec_dose(fit_prob)
  # pcs measure 
  n_correct = sum((rec_dose-true_mtd) == 0)
  pcs = n_correct/(nrow(data)) 
  return(pcs)
}
