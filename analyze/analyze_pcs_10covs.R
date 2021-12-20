#### LOAD: sim_lasso_scen#_10_wpcs (lists include pcs) 

#########################################################
#############          Scen 1                  ##########
#########################################################
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_lasso_scen1_10))
for (i in 1:length(sim_lasso_scen1_10)){
  temp_data_obs[[i]] = sim_lasso_scen1_10[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_lasso_scen1_10)), temp_pcs_obs)
avg_pcs_obs_lasso_10 = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_lasso_10 = sum(pcs_obs)/n())
colnames(avg_pcs_obs_lasso_10) = c("N", "pcs")

round(avg_pcs_obs_lasso_10, 3) %>% gt() 


#########################################################
#############          Scen 2                  ##########
#########################################################
sim_lasso_scen2_10 = sim_lasso_scen2_10[-which(sapply(sim_lasso_scen2_10, is.null))] # remove bug (4/500)
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_lasso_scen2_10))
for (i in 1:length(sim_lasso_scen2_10)){
  temp_data_obs[[i]] = sim_lasso_scen2_10[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_lasso_scen2_10)), temp_pcs_obs)
avg_pcs_obs_lasso_10 = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_lasso_10 = sum(pcs_obs)/n())
colnames(avg_pcs_obs_lasso_10) = c("N", "pcs")


round(avg_pcs_obs_lasso_10, 3) %>% gt() 

#########################################################
#############          Scen 3                  ##########
#########################################################
sim_lasso_scen3_10 = sim_lasso_scen3_10[-which(sapply(sim_lasso_scen3_10, is.null))] 
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_lasso_scen3_10))
for (i in 1:length(sim_lasso_scen3_10)){
  temp_data_obs[[i]] = sim_lasso_scen3_10[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_lasso_scen3_10)), temp_pcs_obs)
avg_pcs_obs_lasso_10 = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_lasso_10 = sum(pcs_obs)/n())
colnames(avg_pcs_obs_lasso_10) = c("N", "pcs")

round(avg_pcs_obs_lasso_10, 3) %>% gt() 



#########################################################
#############          Scen 4                  ##########
#########################################################
sim_lasso_scen4_10 = sim_lasso_scen4_10[-which(sapply(sim_lasso_scen4_10, is.null))] 
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_lasso_scen4_10))
for (i in 1:length(sim_lasso_scen4_10)){
  temp_data_obs[[i]] = sim_lasso_scen4_10[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_lasso_scen4_10)), temp_pcs_obs)
avg_pcs_obs_lasso_10 = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_lasso_10 = sum(pcs_obs)/n())
colnames(avg_pcs_obs_lasso_10) = c("N", "pcs")

round(avg_pcs_obs_lasso_10, 3) %>% gt() 