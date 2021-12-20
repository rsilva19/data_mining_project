
#########################################################
#############          Scen 1                  ##########
#########################################################
sim_blasso_scen1_5 = sim_blasso_scen1_5_2[-which(sapply(sim_blasso_scen1_5_2, is.null))] # remove bug (4/500)
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_blasso_scen1_5))
for (i in 1:length(sim_blasso_scen1_5)){
  temp_data_obs[[i]] = sim_blasso_scen1_5[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_blasso_scen1_5)), temp_pcs_obs)
avg_pcs_obs_blasso = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_blasso = sum(pcs_obs)/n())
colnames(avg_pcs_obs_blasso) = c("N", "pcs")

round(avg_pcs_obs_blasso, 3) %>% gt() 



#########################################################
#############          Scen 2                  ##########
#########################################################
sim_blasso_scen2_5 = sim_blasso_scen2_5[-which(sapply(sim_blasso_scen2_5, is.null))] # remove bug (4/500)
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_blasso_scen2_5))
for (i in 1:length(sim_blasso_scen2_5)){
  temp_data_obs[[i]] = sim_blasso_scen2_5[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_blasso_scen2_5)), temp_pcs_obs)
avg_pcs_obs_blasso = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_blasso = sum(pcs_obs)/n())
colnames(avg_pcs_obs_blasso) = c("N", "pcs")

round(avg_pcs_obs_blasso, 3) %>% gt() 

#########################################################
#############          Scen 3                  ##########
#########################################################
sim_blasso_scen3_5 = sim_blasso_scen3_5[-which(sapply(sim_blasso_scen3_5, is.null))] # remove bug (4/500)
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_blasso_scen3_5))
for (i in 1:length(sim_blasso_scen3_5)){
  temp_data_obs[[i]] = sim_blasso_scen3_5[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_blasso_scen3_5)), temp_pcs_obs)
avg_pcs_obs_blasso = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_blasso = sum(pcs_obs)/n())
colnames(avg_pcs_obs_blasso) = c("N", "pcs")

round(avg_pcs_obs_blasso, 3) %>% gt() 



#########################################################
#############          Scen 4                  ##########
#########################################################
sim_blasso_scen4_5 = sim_blasso_scen4_5[-which(sapply(sim_blasso_scen4_5, is.null))] 
nsubj = c(24,33,42, 51,60,69,78,87,96,102)
temp_data_obs = vector("list", length(sim_blasso_scen4_5))
for (i in 1:length(sim_blasso_scen4_5)){
  temp_data_obs[[i]] = sim_blasso_scen4_5[[i]]$pcs_results_obs
  colnames(temp_data_obs[[i]]) = "pcs_obs"
}
temp_pcs_obs = data.frame(do.call(rbind, temp_data_obs))
temp_pcs_all = cbind(n = rep(nsubj, length(sim_blasso_scen4_5)), temp_pcs_obs)
avg_pcs_obs_blasso = temp_pcs_all %>% group_by(n) %>% 
  summarise(avg_pcs_obs_blasso = sum(pcs_obs)/n())
colnames(avg_pcs_obs_blasso) = c("N", "pcs")

round(avg_pcs_obs_blasso, 3) %>% gt() 
