
#########################################################
#############          Scen 1                  ##########
#########################################################
## convert sparse matrix to 
sim_beta = vector("list", length(sim_lasso_scen1_5))
for(i in 1:length(sim_beta)){
  sim_beta[[i]] = ifelse(is.na(sim_lasso_scen1_5[[i]]$model$beta), 0, sim_lasso_scen1_5[[i]]$model$beta)
}
# take out intercept
betas = data.frame(do.call(rbind, sim_beta))[, -1] 
colnames(betas) = c("Z1", "Z2", "Z3", "Z4", "Z5")
p_chosen = apply(betas, 2, function(c) sum(c!=0))/nrow(betas)
beta_tbl = data.frame(Predictor=names(p_chosen), "P_chosen"=round(p_chosen,3), row.names=NULL)
colnames(beta_tbl) = c("Predictor", "Lasso")
beta_tbl %>% 
  gt() %>% 
  tab_header(
    title = md("Probability Covariate Chosen"),
    #subtitle = md("`gtcars` is an R dataset")
  ) %>% 
  tab_style(
    style = cell_fill(color = "darkseagreen1"),
    locations = cells_body(
      rows = c(3))
  )  
#########################################################
#############          Scen 2                  ##########
#########################################################
## convert sparse matrix to 
sim_beta = vector("list", length(sim_lasso_scen2_5))
for(i in 1:length(sim_beta)){
  sim_beta[[i]] = ifelse(is.na(sim_lasso_scen2_5[[i]]$model$beta), 0, sim_lasso_scen2_5[[i]]$model$beta)
}
# take out intercept
betas = data.frame(do.call(rbind, sim_beta))[, -1]
colnames(betas) = c("Z1", "Z2", "Z3", "Z4", "Z5")
p_chosen = apply(betas, 2, function(c) sum(c!=0))/nrow(betas)
beta_tbl = data.frame(Predictor=names(p_chosen), "P_chosen"=round(p_chosen,3), row.names=NULL)
colnames(beta_tbl) = c("Predictor", "Lasso")
beta_tbl %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "darkseagreen1"),
    locations = cells_body(
      rows = c(3))
  )  


#########################################################
#############          Scen 3                  ##########
#########################################################
## convert sparse matrix to 
sim_beta = vector("list", length(sim_lasso_scen3_5))
for(i in 1:length(sim_beta)){
  sim_beta[[i]] = ifelse(is.na(sim_lasso_scen3_5[[i]]$model$beta), 0, sim_lasso_scen3_5[[i]]$model$beta)
}
# take out intercept
betas = data.frame(do.call(rbind, sim_beta))[, -1] 
colnames(betas) = c("Z1", "Z2", "Z3", "Z4", "Z5")
p_chosen = apply(betas, 2, function(c) sum(c!=0))/nrow(betas)
beta_tbl = data.frame(Predictor=names(p_chosen), "P_chosen"=round(p_chosen,3), row.names=NULL)
colnames(beta_tbl) = c("Predictor", "Lasso")
beta_tbl %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "darkseagreen1"),
    locations = cells_body(
      rows = c(3))
  )  


#########################################################
#############          Scen 4                  ##########
#########################################################
## convert sparse matrix to 
sim_beta = vector("list", length(sim_lasso_scen4_5))
for(i in 1:length(sim_beta)){
  sim_beta[[i]] = ifelse(is.na(sim_lasso_scen4_5[[i]]$model$beta), 0, sim_lasso_scen4_5[[i]]$model$beta)
}
# take out intercept
betas = data.frame(do.call(rbind, sim_beta))[, -1] 
colnames(betas) = c("Z1", "Z2", "Z3", "Z4", "Z5")
p_chosen = apply(betas, 2, function(c) sum(c!=0))/nrow(betas)
beta_tbl = data.frame(Predictor=names(p_chosen), "P_chosen"=round(p_chosen,3), row.names=NULL)
colnames(beta_tbl) = c("Predictor", "Lasso")
beta_tbl = rbind(c("0", 0.526), beta_tbl)
beta_tbl %>% 
  gt() %>% 
  tab_style(
    style = cell_fill(color = "darkseagreen1"),
    locations = cells_body(
      rows = c(1))
  )  
# count number of times all betas were shrunk to 0
temp= NULL
for(i in 1:nrow(betas)){
  temp = c(temp, sum(betas[i,]))
}
length(which(temp == 0))/length(sim_lasso_scen4_5)  #0.526: number of times all betas where 0 (no covariate selected)/ total
