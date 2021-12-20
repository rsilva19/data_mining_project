### visualizing /comparing lasso and blasso together  - figs/tables
beta_tbl_lasso = beta_tbl
t_4_10 = 
  cbind(beta_tbl_lasso, Blasso = beta_tbl %>% pull(Blasso)) %>% 
  gt() %>% 
  tab_header(
    title = md("Scenario 4"),
    #subtitle = md("`gtcars` is an R dataset")
  ) %>% 
  tab_style(
    style = cell_fill(color = "darkseagreen1"),
    locations = cells_body(
      rows = c(1))
  )  

avg_pcs_obs_lasso = avg_pcs_obs
pcs_4_10 = cbind(round(avg_pcs_obs_lasso, 2), 
      Blasso = round(avg_pcs_obs %>% pull(Blasso),2)) %>% 
        gt() %>% 
        tab_header(
          title = md("Scenario 4"),
          #subtitle = md("`gtcars` is an R dataset")
        ) 

##########################################################
#################       convergence      #################
##########################################################

# load convg_scen1 in saved lasso data
lasso_convg = convg_scen1 %>% mutate(method = rep("lasso", length(convg_scen1)))
blasso_convg = convg_scen1_blasso %>% mutate(method = rep("Blasso", length(convg_scen1_blasso)))
dat_convg = rbind(lasso_convg, blasso_convg)

dat_convg %>% 
  ggplot(aes(x =n, y = pcs, color = factor(method))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(
    title = "Scenario 1: PCS Convergence ",
    x = "N subjects",
    y = "Probability of ",
    color = "Covariate"
  )


convg_lasso = data.frame(convg_scen1)[1:66,] %>% mutate(method = rep("Lasso", nrow(.)))
convg_blasso = data.frame(convg_scen_blasso_1) %>% mutate(method = rep("BLasso", nrow(.)))
covg_dat = rbind(convg_lasso, convg_blasso)
covg_dat$pcs = as.vector(covg_dat$pcs)
covg_dat %>% 
  ggplot(aes(x = n, y = pcs, color = factor(method))) +
  #geom_point() +
  geom_line() +
  theme_bw() +
  labs(
    title = "Scenario 1: PCS Convergence ",
    x = "N subjects",
    y = "PCS",
    color = "Method")

### PCS of scenarios 1-4
avg_pcs_obs_b_5 = avg_pcs_obs_blasso %>% mutate(method = rep("BLasso", nrow(.)), 
                                                   num_cov = rep("5", nrow(.)))
avg_pcs_obs_l_5 = avg_pcs_obs_lasso %>% mutate(method = rep("Lasso", nrow(.)),
                                                 num_cov = rep("5", nrow(.)))
avg_pcs_obs_b_10 = avg_pcs_obs_blasso_10 %>% mutate(method = rep("BLasso", nrow(.)),
                                                   num_cov = rep("10", nrow(.)))
avg_pcs_obs_l_10 = avg_pcs_obs_lasso_10 %>% mutate(method = rep("Lasso", nrow(.)),
                                                 num_cov = rep("10", nrow(.)))

pcs_dat_4 = rbind(avg_pcs_obs_b_5 , avg_pcs_obs_l_5 ,avg_pcs_obs_l_10 ,avg_pcs_obs_b_10)

pcs_4_5 = pcs_dat_4 %>% 
  ggplot(aes(x = N, y = pcs, color = factor(method), linetype = factor(num_cov))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(
    title = "Scenario 4",
    x = "N subjects",
    y = "PCS",
    color = "Method", 
    linetype = "Number of Covariates")


library(ggpubr)
ggarrange(pcs_1_5,pcs_2_5,pcs_3_5,pcs_4_5, ncol=4, nrow=1, common.legend = TRUE, legend="bottom", align = "h", widths = 1,
              heights = 1)

