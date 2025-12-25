library(spdep)
library(spatialreg)


full_data_mod2 <- full_data_mod %>%
  filter(
    !is.na(UrbanDisadvantage),
    !is.na(HispanicComm),
    !is.na(BlackComm)
  )

# Compute Moran's I
neighbors <- poly2nb(full_data_mod, queen = TRUE)
weights   <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

#CR Tracts as Treatment

control_vars <- c("UrbanDisadvantage", "BlackComm", "HispanicComm")

full_data_mod$treatment <- as.factor(full_data_mod$cr_tracts)

# transit


lag_model_cr <- lagsarlm(
  as.formula(
    paste(
      "log_transit_cost_burden ~ treatment + Year_2021 + si_index_high + operator + treatment*Year_2021 + si_index_high*treatment*Year_2021 + ",
      paste(control_vars, collapse = " + ")
    )
  ),
  data = full_data_mod,
  listw = weights,
  method = "Matrix",
  zero.policy = TRUE
  
)


#RT Tracts

full_data_mod$treatment <- as.factor(full_data_mod$rt_tracts)

# transit


lag_model_rt <- lagsarlm(
  as.formula(
    paste(
      "log_transit_cost_burden ~ treatment + Year_2021 + si_index_high + operator + treatment*Year_2021 + si_index_high*treatment*Year_2021 + ",
      paste(control_vars, collapse = " + ")
    )
  ),
  data = full_data_mod,
  listw = weights,
  method = "Matrix",
  zero.policy = TRUE
)


#Bus Tracts

full_data_mod$treatment <- as.factor(full_data_mod$bus_tracts)

#transit


lag_model_bus <- lagsarlm(
  as.formula(
    paste(
      "log_transit_cost_burden ~ treatment + Year_2021 + si_index_high + operator + treatment*Year_2021 + si_index_high*treatment*Year_2021 + ",
      paste(control_vars, collapse = " + ")
    )
  ),
  data = full_data_mod,
  listw = weights,
  method = "Matrix",
  zero.policy = TRUE
)

#CR Tracts Only

full_data_mod$treatment <- as.factor(full_data_mod$cr_tracts_only)

#transit

lag_model_cr_only <- lagsarlm(
  as.formula(
    paste(
      "log_transit_cost_burden ~ treatment + Year_2021 + si_index_high + operator + treatment*Year_2021 + si_index_high*treatment*Year_2021 + ",
      paste(control_vars, collapse = " + ")
    )
  ),
  data = full_data_mod,
  listw = weights,
  method = "Matrix",
  zero.policy = TRUE
)


table(full_data_mod$treatment, full_data_mod$Year_2021, full_data_mod$si_index_high)


#Switched - CR Tracts Only Predicting Log Social Vulnerability 

full_data_mod$treatment <- full_data_mod$log_transit_cost_burden

error_model_cr_only_si_index_transit <- errorsarlm(as.formula(
  paste(
    "log_si_index  ~ treatment + cr_tracts_only + operator + Year_2021 + cr_tracts_only*Year_2021 + treatment*Year_2021*cr_tracts_only +",
    paste(control_vars, collapse = " + ")
  )
), data = full_data_mod, listw = weights, method = "Matrix", zero.policy = TRUE)


error_model_cr_si_index_transit <- errorsarlm(as.formula(
  paste(
    "log_si_index  ~ treatment + cr_tracts + operator + Year_2021 + cr_tracts*Year_2021 + treatment*Year_2021*cr_tracts +",
    paste(control_vars, collapse = " + ")
  )
), data = full_data_mod, listw = weights, method = "Matrix", zero.policy = TRUE)


moran.test(residuals(error_model_cr_only_si_index_transit), weights)
# Going with error model here because of lower AIC/BIC


full_data_mod$treatment <- full_data_mod$log_transportation_cost_burden


error_model_cr_only_si_index_transport <- errorsarlm(as.formula(
  paste(
    "log_si_index  ~ treatment + cr_tracts_only + operator + Year_2021 + cr_tracts_only*Year_2021 + treatment*Year_2021*cr_tracts_only +",
    paste(control_vars, collapse = " + ")
  )
), data = full_data_mod, listw = weights, method = "Matrix", zero.policy = TRUE)



error_model_cr_si_index_transport <- errorsarlm(as.formula(
  paste(
    "log_si_index  ~ treatment + cr_tracts + operator + Year_2021 + cr_tracts*Year_2021 + treatment*Year_2021*cr_tracts +",
    paste(control_vars, collapse = " + ")
  )
), data = full_data_mod, listw = weights, method = "Matrix", zero.policy = TRUE)



