
library(stargazer)


#transit cost burden

# Extract lag coefficients and standard errors
lag_coefs <- sapply(
  list(lag_model_cr, lag_model_rt, lag_model_bus, lag_model_cr_only),
  function(model) model$rho
)

lag_se <- sapply(
  list(lag_model_cr, lag_model_rt, lag_model_bus, lag_model_cr_only),
  function(model) sqrt(model$rho.se)
)

# Compute z-scores and p-values
lag_z <- lag_coefs / lag_se
lag_p <- 2 * (1 - pnorm(abs(lag_z)))

# Significance stars
sig_stars <- ifelse(lag_p < 0.001, "***",
                    ifelse(lag_p < 0.01, "**",
                           ifelse(lag_p < 0.05, "*",
                                  ifelse(lag_p < 0.1, ".", ""))))

# Format rho with SE and significance
rho_row <- paste0(
  round(lag_coefs, 3), " (", round(lag_se, 3), ")", sig_stars
)

# Add this to your stargazer table
html_output <- stargazer(
  lag_model_cr,
  lag_model_cr_only,
  lag_model_rt,
  lag_model_bus,
  type = "html",
  title = "Spatial Lag Models for Transit Cost Burden",
 dep.var.labels = "Log Transit Cost Burden",  # <- this puts it above column labels
  column.labels = c(
    "Commuter Rail Access",
    "Commuter Rail Access ONLY",
    "Rapid Transit Access",
    "Bus Access"
  ),
  covariate.labels = c(
    "Mode Access Treatment",
    "Year 2021",
    "SI Index High", 
    "Treatment * Year 2021",
    "Treatment * SI Index High",
    "Year 2021 * SI Index High",
    "SI Index High * Treatment * Year 2021"
  ),
  add.lines = list(c("Lag Coefficient (rho)", rho_row)),
  omit.stat = c("f", "ser"),
  omit = c("operator", control_vars),
  notes = "Standard errors are robust to spatial correlation. Factors combining demographic and income variables are included as controls but omitted from the table.",
  notes.align = "l"
)



# Save as an HTML file
write(html_output, file = "spatial_lag_models_transit_cost_burden.html")
write(html_output, file = "spatial_lag_models_transit_cost_burden.tex")

#SI index 
html_output_2 <- stargazer(
  error_model_cr_only_si_index_transit,
  error_model_cr_only_si_index_transport,
  type = "html",
  # Output as HTML
  title = "Spatial Error Models: Cost Burden Effect on SI Index (Commuter Rail Only)",
  dep.var.labels = "Log SI Index",
  column.labels = c(
    "Transit Cost Burden",
    "Transport Cost Burden"
  ),
  covariate.labels = c(
    "Log Cost Burden", 
    "Mode Access Treatment - CR Tracts Only",
    "Year 2021",
    "CR Tracts Only * Year 2021",
    "Log Cost Burden * Year 2021",
    "CR Tracts Only * Log Cost Burden",
    "Log Cost Burden * CR Tracts Only * Year 2021"
  ),
  omit.stat = c("f", "ser"),
  omit = c("operator", control_vars),
  notes = "Standard errors are robust to spatial correlation. Factors combining demographic and income variables are included as controls but omitted from the table.",
  notes.align = "l"
)

write(html_output_2, file = "spatial_error_models_si_index.html")
write(html_output_2, file = "spatial_error_models_si_index.tex")
