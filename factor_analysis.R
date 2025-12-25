
install.packages(c("tidycensus", "tidyverse", "psych"))
library(tidycensus)
library(tidyverse)
library(psych)
library(tidyverse)

options(tigris_use_cache = TRUE)
census_api_key("341524e3bf5c09cd8f56a4940dd6e14806d3960b", install = TRUE)
library(tidyverse)
library(tidycensus)
library(sf)

library(tidyverse)
library(tidycensus)
library(sf)

get_acs_data <- function(year, tract_ids, state_fips = NULL) {
  
  # Core ACS variables
  vars <- c(
    income         = "B19013_001",
    hisp           = "B03003_003",   # Hispanic
    nh_black       = "B03002_004",   # Non-Hisp Black
    nh_asian       = "B03002_006",   # Non-Hisp Asian
    renters        = "B25003_003",
    renters_total  = "B25003_001",
    zero_veh       = "B08201_002",
    veh_total      = "B08201_001",
    total_pop      = "B01001_001"
  )
  
  # Age 65+ ACS bins
  age65_codes <- paste0("B01001_", sprintf("%03d", c(20:25, 44:49)))
  
  # Name them uniquely to avoid duplicate "E"/"M"
  names(age65_codes) <- paste0("age65_", c(20:25, 44:49))
  
  # All variables with unique names
  all_vars <- c(vars, age65_codes)
  
  df <- get_acs(
    geography = "tract",
    variables = all_vars,
    state = state_fips,
    year = year,
    survey = "acs5",
    geometry = FALSE,
    output = "wide"
  ) %>%
    filter(GEOID %in% tract_ids) %>%
    mutate(
      pct_65plus   = rowSums(across(ends_with("E") & starts_with("age65_"))) / total_popE * 100,
      pct_hisp     = hispE       / total_popE * 100,
      pct_nh_black = nh_blackE   / total_popE * 100,
      pct_nh_asian = nh_asianE   / total_popE * 100,
      pct_zero_veh = zero_vehE   / veh_totalE * 100,
      pct_renters  = rentersE    / renters_totalE * 100
    ) %>%
    select(
      GEOID, incomeE, pct_65plus, pct_hisp, pct_nh_black, pct_nh_asian,
      pct_zero_veh, pct_renters
    ) %>%
    rename(med_inc = incomeE) %>%
    mutate(year = year)
  
  df
}


# Wrapper for multiple states
get_acs_year_all_states <- function(year, tract_ids) {
  
  tract_state <- substr(tract_ids, 1, 2)
  states <- unique(tract_state)
  
  purrr::map_dfr(states, function(st) {
    
    tracts_for_state <- tract_ids[tract_state == st]
    
    get_acs_data(
      year = year,
      tract_ids = tracts_for_state,
      state_fips = st
    )
  })
}

# Run
full_data <- read_sf("thesis_tract_data.geojson")
tract_ids <- unique(full_data$GEOID)

acs2019 <- get_acs_year_all_states(2019, tract_ids)
acs2021 <- get_acs_year_all_states(2021, tract_ids)

acs_all <- bind_rows(acs2019, acs2021)

fa_vars <- acs_all %>%
  select(
    GEOID, year,
    med_inc, pct_65plus, pct_hisp, pct_nh_black,
    pct_nh_asian, pct_zero_veh, pct_renters
  )

fa_numeric <- fa_vars %>%
  select(-GEOID, -year) %>%
  scale() %>%              # standardize (mean 0, sd 1)
  as.data.frame()

fa.parallel(fa_numeric, fa = "fa")

fa_result <- fa(
  fa_numeric,
  nfactors = 3,
  rotate = "oblimin",
  fm = "ml"      # maximum likelihood
)

print(fa_result, cutoff = 0.3)


scores <- as.data.frame(fa_result$scores)
colnames(scores) <- c("UrbanDisadvantage", "BlackComm", "HispanicComm")

scores <- bind_cols(
  fa_vars %>% select(GEOID, year),
  scores
)

scores$year <- as.character(scores$year)

full_data <- full_data %>%
  left_join(scores, by = c("GEOID", c("Year" = "year")))



full_data$cr_tracts_only <- ifelse(full_data$mode_group == "Only CR Access", 1, 0)

full_data$log_si_index <- full_data$log_si_index_mod


#TRANSPORT COST BURDEN CORRECTION
full_data_mod <- full_data %>%
  mutate(
    inflation_adjusted_transportation_cost_burden = inflation_adjusted_transport_cost / inflation_adjusted_median_income,
    log_transportation_cost_burden = log(inflation_adjusted_transportation_cost_burden)
  ) %>%
  filter(!is.na(si_index) & !is.na(log_transportation_cost_burden))

st_write(full_data_mod, "full_data_final.geojson")
