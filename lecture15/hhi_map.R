## map visualization of census data:

library(tidycensus)
options(tigris_use_cache = TRUE)

dc_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = c("DC"), 
  year = 2020,
  geometry = TRUE
)

mo_income <- get_acs(
  geography = "county", 
  variables = "B19013_001",
  state = c("MO"), 
  year = 2020,
  geometry = TRUE
)

plot(dc_income['estimate'], 
     main = 'Estimate of household income in DC by US Census tract')


plot(mo_income['estimate'], 
     main = 'Estimate of household income in MO by county')


plot(mo_income['moe'], 
     main = 'Margins of Error estimate of HHI in MO by county')
