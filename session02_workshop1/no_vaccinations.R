fubar <- vaccinations %>% 
  filter(date == "09/01/2021") %>% 
  filter (series_complete_pop_pct == 0)


fubar %>% 
  count(recip_state)

vaccinations %>% 
  filter(recip_state == "TX") %>% 
  count(fips)
