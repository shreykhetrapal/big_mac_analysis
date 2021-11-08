library(tidyverse)
library(janitor)
library(lubridate)

# Big mac data and Cattle data are not matching for a few years due to the 'date' not matching. 
# So, we will only match by the year and the month 
  
bm_all_data %>% 
  mutate(year = date %>% year(), 
         month = date %>% month(),
         year_month = paste0(year, "-", month)) -> bm_date

cattle_raw_data %>%
  mutate(year = date %>% year(), 
         month = date %>% month(),
         year_month = paste0(year, "-", month)) ->cattle_date


cattle_date %>% 
  select(year_month, adj_close) %>% 
  full_join(bm_date, by = "year_month") %>% 
  select(-c(year, month, date)) %>% 
  pivot_longer(cols = c(local_price, adj_close)) %>%
  drop_na() %>% 
  distinct() -> temp

temp %>% 
  group_by(year_month, name) %>% 
  arrange(-value) %>% 
  filter(currency_code == "USD") %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(year_month) %>% 
  #filter(name == "local_price") %>% 
  ggplot(aes(x = year_month, y = value, color = name, group = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_smooth() -> p

p
