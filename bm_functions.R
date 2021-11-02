library(tidyverse)
library(janitor)
library(lubridate)

# Cattle Data
"Live Cattle Futures Data - Sheet1.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = date %>% mdy()) %>% 
  mutate(across(.cols = is.character, .fns = parse_number)) -> cattle_raw_data

# Plotting cattle data
raw_data %>% 
  ggplot() +
  geom_line(aes(date, adj_close))

# Reading Big Mac data 
"big-mac-historical-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  filter(name == "United States") -> bm_historical

"big-mac-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() %>%
  filter(name == "United States") -> bm_source

bm_historical %>% 
  select(date, local_price, currency_code) %>% 
  rbind(bm_source %>% 
          select(date, local_price, currency_code)) -> bm_all_data

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
  slice(1) %>% 
  ungroup() %>% 
  arrange(year_month) %>% 
  #filter(name == "local_price") %>% 
  ggplot(aes(x = year_month, y = value, color = name, group = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_smooth() -> p

ggsave("plot1.png", width = 15, height = 8, dpi = 300)  
