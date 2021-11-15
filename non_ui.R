# Non UI Script 

library(tidyverse)
library(patchwork)
library(ggthemes)
library(janitor)
library(lubridate)

source("bm_functions.R")

# Load all data 

"./data_raw/Live Cattle Futures Data - Sheet1.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = date %>% mdy()) %>% 
  mutate(across(.cols = is.character, .fns = parse_number)) -> cattle_raw_data

# Big Mac data 
"./data_raw/big-mac-historical-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_historical

"./data_raw/big-mac-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_source

# Combining the two big mac files 
bm_historical %>% 
  rbind(bm_source %>% 
          select(-gdp_dollar)) %>% 
  mutate(date = date %>% as.Date())-> bm_all_data

# Filling in missing dates 
fill_missing_dates(cattle_raw_data) -> cattle_data_clean

# Visualising missing data 
cattle_data_clean %>% 
  visdat::vis_dat() -> p1

cattle_data_clean %>% 
  visdat::vis_miss() -> p2

p1+p2  -> p3

#ggsave("missing_data.png", device = "png", width = 15)

# Joining in data from minimum date of BMI and max date of cattle_raw
tibble(date = seq.Date(min(bm_all_data$date), max(cattle_data_clean$date), by = "1 day")) %>% 
  left_join(cattle_data_clean, by = "date") %>%
  select(date, adj_close) %>% 
  left_join(bm_all_data, by = "date") %>% 
  select(date, adj_close, name, currency_code, local_price) -> joined_data_for_graph
  

joined_data_for_graph %>% 
  pivot_longer(cols = c("adj_close", "local_price"), 
               names_to = "category", 
               values_to = "price") %>% 
  #filter(category == "local_price") %>% 
  filter(currency_code == "AUD") %>% 
  ggplot() +
  geom_line(aes(date, price, group = category)) +
  facet_wrap(~category, scales = "free_y")


joined_data_for_graph %>% 
  pivot_longer(cols = c("adj_close", "local_price"), 
               names_to = "category", 
               values_to = "price")-> plot_data

# Non standardised graph 
ggplot() +
  geom_point(data = filter(plot_data, category == "adj_close"), 
            aes(date, price), color = "black", size = 0.2) +
  geom_step(data = filter(plot_data, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "black", size = 1.5) +
  labs(title = "Non standardised graph") +
  #facet_wrap(~category, scales = "free_y") +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

# Non standardised faceted graph 
ggplot() +
  geom_line(data = filter(plot_data, category == "adj_close"), 
             aes(date, price), color = "black", size = 0.5) +
  geom_step(data = filter(plot_data, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "black", size = 1.5) +
  labs(title = "Non standardised graph") +
  facet_wrap(~category, scales = "free_y") +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))


# Data standardisation i.e. dividing by range 
plot_data %>%
  filter(currency_code == "USD") %>% 
  group_by(category) %>% 
  summarise(min = price %>% min(na.rm = T), 
            max = price %>% max(na.rm = T)) %>% 
  mutate(range = max-min) -> range_table

plot_data %>% 
  mutate(price = ifelse(category == "adj_close", price/range_table$range[1], 
                        ifelse(category == "local_price", price/range_table$range[2], 0))) -> plot_data_standard

# Standardised plot 
ggplot() +
  geom_line(data = filter(plot_data_standard, category == "adj_close"), 
             aes(date, price), color ="#4393C3", size = 0.2) +
  geom_step(data = filter(plot_data_standard, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "#053061" , size = 1.5) +
  labs(title = "Standardised graph", 
       y = "standardised Y") +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

# Standardised plot faceted
ggplot() +
  geom_line(data = filter(plot_data_standard, category == "adj_close"), 
            aes(date, price), color = "#4393C3", size = 0.2) +
  geom_step(data = filter(plot_data_standard, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "#053061", size = 1.5) +
  labs(title = "Standardised graph", 
       y = "standardised Y") +
  facet_wrap(~category) +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

# Standardisation by using the formula (X - Mean)/SD

plot_data %>% 
  filter(category == "adj_close") %>% 
  summarise(mean = price %>% mean(na.rm = T), 
            sd = price %>% sd(na.rm = T)) -> cattle_mean_sd

  
plot_data %>% 
  filter(category == "local_price" & currency_code == "USD") %>% 
  summarise(mean = price %>% mean(na.rm = T), 
            sd = price %>% sd(na.rm = T)) -> bmi_mean_sd

plot_data %>% 
  mutate(price = ifelse(category == "adj_close", (price - cattle_mean_sd$mean)/ cattle_mean_sd$sd, 
                        ifelse(category == "local_price", (price - bmi_mean_sd$mean)/ bmi_mean_sd$sd, 0))) -> plot_standard_formula


# Standard Formula without facet

ggplot() +
  geom_line(data = filter(plot_standard_formula, category == "adj_close"), 
            aes(date, price), color ="#4393C3", size = 0.2) +
  geom_step(data = filter(plot_standard_formula, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "#053061" , size = 1.5) +
  labs(title = "Standardised graph", 
       y = "standardised Y") +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))


# Standardised Forumula plot faceted
ggplot() +
  geom_line(data = filter(plot_standard_formula, category == "adj_close"), 
            aes(date, price), color = "#4393C3", size = 0.2) +
  geom_step(data = filter(plot_standard_formula, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "#053061", size = 1.5) +
  labs(title = "Standardised graph", 
       y = "standardised Y") +
  facet_wrap(~category) +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
