# Non UI Script 

library(tidyverse)
library(patchwork)
library(ggthemes)
library(janitor)
library(lubridate)
library(viridis)

source("./R/bm_functions.R")

# Load all data 

"./data/data_raw/Live Cattle Futures Data - Sheet1.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = date %>% mdy()) %>% 
  mutate(across(.cols = is.character, .fns = parse_number)) -> cattle_raw_data

# Big Mac data 
"./data/data_raw/big-mac-historical-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_historical

"./data/data_raw/big-mac-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_source

# Combining the two big mac files 
bm_historical %>% 
  rbind(bm_source %>% 
          select(-gdp_dollar)) %>% 
  mutate(date = date %>% as.Date())-> bm_all_data

# Date adjusted cattle
cattle_raw_data$date %>% min() -> min_date
cattle_raw_data$date %>% max() -> max_date

all_dates <- tibble(date = seq.Date(min_date, max_date, by = "1 day"))

# Joined and filled missing dates
all_dates %>% 
  left_join(cattle_raw_data, by = "date") -> date_adjusted_cattle

# Visualising missing data 
date_adjusted_cattle %>% 
  select(-volume) %>% 
  visdat::vis_dat() +
  theme(legend.position = "bottom") -> p1

date_adjusted_cattle %>% 
  select(-volume) %>% 
  visdat::vis_miss() -> p2

p1+p2 -> p3

#ggsave("missing_data.png", device = "png", width = 15, dpi = 500, height = 8)

# Visualising missing days 
date_adjusted_cattle %>% 
  mutate(day = date %>% wday(label = T)) %>% 
  group_by(day) %>% 
  summarise(count = open %>% is.na() %>% sum()) %>% 
  arrange(count) %>% 
  mutate(day = day %>% as.factor() %>% fct_inorder()) %>% 
  ggplot() +
  geom_col(aes(day, count, fill = day)) +
  coord_flip() +
  theme_minimal() +
  ggthemes::scale_fill_tableau() +
  theme(legend.position = "none")

# Filling in missing dates 
fill_missing_dates(cattle_raw_data) -> cattle_data_clean

# Graph of number of missing values by days 
date_adjusted_cattle %>% 
  mutate(day = date %>% wday(label = T)) %>% 
  group_by(day) %>% 
  summarise(count = open %>% is.na() %>% sum()) %>% 
  arrange(count) %>% 
  mutate(day = day %>% as.factor() %>% fct_inorder()) %>% 
  ggplot() +
  geom_col(aes(day, count, fill = day)) +
  coord_flip() +
  theme_minimal() +
  ggthemes::scale_fill_tableau() +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 15, face = "bold"))

# Plotting cattle prices only 
cattle_data_clean %>% 
  ggplot() +
  geom_line(aes(date, adj_close)) +
  labs(title = "Cattle Future Prices") +
  ggthemes::theme_economist() +
  theme(axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) -> cattle_futures_plot

#ggsave("cattle_futures_plot.png", device = "png", width = 15, dpi = 300, height = 8)

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
             aes(date, price), color = "#613715", size = 0.5) +
  geom_step(data = filter(plot_data, category == "local_price" & currency_code == "USD"), 
            aes(date, price), color = "#DA291C", size = 1.5) +
  labs(title = "") +
  facet_wrap(~category, scales = "free_y") +
  ggthemes::theme_stata() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) -> non_standard_plot

#ggsave("cattle_big_mac_non_standard.png", device = "png", width = 15, dpi = 300, height = 8)

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
        plot.title = element_text(hjust = 0.5)) -> standardised_plot

ggsave("standardised_cattle_big_mac.png", device = "png", width = 15, dpi = 300, height = 8)

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

# Conclusion graph

bm_all_data %>% 
  left_join(cattle_data_clean, by = "date") %>% 
  filter(currency_code == "AUD") %>% 
  select(date, currency_code, local_price, adj_close, dollar_ex) -> conclusion

conclusion$local_price %>% mean(na.rm = T) -> mean_local
conclusion$local_price %>% sd(na.rm = T) -> sd_local

conclusion$adj_close %>% mean(na.rm = T) -> mean_adj
conclusion$adj_close %>% sd(na.rm = T) -> sd_adj

conclusion$dollar_ex %>% mean(na.rm = T) -> mean_dollar
conclusion$dollar_ex %>% sd(na.rm = T) -> sd_dollar

conclusion %>% 
  mutate(local_price = (local_price-mean_local)/sd_local, 
         adj_close = (adj_close-mean_adj)/sd_adj, 
         dollar_ex = (dollar_ex-mean_dollar)/sd_dollar) %>% 
  pivot_longer(cols = c("local_price", "adj_close", "dollar_ex")) %>% 
  ggplot() +
  geom_line(aes(date, value, group = name, color = name))

