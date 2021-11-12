library(tidyverse)
library(janitor)
library(lubridate)

# Big mac data and Cattle data are not matching for a few years due to the 'date' not matching. 
# So, we will only match by the year and the month 
  
# bm_all_data %>% 
#   mutate(year = date %>% year(), 
#          month = date %>% month(),
#          year_month = paste0(year, "-", month)) -> bm_date

# cattle_raw_data %>%
#   mutate(year = date %>% year(), 
#          month = date %>% month(),
#          year_month = paste0(year, "-", month)) ->cattle_date


# cattle_date %>% 
#   select(year_month, adj_close) %>% 
#   full_join(bm_date, by = "year_month") %>% 
#   select(-c(year, month, date)) %>% 
#   pivot_longer(cols = c(local_price, adj_close)) %>%
#   drop_na() %>% 
#   distinct() -> temp

# temp %>% 
#   group_by(year_month, name) %>% 
#   arrange(-value) %>% 
#   filter(currency_code == "USD") %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   arrange(year_month) %>% 
#   #filter(name == "local_price") %>% 
#   ggplot(aes(x = year_month, y = value, color = name, group = name)) +
#   geom_line() +
#   facet_wrap(~name, scales = "free_y") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   geom_smooth() -> p

bm_index_graphs <- function(bm_all_data){
  
  
  # Calculating the current USD price 
  bm_all_data %>% 
    filter(currency_code == "USD") %>% 
    filter(date == max(date)) %>% 
    pull(local_price) -> USD_current
  
  bm_all_data %>% 
    select(date, name, currency_code, local_price, dollar_ex) %>% 
    # calculating data for the graphs 
    mutate(implied_exchange_rate = local_price/USD_current) %>% 
    mutate(difference = implied_exchange_rate-dollar_ex) %>% 
    mutate(final = difference/dollar_ex) %>% 
    # filtering dates for 2020
    filter(date == max(date)) %>% 
    mutate(final = final * 100) %>% 
    mutate(color_select = ifelse(final < 0, "#ef1f3a",
                                 ifelse(final == 0, "#395662", "#00c0d5"))) %>% 
    arrange(final) %>% 
    mutate(name = name %>% as.factor() %>% fct_inorder()) -> temp
  
  temp$color_select -> color_vector
  
  temp %>% 
    ggplot() +
    geom_segment(aes(x = name, xend = name, y = 0, yend = final)) +
    geom_point(aes(x = name, y = final, color = color_vector)) +
    theme_minimal() +
    scale_color_manual(values = c("#ef1f3a", "#395662", "#00c0d5")) +
    scale_y_continuous(position = "right") +
    geom_hline(aes(yintercept = 0, color = "#395662")) +
    #geom_text(aes(x = 7, y = 5), label = "Overvalued", color = "#ef1f3a") +
    #geom_text(aes(x = 7.7, y = -5), label = "Undervalued", color = "#00c0d5") +
    theme(axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          panel.grid = element_blank(), 
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) -> p
  
  p 
  
  
}
