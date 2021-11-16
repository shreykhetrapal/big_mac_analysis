library(tidyverse)
library(janitor)
library(lubridate)

bm_index_graphs <- function(bm_all_data, country_selected = "Britain"){
  
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
    mutate(name = name %>% as.factor() %>% fct_inorder()) %>% 
    mutate(shape_point = ifelse(name == country_selected, "shape1", "shape2")) %>% 
    mutate(shape_point = shape_point %>% as.factor() %>% fct_inorder()) -> temp
  
  temp$color_select -> color_vector
  
  temp %>% 
    ggplot() +
    geom_segment(aes(x = name, xend = name, y = 0, yend = final), size = 0.1) +
    geom_point(aes(x = name, y = final, color = color_vector, shape = shape_point), size = 2) +
    theme_minimal() +
    scale_color_manual(values = c("#00c0d5", "#395662", "#ef1f3a")) +
    scale_y_continuous(position = "right") +
    scale_shape_manual(values = c(19, 4)) +
    geom_hline(aes(yintercept = 0, color = "#395662")) +
    geom_label(aes(x = 7, y = 5), label = "Overvalued", color = "#00c0d5", fontface = "bold") +
    geom_label(aes(x = 7.5, y = -5), label = "Undervalued", color = "#ef1f3a", fontface = "bold") +
    #ggrepel::geom_label_repel(data = filter(temp, name == "India"), aes(name, final- 5 , label = name)) +
    labs(title = "",
         y = "Percent Valuation",
         caption = "Base currency : USD") +
    theme(axis.text.x = element_blank(), 
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          legend.position = "none") -> p
  
  p
}

# Fill missing dates and adjust for weekend values 
fill_missing_dates <- function(cattle_raw_data){
  
  cattle_raw_data$date %>% min() -> min_date
  cattle_raw_data$date %>% max() -> max_date
  
  all_dates <- tibble(date = seq.Date(min_date, max_date, by = "1 day"))
  
  # Joined and filled missing dates
  all_dates %>% 
    left_join(cattle_raw_data, by = "date") -> date_adjusted_cattle
  
  # Now, need to adjust for weekend missing values
  date_adjusted_cattle %>% 
    filter(is.na(open)) -> na_dates
  
  for(i in 1:nrow(na_dates)){
    
    print(i)
    
    # Take the first NA date 
    na_dates$date[i] -> first_date
    
    date_adjusted_cattle %>% 
      filter(date >= first_date) %>% 
      # Drop all missing values based on the open calumn
      drop_na(open) %>% 
      # Slice first row with available data 
      slice(1) %>% 
      pull(open) -> my_first_row
    
    
    date_adjusted_cattle %>% 
      mutate(adj_close= ifelse(date == first_date,my_first_row[1],adj_close)) -> date_adjusted_cattle
  }
  
  date_adjusted_cattle
  
}

standardize_values <- function(plotting_data, selected_currency){
  
  # Standardisation by using the formula (X - Mean)/SD
  
  plotting_data %>% 
    filter(category == "adj_close") %>% 
    summarise(mean = price %>% mean(na.rm = T), 
              sd = price %>% sd(na.rm = T)) -> cattle_mean_sd
  
  
  plotting_data %>% 
    # Filtering the selected currency
    filter(category == "local_price" & currency_code == selected_currency) %>% 
    summarise(mean = price %>% mean(na.rm = T), 
              sd = price %>% sd(na.rm = T)) -> bmi_mean_sd
  
  plotting_data %>% 
    mutate(sd_price = ifelse(category == "adj_close", (price - cattle_mean_sd$mean)/ cattle_mean_sd$sd, 
                          ifelse(category == "local_price" & currency_code == selected_currency, (price - bmi_mean_sd$mean)/ bmi_mean_sd$sd, NA))) -> plot_standard_formula
  
  plot_standard_formula
  
}

plot_standardised <- function(plotting_data, selected_currency){
  
  # Standardising values
  standardize_values(plotting_data, selected_currency) -> plot_standard_formula
  
  # Standardised Forumula plot faceted
  ggplot() +
    geom_line(data = filter(plot_standard_formula, category == "adj_close"), 
              aes(date, sd_price, label = price), color = "#4393C3", size = 0.2) +
    geom_step(data = filter(plot_standard_formula, category == "local_price" & currency_code == selected_currency), 
              aes(date, sd_price, label = price), color = "#053061", size = 1.5) +
    labs(title = "Standardised graph", 
         y = "standardised Y") +
    facet_wrap(~category) +
    ggthemes::theme_stata() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  
}

# Calculate correlations 
cor_calculate <- function(plotting_data, country_selected = "USD"){
  
  standardize_values(plotting_data, country_selected) %>% 
    filter(currency_code == country_selected) %>% 
    filter(category == "adj_close") %>% 
    pull(price) -> cattle_price
  
  standardize_values(plotting_data, country_selected) %>% 
    filter(currency_code == country_selected) %>% 
    filter(category == "local_price") %>% 
    pull(price) -> bm_price
  
  tibble(cp = cattle_price,
         bmi = bm_price) %>% 
    drop_na() %>% 
    nrow() -> total_points
  
  return(list(cor_results = cor.test(cattle_price, bm_price), 
              points = total_points))
  
  
}

