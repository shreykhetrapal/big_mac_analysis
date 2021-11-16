library(janitor)
library(lubridate)
library(tidyverse)
library(data.table)
library(docstring)

# read csv, normalize column names, convert date column type into date, sort by date
cattle_data = fread('./data_raw/Live Cattle Futures Data - Sheet1.csv') %>%
  clean_names() %>%
  mutate(date = date %>% mdy()) %>%
  arrange(date)

# add missing dates
cattle_data_full_dates <- cattle_data %>% 
                          complete(date = seq.Date(min(date), max(date), by="day"))

fill_missing_price <- function(data) {
  #' for every date with missing adjusted close price value, replace NA with 
  #' the nearest future date open price
  #' 
  #' replace the adj close price value with nearest open value of future date
  temp = data
  
  # get dataframe of dates with missing values
  na_dates <- temp %>% filter(is.na(open))
  
  # for every missing date, replace the adj close price value 
  # with nearest open value of future date
  for(i in 1:nrow(na_dates)){
    
    if (i %% 100 == 0) {
      print(i)
      }
    
    # get first non-NA open value in nearest future date
    my_first_row <- temp %>% 
                    filter(date >= na_dates$date[i]) %>%
                    drop_na(open) %>% 
                    slice(1) %>%
                    pull(open)
    
    temp <- temp %>% 
            mutate(adj_close = ifelse(date == na_dates$date[i],
                                              my_first_row[1],
                                              adj_close))
  }
  return(temp)
}

adjusted_cattle_data <- fill_missing_price(cattle_data_full_dates)

# filter to take only required columns
final_data <- adjusted_cattle_data %>% select(date, adj_close)

# write adjusted data to /output_data folder
fwrite(final_data, './data_output/processed_cattle.csv')


