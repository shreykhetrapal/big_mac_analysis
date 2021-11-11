library('tidyverse')
library('data.table')

#big_mac_countries = c('ARG', 'AUS', 'BRA', 'GBR', 'CAN', 'CHL', 'CHN', 'CZE', 'DNK',
#                      'EGY', 'HKG', 'HUN', 'IDN', 'ISR', 'JPN', 'MYS', 'MEX', 'NZL',
#                      'NOR', 'PER', 'PHL', 'POL', 'RUS', 'SAU', 'SGP', 'ZAF', 'KOR',
#                      'SWE', 'CHE', 'TWN', 'THA', 'TUR', 'ARE', 'USA', 'COL', 'CRI',
#                      'PAK', 'LKA', 'UKR', 'URY', 'IND', 'VNM', 'GTM', 'HND', 'VEN',
#                      'NIC', 'AZE', 'BHR', 'HRV', 'JOR', 'KWT', 'LBN', 'MDA', 'OMN',
#                      'QAT', 'ROU', 'EUZ')

big_mac_countries = c('ARG', 'CHN', 'DKK', 'CHF', 'IDN', 'USA')
#base_currencies = c('USD', 'EUR', 'GBP', 'JPY', 'CNY')
base_currencies = c('USD', 'CNY')

# read data, sort by date and then by country name, for easy reading;
# index on currency_code for faster joining, and
# remove lines where the local price is missing
big_mac_data = fread('./data_raw/big-mac-source-data.csv', 
                     na.strings = '#N/A',
                     key = 'date,name', 
                     index = 'currency_code') %>%
  .[!is.na(local_price)] %>%
  .[, !"GDP_dollar"]

big_mac_data_old = fread('./data_raw/big-mac-historical-source-data.csv', 
                     na.strings = '#N/A',
                     key = 'date,name', 
                     index = 'currency_code') %>%
  .[!is.na(local_price)]

big_mac_data_old$date <- as.Date(big_mac_data_old$date, format="%Y-%M-%D")
big_mac_data$date <- as.Date(big_mac_data$date, format="%Y-%M-%D")

# combine datasets
big_mac_data <- rbind(big_mac_data, big_mac_data_old) %>% arrange(date, name)
                     
# convert to uniform currency (get dollar price for every country)
big_mac_data$dollar_price = big_mac_data$local_price / big_mac_data$dollar_ex

# remove lines where dollar price is missing
# filter for listed countries in list above
big_mac_index = big_mac_data[!is.na(dollar_price) & iso_a3 %in% big_mac_countries
  ,.(date, iso_a3, currency_code, name, local_price, dollar_ex, dollar_price)]

# create new column for each base currency, 
for(currency in base_currencies) {
  big_mac_index[, (currency) := dollar_price / .SD[currency_code == currency]$dollar_price - 1,by=date]
}

# 
big_mac_index[, (base_currencies) := lapply(.SD, round, 5L), .SDcols=base_currencies]

# write adjusted data to /output_data folder
fwrite(big_mac_index, './data_output/processed_bmi.csv')