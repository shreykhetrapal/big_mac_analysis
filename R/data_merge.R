library(tidyverse)
library(data.table)

cattle <- fread('./data/data_output/processed_cattle.csv')
bmi <- fread('./data/data_output/processed_bmi.csv')

data <- bmi %>% left_join(cattle, by = "date")

# write merged data to /output_data folder
fwrite(data, './data/data_output/merged_data.csv')
