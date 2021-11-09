library('tidyverse')
library('data.table')

cattle_data = fread('./Live Cattle Futures Data - Sheet1.csv')

# convert date column type into date
cattle_data$Date <- as.Date(cattle_data$Date, format="%m/%d/%Y")
head(cattle_data)

# fill missing dates (min - max)
cattle_data <- cattle_data %>% 
               arrange(Date) %>%
               complete(Date = seq.Date(min(Date), max(Date), by="day"))

# fill missing values with the next day open price
head(cattle_data,10)

tail(cattle_data)

# merge between the big mac raw index and cattle data