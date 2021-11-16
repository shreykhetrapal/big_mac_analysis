# Extended Big Mac Index

This repository consists of code on how do we add the cattle data into the existing calculation of Big Mac Index from The Economist.

This repo can be divided into 2 parts:

## Data Wrangling section

a. Data acquisition
manual upload of csv files stored in `.\data_raw`

b. Data preprocessing for Big Mac Index
run:
```
$ Rscript raw_preprocessing_big_mac.R
```

c. Data preprocessing for Cattle Data
run:
```
$ Rscript raw_preprocessing_cattle.R
```


## Visualization section
  TBD
  
# Installations
add sentences on how to install R and R kernel in jupyter notebook (if needed)

## R

## Jupyter Notebook

## R kernel in Jupyter Notebook


## Libraries
- tidyverse
- janitor
- data.table
- lubridate
- docstring

run:
```
> install.packages(c('tidyverse', 'janitor', 'data.table', 'docstring'))
```