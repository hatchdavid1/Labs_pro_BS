library(tidyverse)
library(readxl)
library(h2o)

# Reading Data
path  <- '/Users/davidhatch/Documents/labs_pro/lab_6_h20/H2O_automl_lab/data/bank_term_deposit_marketing_analysis.xlsx'
sheets  <- excel_sheets(path)

# Look at the data from each sheet
sheets %>%
    map(~read_excel(path, sheet=.)) %>%
    set_names(sheets)

# Apply a vlookup logic joined by id
data_joined_tbl  <- sheets[4:7] %>% 
    map(~ read_excel(path=path, sheet = .)) %>% 
    reduce(left_join)

# Quick df view for the resulting join
data_joined_tbl

# h2o initialization
h2o.init()

###### h2o data preparation ######
# Converting string to columns to factor type
