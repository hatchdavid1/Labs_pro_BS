#### Anomalize Lab #### 
#### Loading main libraries ####
library(vroom)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(rsample)
library(anomalize)

#### General Structure #### 
websites_raw_tbl  <- vroom::vroom('/Users/davidhatch/Documents/labs_pro/lab_anomalize_package/lab_12_r_programming_rlang/data/train_1.csv', delim = ',')

websites_raw_tbl  <- websites_raw_tbl %>% rowid_to_column(var = 'id')

set.seed(123)
websites_sample_tbl <- websites_raw_tbl %>%
  sample_n(size = 9) %>% 
  gather(key = 'date', value = 'visits', -id, -Page) %>%
  replace_na(replace = list(visits = 0)) %>%
  mutate(date = ymd(date))

