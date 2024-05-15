#### Portoflio Optimization - Nonlinear Programming #### 
#### Loading Main Libraries #### 
# Optimization solver 
library(ROI)
library(ROI.plugin.alabama)
# Finance 
library(tidyquant)
# Visualization 
library(plotly)
# Core 
library(tidyverse)
# Timing 
library(tictoc)


#### Data ####  BBY  CCI  EBAY  JOE  ORCL  VLTO
assets <- c ('BBY'  ,'CCI'  ,'EBAY'  ,'JOE'  ,'ORCL'  ,'VLTO') %>% sort()

stock_prices_tbl  <- tq_get(assets, from = '2022-01-01', to = '2024-01-01')
stock_return_tbl  <- stock_prices_tbl %>%
    select(symbol, date, adjusted) %>% 
    group_by(symbol) %>% 
    tq_transmute(adjusted, mutate_fun = periodReturn, period = 'yearly', col_rename = 'returns')

returns_matrix_tbl  <- stock_return_tbl %>%
    spread(symbol, returns) %>% 
    select(assets)

returns_matrix_tbl


#### Optimization #### 
















