## Portfolio Analysis and optimization 
#### Loading main Libraries #### 
library(tidyquant)
library(tidyverse)
library(plotly)
library(furrr)

#### Importing Data #### 
# How to get stock prices
tq_get('AAPL', from = '2020-01-01', to = '2024-01-01')
# How to get economic data
tibble(symbols = c('MKTGDPCNA646NWDB'), 
       name = c('GDP China')) %>%
         tq_get(get = 'economic.data', from = '1960-01-01')

# Transforming to returns 
end  <- '2024-01-01' %>% ymd()
start  <- end -years(5) + days(1)
#### Getting the components ####
returns_m_components_tbl  <- c('BAC', 'CAT', 'CPNG', 'DIS', 'ECL', 'FICO', 'KOF', 'ONON', 'STLA', 'VLTO', 'WFC', 'WM') %>%
  tq_get(get = 'stock.prices', 
         from = start, 
         to = end) %>% 
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn, period = 'monthly',
               col_rename = 'ret', type = 'log') %>%
  ungroup()
#### Original version 
# returns_m_components_tbl <- c("AAPL", "GOOG", "NFLX") %>%
#   tq_get(get  = "stock.prices",
#          from = start,
#          to   = end) %>%
#   group_by(symbol) %>%
#   tq_transmute(select     = adjusted, 
#                mutate_fun = periodReturn, 
#                period     = "monthly") %>%
#   ungroup()


returns_m_components_tbl


