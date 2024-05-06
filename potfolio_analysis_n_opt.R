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
               col_rename = 'monthly.returns', type = 'log') %>%
  ungroup()
returns_m_components_tbl
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

#### Benchmark_tbl #### 
returns_m_benchmark_tbl <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = start,
         to   = end) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", type = 'log') %>%
  add_column(symbol = "XLK", .before = 1)

returns_m_benchmark_tbl


#### Performance analysis components #### 
returns_m_tbl <- returns_m_components_tbl %>%
  bind_rows(returns_m_benchmark_tbl)

returns_m_tbl %>%
  group_by(symbol) %>%
  tq_performance(Ra = monthly.returns, 
                 performance_fun = SharpeRatio.annualized, 
                 scale = 12, 
                 Rf    = 0) %>%
  ungroup() %>%
  mutate(symbol = as_factor(symbol)) %>%
  
  ggplot(aes(symbol, `AnnualizedSharpeRatio(Rf=0%)`)) +
  geom_col(fill = "#2c3e50") +
  geom_text(aes(label = `AnnualizedSharpeRatio(Rf=0%)` %>% round(2)), 
            color = "white", nudge_y = -0.05) +
  theme_tq() +
  labs(
    title = "Sharpe Ratio: Stocks vs Benchmark",
    x = "", y = "Annualized Sharpe Ratio (Rf = 0%)"
  )

#### Portolio Aggregation  ####

wts_tbl  <- returns_m_components_tbl %>%
  distinct(symbol) %>%
  mutate(weights = c(0.083, 0.083, 0.083, 0.083, 
                     0.083, 0.0833, 0.083, 0.083, 0.083, 0.083, 0.083, 0.083))

wts_tbl

returns_m_portfolio_tbl <- returns_m_components_tbl %>%
  tq_portfolio(symbol, RA, 
               weights = wts_tbl,
               rebalance_on = "quarters",
               col_rename   = "monthly.returns"
  )

returns_m_portfolio_tbl


