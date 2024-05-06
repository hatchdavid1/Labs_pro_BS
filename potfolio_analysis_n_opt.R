## Portfolio Analysis and optimization 
#### Loading main Libraries #### 
library(tidyquant)
library(tidyverse)
library(dplyr)
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

#### Portfolio Aggregation  ####

wts_tbl  <- returns_m_components_tbl %>%
  distinct(symbol) %>%
  mutate(weights = c(0.083, 0.083, 0.083, 0.083, 
                     0.083, 0.0833, 0.083, 0.083, 0.083, 0.083, 0.083, 0.083))

wts_tbl

returns_m_portfolio_tbl <- returns_m_components_tbl %>%
  tq_portfolio(symbol, monthly.returns, 
               weights = wts_tbl,
               rebalance_on = "quarters",
               col_rename   = "monthly.returns"
  )

returns_m_portfolio_tbl

#### Performance Analysis and Portfolio ####
returns_m_portfolio_merged_m_tbl  <- returns_m_portfolio_tbl %>% 
  add_column(symbol = 'Portfolio', .before = 1) %>%
  bind_rows(returns_m_benchmark_tbl)

returns_m_portfolio_merged_m_tbl %>% 
  group_by(symbol) %>% 
  tq_performance(monthly.returns, 
                 performance_fun = SharpeRatio.annualized, 
                scale = 12)

#### Optimization #### 
# Getting Random Portfolio weights
weight_iterator <- function(assets, iter = 100, seed = NULL) {
  
  n <- length(assets)
  
  if (!is.null(seed)) set.seed(seed)
  mtx <- matrix(runif(n = iter*n, min = 0, max = 1), nrow = 12)
  
  mtx_normalized <- mtx %*% diag(1/colSums(mtx))
  
  vectorized_output <- as.vector(mtx_normalized)
  
  return(vectorized_output)
  
}

# Inputs
assets <- c('BAC', 'CAT', 'CPNG', 'DIS', 'ECL', 'FICO', 'KOF', 'ONON', 'STLA', 'VLTO', 'WFC', 'WM')
iter  <- 250

# Generating Random Portfolios
weights_tbl <- tibble(
  portfolio_id = rep(1:iter, each = length(assets)),
  symbol  = rep(assets, times = iter),
  weights = weight_iterator(assets, iter = iter, seed = 123) 
) %>%
  group_by(portfolio_id)

# Calculating Performance for each portfolio 
plan("multisession")
# Original -> plan(multiprocess)
portfolio_optim_tbl <- weights_tbl %>%
  #nest(.key = portfolio_weights) %>% # .key is deprecated (all this did was name the new column)
  nest() %>%
  rename(portfolio_weights = data) %>%
  
  # Map tq_portfolio() to nested weights
  mutate(portfolio_agg = furrr::future_pmap(portfolio_weights, ~ tq_portfolio(
    data = returns_m_components_tbl,
    assets_col  = symbol, 
    returns_col = monthly.returns,
    weights     = .x,
    rebalance_on = "quarters"
  ))) %>%
  
  # Map tq_performance() to nested portfolio aggregations
  mutate(sharp_ratio = map(portfolio_agg, ~ tq_performance(
    data = .x,
    Ra = portfolio.returns,
    performance_fun = SharpeRatio.annualized,
    scale = 12
  ))) 

portfolio_optim_tbl


best_portfolio_tbl <- portfolio_optim_tbl %>%
  unnest(sharp_ratio) %>%
  filter(`AnnualizedSharpeRatio(Rf=0%)` == max(`AnnualizedSharpeRatio(Rf=0%)`)) 

best_portfolio_tbl %>%
  select(portfolio_id, `AnnualizedSharpeRatio(Rf=0%)`)

best_portfolio_tbl %>%
  pull(portfolio_weights) %>%
  pluck(1)


portfolio_optim_flattened_tbl <- portfolio_optim_tbl %>%
  select(-portfolio_agg) %>%
  unnest(sharp_ratio) %>%
  unnest(portfolio_weights) %>%
  spread(symbol, weights) %>%
  rename(SharpeRatio = `AnnualizedSharpeRatio(Rf=0%)`)

portfolio_optim_flattened_tbl


