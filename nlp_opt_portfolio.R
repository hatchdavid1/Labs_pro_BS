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
assets <- c ("META", "AMZN", "AAPL", "GOOG", "NFLX") %>% sort()

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
# Static variables 
cov_mtx  <- cov(returns_matrix_tbl)

stats_tbl  <- stock_return_tbl %>% 
    summarise(
        mean = mean(returns), 
        stdev = sd(returns)
    )

#### Functions #### 
calc_portfolio_variance  <- function(weights){
    t(weights) %*% (cov_mtx %*% weights) %>% as.vector()
}

calc_portfolio_variance(c(1,0,0,0,0))

calc_portfolio_return  <- function(weights){
    stats  <- stats_tbl$mean
    sum(stats * weights)
}

calc_portfolio_return(c(1,0,0,0,0))

#### Objective #### 
n_assets <- length(assets)

model_nlp <- OP(
    objective   = F_objective(F = calc_portfolio_variance, n = n_assets, names = assets),
    constraints = rbind(
        F_constraint(F = calc_portfolio_return, dir = ">=", rhs = 0.40),
        
        L_constraint(diag(n_assets), rep(">=", n_assets), rep(0, n_assets)),
        L_constraint(diag(n_assets), rep("<=", n_assets), rep(1, n_assets)),
        L_constraint(rep(1, n_assets), "==", 1)
    ),
    maximum = FALSE
)

tic()
sol <- ROI_solve(model_nlp, solver = "alabama", start = rep(1/n_assets, n_assets))
toc()

sol$objval

solution(sol) %>% round(2)

#### Simulation - Iterative Optimiaztion #### 
optimize_portfolio <- function(required_return = 0.4) {
    
    model_nlp <- OP(
        objective   = F_objective(F = calc_portfolio_variance, n = n_assets, names = assets),
        constraints = rbind(
            F_constraint(F = calc_portfolio_return, dir = ">=", rhs = required_return),
            L_constraint(diag(n_assets), rep(">=", n_assets), rep(0, n_assets)),
            L_constraint(diag(n_assets), rep("<=", n_assets), rep(1, n_assets)),
            L_constraint(rep(1, n_assets), "==", 1)
        ),
        maximum = FALSE
    )
    
    sol <- ROI_solve(model_nlp, solver = "alabama", start = rep(1/n_assets, n_assets))
    
    return(
        bind_cols(
            tibble(return_constraint = required_return),
            tibble(portfolio_return  = calc_portfolio_return(sol$solution)),
            tibble(portfolio_stdev   = (sol$objval)^0.5),
            enframe(sol$solution) %>% spread(key = name, value = value))
    )
    
}

optimize_portfolio(0.4) 




