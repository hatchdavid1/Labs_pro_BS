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


#### Simulation #### 
tic()
portfolio_sim_result_tbl  <- seq(.1, .5, lenght.out = 20) %>% 
    map_dfr(optimize_portfolio)
toc()

portfolio_sim_result_tbl

#### Visualization #### 
# Heatmap 
plot_heatmap <- function(data) {
    
    data_transformed_tbl <- data %>%
        mutate(sharpe_ratio = portfolio_return / portfolio_stdev) %>%
        mutate(portfolio_id = row_number()) %>%
        gather(key = stock, value = weight,
               -sharpe_ratio, -portfolio_return, -portfolio_stdev, 
               -portfolio_id, -return_constraint,
               factor_key = TRUE) %>%
        mutate(return_objective = scales::percent(return_constraint)) %>%
        mutate(label_text = str_glue("Return Objective: {scales::percent(return_constraint)}
                                     Portfolio Return: {scales::percent(portfolio_return)}
                                     Portfolio Sharpe: {round(sharpe_ratio, 2)}
                                     Portfolio StdDev: {round(portfolio_stdev, 2)}"))
    
    g <- data_transformed_tbl %>%
        ggplot(aes(stock, y = return_objective, fill = weight)) +
        geom_tile() +
        geom_point(aes(text = label_text), size = 0.1, alpha = 0) +
        scale_fill_gradient(low = "#FFFFFF", high = "#2c3e50") +
        geom_text(aes(label = scales::percent(weight)), size = 3) +
        theme_tq() +
        labs(title = "Optimized Portfolio Weights", x = "Stock", y = "Return Objective")
    
    ggplotly(g, tooltip = "text")
    
}

portfolio_sim_result_tbl %>% plot_heatmap()













