#### Loading Libraries ##### 

# General libraries 
library(tidyverse)

# Visualization libraries
library(tidyquant)
library(plotly)

# Modeling libraries
library(parsnip)
library(rsample)
library(yardstick)
library(broom)

# Connector libraries
library(rpart)
library(rpart.plot)
library(xgboost)

# Loading pre built functions 
source('/Users/davidhatch/Documents/labs_pro/lab_6_1_parsnip/intro_to_parsnip/scripts/plot_price_vs_weight.R')
source('/Users/davidhatch/Documents/labs_pro/lab_6_1_parsnip/intro_to_parsnip/scripts/calc_metrics.R')
source('/Users/davidhatch/Documents/labs_pro/lab_6_1_parsnip/intro_to_parsnip/scripts/plot_predictions.R')

### Setting up the Data ####
# Loading Data
price_vs_weight_tbl  <- read_csv('/Users/davidhatch/Documents/labs_pro/lab_6_1_parsnip/intro_to_parsnip/data/price_vs_weight_tbl.csv')

# Quick view to price vs weight tbl
price_vs_weight_tbl

# Loading pre engineered features
engineered_features_tbl  <- read_csv('/Users/davidhatch/Documents/labs_pro/lab_6_1_parsnip/intro_to_parsnip/data/engineered_features.csv')


# Quick view to engineered features
engineered_features_tbl

# Visualization function for price vs weight
plot_price_vs_weight()

# Joining data and removing values with low counts for Product Families
pricing_model_tbl  <- price_vs_weight_tbl %>% 
    left_join(engineered_features_tbl, by= 'row_id' ) %>%
    filter(!(ProductFamily %in% c('Trail', 'TT and TRI')))

# Quick vire to pricing model tbl
pricing_model_tbl

### Data Splitting ###
# Setting the seed
set.seed(1)
# Splitting the set into 80/20 proportion to train and test
split_obj  <- rsample::initial_split(pricing_model_tbl, prop = .8, 
                                     strata='ModelBase' ) 

# Splitting into train and test sets using pre load functions training & testing
train_tbl  <- split_obj %>% training()
test_tbl  <- split_obj %>% testing()

### Machine Learning with parsnip #### 
### Linear Regression ###
?linear_reg # Step 1 Picking a parsnip algorithm and setting parameters
?set_engine # Step 2 Set an engine, Retunrs a model spec
?fit.model_spec # Step 3 Fit model specifications to data






