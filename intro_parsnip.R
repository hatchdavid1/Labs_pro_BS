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
#### Linear Regression ####
?linear_reg # Step 1 Picking a parsnip algorithm and setting parameters
?set_engine # Step 2 Set an engine, Retunrs a model spec
?fit.model_spec # Step 3 Fit model specifications to data

# Specify Model, Setting Engine and Fitting Model to Data
model_01_lm  <- linear_reg('regression') %>%
    set_engine('lm') %>% 
    fit(Price_num ~Category + ProductFamily + Weight_lb, 
        data = train_tbl %>% select(-row_id, Model))

# Getting the Prediction 
model_01_lm %>% predict(new_data = test_tbl)

# Visualizing the results for the Linear Regression using the pre load plot predictions function 
model_01_lm %>% plot_predictions(new_data = test_tbl)

# Calculating Linear Regression Performance with calc metrics pre loaded function
model_01_lm %>% calc_metrics(new_data = test_tbl, truth = Price_num)

#### Linear Regression with Engineered Features ####
model_02_lm  <- linear_reg('regression') %>%
    set_engine('lm') %>% 
    fit(Price_num ~., 
        data = train_tbl %>% select(-row_id, -Model))

# Getting the predictions
model_02_lm %>% predict(new_data = test_tbl)

# Visualizing the results for the Linear Regression using the pre load plot predictions function 
model_02_lm %>% plot_predictions(new_data = test_tbl)

# Calculating Linear Regression Performance with calc metrics pre loaded function
model_02_lm %>% calc_metrics(new_data = test_tbl, truth = Price_num)


# Explanation | which features are generating the new and better results 
model_02_lm$fit %>% broom::tidy() %>%
    arrange(p.value)

##### Decision Trees ####
# Parameter for model selected previously by doing a Cross Validation
model_03_rpart  <- decision_tree(
    mode = 'regression', 
    cost_complexity = .0001, 
    tree_depth = 5, 
    min_n = 6) %>% set_engine('rpart') %>%
    fit(Price_num ~., data = train_tbl %>% select(-row_id, -Model))

# Calculating Decision Trees metrics with calc
model_03_rpart %>% calc_metrics(test_tbl, truth = Price_num)

# Plotting decision Trees Results with plot_predictions pre loaded function
model_03_rpart %>% plot_predictions(new_data = test_tbl)

# Explanation | which features are generating the new and better results 
model_03_rpart$fit %>% 
    rpart.plot(
        fallen.leaves = FALSE, 
        extra = 101, 
        roundint = FALSE, 
        main = 'Model 03: Decision Tree', 
        cex = .8
    )
#### XGBoost ####
# Parameter for model selected previously by doing a Cross Validation
model_04_xgboost  <- boost_tree(
    mode = 'regression', 
    mtry = 30, 
    trees = 500, 
    min_n = 2, 
    tree_depth = 6, 
    learn_rate = .35, 
    loss_reduction = .0001) %>%
    set_engine('xgboost') %>%
    fit(Price_num ~., data = train_tbl %>% select(-row_id, -Model))

# Calculating Decision Trees metrics with calc
model_04_xgboost %>% calc_metrics(test_tbl, truth = Price_num)

# Plotting decision Trees Results with plot_predictions pre loaded function
model_04_xgboost %>% plot_predictions(new_data = test_tbl)

# Explanation | which features are generating the new and better results 
model_04_xgboost$fit %>%
    xgb.importance(model = .) %>%
    xgb.plot.importance(main = "XGBoost Feature Importance")

