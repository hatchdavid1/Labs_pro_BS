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
data_joined_tbl  <- data_joined_tbl %>%
    mutate_if(is.character, as.factor)

# Converting the df to tan h2oFrame
train  <- as.h2o(data_joined_tbl)

# Quick h2oFrame view
h2o.describe(train)

# Specifying the dependant feature
y  <- "TERM_DEPOSIT" 

# Create a new df excluding the dependant variable
x  <- setdiff(names(train), c(y, 'ID'))
    
# Executing an automl run for 15 mdodels
aml  <- h2o.automl(
    y= y,  
    x = x, 
    training_frame = train, 
    project_name = 'term_deposit', 
    max_models = 15, 
    seed = 42)

# Getting the result leaderboard for the 15 models 
lb  <- aml@leaderboard

# Printing the general result from leaderboard
print(lb)

# Printing just the results for each model 
print(lb, n = nrow(lb))

# Exploring the results
# Getting the ids for each model from the autmoml
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]

# Getting all models as one stacked ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])

# Getting the metalearner which is the stacked ensembled called from h2o
metalearner <- h2o.getModel(se@model$metalearner$name)

# Showing the feature importance for the metalearner or stacked ensemnble algorithm
h2o.varimp(metalearner)

# Plotting the base learner contributions to the ensemble.
h2o.varimp_plot(metalearner)


# 4.5 Variable Importance ----

# Getting the Xgboost which is the second best in order to compare
xgb <- h2o.getModel(grep("XGBoost", model_ids, value = TRUE)[1])

# Showing the feature importance for the top XGBoost model
h2o.varimp(xgb)

# Plotting the base learner contributions to the ensemble.
h2o.varimp_plot(xgb)
    
