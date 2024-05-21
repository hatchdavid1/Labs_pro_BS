#### Market Basket Analysis #### 
#### Loading Main Libraries 
# Core & Viz
library(vroom)
library(tidyverse)
library(tidyquant)
library(plotly)

# Modeling 
library(recommenderlab)
library(arules)
library(arulesViz)

#### Data #### 
orders_products_tbl  <- vroom("/Users/davidhatch/Documents/labs_pro/lab_market_basket/learning_lab_11_market_basket_analysis/00_Data/order_products__train.csv", delim = ",")
orders_products_tbl

orders_tbl  <- vroom("/Users/davidhatch/Documents/labs_pro/lab_market_basket/learning_lab_11_market_basket_analysis/00_Data/orders.csv", delim = ",")
orders_tbl

products_tbl  <- vroom("/Users/davidhatch/Documents/labs_pro/lab_market_basket/learning_lab_11_market_basket_analysis/00_Data/products.csv", delim = ",")
products_tbl

aisles_tbl  <- vroom("/Users/davidhatch/Documents/labs_pro/lab_market_basket/learning_lab_11_market_basket_analysis/00_Data/aisles.csv", delim = ",")
aisles_tbl

departmentes_tbl  <- vroom("/Users/davidhatch/Documents/labs_pro/lab_market_basket/learning_lab_11_market_basket_analysis/00_Data/departments.csv", delim = ",")
departmentes_tbl


#### Data Understanding #### 
orders_combined_tbl  <- orders_products_tbl %>% 
    left_join(orders_tbl) %>% 
    left_join(products_tbl) %>% 
    left_join(aisles_tbl) %>% 
    left_join(departmentes_tbl) %>% 
    select(eval_set, user_id, 
           contains("order"), contains("product"), 
           contains("aisle"), contains("department"), everything())

orders_combined_tbl %>% glimpse()

#### Which products are purchased most frequently 
item_frequency_tbl  <- orders_combined_tbl %>% 
    count(product_name, product_id, aisle, department) %>% 
    arrange(desc(n)) %>% 
    mutate(
        pct = n / sum(n), 
        cumulative_pct = cumsum(pct), 
        popular_product = ifelse(cumulative_pct  <= .5, "Yes", "No")
    ) %>% 
    rowid_to_column(var = "rank") %>% 
    mutate(label_text = str_glue("Rank: {rank}
                                 Product: {product_name}
                                 ProductID: {product_id}
                                 Aisle: {aisle}
                                 Department: {department}
                                 Count: {n}
                                 Pct: {scales::percent(pct)}
                                 Cumulative Pct: {scales::percent(cumulative_pct)}"))

item_frequency_tbl

g  <- item_frequency_tbl %>% 
    slice(1:5000) %>% 
    ggplot(aes(rank, n)) + 
    geom_point(aes(size = n, color = popular_product, text = label_text), alpha = .2) + 
    theme_tq() +
    scale_color_tq() + 
    theme(legend.direction = "vertical", 
          legend.position = "right") + 
    labs(title = " Item Frequency", 
         subtitle = "Top Items Account for Majority of Purchases")

ggplotly(g, tooltip = 'text')


#### Frequency from customers side #### 
user_frequency_tbl  <- orders_combined_tbl %>% 
    distinct(user_id, order_id) %>% 
    count(user_id) %>% 
    arrange(desc(n)) %>% 
    mutate(
        pct = n/sum(n), 
        cumulative_pct = cumsum(pct), 
        popular_customer = ifelse(cumulative_pct  <= .5, "Yes", "No") 
    ) %>% 
    rowid_to_column(var = "rank")

user_frequency_tbl

g <- user_frequency_tbl %>%
    slice(1:5000) %>%
    ggplot(aes(rank, n)) +
    geom_point(aes(size = n, color = popular_customer), alpha = 0.2) +
    theme_tq() +
    scale_color_tq() +
    theme(legend.direction = "vertical", 
          legend.position  = "right") +
    labs(title = "User Frequency", 
         subtitle = "How Often Do You Shop? - No Frequency! Everyone is 1st time.")

ggplotly(g)


##### Products per Customer frequency #### 
user_item_frequency_tbl <- orders_combined_tbl %>%
    count(user_id) %>%
    arrange(desc(n)) %>%
    mutate(
        pct = n / sum(n),
        cumulative_pct = cumsum(pct),
        popular_customer = ifelse(cumulative_pct <= 0.5, "Yes", "No")
    ) %>%
    rowid_to_column(var = "rank") 


user_item_frequency_tbl

g <- user_item_frequency_tbl %>%
    slice(1:10000) %>%
    ggplot(aes(rank, n)) +
    geom_point(aes(size = n, color = popular_customer), alpha = 0.2) +
    theme_tq() +
    scale_color_tq() +
    theme(legend.direction = "vertical", 
          legend.position  = "right") +
    labs(title = "User Frequency", 
         subtitle = "Yes - Some Customers have larger baskets")

ggplotly(g)

#### Condensing User-item / Transaction-item Matriz for evaluation #### 
# Popular products 
top_products_vec <- item_frequency_tbl %>% 
    filter(popular_product == 'Yes') %>% 
    pull(product_name)

# Filtering by names 
top_products_basket_vec <- orders_combined_tbl %>%
    filter(product_name %in% top_products_vec)

# Large Basket 
top_user_vec  <- user_item_frequency_tbl %>% 
    filter(rank  < 2500) %>% 
    pull(user_id)

market_basket_condensed_tbl  <- top_products_basket_vec %>%
    filter(user_id %in% top_user_vec)

market_basket_condensed_tbl

#### Transforming data to a Binary Rating Matrix #####
# Basket contains item 1 or 0 

user_item_tbl  <- market_basket_condensed_tbl %>% 
    select(user_id, product_name) %>% 
    mutate(value = 1) %>% 
    spread(product_name, value, fill = 0)

user_item_rlab <- user_item_tbl %>% 
    select(-user_id) %>% 
    as.matrix() %>% 
    as('binaryRatingMatrix')

user_item_rlab

# Calculating relationships using arules package 
user_item_rlab@data

user_item_rlab@data %>% summary()

user_item_rlab@data %>% glimpse()

# "dev.off()"
itemFrequencyPlot(user_item_rlab@data, topN = 20, type = 'absolute', 
                  xlab = "Items", ylab = "Frequency (absolute)", 
                  col = "streetblue", 
                  main = "Absolute Frequency Plot")
# Creating recipes 
recommenderRegistry$get_entries()

eval_recipe <- user_item_rlab %>%
    evaluationScheme(method = "cross-validation", k = 5, given = -1)

eval_recipe

# Association Rules 
algorithms_list <- list(
    "association rules1"   = list(name  = "AR", 
                                  param = list(supp = 0.01, conf = 0.01)),
    "association rules2"  = list(name  = "AR", 
                                 param = list(supp = 0.01, conf = 0.1)),
    "association rules3"  = list(name  = "AR", 
                                 param = list(supp = 0.01, conf = 0.5)),
    "association rules4"  = list(name  = "AR", 
                                 param = list(supp = 0.1, conf = 0.5))
)

results_rlab_arules <- eval_recipe %>%
    recommenderlab::evaluate(
        method    = algorithms_list, 
        type      = "topNList", 
        n         = 1:10)

plot(results_rlab_arules, annotate = TRUE)


# All algorithms 
algorithms_list <- list(
    "random items"        = list(name  = "RANDOM",
                                 param = NULL),
    "popular items"       = list(name  = "POPULAR",
                                 param = NULL),
    "user-based CF"       = list(name  = "UBCF",
                                 param = list(method = "Cosine", nn = 500)),
    "item-based CF"       = list(name  = "IBCF",
                                 param = list(k = 5)),
    "association rules2"  = list(name  = "AR", 
                                 param = list(supp = 0.01, conf = 0.1))
)


results_rlab <- eval_recipe %>%
    recommenderlab::evaluate(
        method    = algorithms_list, 
        type      = "topNList", 
        n         = 1:10)

plot(results_rlab, annotate = TRUE)


##### Building Models #### 
# Association Rules model 
model_ar <- recommenderlab::Recommender(
    data = user_item_rlab, 
    method = "AR", 
    param = list(supp = 0.01, conf = 0.10))

# UBCF Model 
model_ucbf <- recommenderlab::Recommender(
    data = user_item_rlab, 
    method = "UBCF", 
    param  = list(method = "Cosine", nn = 500))

#### Checking relationships #### 
rules <- model_ar@model$rule_base

inspectDT(rules)

plot(rules, method = "scatterplot", 
     marker = list(opacity = .7, size = ~lift), 
     colors = c("blue", "green"),
     engine = "plotly")

sort(rules, by = "lift", decreasing = TRUE)[1:20] %>%
    inspect() 

plot(rules, method = "graph")

plot(model_ar@model$rule_base, method = "graph", 
     control=list(layout=igraph::in_circle()))

#### Prediction #### 
# Preparing data 
new_user_basket <- c("Banana", "Organic Whole Milk")

new_user_basket_rlab <- tibble(items = user_item_rlab@data %>% colnames()) %>%
    mutate(value = as.numeric(items %in% new_user_basket)) %>%
    spread(key = items, value = value) %>%
    as.matrix() %>%
    as("binaryRatingMatrix")

new_user_basket_rlab

# Association Rules
prediction_ar <- predict(model_ar, newdata = new_user_basket_rlab, n = 3)

tibble(items = prediction_ar@itemLabels) %>%
    slice(prediction_ar@items[[1]])

# UBCF
prediction_ucbf <- predict(model_ucbf, newdata = new_user_basket_rlab, n = 3)

tibble(items = prediction_ucbf@itemLabels) %>%
    slice(prediction_ucbf@items[[1]])





