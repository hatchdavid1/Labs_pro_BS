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