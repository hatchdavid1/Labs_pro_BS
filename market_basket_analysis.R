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


