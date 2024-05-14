# Webscraping tutorial 
#### Loading Main Libraries ####
library(tidyverse)
library(rvest)
library(furrr)
library(fs)
library(xopen)

#### Collecting data from the url #### 
url  <- "https://www.cannondale.com/en/USA/Products/ProductCategory.aspx?nid=d785e3b8-7c0e-4145-9713-6023539b88fa"
xopen(url)

road_bikes_tbl  <- tibble(
    product_family = c("Endurance Road", "Elite Road", "Gravel - All Road", "Cyclocross",
                     "TT and TRI"), 
    nid = c("d785e3b8-7c0e-4145-9713-6023539b88fa",
            "39601f9a-273e-4fb0-9b81-f79c21570c4a",
            "0a51292a-c51b-4434-bdec-16a9317b9111",
            "53adef1d-8853-4f05-a36c-30cea667b6ec",
            "7388ffd1-a7df-443c-88b5-31a1210460b8")
) %>%
    add_column(category = 'Road', .before = 1 )

road_bikes_tbl

mountain_bikes_tbl <- tibble(
    product_family = c("Cross Country", "Trail", "All Mountain", "Enduro", 
                       "Sport", "Fat Bike"),
    nid = c(
        "c4f0dee5-d6fb-489a-9624-11286eba7b94",
        "ec39d358-0637-4436-9e68-d65316db8fb2",
        "480c9613-1c53-4432-9b58-e515215f1501",
        "919ae01a-f75e-464a-8e70-c5f4730be7f3",
        "5bcdf9e1-acbc-4254-8c5f-0815ddcc0144",
        "af33c7a2-16b2-4045-a4c3-904acf0b6b40"
    )
) %>%
    add_column(category = "Mountain", .before = 1)

urban_fitness_tbl <- tibble(
    product_family = c("Urban", "Fitness"),
    nid = c(
        "85bbe445-6160-4420-b7ff-e602ddeda578",
        "145e116a-7b63-4cc3-bfa8-5ce153a55c45"
    )
) %>%
    add_column(category = "Urban & Fitness", .before = 1)

urban_fitness_tbl

electric_tbl <- tibble(
    product_family = c("Mountain", "Road", "Fitness", "Urban"),
    nid = c(
        "fcc56448-6bcd-418d-ac5d-b50e7a8da098",
        "231e8af7-f448-4f96-9a10-9cdbdfc087c4",
        "3db2e803-0f38-40a1-91c7-0fefe5e90a86",
        "c9ec62c4-d857-4d3d-aba6-d094f84e058b"
        
    )
) %>%
    add_column(category = "Electric", .before = 1)

electric_tbl

#### Combining product Families ####
bike_category_nids_tbl <- bind_rows(
    road_bikes_tbl, mountain_bikes_tbl, urban_fitness_tbl, electric_tbl) %>%
    mutate(
        url = str_glue("https://www.cannondale.com/en/USA/Products/ProductCategory.aspx?nid={nid}")
    )

bike_category_nids_tbl

#### Examine Product Family Page #### 
url <- "https://www.cannondale.com/en/USA/Products/ProductCategory.aspx?nid=85bbe445-6160-4420-b7ff-e602ddeda578"
xopen(url)

html <- read_html(url)

html

product_id_tbl <- html %>% 
    html_nodes(".relatedProducts") %>%
    html_attr("id") %>%
    enframe(name = "position", value = "product_id") %>%
    mutate(url = str_glue("https://www.cannondale.com/en/USA/Bike/ProductDetail?Id={product_id}&parentid=undefined"))

product_id_tbl

#### Function to get Bike IDs
get_bike_ids <- function(url) {
    read_html(url) %>%
        html_nodes(".relatedProducts") %>%
        html_attr("id") %>%
        enframe(name = "position", value = "product_id") %>%
        mutate(url = str_glue("https://www.cannondale.com/en/USA/Bike/ProductDetail?Id={product_id}&parentid=undefined"))
    
}

get_bike_ids(url)


#### Scale up to all Product Categories #### 
plan("multisession")
bike_ids_tbl <- bike_category_nids_tbl %>%
    mutate(bike_ids = future_map(url, get_bike_ids))

bike_ids_tbl

bike_ids_tbl <- bike_ids_tbl %>%
    select(-url) %>%
    unnest(bike_ids)

#### Webscrape the bikes information #### 

url <- "https://www.cannondale.com/en/USA/Bike/ProductDetail?Id=794906db-5bb4-4c36-b6dd-a98682c57be0&parentid=undefined"
xopen(url)

read_html(url) %>%
    html_nodes(".productTitleHeader") %>%
    html_text()

read_html(url) %>%
    html_nodes(".price") %>%
    html_text()

read_html(url) %>%
    html_nodes("#componentBox") %>%
    html_nodes(".overview") %>%
    html_nodes(".cell") %>%
    html_nodes("h4") %>%
    map(html_text) %>%
    unlist()

read_html(url) %>%
    html_nodes("#componentBox") %>%
    html_nodes(".overview") %>%
    html_nodes(".cell") %>%
    html_nodes("p") %>%
    map(html_text) %>%
    unlist() %>%
    str_remove_all("\n") %>%
    str_remove_all("\r") %>%
    str_trim()

#### Function to get bike Features #### 
get_bike_features <- function(url) {
    
    bike_name <- read_html(url) %>%
        html_nodes(".productTitleHeader") %>%
        html_text() %>%
        str_remove_all("\r") %>%
        str_remove_all("\n")
    
    price <- read_html(url) %>%
        html_nodes(".price") %>%
        html_text()
    
    features <- read_html(url) %>%
        html_nodes("#componentBox") %>%
        html_nodes(".overview") %>%
        html_nodes(".cell") %>%
        html_nodes("h4") %>%
        map(html_text) %>%
        unlist()
    
    feature_descriptions <- read_html(url) %>%
        html_nodes("#componentBox") %>%
        html_nodes(".overview") %>%
        html_nodes(".cell") %>%
        html_nodes("p") %>%
        map(html_text) %>%
        unlist() %>%
        str_remove_all("\n") %>%
        str_remove_all("\r") %>%
        str_trim()
    
    tibble(
        features = features,
        description = feature_descriptions
    ) %>%
        add_row(features = "Price", description = price, .before = 1) %>%
        add_row(features = "Model", description = bike_name, .before = 1)
    
}

get_bike_features(url)

#### Scale all bike features ### 
bike_features_raw_tbl <- bike_ids_tbl %>%
    mutate(bike_features = map(url, get_bike_features))

## Parallel Process with furrr 
plan('multisession')
bike_features_raw_tbl <- bike_ids_tbl %>%
    mutate(bike_features = future_map(url, get_bike_features))


#### Creating Directory #### 
fs::dir_create("data")
bike_features_raw_tbl %>% write_rds("data/bike_features_raw_tbl1.rds")

bike_features_raw_tbl <- read_rds("data/bike_features_raw_tbl1.rds")

##### Post Processing the Directories #### 
cannondale_bikes_2019_tbl <- bike_features_raw_tbl %>%
    select(-url) %>%
    unnest() %>%
    mutate(
        category = as_factor(category),
        product_family = as_factor(product_family),
        features = as_factor(features)) %>%
    spread(key = features, value = description) %>%
    rename(ProductFamily = product_family,
           WebsitePosition = position,
           ProductId = product_id) %>%
    rename_all(~str_remove_all(., " "))

cannondale_bikes_2019_tbl



cannondale_bikes_2019_tbl %>%
    write_csv("data/cannondale_bikes_20191.csv")

read_csv("data/cannondale_bikes_20191.csv")















