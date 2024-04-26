#### Anomalize Lab #### 
#### Loading main libraries ####
library(vroom)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(rsample)
library(tibbletime)
library(anomalize)

#### General Structure #### 
# Loading data  (Kaggle competition)
websites_raw_tbl  <- vroom::vroom('/Users/davidhatch/Documents/labs_pro/lab_anomalize_package/lab_12_r_programming_rlang/data/train_1.csv', delim = ',')

websites_raw_tbl <- websites_raw_tbl %>%
  rowid_to_column(var = "id")

set.seed(123)
websites_sample_tbl <- websites_raw_tbl %>%
  sample_n(size = 9) %>%
  gather(key = "date", value = "visits", -id, -Page) %>%
  replace_na(replace = list(visits = 0)) %>%
  mutate(date = ymd(date))

websites_sample_tbl %>%
  ggplot(aes(date, visits, color = Page)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Page, scales = "free_y") +
  theme_tq() + 
  scale_color_viridis_d() +
  theme(legend.position = "none") +
  expand_limits(y = 0)

#### Anomaly Detection #### 

websites_sample_tbl %>%
  
  filter(Page %>% str_detect("Kit_Harington")) %>%
  
  time_decompose(target = visits, method = "stl") %>%
  anomalize(target = remainder, method = "iqr", alpha = 0.3) %>%
  time_recompose() %>%
  
  plot_anomalies(time_recomposed = TRUE)

websites_sample_tbl %>%
  
  group_by(Page) %>%
  
  time_decompose(target = visits, method = "stl") %>%
  anomalize(target = remainder, method = "iqr", alpha = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(ncol = 3, time_recomposed = TRUE)