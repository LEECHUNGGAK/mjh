# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)

d <- read_excel("C:/Users/Administrator/work/1_임상의학연구소/1_통계/orthopedic_surgery/data/21.03.29 수정 SBS 논문 data.xlsx",
                col_types = "text",
                skip = 1) %>% 
    select(40, 5) %>% 
    rename(sbs = 1, age = 2) %>% 
    mutate_all(as.integer) %>% 
    mutate(sbs = as.character(sbs)) %>% 
    drop_na(sbs)

# Analyze -----------------------------------------------------------------
ggplot(data = d, aes(x = age)) +
    geom_boxplot()

d %>% 
    filter(age > 0) %>% 
    group_by(sbs) %>% 
    summarize(n = sum(!is.na(sbs)),
              mean = mean(age, na.rm = TRUE),
              sd = sd(age, na.rm = TRUE),
              se = sd / sqrt(n))
