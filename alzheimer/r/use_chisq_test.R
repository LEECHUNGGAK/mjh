library(tidyverse)

source("C:/Users/Administrator/wd/mjh/packages/lcgtool/print_stats.R")

setwd("C:/Users/Administrator/wd/alzheimer")

dat <- read_csv("data/ml/ml_top3b_t2.csv") %>% 
    mutate(top3b_three_snv = ifelse(
        coordinate_22312315 + coordinate_22312350 + coordinate_22312351 == 3,
        1,
        0))

print_chisq_test(dat, "top3b_three_snv", "dementia")
