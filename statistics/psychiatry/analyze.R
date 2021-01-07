library(tidyverse)

setwd("c:/Users/Administrator/wd/statistics/psychiatry")

raw <- read_csv("data/data.csv", col_types = cols(.default = "c"))

dat1 <- raw %>% 
    select(1:str_which(colnames(raw), "sex2")-1) %>% 
    select_if(~sum(!is.na(.)) > 0)
## Remove empty rows
## Divide raw into three data frames

