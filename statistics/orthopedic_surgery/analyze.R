# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)

v <- c("post_er_at_side", "post_er_90", "post_ir_90", "post_ir_back",
       "post_er_strength", "post_ir_strength")

d_raw <- read_excel("C:/Users/Administrator/work/1_통계자문/orthopedic_surgery/OS 이용걸교수님 논문통계데이터.xlsx",
                    col_types = "text",
                    skip = 1) %>% 
    rename(post_er_at_side = 27, post_er_90 = 28, post_ir_90 = 29,
           post_ir_back = 31, post_er_strength = 32, post_ir_strength = 33,
           sbs = 40) %>% 
    select(sbs, all_of(v)) %>% 
    mutate(sbs = as.factor(as.integer(sbs)),
           post_er_at_side = as.integer(post_er_at_side),
           post_er_90 = as.integer(post_er_90),
           post_ir_90 = as.integer(post_ir_90),
           post_ir_back = as.integer(post_ir_back),
           post_er_strength = as.double(post_er_strength),
           post_ir_strength = as.double(post_ir_strength)) %>% 
    drop_na(sbs)


# Analyze -----------------------------------------------------------------
for (i in v) {
    aov(i ~ sbs, data = d_raw)
}