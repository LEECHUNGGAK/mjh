# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)

d_raw <- read_excel("C:/Users/Administrator/work/1_statistics/orthopedic_surgery/rTSA total list before looking at SBS 최종.xlsx",
                    col_types = "text",
                    skip = 1) %>% 
    rename(post_er_at_side = 26, post_er_90 = 27, post_ir_90 = 28,
           post_ir_back = 29, post_er_strength = 30, post_ir_strength = 31,
           sbs = 38)

v <- c("post_er_at_side", "post_er_90", "post_ir_90", "post_ir_back",
       "post_er_strength", "post_ir_strength")

# Analyze -----------------------------------------------------------------
for (i in v) {
    aov(i ~ sbs, data = d_raw)
}