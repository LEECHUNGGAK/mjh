# Set Environment ---------------------------------------------------------
library(tidyverse)
library(readxl)

master_df <- read_csv("C:/Users/Administrator/wd/alzheimer/data/master_data_t2_v3.csv")

x_df <- master_df %>% 
    drop_na(top3b_three_snv) %>% 
    select(id, top3b_three_snv) %>% 
    group_by(top3b_three_snv) %>% 
    sample_n(25)

for (id_var in x_df$id) {
    tmp_df <- read_(file.path("data/raw/NGS_result", paste0(id_var, ".xls")))

}
