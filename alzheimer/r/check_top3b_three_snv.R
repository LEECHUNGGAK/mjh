# Set Environment ---------------------------------------------------------
library(tidyverse)
library(readxl)

master_df <- read_csv("C:/Users/Administrator/wd/alzheimer/data/master_data_t2_v3.csv")
set.seed(20201012)
x_df <- master_df %>% 
    drop_na(top3b_three_snv) %>% 
    select(id, top3b_three_snv) %>% 
    group_by(top3b_three_snv) %>% 
    sample_n(25)

y_df <- data.frame()
for (id_var in x_df$id) {
    tmp_df <- read_xls(file.path("data/raw/NGS_result", paste0(id_var, ".xls"))) %>% 
        filter(Gene == "TOP3B" & Coordinate %in% c(22312315, 22312350, 22312351))
    
    if (nrow(tmp_df) > 0) {
        y_df <- y_df %>% 
            bind_rows(tmp_df %>% 
                         select(Gene, Variant, Coordinate, Genotype, Exonic, Consequence) %>% 
                         mutate(id = id_var))
    }
}
result_df <- x_df %>% 
    left_join(y_df, by = "id")

write_excel_csv(result_df, "output/check_top3b_three_snv.csv")
