library(tidyverse)
library(MatchIt)

setwd("C:/Users/Administrator/wd/alzheimer")

match_df <- read_csv("data/master_data_t2_v4.csv") %>% 
    mutate(match_outcome = ifelse(presence_major == 0 & dementia == 0, 0, 
                                  ifelse(presence_major == 1 & dementia == 1, 1, NA))) %>% 
    drop_na(age, match_outcome) %>% 
    select(ID, age, match_outcome)

match_df %>% 
    group_by(match_outcome) %>% 
    summarize(mean = mean(age))

matched <- matchit(match_outcome ~ age, match_df, method = "exact", ratio = 1)
summary(matched)
matched_df <- match.data(matched) %>% 
    arrange(subclass)

matched_df %>% 
    group_by(match_outcome) %>% 
    summarize(mean = mean(age))

write_excel_csv(matched_df, "data/matched_data.csv")
