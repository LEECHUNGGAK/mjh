# Set Environment -----------------------------------------------------------
library(tidyverse)
library(MatchIt)
library(xlsx)

setwd("C:/Users/Administrator/wd/alzheimer")


# Match ----------------------------------------------------------------
match_df <- read_csv("data/master_data_t2_v4.csv") %>% 
    mutate(match_outcome = ifelse(presence_major == 0 & dementia == 0, 0, 
                                  ifelse(presence_major == 1 & dementia == 1, 1, NA))) %>% 
    drop_na(age, match_outcome) %>% 
    select(id, age, match_outcome)

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


# Reduce n by 20 and Join Master Data -------------------------------------
matched_df <- read_csv("data/matched_data.csv")
master_df <- read_csv("data/master_data_t2_v4.csv")
weight_df <- matched_df %>% 
    group_by(subclass, age, match_outcome) %>% 
    summarize(n = n()) %>% 
    mutate(weights = 1- (n / sum(n))) %>% 
    arrange(desc(weights))

weight_df %>% 
    mutate(match_outcome = recode(match_outcome, TRUE = "Dementia & 3 SNV (+)",
                                  FALSE = "Noraml & 3 SNV (-)")) %>% 
    View()

for (outcome_var in c(TRUE, FALSE)) {
    if (outcome_var) {
        sheetName <- "치매, 3SNV(+)"
        append <- FALSE
    } else {
        sheetName <- "정상, 3SNV(-)"
        append <- TRUE
    }
    temp_df <- matched_df %>% 
        filter(match_outcome == outcome_var) %>% 
        left_join(weight_df, by = c("subclass", "match_outcome")) %>% 
        sample_n(size = 20, weight = weights.y) %>% 
        select(id) %>% 
        left_join(master_df, by = "id")
    
    print(temp_df %>% 
              summarize(mean = mean(age),
                        sd = sd(age)))
    write.xlsx(temp_df, "data/matched_data.xlsx", sheetName = sheetName,
               append = append)
}

write.xlsx(master_df %>% 
               filter(dementia == 1 & presence_major == 0),
           "data/matched_data.xlsx", sheetName = "치매, 3SNV(-)",
           append = append)
write.xlsx(master_df %>% 
               filter(dementia == 0 & presence_major == 1),
           "data/matched_data.xlsx", sheetName = "정상, 3SNV(+)",
           append = append)
