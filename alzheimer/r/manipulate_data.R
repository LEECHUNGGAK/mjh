# Set up Environment ------------------------------------------------------
setwd("C:/Users/Administrator/wd/alzheimer")

library(tidyverse)


# Manipulate Data ---------------------------------------------------------
d0 <- read_csv("data/data.csv")

d1 <- d0 %>% 
    select(Sample, Coordinate) %>% 
    distinct() %>% 
    mutate(Coordinate = paste0("c_", Coordinate),
           coordinate_value = 1,
           dementia = ifelse(str_sub(Sample, 1, 1) == "N", 0, 1)) %>% 
    spread(Coordinate, coordinate_value) %>% 
    replace(is.na(.), 0) %>% 
    mutate(coordinate_count = rowSums(.[3:ncol(.)]))

write_csv(d1, "data/m_data.csv")

d1 <- read_csv("data/m_data.csv")


# Extract Distinct Coordinate ---------------------------------------------
distinct_coordinate <- d0 %>% 
    distinct(Coordinate)

write_csv(distinct_coordinate, "data/distinct_coordinate.csv")

distinct_coordinate <- read_csv("data/distinct_coordinate.csv")


# Draw a Graph: The Sum of Coordinate Count by the Patient ------------------------------
d1 %>%
    group_by(coordinate_count) %>% 
    summarize(n = n()) %>% 
    ggplot(aes(x = coordinate_count, y = n)) +
    geom_bar(stat = "identity") +
    labs(x = "The Sum of Coordinate Count", y = "The Number of Patients") +
    scale_x_continuous(breaks = 1:max(d1$coordinate_count))
ggsave("The_Sum_of_Coordinate_Count.png")


# The Number of Patients by Coordinate Combination ------------------------
coo_combn_dataframe <- d1

for (var_choose in 3:1) {
    coo_combn <- combn(pull(distinct_coordinate), var_choose)
    
    for (i in 1:ncol(coo_combn)) {
        col_name <- paste(c("c", coo_combn[, i]), collapse = "_")
        
        for (j in 1:var_choose) {
            assign(paste0("col", j), paste0("c_", coo_combn[j, i]))
        }
        
        if (var_choose == 3) {
            coo_combn_dataframe <- coo_combn_dataframe %>% 
                mutate(!!col_name := ifelse(!!sym(col1) + !!sym(col2) + !!sym(col3) == 3, 1, 0))
        } else if (var_choose == 2) {
            coo_combn_dataframe <- coo_combn_dataframe %>% 
                mutate(!!col_name := ifelse((!!sym(col1) + !!sym(col2) == 2) &
                                                coordinate_count == 2, 1, 0))
        } else {
            coo_combn_dataframe <- coo_combn_dataframe %>% 
                mutate(!!col_name := ifelse((!!sym(col1) == 1) &
                                                coordinate_count == 1, 1, 0))
        }
    }
}

write_csv(coo_combn_dataframe, "data/combination_of_coordinate.csv")

# The number of patients in non-zero coordinate combination
sum_coo_combn_dataframe <- coo_combn_dataframe[, 3:ncol(coo_combn_dataframe)] %>% 
    select(-coordinate_count) %>% 
    summarize_all(list(sum)) %>% 
    gather("combination_of_coordinate", "value") %>% 
    filter(value > 0) %>%
    arrange(str_length(combination_of_coordinate), desc(value))

write_csv(sum_coo_combn_dataframe, "The_Number_of_Patients_by_Coordinate_Combination.csv")

# The number of patients in zero coordinate combination
sum_zero_coo_combn_dataframe <- coo_combn_dataframe[, 3:ncol(coo_combn_dataframe)] %>% 
    select(-coordinate_count) %>% 
    summarize_all(list(sum)) %>% 
    gather("combination_of_coordinate", "value") %>% 
    filter(value == 0)

write_csv(sum_zero_coo_combn_dataframe, "data/zero_cordinate_combination_data.csv")

# Write non-zero coordinate combination data frame
non_zero_coo_combn_dataframe <- coo_combn_dataframe %>% 
    select(-sum_zero_coo_combn_dataframe$combination_of_coordinate)

write_csv(non_zero_coo_combn_dataframe, "data/non_zero_coordinate_combination_data.csv")
