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
  replace(is.na(.), 0)

write_csv(d1, "data/m_data.csv")

d1 <- read_csv("data/m_data.csv")


# Extract Distinct Coordinate ---------------------------------------------
distinct_coordinate <- d0 %>% 
  distinct(Coordinate)

write_csv(distinct_coordinate, "data/distinct_coordinate.csv")

distinct_coordinate <- read_csv("data/distinct_coordinate.csv")


# The Sum of Coordinate Count by the Patient ------------------------------
coo_count <- d1 %>% 
  mutate(sum = rowSums(.[3:ncol(.)])) %>% 
  select(Sample, dementia, sum)

coo_count %>% 
  group_by(sum) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = sum, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "The Sum of Coordinate Count", y = "The Number of Patients") +
  scale_x_continuous(breaks = 1:max(coo_count$sum))
ggsave("Coordinate_Count.png")


# The Number of Patients by Coordinate Combination ------------------------
coo_combn <- combn(pull(distinct_coordinate), 3)

for (i in 1:ncol(coo_combn)) {
  col_name <- paste(c("c", coo_combn[, i]), collapse = "_")
  col1 <- paste0("c_", coo_combn[1, i])
  col2 <- paste0("c_", coo_combn[2, i])
  col3 <- paste0("c_", coo_combn[3, i])
  if (i == 1) {
    coo_combn_dataframe <- d1 %>% 
      mutate(!!col_name := ifelse(!!sym(col1) + !!sym(col2) + !!sym(col3) == 3, 1, 0))
  } else {
    coo_combn_dataframe <- coo_comb_dataframe %>% 
      mutate(!!col_name := ifelse(!!sym(col1) + !!sym(col2) + !!sym(col3) == 3, 1, 0))
  }
}

write_csv(coo_combn_dataframe, "data/combination_of_coordinate.csv")

# The number of patients in non-zero coordinate combination
sum_coo_combn_dataframe <- coo_combn_dataframe[, 41:ncol(coo_combn_dataframe)] %>% 
  summarize_all(funs(sum)) %>% 
  gather("combination_of_coordinate", "value") %>% 
  filter(value > 0)

write_csv(sum_coo_combn_dataframe, "number_of_patients_by_coordinate_combination.csv")

# The number of patients in zero coordinate combination
sum_zero_coo_combn_dataframe <- coo_combn_dataframe[, 41:ncol(coo_combn_dataframe)] %>% 
  summarize_all(funs(sum)) %>% 
  gather("combination_of_coordinate", "value") %>% 
  filter(value == 0)

write_csv(sum_zero_coo_combn_dataframe, "sum_is_zero_coordinate_combination.csv")

# Write non-zero coordinate combination data frame
non_zero_coo_combn_dataframe <- coo_combn_dataframe %>% 
  select(-sum_zero_coo_combn_dataframe$combination_of_coordinate)

write_csv(non_zero_coo_combn_dataframe, "data/non_zero_coordinate_combination_data.csv")
