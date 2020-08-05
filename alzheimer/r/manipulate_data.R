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

coo_combn_dataframe <- read_csv("data/combination_of_coordinate.csv")

# The number of patients in non-zero coordinate combination
sum_coo_combn_dataframe <- coo_combn_dataframe[, 3:ncol(coo_combn_dataframe)] %>% 
    select(-coordinate_count) %>% 
    summarize_all(list(sum)) %>% 
    gather("combination_of_coordinate", "value") %>% 
    filter(value > 0) %>%
    arrange(str_length(combination_of_coordinate), desc(value))

write_csv(sum_coo_combn_dataframe, "The_Number_of_Patients_by_Coordinate_Combination.csv")

sum_coo_combn_dataframe <- read_csv("The_Number_of_Patients_by_Coordinate_Combination.csv")

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


# Master Data -------------------------------------------------------------
master_target_dataframe <- read_csv("data/master_target.csv")
master_comparator_dataframe <- read_csv("data/master_comparator.csv",
                                        col_types = cols(MMSE = col_character(),
                                                         CDR = col_character(),
                                                         GDS = col_character()))

level_of_education_dataframe <- data.frame(최종학력 = c(
    "무학", "초중퇴", "초졸", "중중퇴", "중졸", "고중퇴", "고졸", "전문대졸",
    "전대졸", "초급대졸", "대중퇴", "대졸", "대학원졸", "석사"),
    level_of_education = c(0, 3.5, 6, 7.5, 9, 10.5, 10.5, 10.5, 12, 14.5, 14,
                           16, 18, 18))

cardiac_disease_character <- paste0(
    "심부전|협심증|부정맥|심혈관|정맥혈전증|심근경색|스탠트|스텐드|심장약|심장|MI|CAOD|PTCA|",
    "Atrial tachycardia|Cardiomegaly|AMI|HF"
)

master_dataframe <- master_target_dataframe %>% 
    mutate(Dementia = TRUE) %>% 
    bind_rows(master_comparator_dataframe %>% 
                  mutate(Dementia = FALSE)) %>% 
    mutate(Smoking = ifelse(str_detect(흡연, "O") | str_detect(흡연, "P"), TRUE, FALSE),
           Family_history_of_dementia = ifelse(str_detect(치매가족력, "N") | str_detect(치매가족력, "X"), FALSE, TRUE),
           ApoE_E4 = ifelse(str_detect(ApoE, "E4"), TRUE, FALSE),
           Hypertension = ifelse(str_detect(병력, "고혈압|혈압약|HTN"), TRUE, FALSE),
           Diabetes = ifelse(str_detect(병력, "당뇨|DM"), TRUE, FALSE),
           Hyperlipidemia = ifelse(str_detect(병력, "고지혈증|Hyperlipidemia"), TRUE, FALSE),
           Cardiac_disease = ifelse(str_detect(병력, cardiac_disease_character), TRUE, FALSE)) %>% 
    left_join(level_of_education_dataframe,
              by = "최종학력")

write_excel_csv(master_dataframe, "data/m_master_data.csv")
