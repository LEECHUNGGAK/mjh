# Set up Environment ------------------------------------------------------
setwd("C:/Users/Administrator/wd/alzheimer")

library(tidyverse)
library(lubridate)
library(readxl)

# Manipulate SNV Coordinate Data ---------------------------------------------------------
d0 <- read_csv("data/data.csv")

d1 <- d0 %>% 
    select(Sample, Coordinate) %>% 
    distinct() %>% 
    mutate(Coordinate = paste0("c_", Coordinate),
           coordinate_value = 1,
           dementia = ifelse(str_sub(Sample, 1, 1) == "N", 0, 1)) %>% 
    spread(Coordinate, coordinate_value, fill = 0) %>% 
    mutate(coordinate_count = rowSums(.[3:ncol(.)]))

write_csv(d1, "data/m_data.csv")

d1 <- read_csv("data/m_data.csv")

top3b_major_coo_dataframe <- d0 %>% 
    select(Sample, Coordinate) %>% 
    filter(Coordinate %in% c(22312315, 22312350, 22312351)) %>% 
    mutate(Coordinate = paste0("presence_", Coordinate),
           value = 1) %>% 
    spread(Coordinate, value, fill = 0) %>% 
    rename(ID = Sample)

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

sum_zero_coo_combn_dataframe <- read_csv("data/zero_cordinate_combination_data.csv")

# Write non-zero coordinate combination data frame
non_zero_coo_combn_dataframe <- coo_combn_dataframe %>% 
    select(-sum_zero_coo_combn_dataframe$combination_of_coordinate) %>% 
    rename(ID = Sample)

write_csv(non_zero_coo_combn_dataframe, "data/non_zero_coordinate_combination_data.csv")

non_zero_coo_combn_dataframe <- read_csv("data/non_zero_coordinate_combination_data.csv")


# Manipulate Master Data -------------------------------------------------------------
master_target_dataframe <- read_csv("data/master_target.csv")
master_comparator_dataframe <- read_csv("data/master_comparator.csv",
                                        col_types = cols(MMSE = col_character(),
                                                         CDR = col_character(),
                                                         GDS = col_character()))

years_of_education_dataframe <- data.frame(최종학력 = c(
    "무학", "초중퇴", "초졸", "중중퇴", "중졸", "고중퇴", "고졸", "전문대졸",
    "전대졸", "초급대졸", "대중퇴", "대졸", "대학원졸", "석사"),
    years_of_education = c(0, 3.5, 6, 7.5, 9, 10.5, 10.5, 10.5, 12, 14.5, 14,
                           16, 18, 18))

cardiac_disease_character <- paste0(
    "심부전|협심증|부정맥|심혈관|심근경색|스탠트|스텐드|심장약|심장|MI|CAOD|PTCA|",
    "Atrial tachycardia|Cardiomegaly|AMI|HF"
)
# 2020-08-07-Exclude phlebemphraxis

master_dataframe <- master_target_dataframe %>% 
    mutate(Dementia_a = TRUE) %>% 
    bind_rows(master_comparator_dataframe %>% 
                  mutate(Dementia_a = FALSE)) %>% 
    mutate(Smoking = ifelse(str_detect(흡연, "O") | str_detect(흡연, "P"), TRUE, FALSE),
           Family_history_of_dementia = ifelse(str_detect(치매가족력, "N") | str_detect(치매가족력, "X"), FALSE, TRUE),
           ApoE_E4 = ifelse(str_detect(ApoE, "E4"), TRUE, FALSE),
           Hypertension = ifelse(str_detect(병력, "고혈압|혈압약|HTN"), TRUE, FALSE),
           Diabetes = ifelse(str_detect(병력, "당뇨|DM"), TRUE, FALSE),
           Hyperlipidemia = ifelse(str_detect(병력, "고지혈증|Hyperlipidemia"), TRUE, FALSE),
           Cardiac_disease = ifelse(str_detect(병력, cardiac_disease_character), TRUE, FALSE)) %>% 
    rename(ID = No) %>% 
    left_join(years_of_education_dataframe,
              by = "최종학력") %>% 
    full_join(top3b_major_coo_dataframe,
              by = "ID") %>% 
    left_join(non_zero_coo_combn_dataframe %>% 
                  mutate(Source = ifelse(str_length(ID) == 8, 1, 2)),
              by = "ID") %>% 
    mutate(Source = ifelse(is.na(Source), 0, Source),
           Dementia = ifelse((!is.na(Dementia_a) & Dementia_a == TRUE) |
                                 (!is.na(dementia) & dementia == 1), TRUE,
                             ifelse((!is.na(Dementia_a) & Dementia_a == FALSE) |
                                        (!is.na(dementia) & dementia == 0), FALSE, NA))) %>% 
    select(-c(Dementia_a, dementia)) %>% 
    relocate(Dementia, .after = presence_22312351) %>% 
    relocate(Source, .before = presence_22312315) %>% 
    drop_na(ID)

write_excel_csv(master_dataframe, "data/m_master_data.csv")

master_dataframe <- read_csv("data/m_master_data.csv")


# Insert Data into MariaDB ------------------------------------------------
# Person
person_table <- master_dataframe %>% 
    select(병록번호, 성별, 생년월일) %>% 
    rename(person_id = 1,
           gender = 2,
           birth_date = 3) %>% 
    drop_na(person_id) %>% 
    mutate(gender = ifelse(gender == "M", "Male", "Female"))

write_csv(person_table, "data/mariadb/person_table.csv", na = "NULL")

# Measurement
## MMSE
measurement_table <- master_dataframe %>% 
    select(병록번호, MMSE, `MMSE 시행날짜`) %>% 
    separate_rows(MMSE, `MMSE 시행날짜`, sep = "\n") %>% 
    mutate(measurement_concept = "Mini mental test examination",
           measurement_date = ymd(`MMSE 시행날짜`)) %>% 
    select(-`MMSE 시행날짜`) %>% 
    rename(person_id = 병록번호,
           value_as_number = MMSE) %>%
    ## CDR
    bind_rows(master_dataframe %>% 
                  select(병록번호, CDR, `CDR 시행날짜`) %>% 
                  separate_rows(CDR, `CDR 시행날짜`, sep = "\n") %>% 
                  mutate(measurement_concept = "Clinical dementia rating",
                         measurement_date = ymd(`CDR 시행날짜`)) %>% 
                  select(-`CDR 시행날짜`) %>% 
                  rename(person_id = 병록번호,
                         value_as_number = CDR)) %>% 
    ## CDR-sum of boxes
    bind_rows(master_dataframe %>% 
                  select(병록번호, `CDR_Sum of box`, `CDR 시행날짜`) %>% 
                  separate_rows(`CDR_Sum of box`, `CDR 시행날짜`, sep = "\n") %>% 
                  mutate(measurement_concept = "Clinical dementia rating-sum of boxes",
                         measurement_date = ymd(`CDR 시행날짜`)) %>% 
                  select(-`CDR 시행날짜`) %>% 
                  rename(person_id = 병록번호,
                         value_as_number = `CDR_Sum of box`)) %>% 
    ## GDR
    bind_rows(master_dataframe %>% 
                  select(병록번호, 최근신경인지검사일, GDS) %>% 
                  separate_rows(최근신경인지검사일, GDS, sep = "\n") %>% 
                  mutate(measurement_concept = "Geriatric depression scale",
                         measurement_date = ymd(최근신경인지검사일)) %>% 
                  select(-최근신경인지검사일) %>% 
                  rename(person_id = 병록번호,
                         value_as_number = GDS)) %>% 
    drop_na() %>%
    filter(value_as_number != "ND") %>% 
    relocate(person_id, measurement_concept, measurement_date, value_as_number)

write_csv(measurement_table, "data/mariadb/measurement_table.csv")

## Height, Weight, SBP, DBP, Pulse rate
measurement_table_2 <- master_dataframe %>% 
    select(병록번호, 등재일, 키, 체중, SBP, DBP, 맥박) %>% 
    rename(Height = 키, Weight = 체중, `Systolic blood pressure` = SBP,
           `Diastolic blood pressure` = DBP, `Pulse rate` = 맥박,
           measurement_date = 등재일, person_id = 병록번호) %>% 
    gather(key = measurement_concept, value = value_as_number,
           Height, Weight, `Systolic blood pressure`, `Diastolic blood pressure`,
           `Pulse rate`) %>% 
    drop_na() %>% 
    relocate(measurement_date, .after = measurement_concept)

write_csv(measurement_table_2, "data/mariadb/measurement_table_2.csv")

## Observation - value_as_string
observation_concenpt_dataframe <- data.frame(
    observation_concept = c("Family_history_of_dementia", "Smoking",
                            "Hypertension", "Diabetes", "Hyperlipidemia",
                            "Cardiac_disease"),
    value_as_string = c("Dementia", "TRUE", "Hypertension", "Diabetes",
                        "Hyperlipidemia", "Cardiac disease")
)

observation_table <- master_dataframe %>% 
    select(병록번호, 등재일, Family_history_of_dementia, Smoking, Hypertension,
               Diabetes, Hyperlipidemia, Cardiac_disease) %>% 
    gather(key = observation_concept, value = value,
           Family_history_of_dementia, Smoking, Hypertension, Diabetes,
           Hyperlipidemia, Cardiac_disease) %>% 
    rename(person_id = 병록번호, observation_date = 등재일) %>% 
    filter(!is.na(person_id) & value == TRUE) %>% 
    left_join(observation_concenpt_dataframe, by = "observation_concept") %>% 
    mutate(observation_concept = ifelse(
        observation_concept == "Family_history_of_dementia", "Family history", 
        ifelse(observation_concept == "Smoking", "Smoking", "Past history")
    )) %>% 
    select(-value) %>% 
    relocate(observation_date, .after = observation_concept)

write_csv(observation_table, "data/mariadb/observation_table.csv")

## Observation - value_as_number
observation_table_2 <- master_dataframe %>% 
    select(병록번호, 등재일, years_of_education) %>% 
    gather(key = observation_concept, value = value_as_number,
           years_of_education) %>% 
    drop_na() %>% 
    mutate(observation_concept = "Years of education") %>% 
    rename(person_id = 병록번호, observation_date = 등재일) %>% 
    relocate(observation_date, .after = observation_concept)

write_csv(observation_table_2, "data/mariadb/observation_table_2.csv")


# Manipulate ApoE Data ----------------------------------------------------
apoe_outcome_dataframe <- data.frame()

for (i in list.files("data/raw/NGS_result", full.names = TRUE)) {
    apoe_tmp_dataframe <- read_excel(i) %>% 
        select(Gene, Variant, Coordinate, Genotype, Exonic, Consequence) %>% 
        filter(Gene == "APOE") %>% 
        mutate(ID = str_extract(i, "(N|P)-\\d+"))
    
    apoe_outcome_dataframe <- bind_rows(apoe_outcome_dataframe, apoe_tmp_dataframe)
}

apoe_outcome_dataframe <- apoe_outcome_dataframe %>% 
    left_join(read_csv("data/APOE_genotyping_data.csv") %>% 
                  rename(ID = No., Genotype_detail = APOE),
              by = "ID")

write_csv(apoe_outcome_dataframe, "data/m_apoe_data.csv")
