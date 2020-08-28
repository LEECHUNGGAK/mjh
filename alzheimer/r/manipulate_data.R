# Set up Environment ------------------------------------------------------
setwd("C:/Users/Administrator/wd/alzheimer")

library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)


# Manipulate TOP3B SNV Coordinate Data ---------------------------------------------------------
d0 <- read_csv("data/material/data.csv")

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

write_csv(top3b_major_coo_dataframe, "data/top3b_major_coordinate.csv")

top3b_major_coo_dataframe <- read_csv("data/top3b_major_coordinate.csv")

# Extract Distinct Coordinate
distinct_coordinate <- d0 %>% 
    distinct(Coordinate)

write_csv(distinct_coordinate, "data/distinct_coordinate.csv")

distinct_coordinate <- read_csv("data/distinct_coordinate.csv")

# Draw a Graph: The Sum of Coordinate Count by the Patient
d1 %>%
    group_by(coordinate_count) %>% 
    summarize(n = n()) %>% 
    ggplot(aes(x = coordinate_count, y = n)) +
    geom_bar(stat = "identity") +
    labs(x = "The Sum of Coordinate Count", y = "The Number of Patients") +
    scale_x_continuous(breaks = 1:max(d1$coordinate_count))
ggsave("The_Sum_of_Coordinate_Count.png")

# The Number of Patients by Coordinate Combination
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


# Function; process_master_data -------------------------------------------
process_master_data <- function(master_data,
                                years_of_education = FALSE) {
    cardiac_disease_character <- paste0(
        "심부전|협심증|부정맥|심혈관|심근경색|스탠트|스텐드|심장약|심장|MI|CAOD|PTCA|",
        "Atrial tachycardia|Cardiomegaly|AMI|HF"
    )
    # 2020-08-07: Exclude phlebemphraxis
    
    years_of_education_df <- data.frame(
        최종학력 = c("무학", "초중퇴", "초졸", "중중퇴", "중졸", "고중퇴",
                 "고졸", "전문대졸", "전대졸", "초급대졸", "대중퇴", "대졸",
                 "대학원졸", "석사"),
        years_of_education = c(0, 3.5, 6, 7.5, 9, 10.5, 10.5, 10.5, 12, 14.5,
                               14, 16, 18, 18)
    )
    
    result <- master_data %>% 
        mutate(smoking = ifelse(str_detect(흡연, "O|P|Current smoker"), TRUE, FALSE),
               family_history_of_dementia = ifelse(str_detect(치매가족력, "N|X|없음"), FALSE, TRUE),
               apoe_e4 = ifelse(str_detect(ApoE, "E4"), TRUE, FALSE),
               hypertension = ifelse(str_detect(병력, "고혈압|혈압약|HTN"), TRUE, FALSE),
               diabetes = ifelse(str_detect(병력, "당뇨|DM"), TRUE, FALSE),
               hyperlipidemia = ifelse(str_detect(병력, "고지혈증|Hyperlipidemia"), TRUE, FALSE),
               cardiac_disease = ifelse(str_detect(병력, cardiac_disease_character), TRUE, FALSE),
               stroke = ifelse(str_detect(병력, "뇌졸중"), TRUE, FALSE))
    # 2020-08-17: Add stroke column
    
    if (years_of_education) {
        result <- result %>% 
            left_join(years_of_education_df,
                      by = "최종학력")
    }
    
    return(result)
}


# Manipulate Master Data -------------------------------------------------------------
master_target_dataframe <- read_csv("data/material/master_target.csv")
master_comparator_dataframe <- read_csv("data/material/master_comparator.csv",
                                        col_types = cols(MMSE = col_character(),
                                                         CDR = col_character(),
                                                         GDS = col_character()))

master_dataframe <- master_target_dataframe %>% 
    mutate(Dementia_a = TRUE) %>% 
    bind_rows(master_comparator_dataframe %>% 
                  mutate(Dementia_a = FALSE))

master_dataframe <- process_master_data(master_dataframe)

master_dataframe <- master_dataframe %>% 
    rename(ID = No) %>% 
    full_join(top3b_major_coo_dataframe %>% 
                  mutate(top3b = TRUE),
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

# write_excel_csv(master_dataframe, "data/master_data.csv")

master_dataframe <- read_csv("data/master_data.csv")

# Master Data Type 2
# Exclude combinations of coordinate
master_df_t2 <- master_target_dataframe %>% 
    bind_rows(master_comparator_dataframe) %>% 
    mutate(master = TRUE)

master_df_t2 <- process_master_data(master_df_t2,
                                    years_of_education = TRUE)

master_df_t2 <- master_df_t2 %>% 
    rename(ID = No) %>% 
    full_join(top3b_major_coo_dataframe %>% 
                  mutate(top3b = TRUE),
              by = "ID") %>% 
    mutate(dementia = ifelse(str_sub(ID, 1, 1) == "N", 0, 1)) %>% 
    relocate(dementia, .after = presence_22312351) %>% 
    drop_na(ID)

write_excel_csv(master_df_t2, "data/master_data_t2.csv")

master_df_t2 <- read_csv("data/master_data_t2.csv")

# Bind rows cancerrop Data
cancerrop_df <- read_csv("data/material/cancerrop_patient.csv") %>% 
    rename(`치매 약물` = 33) %>% 
    mutate(dementia = 1) %>% 
    bind_rows(read_csv("data/material/cancerrop_normal.csv",
                       col_types = cols(최근신경인지검사일 = col_character(),
                                                 MMSE = col_character(),
                                                 CDR = col_character(),
                                                 `CDR_Sum of box` = col_character(),
                                                 GDS = col_character(),
                                                 최근PET검사일 = col_date())) %>% 
                  mutate(dementia = 0) %>% 
                  rename(최근CT검사일 = CT검사일,
                           최근MRI검사일 = MRI검사일)) %>% 
    mutate(생년월일 = ymd(paste0("19", 생년월일)),
               외래방문일자 = as.character(외래방문일자),
               최근CT검사일 = as.character(최근CT검사일),
               최근MRI검사일 = as.character(최근MRI검사일),
               최근PET검사일 = as.character(최근PET검사일),
               최종학력 = as.character(최종학력)) %>% 
    drop_na(No) %>% 
    rename(ID = No)

cancerrop_df <- process_master_data(cancerrop_df)

master_df_t2_v2 <- master_df_t2 %>% 
    bind_rows(cancerrop_df %>% 
                  mutate(cancerrop = TRUE))

write_excel_csv(master_df_t2_v2, "data/master_data_t2_v2.csv")

master_df_t2_v2 <- read_csv("data/master_data_t2_v2.csv")


# Manipulate ApoE Data ----------------------------------------------------
apoe_dataframe <- data.frame()

for (i in list.files("data/raw/NGS_result", full.names = TRUE)) {
    apoe_tmp_dataframe <- read_excel(i) %>% 
        select(Gene, Variant, Coordinate, Genotype, Exonic, Consequence) %>% 
        filter(Gene == "APOE") %>% 
        mutate(ID = str_extract(i, "(N|P)-\\d+"))
    
    apoe_dataframe <- bind_rows(apoe_dataframe, apoe_tmp_dataframe)
}

m_apoe_dataframe <- apoe_dataframe %>% 
    left_join(read_csv("data/material/APOE_genotyping_data.csv") %>% 
                  rename(ID = No., Genotype_detail = APOE),
              by = "ID") %>% 
    mutate(dementia = ifelse(str_sub(ID, 1, 1) == "N", 0, 1))

write_csv(m_apoe_dataframe, "data/m_apoe_data.csv")

m_apoe_dataframe <- read_csv("data/m_apoe_data.csv")

# Pivot wider
long_apoe_dataframe <- m_apoe_dataframe %>% 
    select(ID, Coordinate, dementia) %>% 
    distinct() %>% 
    mutate(Coordinate = paste0("c_", Coordinate),
           Coordinate_value = 1) %>% 
    spread(key = Coordinate, value = Coordinate_value, fill = 0)

write_csv(long_apoe_dataframe, "data/long_apoe_data.csv")


# Create ApoE Plot --------------------------------------------------------
plot_dataframe <- m_apoe_dataframe %>% 
    select(ID, Coordinate, dementia) %>% 
    mutate(dementia = recode(dementia, `1` = TRUE, `0` = FALSE)) %>% 
    # Recode function does not work on numeric vector. Use grave accent.
    distinct() %>% 
    group_by(dementia, Coordinate) %>% 
    summarize(n = n()) %>% 
    mutate(Coordinate = as.character(Coordinate)) %>%
    group_by(Coordinate) %>% 
    mutate(proportion = n / sum(n)) %>% 
    as.data.table()

asterisk_dataframe <- plot_dataframe %>% 
    select(-n) %>% 
    spread(key = dementia, value = proportion) %>% 
    replace(is.na(.), 0) %>% 
    rename(normal_prop = 2, patient_prop = 3) %>% 
    mutate(asterisk = patient_prop > normal_prop)

apoe_plot <- ggplot(data = plot_dataframe, aes(x = reorder(Coordinate, -n), y = n, fill = dementia)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = round(proportion, 2)), position = position_stack(vjust = 0.5)) +
    labs(x = "Coordinate", y = "The Number of Patients")

for (i in 1:nrow(asterisk_dataframe)) {
    if (asterisk_dataframe$asterisk[i] == TRUE) {
        coordinate <- asterisk_dataframe$Coordinate[i]
        apoe_plot <- apoe_plot +
            annotate("text",
                     x = coordinate,
                     y = plot_dataframe[Coordinate == coordinate, sum(n)] + 5,
                     label = "*")
    }
}
ggsave("ApoE_Count_by_Coordinate.png", apoe_plot)


# Preprocess Karyotype Data ------------------------------------------------
karyotype_dataframe <- read_csv("data/material/karyotype_patient.csv") %>% 
    bind_rows(read_csv("data/material/karyotype_normal.csv")) %>% 
    mutate(의뢰날짜 = ymd(의뢰날짜),
               karyotype = TRUE,
               식별코드 = str_replace(식별코드, "_", "-")) %>% 
    select(의뢰날짜, 관리번호, 식별코드, `결과 정상=0, 염색체 (상염색체, 성염색체)이상 =1`, `성염색체 이상 =1`,
               `Turner 45X`, XXX, `상염색체 이상`) %>% 
    rename(registration_date = 1,
           management_number = 2,
           ID = 3,
           abnormal_chromosome = 4,
           abnormal_sex_chromosome = 5,
           turner = 6,
           xxx = 7,
           abnormal_autosome = 8) %>% 
    replace_na(list(abnormal_chromosome = 0,
                    abnormal_sex_chromosome = 0,
                    abnormal_autosome = 0,
                    turner = 0,
                    xxx = 0))

master_df_t2_v3 <- master_df_t2_v2 %>% 
    rename(registration_date = 등재일) %>% 
    left_join(karyotype_dataframe %>% 
                  mutate(karyotype = TRUE),
              by = "ID") %>% 
    mutate(registration_date.x = ifelse(is.na(registration_date.x),
                                        registration_date.y,
                                        registration_date.x),
           성별 = ifelse(성별 == "M", "Male", "Female")) %>% 
    rename(registration_date = registration_date.x,
           mmse_date = `MMSE 시행날짜`,
           cdr_date = `CDR 시행날짜`,
           cdr_sum_of_box = `CDR_Sum of box`,
           age = 나이,
           gender = 성별) %>% 
    mutate(registration_date = as.Date(registration_date, origin = "1970-01-01"),) %>% 
    select(-registration_date.y)

write_excel_csv(master_df_t2_v3, "data/master_data_t2_v3.csv")

master_df_t2_v3 <- read_csv("data/master_data_t2_v3.csv")

# Preprocess Chip Data ------------------------------------------------------
chip_df <- read_csv("data/material/chip_patient.csv") %>%
    rename(chip_abnormal_chromosome = `염색체 이상 (상염색체, 성염색체)=1`) %>% 
    bind_rows(read_csv("data/material/chip_normal.csv") %>% 
                  rename(chip_abnormal_chromosome = `염색체(상염색체, 성염색체) 이상 =1`)) %>%
    mutate(식별코드 = str_replace(식별코드, "_", "-")) %>% 
    rename(registration_date = `의뢰\n날짜`,
           `농도 (ng/ul)` = `농도 \n(ng/ul)`,
           `DNA농도(ng)` = `DNA농도\n(ng)`,
           chip_abnormal_autosome = `상염색체 이상 =1`,
           chip_abnormal_sex_chromosome = `성염색체 이상=1`,
           management_number = 관리번호,
           ID = 식별코드) %>% 
    replace_na(list(chip_abnormal_sex_chromosome = 0,
                    chip_abnormal_autosome = 0,
                    chip_abnormal_chromosome = 0)) %>% 
    select(-c(No., 대리점, 의료기관명, 수진자명, `성별/나이`, `정상or 환자`,
              기타기록사항))

master_df_t2_v4 <- master_df_t2_v3 %>% 
    left_join(chip_df %>% 
                  mutate(chip = TRUE),
              by = "ID") %>% 
    rename(mmse_date = `MMSE 시행날짜`,
           cdr_date = `CDR 시행날짜`,
           cdr_sum_of_box = `CDR_Sum of box`) %>% 
    mutate(registration_date.x = ifelse(is.na(registration_date.x),
                                        registration_date.y,
                                        registration_date.x)) %>% 
    rename(registration_date = registration_date.x) %>% 
    mutate(registration_date = as.Date(registration_date, origin = "1970-01-01")) %>% 
    select(-registration_date.y)

write_excel_csv(master_df_t2_v4, "data/master_data_t2_v4.csv")
