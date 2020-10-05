# Set up Environment ------------------------------------------------------
setwd("C:/Users/Administrator/wd/alzheimer")

library(tidyverse)
library(lubridate)
library(readxl)


# Functions -------------------------------------------
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
        mutate(smoking = ifelse(str_detect(흡연, "^O|^P$|Current smoker"), 1, 0),
               family_history_of_dementia = ifelse(str_detect(치매가족력, "N|X|없음"), 0, 1),
               apoe_e4 = ifelse(str_detect(ApoE, "E4"), 1,
                                ifelse(str_detect(ApoE, "ND"), NA, 0)),
               hypertension = ifelse(str_detect(병력, "고혈압|혈압약|HTN"), 1, 0),
               diabetes = ifelse(str_detect(병력, "당뇨|DM"), 1, 0),
               hyperlipidemia = ifelse(str_detect(병력, "고지혈증|Hyperlipidemia"), 1, 0),
               cardiac_disease = ifelse(str_detect(병력, cardiac_disease_character), 1, 0),
               stroke = ifelse(str_detect(병력, "뇌졸중"), 1, 0))
    # 2020-08-17: Add stroke column
    
    
    if (years_of_education) {
        result <- result %>% 
            left_join(years_of_education_df,
                      by = "최종학력")
    }
    
    result <- result %>% 
        select(-c(최종학력, 흡연, 치매가족력, 병력))
    
    return(result)
}

preprocess_karyotype_data <- function(data) {
    result <- data %>% 
        drop_na(식별코드) %>% 
        select(-c(병원명, 환자명, 검체, 우선순위, Karyotype,
                     결과상세)) %>% 
        separate(`성별/나이`, c("gender", "age"), "/") %>% 
        rename(management_number = 1,
               registration_date = 2,
               id = 3,
               abnormal_chromosome = 6,
               abnormal_sex_chromosome = 7,
               klinefelter = 8,
               turner = 9,
               xxx = 10,
               abnormal_autosome = 11,
               include_marker_chromosome = 12) %>% 
        mutate(registration_date = ymd(registration_date),
               id = str_replace(id, "'", ""),
               gender = recode(gender, 남 = "Male", 여 = "Female"),
               age = as.integer(age),
               karyotype = TRUE) %>% 
        replace_na(list(abnormal_chromosome = 0, abnormal_sex_chromosome = 0,
                        klinefelter = 0, turner = 0, xxx = 0,
                        abnormal_autosome = 0, include_marker_chromosome = 0))
    
    return(result)
}

coalesce_join <- function(x, y, by, join, suffix = c(".x", ".y")) {
    joined <- join(x, y, by = by)
    
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    
    to_coalesce <- unique(substr(to_coalesce, 1, nchar(to_coalesce) - nchar(suffix_used)))
    
    coalesced <- map_dfc(to_coalesce, ~coalesce(joined[[paste0(.x, suffix[1])]],
                                                joined[[paste0(.x, suffix[2])]]))
    
    names(coalesced) <- to_coalesce
    
    bind_cols(joined, coalesced)[cols]
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
    rename(id = No) %>% 
    full_join(top3b_major_coo_dataframe %>% 
                  mutate(top3b = TRUE),
              by = "id") %>% 
    left_join(non_zero_coo_combn_dataframe %>% 
                  mutate(Source = ifelse(str_length(id) == 8, 1, 2)),
              by = "id") %>% 
    mutate(Source = ifelse(is.na(Source), 0, Source),
           Dementia = ifelse((!is.na(Dementia_a) & Dementia_a == TRUE) |
                                 (!is.na(dementia) & dementia == 1), TRUE,
                             ifelse((!is.na(Dementia_a) & Dementia_a == FALSE) |
                                        (!is.na(dementia) & dementia == 0), FALSE, NA))) %>% 
    select(-c(Dementia_a, dementia)) %>% 
    relocate(Dementia, .after = presence_22312351) %>% 
    relocate(Source, .before = presence_22312315) %>% 
    drop_na(id)

# write_excel_csv(master_dataframe, "data/master_data.csv")
# master_dataframe <- read_csv("data/master_data.csv")

# Master Data Type 2
# Exclude combinations of coordinate
top3b_wide_df <- read_csv("data/ngs/top3b_wide.csv") %>% 
    rename(id = master_id)

master_df_t2 <- master_target_dataframe %>% 
    mutate(dementia = 1) %>% 
    bind_rows(master_comparator_dataframe %>% 
                  mutate(dementia = 0)) %>% 
    mutate(master = TRUE,
           성별 = recode(성별, M = "Male", F = "Female")) %>% 
    select(-c(나이, `MMSE 시행날짜`, `CDR 시행날짜`)) %>% 
    rename(id = No, registration_date = 등재일, gender = 성별,
           birth_date = 생년월일, patient_id = 병록번호, 
           measurement_date = 최근신경인지검사일, CDR_SOB = `CDR_Sum of box`) %>% 
    drop_na(id) %>% 
    left_join(top3b_wide_df %>% 
                  mutate(top3b_three_snv = ifelse(
                      coordinate_22312315 + coordinate_22312350 + coordinate_22312351 == 3,
                      1,
                      ifelse(coordinate_22312315 + coordinate_22312350 + coordinate_22312351 == 0,
                             0, NA)
                  )) %>% 
                  select(id, top3b_three_snv),
              by = "id")

master_df_t2 <- process_master_data(master_df_t2,
                                    years_of_education = TRUE)

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
    drop_na(No) %>% 
    select(-c(나이, `MMSE 시행날짜`, `CDR 시행날짜`)) %>% 
    rename(cancerrop_id = No,
           cancerrop_registration_date = 등재일,
           patient_id = 병록번호,
           measurement_date = 최근신경인지검사일,
           gender = 성별,
           birth_date = 생년월일,
           CDR_SOB = `CDR_Sum of box`) %>% 
    mutate(patient_id = as.character(patient_id),
           birth_date = as.character(ymd(paste0("19", birth_date))),
           gender = recode(gender, M = "Male", F = "Female"))

cancerrop_inner_df <- cancerrop_df %>% 
    filter(patient_id %in% master_df_t2$patient_id) %>% 
    select(c(cancerrop_id, cancerrop_registration_date, patient_id,
             measurement_date, MMSE, CDR, CDR_SOB, GDS, 외래방문일자,
             SBP, DBP, 맥박, dementia))

cancerrop_outer_df <- cancerrop_df %>% 
    filter(!patient_id %in% master_df_t2$patient_id) %>% 
    mutate(외래방문일자 = as.character(외래방문일자),
                 최근CT검사일 = as.character(최근CT검사일),
                 최근MRI검사일 = as.character(최근MRI검사일),
                 최근PET검사일 = as.character(최근PET검사일),
                 최종학력 = as.character(최종학력),
                 SBP = as.character(SBP),
                 DBP = as.character(DBP),
                 맥박 = as.character(맥박)) %>% 
    process_master_data()

master_df_t2_v2 <- master_df_t2 %>% 
    inner_join(cancerrop_inner_df %>% 
                   mutate(cancerrop = TRUE),
               by = "patient_id") %>% 
    mutate(measurement_date = paste0(measurement_date.x, "\n", measurement_date.y),
           MMSE = paste0(MMSE.x, "\n", MMSE.y),
           CDR = paste0(CDR.x, "\n", CDR.y),
           CDR_SOB = paste0(CDR_SOB.x, "\n", CDR_SOB.y),
           GDS = paste0(GDS.x, "\n", GDS.y),
           외래방문일자 = ifelse(외래방문일자.x == "-",
                           as.character(외래방문일자.y), 
                           as.character(외래방문일자.x)),
           SBP = paste0(SBP.x, "\n", SBP.y),
           DBP = paste0(DBP.x, "\n", DBP.y),
           맥박 = ifelse(!is.na(맥박), paste0(맥박.x, "\n", 맥박.y), 맥박.x),
           dementia = dementia.x) %>% 
    select(-ends_with(".x"), -ends_with(".y")) %>% 
    bind_rows(master_df_t2 %>% 
                  filter(!patient_id %in% cancerrop_inner_df$patient_id) %>% 
                  mutate(SBP = as.character(SBP),
                         DBP = as.character(DBP),
                         맥박 = as.character(맥박))) %>% 
    bind_rows(cancerrop_outer_df)

master_df_t2_v2 <- master_df_t2_v2 %>% 
    separate_rows(measurement_date, MMSE, CDR, CDR_SOB, GDS, sep = "\n") %>% 
    distinct(id, patient_id, measurement_date, MMSE, CDR, CDR_SOB, GDS) %>% 
    group_by(id, patient_id) %>% 
    summarize(measurement_date = paste(measurement_date, collapse = "\n"),
              MMSE = paste(MMSE, collapse = "\n"),
              CDR = paste(CDR, collapse = "\n"),
              CDR_SOB = paste(CDR_SOB, collapse = "\n"),
              GDS = paste(GDS, collapse = "\n")) %>% 
    left_join(master_df_t2_v2 %>% 
                  select(-c(id, measurement_date, MMSE, CDR, CDR_SOB, GDS)),
              by = "patient_id")

write_excel_csv(master_df_t2_v2, "data/master_data_t2_v2.csv")

master_df_t2_v2 <- read_csv("data/master_data_t2_v2.csv")


# Preprocess Karyotype Data ------------------------------------------------
karyotype_dataframe <- read_csv("data/material/karyotype_patient.csv") %>% 
    bind_rows(read_csv("data/material/karyotype_normal.csv")) %>% 
    mutate(의뢰날짜 = ymd(의뢰날짜),
               karyotype = TRUE,
               식별코드 = str_replace(식별코드, "_", "-")) %>% 
    select(의뢰날짜, 관리번호, 식별코드, `결과 정상=0, 염색체 (상염색체, 성염색체)이상 =1`, `성염색체 이상 =1`,
               `Turner 45X`, XXX, `상염색체 이상`, `marker chr 이상`, `Klinefelter (XXY)`) %>% 
    rename(registration_date = 1,
           management_number = 2,
           id = 3,
           abnormal_chromosome = 4,
           abnormal_sex_chromosome = 5,
           turner = 6,
           xxx = 7,
           abnormal_autosome = 8,
           include_marker_chromosome = 9,
           klinefelter = 10) %>% 
    replace_na(list(abnormal_chromosome = 0,
                    abnormal_sex_chromosome = 0,
                    abnormal_autosome = 0,
                    turner = 0,
                    xxx = 0,
                    include_marker_chromosome = 0,
                    klinefelter = 0))

master_df_t2_v3 <- master_df_t2_v2 %>% 
    left_join(karyotype_dataframe %>% 
                  mutate(karyotype = TRUE),
              by = "id") %>% 
    mutate(registration_date.x = ifelse(is.na(registration_date.x),
                                        registration_date.y,
                                        registration_date.x)) %>% 
    rename(registration_date = registration_date.x) %>% 
    select(-registration_date.y)

write_excel_csv(master_df_t2_v3, "data/master_data_t2_v3.csv")

master_df_t2_v3 <- read_csv("data/master_data_t2_v3.csv")


# Preprocess 2020-09-07 Karyotype Data ------------------------------------
master_df_t2_v4 <- coalesce_join(master_df_t2_v3,
                                 preprocess_karyotype_data(
                                     read_csv("data/material/karyotype_patient_20200907.csv")
                                 ) %>% 
                                     mutate(dementia = 1),
                                 by = "id",
                                 join = full_join)

master_df_t2_v4 <- coalesce_join(master_df_t2_v4,
                                 preprocess_karyotype_data(
                                     read_csv("data/material/karyotype_normal_20200907.csv")
                                 ) %>% 
                                     mutate(dementia = 0),
                                 by = "id",
                                 join = full_join)
write_excel_csv(master_df_t2_v4, "data/master_data_t2_v4.csv")


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
           id = 식별코드) %>% 
    replace_na(list(chip_abnormal_sex_chromosome = 0,
                    chip_abnormal_autosome = 0,
                    chip_abnormal_chromosome = 0)) %>% 
    select(-c(No., 대리점, 의료기관명, 수진자명, `성별/나이`, `정상or 환자`,
              기타기록사항))

master_df_t2_v4 <- master_df_t2_v3 %>% 
    left_join(chip_df %>% 
                  mutate(chip = TRUE),
              by = "id") %>% 
    mutate(registration_date.x = ifelse(is.na(registration_date.x),
                                        registration_date.y,
                                        registration_date.x)) %>% 
    rename(registration_date = registration_date.x) %>% 
    mutate(registration_date = as.Date(registration_date, origin = "1970-01-01")) %>% 
    select(-registration_date.y)

write_excel_csv(master_df_t2_v4, "data/master_data_t2_v4.csv")
