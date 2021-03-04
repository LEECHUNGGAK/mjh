# Set up Environment ------------------------------------------------------
library(tidyverse)
library(lubridate)

setwd("C:/Users/Administrator/work/alzheimer")


# Functions -------------------------------------------
process_master_data <- function(master_data,
                                years_of_education = FALSE) {
    cardiac_disease_character <- paste0(
        "심부전|협심증|부정맥|심혈관|심근경색|스탠트|스텐드|심장약|심장|MI|CAOD|PTCA|",
        "Atrial tachycardia|Cardiomegaly|AMI|HF"
    )
    # 2020-08-07: Exclude phlebemphraxis
    
    years_of_education_df <- data.frame(
        최종학력 = c("^무학", "^초중퇴", "^초졸", "^중중퇴", "^중졸", "^고중퇴",
                 "^고졸", "^전문대졸", "^전대졸", "^초급대졸", "^대중퇴", "^대졸",
                 "^대학원졸", "^석사"),
        years_of_education = c(0, 3, 6, 7.5, 9, 10.5,
                               12, 14, 14, 14, 14, 16,
                               18, 18)
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
        for (i in 1:nrow(result)) {
            education <- result[[i, "최종학력"]]
            
            if (is.na(education)) {
                result[i, "years_of_education"] <- NA
            } else if (str_detect(education, "^\\d+$")) {
                result[i, "years_of_education"] <- as.double(education)
            } else if (!str_detect(education, "\\(") & !str_detect(education, "\\)")) {
                result[i, "years_of_education"] <- years_of_education_df$years_of_education[
                    str_detect(education, years_of_education_df$최종학력)
                ]
            } else if (str_detect(education, "\\(") & str_detect(education, "\\)")) {
                temp_char <- str_extract(education, "\\(\\d+")
                temp_char <- str_remove(temp_char, "\\(")
                result[i, "years_of_education"] <- as.double(temp_char)
            }
        }
    }
    
    return(result)
}

preprocess_karyotype_data <- function(data) {
    result <- data %>% 
        drop_na(식별코드) %>% 
        select(-c(병원명, 환자명, 검체, 우선순위, Karyotype,
                     결과상세)) %>% 
        separate(`성별/나이`, c("gender", "나이"), "/") %>% 
        rename(karyotype_2_no = 1,
               karyotype_registration_date = 2,
               cancerrop_no = 3,
               abnormal_chromosome = 6,
               abnormal_sex_chromosome = 7,
               klinefelter = 8,
               turner = 9,
               xxx = 10,
               abnormal_autosome = 11,
               include_marker_chromosome = 12) %>% 
        mutate(karyotype_registration_date = ymd(karyotype_registration_date),
               cancerrop_no = str_replace(cancerrop_no, "'", ""),
               gender = recode(gender, 남 = "Male", 여 = "Female"),
               나이 = as.integer(나이),
               karyotype = TRUE) %>% 
        replace_na(list(abnormal_chromosome = 0, abnormal_sex_chromosome = 0,
                        klinefelter = 0, turner = 0, xxx = 0,
                        abnormal_autosome = 0, include_marker_chromosome = 0))
    
    return(result)
}

coalesce_join <- function(x, y, by, join = dplyr::full_join, suffix = c(".x", ".y")) {
    joined <- join(x, y, by = by)
    
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    
    to_coalesce <- unique(substr(to_coalesce, 1, nchar(to_coalesce) - nchar(suffix_used)))
    
    coalesced <- data.frame()
    for (i in to_coalesce) {
        for (j in 1:nrow(joined)) {
            c_x <- joined[j, paste0(i, ".x")]
            c_y <- joined[j, paste0(i, ".y")]
            
            if (is.na(c_x) & is.na(c_y)) {
                coalesced[j, i] <- NA
            } else if (is.na(c_x) & !is.na(c_y)) {
                coalesced[j, i] <- c_y
            } else if (!is.na(c_x) & is.na(c_y)) {
                coalesced[j, i] <- c_x
            } else if (c_x == c_y) {
                coalesced[j, i] <- c_x
            } else {
                coalesced[j, i] <- paste(unique(c(str_split(c_x, "\n")[[1]],
                                                  str_split(c_y, "\n")[[1]])),
                                         collapse = "\n") 
            }
        }
    }
    
    # coalesced <- map_dfc(to_coalesce, ~str_replace_all(paste(joined[[paste0(.x, suffix[1])]],
    #                                                          joined[[paste0(.x, suffix[2])]],
    #                                                          sep = "\n"), "(\nNA)|(NA\n)", ""))
    # 
    # names(coalesced) <- to_coalesce
    
    bind_cols(joined, coalesced)[cols]
    
    return(bind_cols(joined, coalesced)[cols])
}


# Manipulate Master Data -------------------------------------------------------------
key_df <- read_csv("data/key/output/key_20201118.csv",
                   col_types = cols(.default = "c"))

master_t_df <- read_csv("data/material/master_p.csv",
                        col_types = cols(.default = "c")) %>% 
    mutate(No = str_replace(No, "^P_", "P-"))
master_c_df <- read_csv("data/material/master_c.csv",
                        col_types = cols(.default = "c")) %>% 
    mutate(No = str_replace(No, "^N_", "N-"),
           최근신경인지검사일 = `MMSE 시행날짜`)
    

master_df <- master_t_df %>% 
    mutate(dementia = 1) %>% 
    bind_rows(master_c_df %>% 
                  mutate(dementia = 0)) %>% 
    mutate(master = TRUE,
           성별 = recode(성별, M = "Male", F = "Female"),
           new_age = ifelse(is.na(생년월일), 나이, floor((ymd(등재일) - ymd(생년월일)) / 365)),
           동의서취득 = str_to_lower(동의서취득),
           채혈여부 = str_to_lower(채혈여부),
           외래방문일자 = ifelse(외래방문일자 ==  "-", NA, 외래방문일자),
           drop = ifelse(str_detect(`탈 락`, "^탈"), 1, 0)) %>% 
    rename(master_no = No, master_registration_date = 등재일, gender = 성별, 
           birth_date = 생년월일, patient_id = 병록번호, name = 이름,
           measurement_date = 최근신경인지검사일, cdr_sob = `CDR_Sum of box`) %>% 
    left_join(key_df %>% 
                  select(patient_id, key_id),
              by = "patient_id") %>% 
    drop_na(master_no) %>% 
    replace_na(list(drop = 0))

master_df <- process_master_data(master_df,
                                 years_of_education = TRUE)

# Bind rows cancerrop Data
cancerrop_df <- read_csv("data/material/cancerrop_patient.csv",
                         col_types = cols(.default = "c")) %>% 
    rename(`치매 약물` = 33) %>% 
    mutate(dementia = 1) %>% 
    bind_rows(read_csv("data/material/cancerrop_normal.csv",
                       col_types = cols(.default = "c")) %>% 
                  mutate(dementia = 0) %>% 
                  rename(최근CT검사일 = CT검사일,
                           최근MRI검사일 = MRI검사일)) %>% 
    drop_na(No) %>% 
    rename(name = 이름,
           cancerrop_no = No,
           cancerrop_registration_date = 등재일,
           patient_id = 병록번호,
           measurement_date = 최근신경인지검사일,
           gender = 성별,
           birth_date = 생년월일,
           cdr_sob = `CDR_Sum of box`) %>% 
    mutate(birth_date = as.character(ymd(paste0("19", birth_date))),
           gender = recode(gender, M = "Male", F = "Female"),
           ApoE = gsub("^(E\\d)(E\\d)$", "\\1/\\2", str_to_upper(ApoE)),
           new_age = ifelse(is.na(birth_date),
                            나이,
                            floor((ymd(cancerrop_registration_date) - ymd(birth_date)) / 365)),
           drop = 0) %>% 
    left_join(key_df %>% 
                  select(patient_id, key_id),
              by = "patient_id")

cancerrop_df <- process_master_data(cancerrop_df,
                                 years_of_education = TRUE)

master_df <- master_df %>% 
    coalesce_join(cancerrop_df, by = "key_id")


# Join TOP3B columns ------------------------------------------------------
top3b_1_1_df <- read_csv("data/ngs/top3b.csv") %>% 
    select(id, Coordinate, Variant) %>% 
    filter(Coordinate %in% c(22312315, 22312350, 22312351)) %>% 
    mutate(Coordinate = paste0("top3b_coordinate_", Coordinate, "_variant")) %>% 
    spread(key = Coordinate, value = Variant) %>% 
    rename(master_no = id) %>% 
    left_join(key_df %>% 
                  select(master_no, key_id),
              by = "master_no")

top3b_1_2_df <- read_csv("data/ngs/top3b_t2.csv") %>% 
    select(id, coordinate_22312315, coordinate_22312350,
           coordinate_22312351) %>% 
    rename(master_no = id) %>% 
    mutate(top3b_three_snv = ifelse(
        coordinate_22312315 + coordinate_22312350 + coordinate_22312351 == 3,
        1,
        0)) %>% 
    set_names(~ str_replace_all(., "coordinate", "top3b_coordinate")) %>% 
    left_join(key_df %>% 
                  select(master_no, key_id),
              by = "master_no")

top3b_2_1_df <- read_csv("data/ngs/top3b_v2.csv") %>% 
    select(id, Coordinate, Variant) %>% 
    filter(Coordinate %in% c(22312315, 22312350, 22312351)) %>% 
    mutate(Coordinate = paste0("top3b_coordinate_", Coordinate, "_variant")) %>% 
    spread(key = Coordinate, value = Variant) %>% 
    rename(ngs_2_no = id) %>% 
    left_join(key_df %>% 
                  select(ngs_2_no, key_id),
              by = "ngs_2_no")

top3b_2_2_df <- read_csv("data/ngs/top3b_v2_t2.csv") %>%
    select(id, coordinate_22312315, coordinate_22312350,
           coordinate_22312351) %>% 
    rename(ngs_2_no = id) %>% 
    mutate(top3b_three_snv = ifelse(
        coordinate_22312315 + coordinate_22312350 + coordinate_22312351 == 3,
        1,
        0)) %>% 
    set_names(~ str_replace_all(., "coordinate", "top3b_coordinate")) %>% 
    left_join(key_df %>% 
                  select(ngs_2_no, key_id),
              by = "ngs_2_no")

master_df <- master_df %>% 
    coalesce_join(top3b_1_1_df, by = "key_id") %>% 
    coalesce_join(top3b_1_2_df, by = "key_id") %>% 
    coalesce_join(top3b_2_1_df, by = "key_id") %>% 
    coalesce_join(top3b_2_2_df, by = "key_id")


# Join APOE columns -------------------------------------------------------
apoe_t2_df <- read_csv("data/ngs/apoe_t2.csv") %>% 
    rename(master_no = id,
           apoe_g_carrier = g_carrier,
           apoe_g_hom = g_hom,
           apoe_e4_carrier = e4_carrier,
           apoe_e4_hom = e4_hom) %>% 
    left_join(key_df %>% 
                  select(master_no, key_id),
              by = "master_no") %>% 
    select(-c(dementia, master_no))

master_df <- master_df %>% 
    full_join(apoe_t2_df,
              by = "key_id")


# Preprocess Karyotype Data ------------------------------------------------
karyotype_df <- read_csv("data/material/karyotype_patient.csv") %>% 
    bind_rows(read_csv("data/material/karyotype_normal.csv")) %>% 
    mutate(의뢰날짜 = ymd(의뢰날짜),
               karyotype = TRUE,
               식별코드 = str_replace(식별코드, "_", "-")) %>% 
    select(의뢰날짜, 관리번호, 식별코드,
               `결과 정상=0, 염색체 (상염색체, 성염색체)이상 =1`,
               `성염색체 이상 =1`, `Turner 45X`, XXX, `상염색체 이상`,
               `marker chr 이상`, `Klinefelter (XXY)`, `성별/나이`) %>% 
    rename(karyotype_registration_date = 1,
           karyotype_no = 2,
           master_no = 3,
           abnormal_chromosome = 4,
           abnormal_sex_chromosome = 5,
           turner = 6,
           xxx = 7,
           abnormal_autosome = 8,
           include_marker_chromosome = 9,
           klinefelter = 10) %>% 
    separate(`성별/나이`, c("gender", "나이"), "/") %>% 
    mutate(gender = recode(gender, 남 = "Male", 여 = "Female"),
           karyotype_registration_date = as.character(karyotype_registration_date)) %>% 
    replace_na(list(abnormal_chromosome = 0,
                    abnormal_sex_chromosome = 0,
                    abnormal_autosome = 0,
                    turner = 0,
                    xxx = 0,
                    include_marker_chromosome = 0,
                    klinefelter = 0)) %>% 
    left_join(key_df %>% 
                  select(master_no, key_id),
              by = "master_no")

master_df <- master_df %>% 
    coalesce_join(karyotype_df %>% 
                      mutate(karyotype = TRUE),
                  by = "key_id")


# Preprocess 2020-09-07 Karyotype Data ------------------------------------
master_df <- coalesce_join(master_df,
                     preprocess_karyotype_data(
                         read_csv("data/material/karyotype_patient_20200907.csv")
                     ) %>% 
                         mutate(dementia = 1,
                                karyotype_registration_date = as.character(karyotype_registration_date)) %>% 
                         left_join(key_df %>% 
                                       select(cancerrop_no, key_id),
                                   by = "cancerrop_no"),
                     by = "key_id")

master_df <- coalesce_join(master_df,
                     preprocess_karyotype_data(
                         read_csv("data/material/karyotype_normal_20200907.csv",
                                  col_types = cols(.default = "c"))
                     ) %>% 
                         mutate(dementia = 0,
                                karyotype_registration_date = as.character(karyotype_registration_date)) %>% 
                         left_join(key_df %>% 
                                       select(cancerrop_no, key_id),
                                   by = "cancerrop_no"),
                     by = "key_id")


# Relocate Columns ---------------------------------------------------------
master_df <- master_df %>% 
    relocate(drop) %>% 
    relocate(patient_id, .after = drop) %>% 
    relocate(cancerrop_no, .after = master_no) %>%
    relocate(ngs_2_no, .after = cancerrop_no) %>% 
    relocate(karyotype_no, .after = ngs_2_no) %>% 
    relocate(karyotype_2_no, .after = karyotype_no)
    
write_excel_csv(master_df, file.path("data/output/", paste0(today(),".csv")))


# Preprocess Chip Data ------------------------------------------------------
# chip_df <- read_csv("data/material/chip_patient.csv") %>%
#     rename(chip_abnormal_chromosome = `염색체 이상 (상염색체, 성염색체)=1`) %>% 
#     bind_rows(read_csv("data/material/chip_normal.csv") %>% 
#                   rename(chip_abnormal_chromosome = `염색체(상염색체, 성염색체) 이상 =1`)) %>%
#     mutate(식별코드 = str_replace(식별코드, "_", "-")) %>% 
#     rename(registration_date = `의뢰\n날짜`,
#            `농도 (ng/ul)` = `농도 \n(ng/ul)`,
#            `DNA농도(ng)` = `DNA농도\n(ng)`,
#            chip_abnormal_autosome = `상염색체 이상 =1`,
#            chip_abnormal_sex_chromosome = `성염색체 이상=1`,
#            management_number = 관리번호,
#            id = 식별코드) %>% 
#     replace_na(list(chip_abnormal_sex_chromosome = 0,
#                     chip_abnormal_autosome = 0,
#                     chip_abnormal_chromosome = 0)) %>% 
#     select(-c(No., 대리점, 의료기관명, 수진자명, `성별/나이`, `정상or 환자`,
#               기타기록사항))
# 
# master_df_t2_v4 <- master_df_t2_v3 %>% 
#     left_join(chip_df %>% 
#                   mutate(chip = TRUE),
#               by = "id") %>% 
#     mutate(registration_date.x = ifelse(is.na(registration_date.x),
#                                         registration_date.y,
#                                         registration_date.x)) %>% 
#     rename(registration_date = registration_date.x) %>% 
#     mutate(registration_date = as.Date(registration_date, origin = "1970-01-01")) %>% 
#     select(-registration_date.y)
# 
# write_excel_csv(master_df_t2_v4, "data/master_data_t2_v4.csv")
