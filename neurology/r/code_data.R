# Setup -------------------------------------------------------------------
library(tidyverse)


# Function ----------------------------------------------------------------
detect_subject <- function(x) {
    o1 <- ifelse(str_detect(x, "어머니"), 1, "")
    o2 <- ifelse(str_detect(x, "아버지"), 2, "")
    o4 <- ifelse(str_detect(x, "자매|동생|누나|언니"), 4, "")
    o5 <- ifelse(str_detect(x, "할머니|외가|이모"), 5, "")
    
    return(str_replace(paste0(o1, o2, o4, o5), "12", "3"))
}


# Process data ------------------------------------------------------------
col_names <- read_csv("C:/Users/Administrator/work/alzheimer/column_names.csv",
                      col_names = FALSE) %>% 
    distinct() %>%
    mutate(value = NA)

col_names_order <- col_names$X1[!(col_names$X1 %in% c("몸무게", "APOE genotype"))]

col_names_t <- col_names %>% 
    spread("X1", "value") %>% 
    select(col_names_order)

old <- read_csv("C:/Users/Administrator/work/alzheimer/data/output/2021-03-05.csv",
                col_types = cols(.default = "c")) %>% 
    filter(drop != 1) %>% 
    rename(object_idx = key_id,
           Hospital_No = patient_id,
           TEST_DAY = measurement_date,
           birthdate = birth_date,
           age = age_new,
           smoking_a = smoking,
           APOE_genotype = ApoE,
           gdeps_total = GDS,
           CDR_STEP = CDR,
           K_MMSE_total_score = MMSE) %>% 
    mutate(selection_num = str_count(TEST_DAY, "\n") + 1,
           sex = ifelse(gender == "Male", 1,
                        ifelse(gender == "Female", 2, NA)),
           amyloid_PET_positivity = ifelse(PET결과 == "positive", 1, 
                                           ifelse(PET결과 == "negative", 0, NA)),
           stool = ifelse(!is.na(분변수거) & 분변수거 != "X", 1, 0),
           stool_date = ifelse(stool == 1, 분변수거, NA),
           alcohol_a = as.double(alcohol_a),
           alcohol_b = as.double(alcohol_b),
           alcohol_c = alcohol_a * alcohol_b,
           smoking_b = cut(as.double(smoking_b),
                           breaks = c(1, 10, 20, 30, 40, 60, Inf),
                           labels = c(1, 2, 3, 4, 5, 6),
                           right = FALSE),
           fh_D = ifelse(family_history_dementia == 1, detect_subject(치매가족력), ""),
           체중 = as.double(체중),
           키 = as.double(키),
           `BMI (Body Mass Index)` = 체중 / (키 / 100)^2,
           혈압 = paste(SBP, DBP, sep = "/"),
           mh_H = ifelse(!is.na(hypertension) & hypertension == 0, 1, 
                         ifelse(!is.na(hypertension) & hypertension == 1, 2, 3)),
           mh_D = ifelse(!is.na(diabetes) & diabetes == 0, 1, 
                         ifelse(!is.na(diabetes) & diabetes == 1, 2, 3)),
           mh_L = ifelse(!is.na(hyperlipidemia) & hyperlipidemia == 0, 1, 
                         ifelse(!is.na(hyperlipidemia) & hyperlipidemia == 1, 2, 3)),
           mh_C = ifelse(!is.na(cardiac_disease) & cardiac_disease == 0, 1, 
                         ifelse(!is.na(cardiac_disease) & cardiac_disease == 1, 2, 3)),
           mh_S = ifelse(!is.na(stroke) & stroke == 0, 1,
                         ifelse(!is.na(stroke) & stroke == 1, 2, 3))) %>% 
    select(colnames(old)[colnames(old) %in% col_names_order])

out <- col_names_t %>% 
    bind_rows(old) %>% 
    slice(-1)
    

write_excel_csv(out, "debug.csv")
