# Generate Key Dataframe --------------------------------------------------
key_master_df <- read_csv("data/material/master_p.csv",
                          col_types = cols(.default = "c")) %>% 
    bind_rows(read_csv("data/material/master_c.csv",
                       col_types = cols(.default = "c"))) %>% 
    drop_na(No) %>% 
    select(병록번호, 이름, 성별, 생년월일, No) %>% 
    rename(patient_id = 1, name = 2, gender = 3, birth_date = 4, master_no = 5) %>% 
    mutate(master_no = str_replace(master_no, "_", "-"),
        gender = recode(gender, M = "Male", F = "Female"))

key_cancerrop_df <- read_csv("data/material/cancerrop_patient.csv",
                             col_types = cols(.default = "c")) %>% 
    bind_rows(read_csv("data/material/cancerrop_normal.csv",
                       col_types = cols(.default = "c")) %>% 
                  rename(최근CT검사일 = CT검사일,
                           최근MRI검사일 = MRI검사일)) %>% 
    drop_na(No) %>% 
    select(병록번호, 이름, 성별, 생년월일, No) %>% 
    rename(patient_id = 1,
           name = 2,
           gender = 3,
           birth_date = 4,
           cancerrop_no = 5) %>% 
    mutate(birth_date = as.character(ymd(paste0("19", birth_date))),
           gender = recode(gender, M = "Male", F = "Female")) %>% 
    bind_rows(read_csv("data/key/csv/cancerrop.csv",
                       col_types = cols(.default = "c")))

key_karyotype_df <- read_csv("data/material/karyotype_patient.csv") %>% 
    bind_rows(read_csv("data/material/karyotype_normal.csv")) %>% 
    select(`성별/나이`, 식별코드, 관리번호) %>% 
    rename(gender = 1, 
           master_no = 2,
           karyotype_no = 3) %>% 
    mutate(gender = ifelse(str_detect(gender, "^남"), "Male",
                           ifelse(str_detect(gender, "^여"), "Female", NA)),
           master_no = str_replace(master_no, "_", "-"))

key_ngs_2_df <- read_csv("data/key/csv/ngs_2.csv",
                         col_types = cols(.default = "c"))

key_karyotype_2_df <- read_csv("data/key/csv/karyotype_2.csv",
                               col_types = cols(.default = "c")) %>% 
    mutate(cancerrop_no = str_sub(cancerrop_no, 2, str_length(cancerrop_no)),
           gender = ifelse(str_detect(gender, "^남"), "Male",
                           ifelse(str_detect(gender, "^여"), "Female", NA)))

key_df <- key_master_df %>% 
    full_join(key_cancerrop_df, by = "patient_id") %>%
    mutate(name = coalesce(name.x, name.y),
           gender = coalesce(gender.x, gender.y),
           birth_date = coalesce(birth_date.x, birth_date.y)) %>% 
    select(-ends_with(".x"), -ends_with(".y")) %>% 
    full_join(key_karyotype_df, by = "master_no") %>% 
    mutate(gender = coalesce(gender.x, gender.y)) %>% 
    select(-ends_with(".x"), -ends_with(".y")) %>% 
    full_join(key_ngs_2_df, by = c("name", "gender", "birth_date")) %>% 
    full_join(key_karyotype_2_df, by = "cancerrop_no") %>% 
    mutate(name = coalesce(name.x, name.y),
           gender = coalesce(gender.x, gender.y)) %>% 
    select(-ends_with(".x"), -ends_with(".y")) %>% 
    mutate(key_id = as.character(1:nrow(.)))
write_excel_csv(key_df, "data/key/output/key_20201118.csv")
key_df <- read_csv("data/key/key.csv")