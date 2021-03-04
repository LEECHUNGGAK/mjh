# Setup -------------------------------------------------------------------
library(tidyverse)

col_names <- read_csv("C:/Users/Administrator/work/alzheimer/column_names.csv",
                col_types = cols(.default = "c"),
                col_names = FALSE) %>% 
    distinct() %>% 
    mutate(value = NA) %>% 
    spread("X1", "value")

old <- read_csv("C:/Users/Administrator/work/alzheimer/data/output/2021-01-12.csv",
                col_types = cols(.default = "c")) %>% 
    rename(object_idx = key_id,
           Hospital_No = patient_id,
           TEST_DAY = measurement_date,
           sex = gender,
           birthdate = birth_date,
           age = age_new,
           education_year = years_of_education,
           mh_H = hypertension,
           mh_D = diabetes,
           mh_L = hyperlipidemia,
           mh_C = cardiac_disease,
           ml_S = stroke,
           fh_D = family_history_dementia)
