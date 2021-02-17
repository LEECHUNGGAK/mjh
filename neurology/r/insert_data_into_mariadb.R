# Person ------------------------------------------------------------------
person_table <- master_dataframe %>% 
    select(병록번호, 성별, 생년월일) %>% 
    rename(person_id = 1,
           gender = 2,
           birth_date = 3) %>% 
    drop_na(person_id) %>% 
    mutate(gender = ifelse(gender == "M", "Male", "Female"))

write_csv(person_table, "data/mariadb/person_table.csv", na = "NULL")


# Measurement -------------------------------------------------------------
# MMSE
measurement_table <- master_dataframe %>% 
    select(병록번호, MMSE, `MMSE 시행날짜`) %>% 
    separate_rows(MMSE, `MMSE 시행날짜`, sep = "\n") %>% 
    mutate(measurement_concept = "Mini mental test examination",
           measurement_date = ymd(`MMSE 시행날짜`)) %>% 
    select(-`MMSE 시행날짜`) %>% 
    rename(person_id = 병록번호,
           value_as_number = MMSE) %>%
    # CDR
    bind_rows(master_dataframe %>% 
                  select(병록번호, CDR, `CDR 시행날짜`) %>% 
                  separate_rows(CDR, `CDR 시행날짜`, sep = "\n") %>% 
                  mutate(measurement_concept = "Clinical dementia rating",
                         measurement_date = ymd(`CDR 시행날짜`)) %>% 
                  select(-`CDR 시행날짜`) %>% 
                  rename(person_id = 병록번호,
                         value_as_number = CDR)) %>% 
    # CDR-sum of boxes
    bind_rows(master_dataframe %>% 
                  select(병록번호, `CDR_Sum of box`, `CDR 시행날짜`) %>% 
                  separate_rows(`CDR_Sum of box`, `CDR 시행날짜`, sep = "\n") %>% 
                  mutate(measurement_concept = "Clinical dementia rating-sum of boxes",
                         measurement_date = ymd(`CDR 시행날짜`)) %>% 
                  select(-`CDR 시행날짜`) %>% 
                  rename(person_id = 병록번호,
                         value_as_number = `CDR_Sum of box`)) %>% 
    # GDR
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

# Height, Weight, SBP, DBP, Pulse rate
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


# Observation - value_as_string -------------------------------------------
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


# Observation - value_as_number -------------------------------------------
observation_table_2 <- master_dataframe %>% 
    select(병록번호, 등재일, years_of_education) %>% 
    gather(key = observation_concept, value = value_as_number,
           years_of_education) %>% 
    drop_na() %>% 
    mutate(observation_concept = "Years of education") %>% 
    rename(person_id = 병록번호, observation_date = 등재일) %>% 
    relocate(observation_date, .after = observation_concept)

write_csv(observation_table_2, "data/mariadb/observation_table_2.csv")