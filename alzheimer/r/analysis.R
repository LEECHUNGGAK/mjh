# Import Packages ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rlang)

setwd("C:/Users/Administrator/wd/alzheimer")


# ApoE chisq.test ---------------------------------------------------------
apoe_df <- read_csv("data/long_apoe_data.csv")

sink("ApoE_Chisq_Test_Result.txt")
for (i in colnames(apoe_df)[3:ncol(apoe_df)]) {
    temp <- table(apoe_df[[i]], apoe_df[, dementia])
    print(i)
    print(temp)
    print(chisq.test(temp))
}
sink()


# Function --------------------------------------
process_measurement_data <- function(data) {
    result <- data %>%
        select(ID, registration_date, dementia, mmse_date, MMSE,
               abnormal_chromosome) %>% 
        separate_rows(MMSE, mmse_date, sep = "\n") %>% 
        mutate(measurement = "MMSE",
               MMSE = as.double(MMSE),
               mmse_date = ymd(mmse_date),
               difference_between_date = abs(difftime(mmse_date, registration_date, units = "days"))) %>% 
        group_by(ID) %>% 
        mutate(rank = dense_rank(difference_between_date)) %>% 
        rename(measurement_date = mmse_date,
               measurement_value = MMSE) %>% 
        bind_rows(data %>%
                      select(ID, registration_date, dementia, cdr_date,
                             cdr_sum_of_box, abnormal_chromosome) %>% 
                      separate_rows(cdr_sum_of_box, cdr_date, sep = "\n") %>% 
                      filter(!is.na(cdr_sum_of_box) & cdr_sum_of_box != "ND") %>% 
                      mutate(measurement = "CDR sum of box",
                             cdr_sum_of_box = as.double(cdr_sum_of_box),
                             cdr_date = ymd(cdr_date),
                             difference_between_date = abs(difftime(cdr_date, registration_date, units = "days"))) %>% 
                      group_by(ID) %>% 
                      mutate(rank = dense_rank(difference_between_date)) %>% 
                      rename(measurement_date = cdr_date,
                             measurement_value = cdr_sum_of_box)) %>% 
        group_by(ID, measurement) %>% 
        mutate(delta = measurement_value - lag(measurement_value),
               delta_per_month = delta /
                   (as.integer(difference_between_date) / 30))
    
    return(result)
}

print_t_test <- function(data, column, statement, compare_delta = FALSE) {
    if (str_detect(column, "MMSE|CDR")) {
        print(paste("Compare", column))
        x <- data %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       eval(parse_expr(paste(statement, "== 1")))) %>% 
            pull(measurement_value)
        y <- data %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       eval(parse_expr(paste(statement, "== 0")))) %>% 
            pull(measurement_value)
        result_table <- data %>% 
            ungroup() %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       !is.na(measurement_value)) %>% 
            select(statement) %>% 
            table()
        
        print(paste("Compare the", column, "(t-test)"))
        print(result_table)
        print(t.test(x, y))
        print(paste("sd of x:", sd(x, na.rm = TRUE)))
        print(paste("sd of y:", sd(y, na.rm = TRUE)))
        
        if (compare_delta) {
            print(paste("Compare the change in", column, "(t-test)"))
            x <- data %>% 
                filter(measurement == column &
                           eval(parse_expr(paste(statement, "== 1")))) %>% 
                pull(delta_per_month)
            y <- data %>% 
                filter(measurement == column &
                           eval(parse_expr(paste(statement, "== 0")))) %>% 
                pull(delta_per_month)
            result_table <- data %>% 
                ungroup() %>% 
                filter(measurement == column &
                           !is.na(delta_per_month)) %>% 
                select(statement) %>% 
                table()
            
            print(result_table)
            print(t.test(x, y))
            print(paste("sd of x:", sd(x, na.rm = TRUE)))
            print(paste("sd of y:", sd(y, na.rm = TRUE)))
        }
    } else {
        x <- data %>%
            filter(eval(parse_expr(paste(statement, "== 1")))) %>%
            pull(column)
        y <- data %>%
            filter(eval(parse_expr(paste(statement, "== 0")))) %>%
            pull(column)
        result_table <- data %>%
            filter(!is.na(column)) %>%
            select(statement) %>% 
            table()
        
        print(paste("Compare the", column, "(t-test)"))
        print(result_table)
        print(t.test(x, y))
        print(paste("sd of x:", sd(x, na.rm = TRUE)))
        print(paste("sd of y:", sd(y, na.rm = TRUE)))
    }
}

print_chisq_test <- function(data, column, statement) {
    x <- table(data %>% 
                   pull(statement),
               data %>% 
                   pull(column))
    
    print(paste("Compare the", column, "(Chi-squared test)"))
    print(x)
    print(chisq.test(x))
}

run_analysis <- function(data, file_name) {
    sink(file_name)
    ## Continuous Variables
    for (i in c("나이", "years_of_education")) {
        print_t_test(data = data, column = i, statement = "dementia")
    }
    
    ## Discrete Variables
    for (i in c("성별", "hypertension", "hyperlipidemia", "diabetes", "stroke",
                "abnormal_chromosome", "abnormal_sex_chromosome",
                "abnormal_autosome", "turner", "xxx")) {
        print_chisq_test(data, column = i, statement = "dementia")
    }
    
    ## MMSE
    measurement_data <- process_measurement_data(data)
    
    print(paste("Compare MMSE between patient and normal group (t-test)"))
    print_t_test(data = measurement_data %>% 
                     filter(measurement == "MMSE" & (rank == 2 | is.na(rank))),
                 column = "measurement_value", statement = "dementia")
    sink()
}


# Manipulate Karyotype & Chip Data ----------------------------------------
master_df_t2_v4 <- read_csv("data/master_data_t2_v4.csv")

karyotype_df <- master_df_t2_v4 %>% 
    filter(karyotype == TRUE)

karyotype_measurement_df <- process_measurement_data(karyotype_df)
# write_excel_csv(karyotype_measurement_df, "data/karyotype_measurement.csv")

chip_df <- master_df_t2_v4 %>% 
    filter(chip == TRUE)

top3b_df <- master_df_t2_v4 %>% 
    filter(top3b == TRUE) %>% 
    mutate(presence_major = ifelse(
        presence_22312315 + presence_22312350 + presence_22312351 == 3,
        TRUE,
        ifelse(presence_22312315 + presence_22312350 + presence_22312351 == 0,
               FALSE, NA)
    ))

top3b_major_df <- top3b_df %>% 
    filter(!is.na(presence_major))

top3b_major_measurement_df <- process_measurement_data(top3b_major_df) %>% 
    left_join(top3b_major_df %>% 
                  select(ID, presence_major),
              by = "ID")
# write_excel_csv(top3b_major_measurement_df, "data/top3b_major_measurement.csv")


# Karyotype & Chip Data Analysis -------------------------------------------------------
run_analysis(karyotype_df, "karyotype_analysis.txt")

## Patient; Abnormal vs. Normal Chromosome
temp <- karyotype_measurement_df %>%
    filter(dementia == 1)

sink("karyotype_analysis_2.txt")
print("Patient; Abnormal vs. Normal Chromosome")
for (i in c("MMSE", "CDR sum of box")) {
    print_t_test(temp, column = i, statement = "abnormal_chromosome",
                 compare_delta = TRUE)
}

## Normal; Abnormal vs. Normal Chromosome
print("Normal; Abnormal vs. Normal Chromosome")
temp <- karyotype_measurement_df %>%
    filter(dementia == 0)

print_t_test(temp, column = "MMSE", statement = "abnormal_chromosome")
sink()

# Major TOP3B Data Analysis ----------------------------------------------------
run_analysis(top3b_df, "top3b_analysis.txt")

## Patient; Abnormal vs. Normal Chromosome
temp <- top3b_measurement_df %>%
    filter(dementia == 1)

sink("top3b_analysis_2.txt")
for (i in c("MMSE", "CDR sum of box")) {
    print_t_test(temp, column = i, statement = "abnormal_chromosome",
                 compare_delta = TRUE)
}

## Normal; Abnormal vs. Normal Chromosome
temp <- top3b_measurement_df %>%
    filter(dementia == 0)

print_t_test(temp, column = "MMSE", statement = "abnormal_chromosome")
sink()
