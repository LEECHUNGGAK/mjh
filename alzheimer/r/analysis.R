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
               abnormal_chromosome, abnormal_sex_chromosome, turner, xxx,
               age, gender) %>% 
        separate_rows(MMSE, mmse_date, sep = "\n") %>% 
        mutate(measurement = "MMSE",
               MMSE = as.double(MMSE),
               mmse_date = ymd(mmse_date),
               diff_btw_re_and_me = difftime(mmse_date, registration_date, units = "days")) %>% 
        group_by(ID) %>% 
        mutate(rank = dense_rank(interaction(abs(diff_btw_re_and_me),
                                             mmse_date,
                                             lex.order = TRUE)),
               diff_btw_me = difftime(mmse_date, lag(mmse_date), units = "days")) %>% 
        rename(measurement_date = mmse_date,
               measurement_value = MMSE) %>% 
        bind_rows(data %>%
                      select(ID, registration_date, dementia, cdr_date,
                             cdr_sum_of_box, abnormal_chromosome,
                             abnormal_sex_chromosome, turner, xxx,
                             age, gender) %>% 
                      separate_rows(cdr_sum_of_box, cdr_date, sep = "\n") %>% 
                      filter(!is.na(cdr_sum_of_box) & cdr_sum_of_box != "ND") %>% 
                      mutate(measurement = "CDR sum of box",
                             cdr_sum_of_box = as.double(cdr_sum_of_box),
                             cdr_date = ymd(cdr_date),
                             diff_btw_re_and_me = difftime(cdr_date, registration_date, units = "days")) %>% 
                      group_by(ID) %>% 
                      mutate(rank = dense_rank(interaction(abs(diff_btw_re_and_me),
                                                           cdr_date,
                                                           lex.order = TRUE)),
                             diff_btw_me = difftime(cdr_date, lag(cdr_date), units = "days")) %>% 
                      rename(measurement_date = cdr_date,
                             measurement_value = cdr_sum_of_box)) %>% 
        group_by(ID, measurement) %>% 
        mutate(delta = measurement_value - lag(measurement_value),
               delta_per_month = delta /
                   (as.integer(diff_btw_me) / 365))
    
    return(result)
}

print_t_test <- function(data, column, condition, compare_delta = FALSE) {
    if (str_detect(column, "MMSE|CDR")) {
        x <- data %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       eval(parse_expr(paste(condition, "== 1")))) %>% 
            pull(measurement_value)
        y <- data %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       eval(parse_expr(paste(condition, "== 0")))) %>% 
            pull(measurement_value)
        result_table <- data %>% 
            ungroup() %>% 
            filter(measurement == column &
                       (rank == 1 | is.na(rank)) &
                       !is.na(measurement_value)) %>% 
            select(condition) %>% 
            table()
        
        print(paste("Compare the", column, "(t-test)"))
        print(result_table)
        try(print(t.test(x, y)))
        print(paste("SD of x:", sd(x, na.rm = TRUE)))
        print(paste("SD of y:", sd(y, na.rm = TRUE)))
        
        if (compare_delta) {
            x <- data %>% 
                filter(measurement == column &
                           eval(parse_expr(paste(condition, "== 1")))) %>% 
                pull(delta_per_month)
            y <- data %>% 
                filter(measurement == column &
                           eval(parse_expr(paste(condition, "== 0")))) %>% 
                pull(delta_per_month)
            result_table <- data %>% 
                ungroup() %>% 
                filter(measurement == column &
                           !is.na(delta_per_month)) %>% 
                select(condition) %>% 
                table()
            
            print(paste("Compare the change in", column, "(t-test)"))
            print(result_table)
            try(print(t.test(x, y)))
            print(paste("SD of x:", sd(x, na.rm = TRUE)))
            print(paste("SD of y:", sd(y, na.rm = TRUE)))
        }
    } else {
        x <- data %>%
            filter(eval(parse_expr(paste(condition, "== 1")))) %>%
            pull(column)
        y <- data %>%
            filter(eval(parse_expr(paste(condition, "== 0")))) %>%
            pull(column)
        result_table <- data %>%
            ungroup() %>% 
            filter(!is.na(column)) %>%
            select(condition) %>% 
            table()
        
        print(paste("Compare the", column, "(t-test)"))
        print(result_table)
        tryCatch(print(t.test(x, y)),
                 error = function(e) {
                     print(paste("Mean of x:", mean(x, na.rm = TRUE)))
                     print(paste("Mean of y:", mean(y, na.rm = TRUE)))
                 })
        print(paste("SD of x:", sd(x, na.rm = TRUE)))
        print(paste("SD of y:", sd(y, na.rm = TRUE)))
    }
}

print_chisq_test <- function(data, column, condition) {
    x <- table(data %>% 
                   pull(condition),
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
        print_t_test(data = data, column = i, condition = "dementia")
    }
    
    ## Discrete Variables
    for (i in c("성별", "hypertension", "hyperlipidemia", "diabetes", "stroke",
                "abnormal_chromosome", "abnormal_sex_chromosome",
                "abnormal_autosome", "turner", "xxx")) {
        print_chisq_test(data, column = i, condition = "dementia")
    }
    
    ## MMSE
    measurement_data <- process_measurement_data(data)
    
    print(paste("Compare MMSE between patient and normal group (t-test)"))
    print_t_test(data = measurement_data, column = "MMSE", condition = "dementia")
    sink()
}


# Manipulate Karyotype Data ----------------------------------------
master_df_t2_v3 <- read_csv("data/master_data_t2_v3.csv")

karyotype_df <- master_df_t2_v3 %>% 
    filter(karyotype == TRUE)

karyotype_measurement_df <- process_measurement_data(karyotype_df)
write_excel_csv(karyotype_measurement_df, "data/karyotype_measurement.csv")

# chip_df <- master_df_t2_v3 %>% 
#     filter(chip == TRUE)

top3b_df <- master_df_t2_v3 %>% 
    filter(top3b == TRUE) %>% 
    mutate(presence_major = ifelse(
        presence_22312315 + presence_22312350 + presence_22312351 == 3,
        TRUE,
        ifelse(presence_22312315 + presence_22312350 + presence_22312351 == 0,
               FALSE, NA)
    ))

top3b_measurement_df <- process_measurement_data(top3b_df)

top3b_major_df <- top3b_df %>% 
    filter(!is.na(presence_major))

top3b_major_measurement_df <- process_measurement_data(top3b_major_df) %>% 
    left_join(top3b_major_df %>% 
                  select(ID, presence_major),
              by = "ID")
# write_excel_csv(top3b_major_measurement_df, "data/top3b_major_measurement.csv")


# Karyotype & Chip Data Analysis -------------------------------------------------------
run_analysis(karyotype_df, "karyotype_analysis.txt")

## Abnormal vs. Normal Chromosome
patient_df <- karyotype_measurement_df %>%
    filter(dementia == 1)
normal_df <- karyotype_measurement_df %>%
    filter(dementia == 0)

sink("karyotype_analysis_2.txt")
for (i in c("abnormal_chromosome", "abnormal_sex_chromosome", "turner",
            "xxx")) {
    ## Patient
    print(paste("Patient;", i))
    
    for (j in c("MMSE", "CDR sum of box", "age")) {
        if (j == "age" & !str_detect(i, "^abnormal_")) {
            next()
        }
        
        print_t_test(patient_df, condition = i,  column = j, compare_delta = TRUE)
    }
    
    ## Normal
    for (j in c("All genders", "Male", "Female")) {
        if (i %in% c("turner", "xxx") & j != "Total") {
            next()
        }
        
        print(paste("Normal", i, j, sep = "; "))
        if (j != "All genders") {
            normal_temp_df <- normal_df %>% 
                filter(gender == j)
        } else {
            normal_temp_df <- normal_df
        }
        
        for (k in c("MMSE", "age")) {
            if (j != "All genders" & k == "MMSE") {
                next()
            }
            
            print_t_test(normal_temp_df, condition = i, column = k)
        }
        
    }
}
sink()


# Major TOP3B Data Analysis ----------------------------------------------------
run_analysis(top3b_df, "top3b_analysis.txt")

## Patient; Abnormal vs. Normal Chromosome
temp <- top3b_measurement_df %>%
    filter(dementia == 1)

sink("top3b_analysis_2.txt")
for (i in c("MMSE", "CDR sum of box")) {
    print_t_test(temp, column = i, condition = "abnormal_chromosome",
                 compare_delta = TRUE)
}

## Normal; Abnormal vs. Normal Chromosome
temp <- top3b_measurement_df %>%
    filter(dementia == 0)

print_t_test(temp, column = "MMSE", condition = "abnormal_chromosome")
sink()
