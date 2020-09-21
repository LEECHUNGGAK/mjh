# Import Packages ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rlang)

setwd("C:/Users/Administrator/wd/alzheimer")


# ApoE chisq.test ---------------------------------------------------------
apoe_df <- read_csv("data/ngs/apoe_data.csv")

work_df <- apoe_df %>% 
    filter(Coordinate == 45409167)

sink("ApoE_Chisq_Test.txt")
for (i in c("g_carrier", "g_hom")) {
    temp <- table(work_df %>% 
                      pull(dementia),
                  work_df %>% 
                      pull(i))
    rownames(temp) <- c("Normal", "Dementia")
    colnames(temp) <- c("Normal", i)
    
    print(i)
    print(temp)
    print(chisq.test(temp))
}
sink()

for (i in colnames(long_apoe_df)[8:ncol(long_apoe_df)]) {
    temp <- table(long_apoe_df %>% 
                      pull(dementia),
                  long_apoe_df %>% 
                      pull(i))
    rownames(temp) <- c("Normal", "Dementia")
    colnames(temp) <- c("Normal", "SNV")
    
    print(i)
    print(temp)
    print(chisq.test(temp))
}
sink()

apoe_df <- read_csv("data/m_apoe_data.csv")

# Check the Variant
apoe_df %>% 
    filter(Coordinate == 45409167 & Genotype == "hom") %>% 
    distinct(Variant)

apoe_genotype <- table(apoe_df %>% 
                           filter(Coordinate == 45409167) %>% 
                           distinct(id, Genotype, dementia) %>% 
                           pull(dementia),
                       apoe_df %>% 
                           filter(Coordinate == 45409167) %>% 
                           distinct(id, Genotype, dementia) %>% 
                           pull(Genotype))
print(apoe_genotype)
print(chisq.test(apoe_genotype))


# Function --------------------------------------
process_measurement_data <- function(data) {
    result <- data %>%
        select(id, registration_date, dementia, mmse_date, MMSE,
               abnormal_chromosome, abnormal_sex_chromosome, turner, xxx,
               age, gender, presence_major, apoe_e4) %>% 
        separate_rows(MMSE, mmse_date, sep = "\n") %>% 
        mutate(measurement = "MMSE",
               MMSE = as.double(MMSE),
               mmse_date = ymd(mmse_date),
               diff_btw_re_and_me = difftime(mmse_date, registration_date, units = "days")) %>% 
        group_by(id) %>% 
        mutate(rank = dense_rank(interaction(diff_btw_re_and_me,
                                             mmse_date,
                                             lex.order = TRUE)),
               diff_btw_me = difftime(mmse_date, lag(mmse_date), units = "days")) %>% 
        rename(measurement_date = mmse_date,
               measurement_value = MMSE) %>% 
        bind_rows(data %>%
                      select(id, registration_date, dementia, cdr_date,
                             cdr_sum_of_box, abnormal_chromosome,
                             abnormal_sex_chromosome, turner, xxx,
                             age, gender, presence_major, apoe_e4) %>% 
                      separate_rows(cdr_sum_of_box, cdr_date, sep = "\n") %>% 
                      filter(!is.na(cdr_sum_of_box) & cdr_sum_of_box != "ND") %>% 
                      mutate(measurement = "CDR sum of box",
                             cdr_sum_of_box = as.double(cdr_sum_of_box),
                             cdr_date = ymd(cdr_date),
                             diff_btw_re_and_me = difftime(cdr_date, registration_date, units = "days")) %>% 
                      group_by(id) %>% 
                      mutate(rank = dense_rank(interaction(diff_btw_re_and_me,
                                                           cdr_date,
                                                           lex.order = TRUE)),
                             diff_btw_me = difftime(cdr_date, lag(cdr_date), units = "days")) %>% 
                      rename(measurement_date = cdr_date,
                             measurement_value = cdr_sum_of_box)) %>% 
        group_by(id, measurement) %>% 
        mutate(delta = measurement_value - lag(measurement_value),
               delta_per_month = delta /
                   (as.integer(diff_btw_me) / 365))
    
    return(result)
}

print_t_test <- function(data, column, condition, compare_delta = FALSE) {
    if (str_detect(column, "MMSE|CDR sum of box")) {
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

run_analysis <- function(data, file_name, condition, top3b = FALSE) {
    sink(file_name)
    ## Continuous Variables
    for (i in c("age", "years_of_education")) {
        print_t_test(data = data, column = i, condition = condition)
    }
    
    discrete_variables_v <- c(
        "gender", "hypertension", "hyperlipidemia", "diabetes", "stroke",
        "abnormal_chromosome", "abnormal_sex_chromosome",
        "abnormal_autosome", "turner", "xxx", "apoe_e4", "include_marker_chromosome"
    )
    
    measurement_data <- process_measurement_data(data)
    
    if (top3b) {
        discrete_variables_v <- c(discrete_variables_v, "presence_major")
        
        measurement_data <- measurement_data %>% 
            left_join(data %>% 
                          select(id, presence_major),
                      by = "id")
    }
    
    ## Discrete Variables
    for (i in discrete_variables_v) {
        print_chisq_test(data, column = i, condition = condition)
    }
    
    ## MMSE
    
    print(paste("Compare MMSE between patient and normal group (t-test)"))
    print_t_test(data = measurement_data, column = "MMSE", condition = condition)
    sink()
}


# Manipulate Karyotype Data ----------------------------------------
master_df_t2_v3 <- read_csv("data/master_data_t2_v3.csv")

master_measurement_df <- process_measurement_data(master_df_t2_v3)
write_csv(master_measurement_df, "data/master_measurement.csv")

# 2020-09-18-Use 2019 karyotype data without adding 2018 karyotype data
karyotype_df <- master_df_t2_v3 %>% 
    filter(karyotype == TRUE)

karyotype_measurement_df <- process_measurement_data(karyotype_df)
# write_excel_csv(karyotype_measurement_df, "data/karyotype_measurement.csv")

# chip_df <- master_df_t2_v3 %>% 
#     filter(chip == TRUE)

top3b_df <- master_df_t2_v3 %>% 
    filter(top3b == TRUE)

top3b_measurement_df <- process_measurement_data(top3b_df)


# Karyotype & Chip Data Analysis -------------------------------------------------------
run_analysis(karyotype_df, "karyotype_analysis.txt", condition = "dementia")

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
        
        if (j == "age") {
            print_t_test(karyotype_df %>% 
                             filter(dementia == 1),
                         condition = i, column = j)
        } else {
            print_t_test(patient_df, condition = i,  column = j, 
                         compare_delta = TRUE)
        }
    }
    print_chisq_test(karyotype_df %>% 
                         filter(dementia == 1),
                     condition = i, column = "apoe_e4")
    
    ## Normal
    for (j in c("All genders", "Male", "Female")) {
        if (i %in% c("turner", "xxx") & j != "All genders") {
            next()
        }
        
        print(paste("Normal", i, j, sep = "; "))
        if (j != "All genders") {
            normal_temp_df <- normal_df %>% 
                filter(gender == j)
            karyotype_temp_df <- karyotype_df %>% 
                filter(dementia == 0 & gender == j)
        } else {
            normal_temp_df <- normal_df
            karyotype_temp_df <- karyotype_df %>% 
                filter(dementia == 0)
        }
        
        for (k in c("MMSE", "age")) {
            if (j != "All genders" & k == "MMSE") {
                next()
            }
            if (k == "age") {
                print_t_test(karyotype_temp_df, condition = i, column = k)
            } else {
                print_t_test(normal_temp_df, condition = i, column = k)
            }
        }
        print_chisq_test(karyotype_temp_df,
                         condition = i, column = "apoe_e4")
    }
}
sink()

sink("apoe_analysis.txt")
for (dementia_var in 0:1) {
    for (condition_var in c("abnormal_chromosome", "abnormal_sex_chromosome",
                            "turner", "xxx")) {
        print(paste("Dementia:", dementia_var, "Condition:", condition_var))
        print_chisq_test(karyotype_df %>% 
                             filter(dementia == dementia_var),
                         condition = condition_var,
                         column = "apoe_e4")
        
        if (dementia_var == 0 & condition_var == "abnormal_sex_chromosome") {
            for (gender_var in c("Male", "Female")) {
                print(paste("Dementia:", dementia_var, "Condition:", condition_var,
                            "Gender:", gender_var))
                print_chisq_test(karyotype_df %>% 
                                     filter(dementia == dementia_var &
                                                gender == gender_var),
                                 condition = condition_var,
                                 column = "apoe_e4")
            }
        }
    }
}
sink()


# Major TOP3B Data Analysis ----------------------------------------------------
run_analysis(top3b_df, "top3b_analysis.txt", condition = "dementia",
             top3b = TRUE)
run_analysis(top3b_df %>% 
                 filter(dementia == 1 & !is.na(presence_major)),
             "top3b_analysis_patient.txt",
             condition = "presence_major")
run_analysis(top3b_df %>% 
                 filter(dementia == 0 & !is.na(presence_major)),
             "top3b_analysis_normal.txt",
             condition = "presence_major")

## Patient; Abnormal vs. Normal Chromosome
sink("top3b_analysis_2.txt")
print("Patient; Abnormal vs. Normal Chromosome")
for (i in c("MMSE", "CDR sum of box")) {
    print_t_test(top3b_measurement_df %>% 
                     filter(dementia == 1 & !is.na(presence_major)),
                 column = i, condition = "presence_major",
                 compare_delta = TRUE)
}

## Normal; Abnormal vs. Normal Chromosome
print("Normal; Abnormal vs. Normal Chromosome")
print_t_test(top3b_measurement_df %>% 
                 filter(dementia == 0 & !is.na(presence_major)),
             column = "MMSE", condition = "presence_major",
             compare_delta = TRUE)
sink()


# Check the Interaction between ApoE E4 and Abnormal Sex Chromosome --------
sink("interaction.txt")
for (i in c("abnormal_sex_chromosome", "presence_major")) {
    print(paste("The Interaction between ApoE E4 and", i))
    f <- as.formula(paste("dementia ~ apoe_e4 +", i, "+ apoe_e4 *", i))
    lr <- glm(formula = f,
              data = master_df_t2_v4,
              family = "binomial")
    print(summary(lr))
    
    temp <- master_df_t2_v4 %>% 
        mutate(interaction := apoe_e4 * !!sym(i)) %>% 
        select(apoe_e4, !!i, interaction, dementia) %>% 
        drop_na()
    
    print("dementia n")
    temp %>% 
        filter(dementia == 1) %>% 
        summarize(n = n()) %>% 
        print()
    
    for (j in 1:3) {
        for (k in c("interaction", "dementia")) {
            print(paste(names(temp)[j], k))
            temp %>% 
                filter_at(j, all_vars(. == 1)) %>%
                group_by_at(k) %>% 
                summarize(n = n()) %>% 
                mutate(proportion = n / sum(n)) %>% 
                print()
        }
    }
}
sink()

karyotype_measurement_df %>% 
    filter(xxx == 1 & measurement == "CDR sum of box") %>% 
    group_by(id) %>% 
    summarize(n = n()) %>% 
    filter(n > 1) %>% 
    left_join(karyotype_measurement_df, by = "id") %>% 
    filter(measurement == "CDR sum of box") %>% 
    select(id, dementia, xxx, registration_date, measurement_date, diff_btw_re_and_me,
           diff_btw_me, rank, measurement_value, measurement, delta, delta_per_month) %>% 
    write_csv("check_xxx_delta_cdr_sob.csv")
