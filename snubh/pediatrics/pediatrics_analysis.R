# Set up the environment --------------------------------------------------
library(tidyverse)

setwd("C:/Users/Administrator/wd/snubh")


# Functions ---------------------------------------------------------------
read_data <- function(file_path) {
    d <- str_split(read_file(file_path), "\n")[[1]]
    
    if (length(d) > 3) {
        if (!str_detect(d[1], "\"FALSE\",\"TRUE\"")) {
            if (str_detect(d[1], "TRUE")) {
                d[1] <- paste0("\"FALSE\",", d[1])
                d[2] <- str_replace(d[2], ",", ",0,")
                d[3] <- str_replace(d[3], ",", ",0,")
            } else {
                d[1] <- str_replace(d[1], "\r", ",\"TRUE\"\r")
                d[2] <- str_replace(d[2], "\r", ",0\r")
                d[3] <- str_replace(d[3], "\r", ",0\r")
            }
            result <- as.matrix(read_csv(d, col_names = FALSE, skip = 1)[, -1])
        } else {
            result <- as.matrix(read_csv(file_path, col_names = FALSE, skip = 1)[, -1])
        }
    } else if (length(d) == 3) {
        if (!str_detect(d[1], "\"FALSE\",\"TRUE\"")) {
            if (str_detect(d[1], "TRUE")) {
                d[1] <- paste0("\"FALSE\",", d[1])
                d[2] <- str_replace(d[2], ",", ",0,")
                d[3] <- str_replace(d[3], ",", ",0,")
            } else {
                d[1] <- str_replace(d[1], "\r", ",\"TRUE\"\r")
                d[2] <- str_replace(d[2], "\r", ",0\r")
                d[3] <- str_replace(d[3], "\r", ",0\r")
            }
        }
        
        if (str_detect(d[2], "TRUE")) {
            d[3] <- d[2]
            d[2] <- "\"FALSE\",0,0\r"
        } else {
            d[3] <- "\"TRUE\",0,0\r"
        }
        
        result <- as.matrix(read_csv(d, col_names = FALSE, skip = 1)[, -1])
    } else if (length(d) < 3) {
        result <- matrix(c(0, 0, 0, 0), ncol = 2)
    }
    
    result[is.na(result)] <- 0
        
    return(result)
}

t.test2 <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal_variance = FALSE) {
    if(equal_variance == FALSE) {
        se <- sqrt((s1^2/n1) + (s2^2/n2))
        # welch-satterthwaite df
        df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    } else {
        # pooled standard deviation, scaled by the sample sizes
        se <- sqrt((1 / n1 + 1 / n2) * ((n1 - 1)*s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)) 
        df <- n1 + n2 - 2
    }
    t <- (m1 - m2 - m0) / se 
    dat <- c(m1 - m2, se, t, 2 * pt(-abs(t), df))    
    names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
    return(dat)
}


# McNemar's Chi-squared Test ----------------------------------------------
file_v <- list.files("C:/Users/Administrator/Documents/snubh/snubh")

sink("mcnemar_test.txt")
for (i in file_v) {
    snubh <- read_data(file.path("snubh", i))
    snuh <- read_data(file.path("snuh", i))
    amc <- read_data(file.path("amc", i))
    
    combine <- snubh + snuh + amc
    print(paste("Compare", i, "(McNemar's Chi-squared Test)"))
    print(combine)
    print(mcnemar.test(combine))
}
sink()


# Student t-Test ----------------------------------------------------------
snubh_df <- read_csv("snubh/snubh_output_v4.csv",
                     col_types = cols(time = col_integer()))
snuh_df <- read_csv("snuh/snuh_output_v4.csv",
                    col_types = cols(time = col_integer()))
amc_df <- read_csv("amc/amc_output_v4.csv",
                   col_types = cols(time = col_integer()))

time_v <- c(0, 3, 6, 12, 24, 36, 48, 60)

weighted_mean_df <- read_csv(
    "C:/Users/Administrator/wd/snubh/weighted_mean_data.csv"
)

t_df <- snubh_df %>% 
    mutate(hospital = "SNUBH") %>% 
    bind_rows(snuh_df %>% 
                  mutate(hospital = "SNUH")) %>% 
    bind_rows(amc_df %>% 
                  mutate(hospital = "AMC")) %>% 
    group_by(measurement, time, dm, group) %>% 
    summarize(total_value = sum(value),
              total_n = sum(n)) %>% 
    mutate(sd = sqrt(total_value / total_n)) %>% 
    left_join(weighted_mean_df,
              by = c("measurement", "time", "dm", "group")) %>% 
    rename(Mean = value)

sink("t_test.txt")
for (measurement_var in c("zbmi", "HBA1C")) {
    for (time_var in c(36, 60)) {
        for (dm_var in c("Type 1", "Type 2")) {
            for (group_var in c("total", "0-4Y", "5-9Y", "10-14Y", "Male",
                                "Female")) {
                if (dm_var == "Type 2" & group_var == "0-4Y") {
                    next()
                }
                t_test_df <- data.frame(m1 = t_df %>% 
                                            filter(measurement == measurement_var &
                                                       time == 3 &
                                                       dm == dm_var &
                                                       group == group_var) %>% 
                                            pull(Mean),
                                        m2 = t_df %>% 
                                            filter(measurement == measurement_var &
                                                       time == time_var &
                                                       dm == dm_var &
                                                       group == group_var) %>% 
                                            pull(Mean),
                                        s1 = t_df %>% 
                                            filter(measurement == measurement_var &
                                                       time == 3 &
                                                       dm == dm_var &
                                                       group == group_var) %>%
                                            pull(sd),
                                        s2 = t_df %>%
                                            filter(measurement == measurement_var &
                                                       time == time_var &
                                                       dm == dm_var &
                                                       group == group_var) %>%
                                            pull(sd),
                                        n1 = t_df %>%
                                            filter(measurement == measurement_var &
                                                       time == 3 &
                                                       dm == dm_var &
                                                       group == group_var) %>%
                                            pull(total_n),
                                        n2 = t_df %>%
                                            filter(measurement == measurement_var &
                                                       time == time_var &
                                                       dm == dm_var &
                                                       group == group_var) %>%
                                            pull(total_n))
                print(paste("Compare", measurement_var, "at", time_var,
                            "month in", dm_var, group_var, "patients"))
                print(t_test_df)
                print(t.test2(m1 = t_test_df$m1,
                              m2 = t_test_df$m2,
                              s1 = t_test_df$s1,
                              s2 = t_test_df$s2,
                              n1 = t_test_df$n1,
                              n2 = t_test_df$n2))
                cat(rep("\n", 2))
            }
        }
    }
}
sink()


# Calculate Weighted Mean -------------------------------------------------
result_df <- data.frame()

for (hospital_var in c("snubh", "snuh", "amc")) {
    temp_df <- read_csv(file.path(hospital_var, "analyze_first/result.csv")) %>% 
        mutate(hospital = hospital_var)
    
    result_df <- bind_rows(result_df, temp_df)
}

weighted_mean_df <- result_df %>% 
    group_by(measurement, time, dm, group) %>% 
    summarize(sum_baseline = sum(sum_baseline),
              sum_followup = sum(sum_followup),
              n = sum(n),
              weighted_mean_baseline = sum_baseline / n,
              weighted_mean_followup = sum_followup / n)
write_excel_csv(weighted_mean_df, "weighted_mean.csv")
