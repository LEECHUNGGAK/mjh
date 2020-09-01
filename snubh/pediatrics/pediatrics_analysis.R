# Set up the environment --------------------------------------------------
library(tidyverse)

setwd("C:/Users/Administrator/Documents/SNUBH")


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
file_v <- list.files("C:/Users/Administrator/Documents/SNUBH/SNUBH")

sink("mcnemar_test.txt")
for (i in file_v) {
    snubh <- read_data(file.path("SNUBH", i))
    snuh <- read_data(file.path("SNUH", i))
    amc <- read_data(file.path("AMC", i))
    
    combine <- snubh + snuh + amc
    print(paste("Compare", i, "(McNemar's Chi-squared Test)"))
    print(combine)
    print(mcnemar.test(combine))
}
sink()


# Student t-Test ----------------------------------------------------------
snubh_df <- read_csv("snubh_output_v4.csv",
                     col_types = cols(time = col_integer()))
snuh_df <- read_csv("snuh_output_v4.csv",
                    col_types = cols(time = col_integer()))
amc_df <- read_csv("amc_output_v4.csv",
                   col_types = cols(time = col_integer()))

time_v <- c("00", "03", "06", "12", "24", "36", "48", "60")

weighted_mean_df <- data.frame(time = as.integer(time_v),
                               measurement = rep(c("zbmi", "HBA1C"), each = 8 * 11),
                               group = c(rep(c("total"), 8 * 2),
                                         rep(c("Male", "Female"), each = 8),
                                         rep("0-4Y", 8),
                                         rep("5-9Y", 8),
                                         rep("10-14Y", 8),
                                         rep(c("Male", "Female"), each = 8),
                                         rep("5-9Y", 8),
                                         rep("10-14Y", 8)),
                               dm = c(rep(c("Type 1", "Type 2"), each = 8),
                                      rep("Type 1", 8 * 5),
                                      rep("Type 2", 8 * 4)),
                               value = c(-0.45,-0.08,-0.08,0.12,0.34,0.54,0.85,0.98, # zbmi / t1d / total
                                         1.53,1.48,1.31,1.7,1.79,1.65,1.91,1.82, # zbmi / t2d / total
                                         -0.54,-0.16,-0.21,-0.03,0.24,0.41,0.76,0.78, # zbmi / t1d / male
                                         -0.39,0,0.02,0.22,0.42,0.65,0.93,1.15, # zbmi / t1d / female
                                         -0.37,0.28,0.04,0.43,0.29,0.34,0.37,0.41, # zbmi / t1d / 0-4y
                                         -0.45,-0.04,-0.03,0.12,0.44,0.65,1.27,1.5, # zbmi / t1d / 5-9y
                                         -0.48,-0.2,-0.14,0.03,0.3,0.54,0.77,0.92, # zbmi / t1d / 10-14y
                                         1.37,1.31,1.26,1.5,1.64,1.72,2,1.64, # zbmi / t2d / male
                                         1.69,1.73,1.38,1.92,1.92,1.57,1.83,2.03, # zbmi / t2d / female
                                         1.47,1.61,0.81,1.63,1.93,2.19,1.52,2.16, # zbmi / t2d / 5-9y
                                         1.53,1.46,1.42,1.71,1.76,1.57,2.01,1.78, # zbmi / t2d / 10-14y
                                         10.24,7.61,7.82,8.09,8.32,8.47,8.63,8.72, # hba1c / t1d / total
                                         9.45,6.52,6.6,7.06,7.89,8.35,8.19,8.81, # hba1c / t2d / total
                                         9.95,7.65,7.67,7.88,8.04,8.19,8.25,8.27, # hba1c / t1d / male
                                         10.46,7.58,7.93,8.24,8.53,8.69,8.94,9.06, # hba1c / t1d / female
                                         9.49,7.6,7.63,7.77,7.72,7.78,7.85,7.83, # hba1c / t1d / 0-4y
                                         9.79,7.24,7.45,7.75,8.08,8.27,8.55,8.77, # hba1c / t1d / 5-9y
                                         10.63,7.81,8.05,8.37,8.64,8.73,8.86,8.91, # hba1c / t1d / 10-14y
                                         9.63,6.35,6.44,7.02,7.54,8.49,8.11,8.42, # hba1c / t2d / male
                                         9.25,6.71,6.79,7.11,8.21,8.24,8.29,9.22, # hba1c / t2d / female
                                         8.58,6.93,6.57,7.23,8.02,8.46,7.41,8.04, # hba1c / t2d / 5-9y
                                         9.58,6.45,6.6,7.04,8.86,8.34,8.29,8.89)) # hba1c / t2d / 10-14y

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
