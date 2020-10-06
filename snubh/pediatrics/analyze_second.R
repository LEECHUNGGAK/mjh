# Set the analysis environment --------------------------------------------
setwd("Set your working directory")


# Set environment ---------------------------------------------------------
library(tidyverse)
library(rlang)


# Make directory ----------------------------------------------------------
output_path <- "analyze_second"
dir.create(output_path)

data_df <- read_csv("data/p_data_v2.csv")
weighted_mean_df <- read_csv("weighted_mean.csv")
result_df <- data.frame()



# Analyze -----------------------------------------------------------------
for (measurement_var in c("zbmi", "HBA1C")) {
    for (time_var in c(36, 60)) {
        col_1 <- sym(paste0(measurement_var, "_03"))
        col_2 <- sym(paste0(measurement_var, "_", time_var))
        for (dm_var in c("Type 1", "Type 2")) {
            for (group_var in c("total", "0-4Y", "5-9Y", "10-14Y", "Male",
                                "Female")) {
                if (dm_var == "Type 2" & group_var == "0-4Y") {
                    next()
                }
                
                temp_data_df <- data_df %>% 
                    filter(!is.na(!!col_1) & !is.na(!!col_2) & DM == dm_var)
                if (str_detect(group_var, "Y$")) {
                    temp_data_df <- temp_data_df %>%
                        filter(age_group == group_var)
                } else if (str_detect(group_var, "ale$")) {
                    temp_data_df <- temp_data_df %>%
                        filter(sex == group_var)
                }
                
                temp_weighted_mean_df <- weighted_mean_df %>% 
                    filter(measurement == measurement_var &
                               time == time_var &
                               dm == dm_var &
                               group == group_var)
                
                result_df <- bind_rows(result_df,
                                  data.frame(measurement = measurement_var,
                                             time = time_var,
                                             dm = dm_var,
                                             group = group_var,
                                             dev_baseline = sum((pull(temp_data_df, !!col_1) -
                                                                     temp_weighted_mean_df$weighted_mean_baseline)^2,
                                                                na.rm = TRUE),
                                             dev_followup = sum((pull(temp_data_df, !!col_2) -
                                                                     temp_weighted_mean_df$weighted_mean_followup)^2,                                                                na.rm = TRUE),
                                             n = nrow(temp_data_df)))
            }
        }
    }
}

weighted_mean_df <- left_join(weighted_mean_df,
                              result_df,
                              by = c("measurement", "time", "dm", "group"))


# Save output -------------------------------------------------------------
write_csv(weighted_mean_df, file.path(output_path, "weighted_sd.csv"))