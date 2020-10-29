# Set the analysis environment --------------------------------------------
setwd("Set your working directory")


# Set environment ---------------------------------------------------------
library(tidyverse)
library(rlang)


# Make directory ----------------------------------------------------------
output_path <- "analyze_second_v2"
dir.create(output_path)

dat_df <- read_csv("data/data_v2.csv")
weighted_mean_df <- read_csv("weighted_mean.csv")
out_df <- data.frame()


# Analyze -----------------------------------------------------------------
for (measurement_var in c("zbmi", "hba1c")) {
    for (time_var in c(36, 60)) {
        col_v <- sym(paste0(measurement_var, "_delta_", time_var))
        for (dm_var in c("Type 1", "Type 2")) {
            for (group_var in c("total", "0-4Y", "5-9Y", "10-14Y", "Male",
                                "Female")) {
                if (dm_var == "Type 2" & group_var == "0-4Y") {
                    next()
                }
                
                tmp_df <- dat_df %>% 
                    filter(!is.na(!!col_v) & DM == dm_var)
                if (str_detect(group_var, "Y$")) {
                    tmp_df <- tmp_df %>%
                        filter(age_group == group_var)
                } else if (str_detect(group_var, "ale$")) {
                    tmp_df <- tmp_df %>%
                        filter(sex == group_var)
                }
                
                tmp_weighted_mean_df <- weighted_mean_df %>% 
                    filter(measurement == measurement_var &
                               time == time_var &
                               dm == dm_var &
                               group == group_var)
                
                out_df <- bind_rows(out_df,
                                    data.frame(measurement = measurement_var,
                                               time = time_var,
                                               dm = dm_var,
                                               group = group_var,
                                               dev = sum((pull(tmp_df, !!col_v) -
                                                              tmp_weighted_mean_df$weighted_mean)^2,
                                                         na.rm = TRUE),
                                               n = sum(!is.na(pull(tmp_df, !!col_v)))))
            }
        }
    }
}


# Save output -------------------------------------------------------------
write_csv(out_df, file.path(output_path, "output.csv"))