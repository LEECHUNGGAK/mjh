# Setup Environment -------------------------------------------------------
library(tidyverse)

setwd("C:/Users/Administrator/wd/alzheimer")


# Draw Plots --------------------------------------------------------------
raw_df <- read_csv("data/master_measurement.csv")

work_df <- raw_df %>% 
    group_by(id, measurement) %>% 
    summarize(n = n()) %>% 
    filter(n >= 2) %>% 
    left_join(raw_df, by = c("id", "measurement")) %>% 
    replace_na(list(diff_btw_me = 0)) %>% 
    mutate(abnormal_chromosome = ifelse(abnormal_chromosome == 0, FALSE, TRUE),
           abnormal_sex_chromosome = ifelse(abnormal_sex_chromosome == 0, FALSE, TRUE),
           turner = ifelse(turner == 0, FALSE, TRUE),
           xxx = ifelse(xxx == 0, FALSE, TRUE),
           top3b_three_snv = ifelse(top3b_three_snv == 0, FALSE, TRUE))

for (measurement_var in c("MMSE", 'CDR sum of box')) {
    if (measurement_var == "MMSE") {
        ylim <- c(0, 30)
    } else {
        ylim <- c(0, 18)
    }
    for (condition_var in c("abnormal_chromosome", "abnormal_sex_chromosome", "turner", "xxx",
                "top3b_three_snv")) {
        plot_df <- work_df %>% 
            filter(measurement == measurement_var) %>% 
            drop_na(condition_var)
        
        plot_df %>% 
            ggplot(aes(x = diff_btw_me, y = measurement_value)) +
            geom_point(aes_string(color = condition_var)) +
            geom_line(aes_string(color = condition_var, group = "id")) +
            scale_y_continuous(breaks = seq(ceiling(min(work_df$measurement_value)),
                                            ceiling(max(work_df$measurement_value)),
                                            by = 1)) +
            scale_color_manual(breaks = c(FALSE, TRUE), values = c("blue", "red")) +
            coord_cartesian(ylim = ylim, expand = FALSE) +
            theme_bw() +
            theme(legend.position = "bottom")
        ggsave(paste0("output/plots/", measurement_var, "_", condition_var, "_paired.png"))
        
        plot_df %>% 
            ggplot(aes(x = diff_btw_me, y = measurement_value)) +
            geom_point(aes_string(color = condition_var)) +
            geom_smooth(aes_string(color = condition_var), method = "lm") +
            scale_y_continuous(breaks = seq(ceiling(min(work_df$measurement_value)),
                                            ceiling(max(work_df$measurement_value)),
                                            by = 1)) +
            scale_color_manual(breaks = c(FALSE, TRUE), values = c("blue", "red")) +
            coord_cartesian(ylim = ylim, expand = FALSE) +
            theme_bw() +
            theme(legend.position = "bottom")
        ggsave(paste0("output/plots/", measurement_var, "_", condition_var, "_lm.png"))
    }
}
