# Set Environment ---------------------------------------------------------
library(tidyverse)
library(caret)

setwd("C:/Users/Administrator/wd/alzheimer")


# Read Data ---------------------------------------------------------------
top3b_df <- read_csv("data/ngs/top3b_data_t2.csv")
apoe_df <- read_csv("data/ngs/apoe_data_t2.csv")

working_df <- top3b_df %>% 
    select(id, dementia, coordinate_22312315, coordinate_22312350,
           coordinate_22312351) %>% 
    left_join(apoe_df %>% 
                  select(id, e4_carrier, g_hom),
              by= "id") %>% 
    mutate(top3b_three_snv = ifelse(coordinate_22312315 + coordinate_22312350 + 
                                        coordinate_22312351 == 3, 1, 0),
           predictions_t1 = ifelse(top3b_three_snv == 1 | e4_carrier == 1, 1, 0),
           predictions_t2 = ifelse(top3b_three_snv == 1 | e4_carrier == 1 | 
                                       g_hom == 1, 1, 0),
           predictions_t3 = ifelse(top3b_three_snv == 1, 1, 0))


# Drow ROC Curve ----------------------------------------------------------
t1_df <- working_df %>% 
    drop_na(predictions_t1)
t2_df <- working_df %>% 
    drop_na(predictions_t2)

t3_df <- working_df %>% 
    drop_na(predictions_t3)

pred_t1 <- prediction(t1_df$predictions_t1, t1_df$dementia)
pred_t2 <- prediction(t2_df$predictions_t2, t2_df$dementia)
pred_t3 <- prediction(t2_df$predictions_t3, t2_df$dementia)

perf_t1 <- performance(pred_t1, "tpr", "fpr")
perf_t2 <- performance(pred_t2, "tpr", "fpr")

plot_path <- "output/roc_curve"

png(file.path(plot_path, "t1.png"))
plot(perf_t1)
title("TOP3B 3 SNV + APOE 4 Carrier")
dev.off()

png(file.path(plot_path, "t2.png"))
plot(perf_t2)
title("TOP3B 3 SNV + APOE 4 Carrier + GG Homozygote")
dev.off()

png(file.path(plot_path, "t1_t2.png"))
plot(perf_t1)
plot(perf_t2, add = TRUE, lty = "dashed")
legend("bottomright",
       c("TOP3B 3 SNV + APOE 4 Carrier",
         "TOP3B 3 SNV + APOE 4 Carrier + GG homozygote"),
       lty = c("solid", "dashed"))
dev.off()

# Calculate AUC
performance(pred_t1, "auc")@y.values
performance(pred_t2, "auc")@y.values
performance(pred_t3, "auc")@y.values


# Confusion Matrix --------------------------------------------------------
confusionMatrix(data = as.factor(working_df$predictions_t1),
                reference = as.factor(working_df$dementia),
                positive = "1")
confusionMatrix(data = as.factor(working_df$predictions_t2),
                reference = as.factor(working_df$dementia),
                positive = "1")
confusionMatrix(data = as.factor(working_df$predictions_t3),
                reference = as.factor(working_df$dementia),
                positive = "1")
