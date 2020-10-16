# Set Environment ---------------------------------------------------------
library(tidyverse)
library(ROCR)
library(caret)
library(epiR)

setwd("C:/Users/Administrator/wd/alzheimer")


# Read Data ---------------------------------------------------------------
top3b_df <- read_csv("data/ngs/top3b_t2.csv")
apoe_df <- read_csv("data/ngs/apoe_t2.csv")

w_df <- top3b_df %>% 
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
pred_t1 <- prediction(w_df$predictions_t1, w_df$dementia)
pred_t2 <- prediction(w_df$predictions_t2, w_df$dementia)
pred_t3 <- prediction(w_df$predictions_t3, w_df$dementia)

perf_t1 <- performance(pred_t1, "tpr", "fpr")
perf_t2 <- performance(pred_t2, "tpr", "fpr")
perf_t3 <- performance(pred_t3, "tpr", "fpr")

plot_path <- "output/roc_curve"

png(file.path(plot_path, "t1.png"))
plot(perf_t1)
title("TOP3B 3 SNV + APOE 4 Carrier")
legend("bottomright",
       paste("AUC:", round(performance(pred_t1, "auc")@y.values[[1]], 4)))
dev.off()

png(file.path(plot_path, "t2.png"))
plot(perf_t2)
title("TOP3B 3 SNV + APOE 4 Carrier + GG Homozygote")
legend("bottomright",
       paste("AUC:", round(performance(pred_t2, "auc")@y.values[[1]], 4)))
dev.off()

png(file.path(plot_path, "t3.png"))
plot(perf_t2)
title("TOP3B 3 SNV")
legend("bottomright",
       paste("AUC:", round(performance(pred_t3, "auc")@y.values[[1]], 4)))
dev.off()

png(file.path(plot_path, "t1_t2_t3.png"))
plot(perf_t1, lty = "longdash")
plot(perf_t2, add = TRUE, lty = "dotted")
plot(perf_t3, add = TRUE)
legend("right",
       c(paste0("TOP3B 3 SNV\n(AUC: ",
               round(performance(pred_t3, "auc")@y.values[[1]], 4),
               ")"), 
         paste0("TOP3B 3 SNV\n+ APOE 4 Carrier\n(AUC: ",
               round(performance(pred_t1, "auc")@y.values[[1]], 4),
               ")"),
         paste0("TOP3B 3 SNV\n+ APOE 4 Carrier\n+ GG homozygote\n(AUC: ",
               round(performance(pred_t2, "auc")@y.values[[1]], 4),
               ")")),
       lty = c("solid", "longdash", "dotted"))
dev.off()


# Confusion Matrix --------------------------------------------------------
epi.tests(table(w_df$predictions_t3, w_df$dementia)[2:1, 2:1])
confusionMatrix(data = as.factor(w_df$predictions_t3),
                reference = as.factor(w_df$dementia),
                positive = "1")

epi.tests(table(w_df$predictions_t1, w_df$dementia)[2:1, 2:1])
confusionMatrix(data = as.factor(w_df$predictions_t1),
                reference = as.factor(w_df$dementia),
                positive = "1")

epi.tests(table(w_df$predictions_t2, w_df$dementia)[2:1, 2:1])
confusionMatrix(data = as.factor(w_df$predictions_t2),
                reference = as.factor(w_df$dementia),
                positive = "1")


# Cancerrop ---------------------------------------------------------------
cancerrop_df <- top3b_df %>% 
    select(id, dementia, n_variant) %>% 
    mutate(predictions = ifelse(n_variant >= 3, 1, 0))

pred_cancerrop <- prediction(cancerrop_df$predictions, w_df$dementia)

perf_cancerrop <- performance(pred_cancerrop, "tpr", "fpr")
perf_cancerrop <- performance(pred_cancerrop, "tpr", "fpr")

plot_path <- "output/roc_curve"

png(file.path(plot_path, "cancerrop.png"))
plot(perf_cancerrop)
legend("bottomright",
       paste("AUC:", round(performance(pred_cancerrop, "auc")@y.values[[1]], 4)))
dev.off()


# Confusion Matrix
confusionMatrix(data = as.factor(cancerrop_df$predictions),
                reference = as.factor(cancerrop_df$dementia),
                positive = "1")
