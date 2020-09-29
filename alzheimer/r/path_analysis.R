library(lavaan)
library(tidyverse)
library(semPlot)

setwd("C:/Users/Administrator/wd/alzheimer/path analysis")

ori_work_data <- data.frame()
for (filename in list.files("data")) {
    ori_work_data <- bind_rows(ori_work_data,
                               read_csv(file.path("data", filename),
                                        col_names = FALSE,
                                        col_types = cols(.default = "c"),
                                        skip = 1))
}

work_data <- ori_work_data %>% 
    select(-1) %>% 
    setNames(., c("id", "registration_year", "age", "gender", "weight",
                  "height", "bmi", "smoking_amount", "smoking_period",
                  "smoking_total", "alcohol_consumption", "drinking_frequency",
                  "drinking_total", "physical_activity", "htn",
                  "diabetes", "cardiac_disease", "dyslipidemia",
                  "breast_ca_icd", "breast_ca_ctx", "breast_ca_ht",
                  "breast_ca_date", "death_date", "death_age", "brain_meta",
                  "dementia_code", "dementia_date", "donepezil", "rivastigmine",
                  "galatamine", "memntine", "time")) %>% 
    mutate(age = as.integer(age),
           bmi = as.double(bmi),
           smoking_total = as.double(smoking_total),
           drinking_total = as.double(drinking_total),
           physical_activity = as.double(physical_activity),
           htn = recode(htn, `0` = FALSE, `1` = TRUE),
           diabetes = recode(diabetes, `0` = FALSE, `1` = TRUE),
           cardiac_disease = recode(cardiac_disease, `0` = FALSE, `1` = TRUE),
           dyslipidemia = recode(dyslipidemia, `0` = FALSE, `1` = TRUE),
           breast_ca = ifelse(breast_ca_icd == "#N/A", FALSE, TRUE),
           breast_ca_ctx = ifelse(breast_ca_ctx %in% c("#N/A", 0), FALSE, TRUE),
           breast_ca_ht = ifelse(breast_ca_ctx %in% c("#N/A", 0), FALSE, TRUE),
           dementia = ifelse(dementia_code == "#N/A", FALSE, TRUE))
write_csv(work_data, "data.csv")

model <- "
breast_ca ~ age + bmi + smoking_total + drinking_total +
physical_activity + htn + diabetes
dementia ~ breast_ca + breast_ca_ctx + age + smoking_total + drinking_total +
physical_activity + bmi + htn + diabetes + dyslipidemia
"

result <- sem(model = model,
              data = work_data)
result@ParTable$est[result@ParTable$est < 0.2] <- 0
semPaths(result, "std", style = "lisrel", layout = "circle2",
         edge.label.cex = 1, edge.color = "black")

obj <- semPlotModel(result)
ori_pars <- obj@Pars
check_pars <- obj@Pars %>% 
    filter(!(edge %in% c("int", "<->") | lhs == rhs))
keep_pars <- obj@Pars %>% 
    filter(edge %in% c("int", "<->") | lhs == rhs)

test_against <- standardizedSolution(result) %>% 
    filter(pvalue < 0.05 & rhs != lhs)
test_against_rev <- test_against %>% 
    rename(tmp = lhs, lhs = rhs) %>% 
    rename(rhs = tmp)
checked_pars <- check_pars %>% 
    semi_join(test_against, by = c("lhs", "rhs")) %>% 
    bind_rows(check_pars %>% 
                  semi_join(test_against_rev, by = c("lhs", "rhs")))
obj@Pars <- keep_pars %>% 
    bind_rows(checked_pars)
anti_join(ori_pars, obj@Pars)

obj@Pars <- obj@Pars %>% 
    mutate(std = ifelse(std < 0.2, 0, std))

semPaths(obj, "std", style = "lisrel", layout = "circle2", edge.label.cex = 1,
         edge.color = "black")


# Descriptive Statistics --------------------------------------------------
work_data %>%
    group_by(breast_ca, breast_ca_ctx) %>% 
    summarize(smoking_mean = mean(smoking_total),
              smoking_sd = sd(smoking_total),
              drinking_mean = mean(drinking_total),
              drinking_sd = sd(drinking_total),
              physical_activity_mean = mean(physical_activity),
              physical_activity_sd = sd(physical_activity)) %>% 
    mutate(group = ifelse(breast_ca == FALSE, "C1", 
                          ifelse(breast_ca == TRUE & breast_ca_ctx == TRUE, "T", "C2"))) %>% 
    relocate(group, .before = breast_ca) %>% 
    write_excel_csv("output/three_groups_descriptive_statistics.csv")

work_data %>%
    group_by(breast_ca) %>% 
    summarize(smoking_mean = mean(smoking_total),
              smoking_sd = sd(smoking_total),
              drinking_mean = mean(drinking_total),
              drinking_sd = sd(drinking_total),
              physical_activity_mean = mean(physical_activity),
              physical_activity_sd = sd(physical_activity)) %>% 
    mutate(group = ifelse(breast_ca == FALSE, "Normal", "Patient")) %>% 
    relocate(group, .before = breast_ca) %>% 
    write_excel_csv("output/two_groups_descriptive_statistics.csv")

ggplot(work_data, aes(x = smoking_total)) +
    geom_histogram()