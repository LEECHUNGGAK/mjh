# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)

options("scipen" = 100)


# Process the data --------------------------------------------------------
dat_user <- read_csv("C:/Users/Administrator/work/user.csv",
                     col_types = cols(.default = "c"))
dat_answer <- read_xlsx("C:/Users/Administrator/work/raw/answer.xlsx") %>% 
    rename(id = 2) %>% 
    select(id) %>% 
    mutate(id = str_remove(id, "\\.0$"),
           receive = "1",
           answer = "1")

position <- "^(조|부|초빙|객원)?교수$|^임상|의료원장|임상강사|소장|전공의"
duty <- paste("(연구부|병|진료부)원장",
              "(교육수련|내과)부장",
              "(외|심장내)과장",
              "(노인의학|원격의료|진료협력|종합건강진단)센터장",
              "(병원문화혁신|건진)본부장",
              "(홍보|기획)실장",
              sep = "|")
id <- c(17001, 08157)

dat <- dat_user %>% 
    filter(str_detect(직위, position) |
               str_detect(직책, duty) |
               사번 %in% id) %>% 
    rename(name = 이름, id = 사번, email = Email) %>% 
    full_join(dat_answer, by = "id") %>% 
    replace_na(list(receive = 0,
                    answer = 0))

write_excel_csv(dat, "C:/Users/Administrator/work/survey.csv")

dat_survey <- read_tsv("C:/Users/Administrator/work/survey.csv",
                       col_types = cols(.default = "c")) %>% 
    full_join(dat_answer, by = "id") %>% 
    mutate(receive = coalesce(receive.x, receive.y),
           answer = coalesce(answer.x, answer.y)) %>% 
    select(-ends_with(".x"), -ends_with(".y"))

write_excel_csv(dat_survey, "C:/Users/Administrator/work/survey2.csv")


# Analyse the cost --------------------------------------------------------
dat_cost <- read_xlsx("C:/Users/Administrator/work/raw/answer.xlsx") %>% 
    select(29) %>% 
    rename(cost = 1) %>% 
    mutate(cost = str_remove(cost, "원[가-힣]*"),
           cost = str_replace(cost, "만", "0000"),
           cost = str_replace(cost, "원", "000"),
           cost = str_remove_all(cost, "[:alpha:]|[가-힣]|\\s|\\?"),
           cost = as.integer(cost),
           cost = ifelse(cost < 10000, cost * 10000, cost))

dat_cost %>% 
    ggplot(aes(x = cost / 10000)) +
    geom_histogram()

summary(dat_cost$cost)   
