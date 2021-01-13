# Set up a environment --------------------------------------------------------------------
library(tidyverse)

setwd("c:/Users/Administrator/wd/statistics/psychiatry")


# Functions ---------------------------------------------------------------
process_data <- function(dat) {
    out <- dat %>% 
        filter(rowSums(!is.na(.)) > 0) %>% 
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}$", "\\1")) %>% 
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}(_\\d+$)", "\\1\\2")) %>%
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}(_\\d+YN)", "\\1\\2"))
    
    return(out)
}

## Done - 2021-01-08 - Rename data frame
## 2021-01-08 - Bind rows three data frames


# Process Data -----------------------------------------------------------------
raw <- read_csv("data/data.csv", col_types = cols(.default = "c"))

dat1 <- raw %>% 
    select(1:str_which(names(raw), "sex2")-1) %>% 
    rename(BO = BO1)
    process_data() %>% 
    mutate(ordinal = 1)

dat2 <- raw %>% 
    select(str_which(names(raw), "sex2"):(str_which(names(raw), "sex3") - 1)) %>% 
    process_data() %>% 
    mutate(ordinal = 2)

dat3 <- raw %>% 
    select(str_which(names(raw), "sex3"):ncol(raw)) %>% 
    setNames(str_replace(names(.), "(^PHQ)(\\d{1}$)", "\\1_\\2")) %>% 
    setNames(str_replace(names(.), "(^GAD)(\\d{1}$)", "\\1_\\2")) %>% 
    process_data() %>% 
    mutate(ordinal = 3)

# ncol(raw) == ncol(dat1) + ncol(dat2) + ncol(dat3)

## Done - 2021-01-07 - Remove empty rows
## Done - 2021-01-07 - Divide raw into three data frames

dat <- dat1 %>% 
    bind_rows(dat2) %>% 
    bind_rows(dat3) %>% 
    relocate(ordinal, .before = sex)

write_excel_csv(dat, "output/data.csv")
