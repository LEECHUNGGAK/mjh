# Set up a environment --------------------------------------------------------------------
library(tidyverse)

setwd("c:/Users/Administrator/wd/statistics/psychiatry")


# Functions ---------------------------------------------------------------
process_data <- function(dat) {
    out <- dat %>% 
        filter(rowSums(!is.na(.)) > 0) %>% 
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}$", "\\1")) %>% 
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}(_\\d+$)", "\\1\\2")) %>%
        setNames(str_replace(names(.), "(^[:alpha:]+)\\d{1}(_\\d+YN)", "\\1\\2")) %>%
    
    return(out)
}
## 2021-01-08 - Rename data frame
## 2021-01-08 - Bind rows three data frames

# Process Data -----------------------------------------------------------------
raw <- read_csv("data/data.csv", col_types = cols(.default = "c"))

dat1 <- raw %>% 
    select(1:str_which(colnames(raw), "sex2")-1)

dat2 <- raw %>% 
    select(str_which(colnames(raw), "sex2"):(str_which(colnames(raw), "sex3") - 1))

dat3 <- raw %>% 
    select(str_which(colnames(raw), "sex3"):ncol(raw))

# ncol(raw) == ncol(dat1) + ncol(dat2) + ncol(dat3)

## Done - 2021-01-07 - Remove empty rows
## Done - 2021-01-07 - Divide raw into three data frames



# Analyze -----------------------------------------------------------------


