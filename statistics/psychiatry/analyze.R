# Set up a environment --------------------------------------------------------------------
library(tidyverse)

setwd("c:/Users/Administrator/wd/statistics/psychiatry")


# Functions ---------------------------------------------------------------
process_data <- function(dat) {
    out <- dat %>% 
        filter(rowSums(!is.na(.)) > 0) %>% 
        rename()
    
    return(out)
}
## 2021-01-08 - Rename data frame
## 2021-01-08 - Bind rows three data frames

# Process Data -----------------------------------------------------------------
raw <- read_csv("data/data.csv", col_types = cols(.default = "c"))

dat1 <- raw %>% 
    select(1:str_which(colnames(raw), "sex2")-1) %>% 
    filter(rowSums(!is.na(.)) > 0) %>% 
    rename()

dat2 <- raw %>% 
    select(str_which(colnames(raw), "sex2"):(str_which(colnames(raw), "sex3") - 1)) %>% 
    filter(rowSums(!is.na(.)) > 0)

dat3 <- raw %>% 
    select(str_which(colnames(raw), "sex3"):ncol(raw)) %>% 
    filter(rowSums(!is.na(.)) > 0)

# ncol(raw) == ncol(dat1) + ncol(dat2) + ncol(dat3)

## Done - 2021-01-07 - Remove empty rows
## Done - 2021-01-07 - Divide raw into three data frames



# Analyze -----------------------------------------------------------------


