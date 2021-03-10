# Setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(irr)

# Functions ---------------------------------------------------------------
process_data <- function(x, method, index) {
    return(x %>%
               drop_na() %>% 
               select(1, 2, 4, 6, seq(index, index + 3)) %>% 
               rename(id = 1, name = 2, condition_date = 3, onset_date = 4,
                      method_date = 5, e = 6, rdrp = 7, result = 8) %>% 
               mutate(method = method) %>% 
               relocate(method, .before = method_date))
}

# Process data ------------------------------------------------------------
r <- read_csv("C:/Users/Administrator/work/1_statistics/1_laboratory_medicine/data/data.csv",
              col_names = FALSE,
              col_type = cols(.default = "c"),
              skip = 1)

s <- data.frame(method = c("sputum", "naso", "saliva"), 
                index = c(8, 12, 16))

for (i in 1:nrow(s)) {
    if (i == 1) {
        d <- process_data(r, s$method[i], s$index[i])
    } else {
        d <- bind_rows(d, process_data(r, s$method[i], s$index[i]))
    }
}

d <- d %>% 
    mutate(condition_date = ymd(condition_date),
           onset_date = ymd(onset_date),
           method_date = ymd(method_date),
           condition_date_diff = method_date - condition_date,
           onset_date_diff = method_date - onset_date)

# Analyze -----------------------------------------------------------------
# Reproducibility
s_combn <- combn(s$method, 2)
sink("C:/Users/Administrator/work/1_statistics/1_laboratory_medicine/output.txt")
for (i in 1:ncol(s_combn)) {
    var_t <- s_combn[1, i]
    var_c <- s_combn[2, i]
    
    t <- data.frame(target = d %>% 
                            filter(method == var_t) %>% 
                            pull(result),
                        comparator = d %>% 
                            filter(method == var_c) %>% 
                            pull(result))
    
    cat(paste0("Target: ", var_t, "\nComparator: ", var_c, "\n"))
    print(table(t))
    print(agree(t))
    print(kappa2(t))
    cat("\n")
}
sink()

# 
