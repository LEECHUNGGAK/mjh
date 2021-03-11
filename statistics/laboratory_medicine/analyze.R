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

analyze <- function(dat, target, comparator) {
    x <- dat %>% 
        filter(method == target) %>% 
        select(id, method, num, result) %>% 
        full_join(dat %>% 
                      filter(method == comparator) %>% 
                      select(id, method, num, result),
                  by = c("id", "num")) %>% 
        select(result.x, result.y) %>% 
        rename(target = 1, comparator = 2)
    
    # x <- data.frame(target = dat %>% 
    #                     filter(method == target) %>% 
    #                     pull(result),
    #                 comparator = dat %>% 
    #                     filter(method == comparator) %>% 
    #                     pull(result))
    
    cat(paste0("Target: ", target, "\nComparator: ", comparator, "\n"))
    print(table(x))
    print(agree(x))
    print(kappa2(x))
    cat("\n")
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
           condition_date_diff = as.integer(method_date - condition_date),
           onset_date_diff = as.integer(method_date - onset_date),
           condition_date_diff_group = cut(condition_date_diff,
                                           breaks = c(1, 7, 14, 21, 28, Inf),
                                           include.lowest = TRUE),
           onset_date_diff_group = cut(onset_date_diff,
                                       breaks = c(1, 7, 14, 21, 28, Inf),
                                       include.lowest = TRUE)) %>% 
    group_by(id, method) %>% 
    mutate(num = row_number()) %>% 
    ungroup()

# Analyze -----------------------------------------------------------------
# Reproducibility
s_combn <- matrix(c("saliva", "naso", "saliva", "sputum"), ncol = 2)

sink("C:/Users/Administrator/work/1_statistics/1_laboratory_medicine/output.txt")

for (i in 1:ncol(s_combn)) {
    var_t <- s_combn[1, i]
    var_c <- s_combn[2, i]
    
    analyze(d, var_t, var_c)
    
    for (j in levels(d$condition_date_diff_group)) {
        for (k in c("condition", "onset")) {
            cat(paste0("Filter: ", k, "_date_diff_group == ", j, "\n"))
            filter_d <- d %>% 
                filter(!!sym(paste0(k, "_date_diff_group")) == j)
            
            analyze(filter_d, var_t, var_c)
        }
    }
}

sink()
