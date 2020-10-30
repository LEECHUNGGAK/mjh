# Set Work Environment ----------------------------------------------------
library(tidyverse)

setwd("C:/Users/Administrator/wd/alzheimer")

dat <- read_csv("data/data_classify_with_ngs.csv")


# Detect Multicollinearity in Categorical Variables -----------------------
print_chisq_test(dat, row = "top3b_three_snv", column = "g_hom")
print_chisq_test(dat, row = "top3b_three_snv", column = "e4_carrier")
print_chisq_test(dat, row = "g_hom", column = "e4_carrier")
