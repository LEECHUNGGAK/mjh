library(tidyverse)
library(readxl)

setwd("C:/Users/Administrator/wd/alzheimer")

dat <- data.frame()
for (file_v in list.files("data/raw/ngs")) {
    dat <- dat %>% 
        bind_rows(read_excel(file.path("data/raw/ngs", file_v)) %>% 
                      select(Start, Ref, Alt, Func.refGene, Gene.refGene,
                             ExonicFunc.refGene, CLNDN, CLNSIG, Otherinfo1) %>% 
                      mutate(master_no = str_remove(file_v, ".xlsx$")) %>% 
                      rename(coordinate = Start, gene = Gene.refGene,
                             genotype = Otherinfo1))
}

dat <- dat %>% 
    mutate(dementia = ifelse(str_sub(master_no, 1, 1) == "N", 0, 1))
