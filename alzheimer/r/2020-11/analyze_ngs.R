# Setup Environment -------------------------------------------------------
library(tidyverse)
library(readxl)

setwd("C:/Users/Administrator/wd/alzheimer")


# Process NGS Data --------------------------------------------------------
dat <- data.frame()
for (file_v in list.files("data/raw/ngs")) {
    dat <- dat %>% 
        bind_rows(read_excel(file.path("data/raw/ngs", file_v)) %>% 
                      select(Start, Ref, Alt, Func.refGene, Gene.refGene,
                             ExonicFunc.refGene, Otherinfo1) %>% 
                      mutate(master_no = str_remove(file_v, ".xlsx$")))
}
dat <- dat %>% 
    rename(coordinate = Start,
           func = Func.refGene,
           exonic_func = ExonicFunc.refGene,
           gene = Gene.refGene,
           genotype = Otherinfo1) %>% 
    mutate(dementia = ifelse(str_sub(master_no, 1, 1) == "N", 0, 1))
write_csv(dat, "data/ngs/output/total.csv")

unique_gene <- unique(dat$gene)
unique_master_no <- unique(dat$master_no)
for (gene_v in unique_gene) {
    tmp <- dat %>% 
        filter(gene == gene_v)
    
    unique_coordinate <- unique(tmp$coordinate)
    distinct_df <- distinct(tmp, coordinate, Ref, func, gene)
    
    out <- data.frame(master_no = rep(unique_master_no, each = nrow(distinct_df)),
                      coordinate = distinct_df$coordinate,
                      Ref = distinct_df$Ref,
                      func = distinct_df$func,
                      gene = distinct_df$gene) %>% 
        left_join(tmp %>% 
                      select(-c(Ref, func, gene)),
                  by = c("master_no", "coordinate")) %>% 
        replace_na(list(exonic_func = "normal", genotype = "homozygous wild type")) %>% 
        mutate(genotype = recode(genotype,
                                 hom = "homozygous mutant type",
                                 het = "heterozygous mutant type"),
               exonic_func = ifelse(exonic_func == ".", "intronic SNV", exonic_func),
               Alt = ifelse(exonic_func == "normal", Ref, Alt)) %>% 
        relocate(gene, .after = master_no) %>% 
        relocate(Alt, .after = Ref) %>% 
        relocate(genotype, .before = Ref)
        
    write_excel_csv(out, file.path("data/ngs/output", paste0(gene_v, ".csv")))
}

#
#
dat <- read_csv(file.path("data/ngs/output/total.csv"))

plot_df <- dat %>% 
    group_by(master_no) %>% 
    summarize(snv_count = sum(exonic_func != "normal"))

ggplot(plot_df, aes(x = snv_count)) +
    geom_histogram() +
    labs(title = "Total SNV Count") + 
    scale_x_continuous(breaks = seq(0, max(plot_df$snv_count), 10)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
ggsave(file.path("output/2020-11/ngs_histogram/total.png"))

#
for (gene_v in unique_gene) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv")))
    
    plot_df <- dat %>% 
        group_by(master_no) %>% 
        summarize(snv_count = sum(exonic_func != "normal"))
    
    ggplot(plot_df, aes(x = snv_count)) +
        geom_histogram() +
        labs(title = paste(gene_v, "SNV Count")) + 
        scale_x_continuous(breaks = seq(0, max(plot_df$snv_count), 1)) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90))
    ggsave(file.path("output/2020-11/ngs_histogram/by NGS", paste0(gene_v, ".png")))
}


# t-Test ------------------------------------------------------------------
t_test_df <- dat %>% 
    group_by(dementia, master_no) %>% 
    summarize(snv_count = sum(exonic_func != "normal"))

t.test(x = t_test_df %>% 
           filter(dementia == 0) %>% 
           pull(snv_count),
       y = t_test_df %>% 
           filter(dementia == 1) %>% 
           pull(snv_count))


# Make Box Plot -----------------------------------------------------------
ggplot(t_test_df %>% 
           mutate(dementia = as.factor(ifelse(dementia == "0", "Normal", "Patient"))),
       aes(x = dementia, y = snv_count, group = dementia)) +
    geom_boxplot() +
    scale_x_discrete()
