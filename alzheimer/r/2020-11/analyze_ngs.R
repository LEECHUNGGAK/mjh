# Setup Environment -------------------------------------------------------
library(tidyverse)
library(readxl)
library(hrbrthemes)

setwd("C:/Users/Administrator/wd/alzheimer")

master_df <- read_csv("data/debug/debug_2020-11-30.csv") %>% 
    select(master_no, gender)
total_df <- read_csv(file.path("data/ngs/output/total.csv"))
unique_gene <- unique(total_df$gene)

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
dat <- read_csv(file.path("data/ngs/output/total.csv"))

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
               Alt = ifelse(exonic_func == "normal", Ref, Alt),
               dementia = ifelse(str_sub(master_no, 1, 1) == "N", 0, 1)) %>% 
        relocate(gene, .after = master_no) %>% 
        relocate(Alt, .after = Ref) %>% 
        relocate(genotype, .before = Ref)
    
    write_excel_csv(out, file.path("data/ngs/output", paste0(gene_v, ".csv")))
}


# t-Test ------------------------------------------------------------------
sink("t_test_on_snv_count.txt")
for (gene_v in unique_gene) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv")))
    
    t_df <- dat %>% 
        group_by(master_no, dementia) %>% 
        summarize(snv_count = sum(exonic_func != "normal"))
    
    cat(paste0("Gene: ", gene_v, "\nStudent's t-Test on SNV Count"))
    print(t.test(x = t_df %>% 
                     filter(dementia == 0) %>% 
                     pull(snv_count),
                 y = t_df %>% 
                     filter(dementia == 1) %>% 
                     pull(snv_count)))
    
    print(table(t_df$dementia))
}
sink()


# Make Box Plot -----------------------------------------------------------
for (gene_v in unique_gene) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv")))
    
    t_df <- dat %>% 
        group_by(master_no, dementia) %>% 
        summarize(snv_count = sum(exonic_func != "normal"))
    
    ggplot(t_df %>% 
               mutate(dementia = as.factor(ifelse(dementia == "0", "Normal", "Patient"))),
           aes(x = dementia, y = snv_count, group = dementia)) +
        geom_boxplot() +
        coord_flip() +
        theme_classic() + 
        theme(axis.title = element_blank(),
              axis.text = element_text(size = 10 * 0.5))
    ggsave(file.path("output/2020-11/ngs_boxplot", paste0(gene_v, ".png")),
           width = 6.5 * 0.2, height = 6.5 * 0.5 * 0.2, units = "in")
}


# Make Histogram ----------------------------------------------------------
master_df <- read_csv("data/debug/debug_2020-11-30.csv") %>% 
    select(master_no, gender)
for (gene_v in unique_gene[c(3, 15, 9)]) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv"))) %>% 
        left_join(master_df,
                  by = "master_no")
    
    plot_df <- dat %>% 
        group_by(dementia, gender, master_no) %>% 
        summarize(snv_count = sum(exonic_func != "normal")) %>% 
        mutate(dementia = ifelse(dementia == 0, "Normal", "Patient"))
    
    ggplot(plot_df, aes(x = snv_count, fill = dementia)) +
        geom_histogram(position = "dodge", binwidth = 0.5) +
        labs(title = paste(gene_v, "SNV Count")) + 
        scale_x_continuous(breaks = seq(0, max(plot_df$snv_count), 1)) +
        theme_ipsum() +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave(file.path("output/2020-11/ngs_histogram", paste0(gene_v, ".png")))
    
    ggplot(plot_df, aes(x = snv_count, fill = dementia)) +
        geom_histogram(position = "dodge", binwidth = 0.5) +
        labs(title = paste(gene_v, "SNV Count")) + 
        scale_x_continuous(breaks = seq(0, max(plot_df$snv_count), 1)) +
        theme_ipsum() +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~gender)
    ggsave(file.path("output/2020-11/ngs_histogram", paste0(gene_v, "_gender.png")),
           width = 6.5 * 1.4, height = 6.5, units = "in")
}


# Chi-squared Test --------------------------------------------------------
sink("output/2020-12/chi-squared_test/chi_squared_test.txt")
for (gene_v in unique_gene[c(3, 15, 9)]) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv"))) %>% 
        mutate(snv = ifelse(exonic_func == "normal", "SNV: Absence", "SNV: Presence"),
               dementia = ifelse(dementia == 0, "Dementia: Normal", "Dementia: Patient")) %>% 
        left_join(master_df,
                  by = "master_no")
    
    unique_coordinate <- sort(unique(dat$coordinate))
    
    for (coordinate_v in unique_coordinate) {
        tmp <- dat %>% 
            filter(coordinate == coordinate_v)
        
        cat(paste0("Gene: ", gene_v, "\n",
                   "Coordinate: ", coordinate_v))
        tab <- table(tmp$dementia, tmp$snv)
        print(tab)
        print(prop.table(tab) %>% round(4))
        print(chisq.test(tab))
    }
}
sink()


# Chi-squared Test 2 --------------------------------------------------------
sink("output/2020-12/chi-squared_test/chi_squared_test_on_fmr1_and_tdrd3.txt")
for (gene_v in unique_gene[c(30, 23)]) {
    dat <- read_csv(file.path("data/ngs/output", paste0(gene_v, ".csv"))) %>% 
        mutate(snv = ifelse(exonic_func == "normal", "SNV: Absence", "SNV: Presence"),
               dementia = ifelse(dementia == 0, "Dementia: Normal", "Dementia: Patient")) %>% 
        left_join(master_df,
                  by = "master_no")
    
    unique_coordinate <- sort(unique(dat$coordinate))
    
    for (coordinate_v in unique_coordinate) {
        tmp <- dat %>% 
            filter(coordinate == coordinate_v)
        
        cat(paste0("Gene: ", gene_v, "\n",
                   "Coordinate: ", coordinate_v))
        tab <- table(tmp$dementia, tmp$snv)
        print(tab)
        print(prop.table(tab) %>% round(4))
        print(chisq.test(tab))
    }
}
sink()