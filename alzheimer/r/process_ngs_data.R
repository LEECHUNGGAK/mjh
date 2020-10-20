# Set Environment ---------------------------------------------------------
library(tidyverse)
library(readxl)

setwd("C:/Users/Administrator/wd/alzheimer")


# Functions ---------------------------------------------------------------
process_ngs_data <- function(data_file_path, result_file_path = FALSE, gene) {
    file_v <- str_remove(list.files(data_file_path), ".xlsx?$")
    
    temp_df <- data.frame()
    
    for (i in list.files(data_file_path, full.names = TRUE)) {
        temp_df <- bind_rows(temp_df,
                             read_excel(i) %>% 
                                 filter(Gene == gene) %>% 
                                 select(Variant, Coordinate, Genotype, Exonic, Consequence) %>% 
                                 mutate(id = str_extract(i, "(N|P)(_N)?-?\\d+")))
    }
    
    result_df <- data.frame(id = str_replace(file_v, ".xls", ""),
                            Coordinate = rep(unique(temp_df$Coordinate),
                                             each = length(file_v))) %>% 
        left_join(temp_df, by = c("id", "Coordinate")) %>% 
        arrange(id) %>% 
        replace_na(list(Variant = "normal", Genotype = "homozygous wild type", Exonic = "no")) %>% 
        mutate(Consequence_value = 1,
               Genotype = recode(Genotype, hom = "homozygous mutant type",
                                 het = "heterozygous mutant type")) %>% 
        spread(key = Consequence, value = Consequence_value, fill = 0) %>% 
        select(-`<NA>`) %>% 
        mutate(dementia = ifelse(str_sub(id, 1, 1) == "N", 0, 1))
    
    if (gene == "APOE") {
        result_df <- result_df %>% 
            left_join(read_csv("data/material/APOE_genotyping_data.csv") %>% 
                          rename(id = 1, apoe = 2),
                      by = "id") %>% 
            mutate(g_carrier = ifelse(Variant != "normal" & 
                                          str_detect(str_sub(Variant, 3), "G"),
                                      1, 0),
                   g_hom = ifelse(Variant != "normal" & 
                                      str_detect(str_sub(Variant, 3), "G/G"),
                                  1, 0))
    }
    # else {
        # result_df <- result_df %>% 
            # rename(five_prime_UTR_variant = `5_prime_UTR_variant`)
    # }
    
    if (result_file_path != FALSE) {
        write_csv(result_df, result_file_path)
    }
}

spread_ngs_data <- function(dat, output_file_path = FALSE) {
    output <- dat %>% 
        select(id, dementia, Coordinate, Variant) %>% 
        mutate(Coordinate = paste0("coordinate_", Coordinate),
               Variant = ifelse(Variant == "normal", 0, 1)) %>% 
        spread(Coordinate, Variant) %>% 
        replace(is.na(.), 0)
    # %>% 
        # mutate(n_variant = rowSums(.[, 3:ncol(.)]))
    
    if (output_file_path != FALSE) {
        write_excel_csv(output, output_file_path)
    }
}

# Manipulate TOP3B SNV Coordinate Data ---------------------------------------------------------
top3b_df <- process_ngs_data(data_file_path = "data/raw/NGS_result",
                             result_file_path = "data/ngs/top3b.csv",
                             gene = "TOP3B")

top3b_df <- read_csv("data/ngs/top3b.csv")

top3b_t2_df <- top3b_df %>% 
    select(id, dementia, Coordinate, Variant) %>% 
    mutate(Coordinate = paste0("coordinate_", Coordinate),
           Variant = ifelse(Variant == "normal", 0, 1)) %>% 
    spread(Coordinate, Variant) %>% 
    mutate(n_variant = rowSums(.[, 3:ncol(.)]))
write_excel_csv(top3b_t2_df, "data/ngs/top3b_t2.csv")

# Add Data
top3b_v2_df <- process_ngs_data(data_file_path = "data/raw/ngs_result_v2",
                                result_file_path = "data/ngs/top3b_v2.csv",
                                gene = "TOP3B")

br_top3b_v2_df <- bind_rows(top3b_df, top3b_v2_df)
write_excel_csv(br_top3b_v2_df, "data/ngs/br_top3b_v2.csv")
spread_ngs_data(br_top3b_v2_df, output_file_path = "data/ngs/top3b_t2_v2.csv")


# Draw a Graph
plot_df <- top3b_df %>% 
    select(id, dementia, Coordinate, Variant) %>% 
    mutate(Variant = ifelse(Variant == "normal", 0, 1)) %>% 
    group_by(Coordinate, dementia) %>% 
    summarize(sum = sum(Variant)) %>% 
    mutate(Coordinate = as.character(Coordinate),
           dementia = ifelse(dementia == 1, TRUE, FALSE))
plot_df %>% 
    ggplot(aes(x = reorder(Coordinate, -sum), y = sum)) +
    geom_bar(aes(fill = dementia), stat = "identity", position = "stack") +
    theme(axis.text.x = element_text(angle = 90))
ggsave("output/top3b_coordinate.png")

# The Sum of Coordinate Count by the Patient
p_top3b_df %>%
    group_by(n_variant) %>% 
    summarize(n = n()) %>% 
    ggplot(aes(x = n_variant, y = n)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = 0:max(plot_df$n_variant))
ggsave("top3b_n_variant.png")

# Make Coordinate Combination DataFrame
combn_top3b_df <- top3b_t2_df
for (i in 3:1) {
    combn_mat <- combn(unique(top3b_df$Coordinate), i)
    
    for (j in 1:ncol(combn_mat)) {
        col_name <- paste(c("coordinate", combn_mat[, j]), collapse = "_")
        
        for (k in 1:i) {
            assign(paste0("col", k), paste0("coordinate_", combn_mat[k, j]))
        }
        
        if (i == 3) {
            combn_top3b_df <- combn_top3b_df %>% 
                mutate(!!col_name := ifelse(!!sym(col1) + !!sym(col2) + !!sym(col3) == 3, 1, 0))
        } else if (i == 2) {
            combn_top3b_df <- combn_top3b_df %>% 
                mutate(!!col_name := ifelse((!!sym(col1) + !!sym(col2) == 2) &
                                                n_variant == 2, 1, 0))
        } else {
            combn_top3b_df <- combn_top3b_df %>% 
                mutate(!!col_name := ifelse((!!sym(col1) == 1) &
                                                n_variant == 1, 1, 0))
        }
    }
}
col_sum <- colSums(combn_top3b_df %>% 
                       select(starts_with("coordinate_")))
zero_col <- names(col_sum)[col_sum == 0]

p_combn_top3b_df <- combn_top3b_df %>% 
    select(-zero_col)
write_csv(p_comb_top3b_df, "data/ngs/combn_top3b_data.csv")

combn_top3b_df <- read_csv("data/ngs/combn_top3b_data.csv")

col_sums <- combn_top3b_df %>% 
    select(-c(id, dementia, n_variant)) %>% 
    colSums()

pp_combn_top3b_df <- combn_top3b_df %>% 
    select(id, dementia, names(col_sums)[col_sums >= 5])

write_excel_csv(pp_combn_top3b_df, "data/ngs/pp_combn_top3b_data.csv")

col_sum_df <- as.data.frame(col_sum) %>% 
    arrange(desc(col_sum)) %>% 
    head(10)

# Manipulate ApoE Data ----------------------------------------------------
apoe_df <- process_ngs_data(data_file_path = "data/raw/NGS_result",
                            result_file_path = "data/ngs/apoe_data.csv",
                            gene = "APOE")

# apoe_check_df <- data.frame(id = file_v) %>% 
#     left_join(apoe_df,
#               by = "id") %>%
#     distinct(id, Gene) %>%
#     group_by(Gene) %>% 
#     sample_n(20) %>% 
#     ungroup() %>% 
#     select(id) %>% 
#     left_join(apoe_df,
#               by = "id")
# write_excel_csv(apoe_check_df, "apoe_check.csv")

apoe_df <- read_csv("data/ngs/apoe_data.csv")

apoe_df_t2 <- apoe_df %>% 
    filter(Coordinate == 45409167) %>% 
    mutate(e4_carrier = ifelse(str_detect(apoe, "E4"), 1, 0),
           e4_hom = ifelse(apoe == "E4/E4", 1, 0)) %>% 
    select(id, dementia, starts_with("g_"), starts_with("e4_"))
write_excel_csv(apoe_df_t2, "data/apoe_data_t2.csv")


# Create ApoE Plot --------------------------------------------------------
plot_dataframe <- m_apoe_df %>% 
    select(id, Coordinate, dementia) %>% 
    mutate(dementia = recode(dementia, `1` = TRUE, `0` = FALSE)) %>% 
    # Recode function does not work on numeric vector. Use grave accent.
    distinct() %>% 
    group_by(dementia, Coordinate) %>% 
    summarize(n = n()) %>% 
    mutate(Coordinate = as.character(Coordinate)) %>%
    group_by(Coordinate) %>% 
    mutate(proportion = n / sum(n)) %>% 
    as.data.table()

asterisk_dataframe <- plot_dataframe %>% 
    select(-n) %>% 
    spread(key = dementia, value = proportion) %>% 
    replace(is.na(.), 0) %>% 
    rename(normal_prop = 2, patient_prop = 3) %>% 
    mutate(asterisk = patient_prop > normal_prop)

apoe_plot <- ggplot(data = plot_dataframe, aes(x = reorder(Coordinate, -n), y = n, fill = dementia)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = round(proportion, 2)), position = position_stack(vjust = 0.5)) +
    labs(x = "Coordinate", y = "The Number of Patients")

for (i in 1:nrow(asterisk_dataframe)) {
    if (asterisk_dataframe$asterisk[i] == TRUE) {
        coordinate <- asterisk_dataframe$Coordinate[i]
        apoe_plot <- apoe_plot +
            annotate("text",
                     x = coordinate,
                     y = plot_dataframe[Coordinate == coordinate, sum(n)] + 5,
                     label = "*")
    }
}
ggsave("ApoE_Count_by_Coordinate.png", apoe_plot)