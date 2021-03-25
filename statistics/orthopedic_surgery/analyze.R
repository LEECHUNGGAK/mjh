# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(car)

setwd("C:/Users/Administrator/work/1_통계자문/orthopedic_surgery")

v <- c("post_er_at_side", "post_er_90", "post_ir_90", "post_ir_back",
       "post_er_strength", "post_ir_strength")

d <- read_excel("OS 이용걸교수님 논문통계데이터.xlsx",
                col_types = "text",
                skip = 1) %>% 
    rename(post_er_at_side = 27, post_er_90 = 28, post_ir_90 = 29,
           post_ir_back = 31, post_er_strength = 32, post_ir_strength = 33,
           sbs = 40) %>% 
    select(sbs, all_of(v)) %>% 
    mutate(sbs = as.factor(as.integer(sbs)),
           post_er_at_side = as.integer(post_er_at_side),
           post_er_90 = as.integer(post_er_90),
           post_ir_90 = as.integer(post_ir_90),
           post_ir_back = as.integer(post_ir_back),
           post_er_strength = as.double(post_er_strength),
           post_ir_strength = as.double(post_ir_strength)) %>% 
    drop_na(sbs)


# Analyze -----------------------------------------------------------------
sink("result.txt")
for (i in v) {
    ggplot(data = d, aes_string(x = "sbs", y = i)) +
        geom_boxplot()
    ggsave(paste0(i, "_boxplot.png"))
    
    ggplot(data = d, aes_string(sample = i)) +
        geom_qq() +
        geom_qq_line() + 
        annotate(geom = "text", x = -Inf, y = Inf, hjust = 0, vjust = 1,
                 label = paste0("Shapiro test p-value: ", sprintf("%0.4f", shapiro.test(pull(d, i))[["p.value"]])))
    ggsave(paste0(i, "_qq.png"))
    
    fml <- as.formula(paste(i, "~ sbs"))
    cat("Levene test: ", i, "\n")
    print(leveneTest(fml, data = d))
    cat("Bartlett test: ", i, "\n")
    print(bartlett.test(fml, data = d))
    cat("AOV: ", i, "\n")
    print(summary(aov(as.formula(paste(i, "~ sbs")), data = d)))
}
sink()
