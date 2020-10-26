rename_table <- function(x, row, column) {
    rownames(x) <- paste0(row, ": ", rownames(x))
    colnames(x) <- paste0(column, ": ", colnames(x))
    
    return(x)
}

print_chisq_test <- function(data, row, column) {
    x <- table(data %>% 
                   pull(row),
               data %>% 
                   pull(column))
    
    y <- chisq.test(x)
    print("Observed")
    print(rename_table(y$observed, row, column))
    print("Expected")
    print(rename_table(y$expected, row, column))
    print(y)
}
