# join_multiple_datasets.r

join_multiple_datasets <- function(inputs) {
  library(dplyr)
  library(purrr)
  
  # Read CSV files if character paths are provided
  datasets <- lapply(inputs, function(x) {
    if (is.character(x)) read.csv(x, stringsAsFactors = FALSE) else x
  })
  
  # Remove empty or invalid datasets
  datasets <- datasets[!sapply(datasets, function(df) is.null(df) || nrow(df) == 0)]
  if (length(datasets) < 2) stop("At least two valid datasets are required.")
  
  # Function to find common columns between two datasets
  get_common_cols <- function(df1, df2) intersect(names(df1), names(df2))
  
  # Sequentially join all datasets on their common columns
  merged <- reduce(datasets, function(df1, df2) {
    common_cols <- get_common_cols(df1, df2)
    if (length(common_cols) == 0) stop("No common columns found for joining.")
    suppressWarnings(
      inner_join(df1, df2, by = common_cols) %>%
        mutate_all(~ ifelse(is.na(.), "", .))
    )
  })
  
  return(merged)
}
