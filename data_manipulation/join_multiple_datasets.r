# join_multiple_datasets.r
#
# Algorithm Description:
# This script provides a function to join multiple datasets (data frames or CSV file paths)
# by their common columns using inner joins. The function reads input datasets, removes
# empty or invalid ones, and then sequentially merges them on their shared columns.
# The merging is performed using dplyr's inner_join, and missing values are replaced with
# empty strings. The algorithm iteratively joins datasets, so its complexity is O(n * m),
# where n is the number of datasets and m is the average number of rows in each dataset.
#
# Example usage:
#   result <- join_multiple_datasets(list("data1.csv", "data2.csv", df3))
#   head(result)
#

#' Join Multiple Datasets by Common Columns
#'
#' This function takes a list of data frames or CSV file paths and joins them sequentially
#' on their common columns using inner joins. It reads CSV files if paths are provided,
#' removes empty or invalid datasets, and merges the remaining datasets. Missing values
#' in the result are replaced with empty strings.
#'
#' @param inputs A list of data frames and/or character strings representing CSV file paths.
#' @return A data frame resulting from the inner join of all valid input datasets on their common columns.
#' @examples
#' # Example 1: Joining three data frames
#' df1 <- data.frame(id = 1:3, val1 = c("A", "B", "C"))
#' df2 <- data.frame(id = 2:3, val2 = c("X", "Y"))
#' df3 <- data.frame(id = 3, val3 = "Z")
#' result <- join_multiple_datasets(list(df1, df2, df3))
#' print(result)
#'
#' # Example 2: Joining CSV files and a data frame
#' result <- join_multiple_datasets(list("file1.csv", "file2.csv", df3))
#' head(result)
library(dplyr)
library(purrr)

join_multiple_datasets <- function(inputs) {
    if (is.character(x)) read.csv(x, stringsAsFactors = FALSE) else x
  })
  
  # Remove empty or invalid datasets
  datasets <- datasets[!sapply(datasets, function(df) is.null(df) || nrow(df) == 0)]
  if (length(datasets) < 2) stop(sprintf("At least two valid datasets are required, but only %d valid dataset(s) found.", length(datasets)))
  
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
