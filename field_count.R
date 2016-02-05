library(stringr)
library(dplyr)
field_count <- function(data, col = "", sep = "[^[:alnum:]]+") {
  field_count <- str_count(data[[col]], pattern = sep) %>%
    na.omit()
  n <- as.integer(max(field_count) + 1) %>%
    print()
}