library(dplyr)
library(tidyr)
library(stringr)
patent_count <- function (data, col = "", col1 =  "", n_results = n_results, sep = "[^[:alnum:]]+") {
  i <- str_count(data[[col]], pattern = sep) %>%
    na.omit()
  i <- as.integer(max(i) + 1)
  p_count <- select_(data, col, col1) %>%
    separate_(col, 1:i, sep = sep, fill = "right") %>%
    mutate(n = sum(col1 = 1)) %>%
    gather(x, col, 1:i, na.rm=TRUE)
  p_count$col <- str_trim(p_count$col, side = "both")
  select(p_count, col, n) %>%
    count(col, wt = n) %>%
    arrange(desc(n)) %>%
    .[1:n_results,] %>%
    print()
} 
