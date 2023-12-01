library(tidyverse)

calibrations <- read_lines("input_01.txt")

calibrations_df <- tibble(calibrations) %>%
  mutate(digits = map_chr(str_extract_all(calibrations, "[[:digit:]]"), ~ str_c(.x, collapse=""))) %>%
  mutate(first_digit = str_sub(digits, 1, 1)) %>%
  mutate(last_digit = str_sub(digits, -1, -1)) %>%
  mutate(two_digits = as.numeric(paste0(first_digit, last_digit)))

# solution part 1
sum(calibrations_df$two_digits)

numbers_as_words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

# will continue part 2 later

