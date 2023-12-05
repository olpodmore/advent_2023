library(tidyverse)

calibrations <- read_lines("input_01.txt")

calibrations_df <- tibble(calibrations) %>%
  mutate(digits = map_chr(str_extract_all(calibrations, "[[:digit:]]"), ~ str_c(.x, collapse=""))) %>%
  mutate(first_digit = str_sub(digits, 1, 1)) %>%
  mutate(last_digit = str_sub(digits, -1, -1)) %>%
  mutate(two_digits = as.numeric(paste0(first_digit, last_digit)))

# solution part 1
sum(calibrations_df$two_digits)

# part 2
find_numbers <- function (x) {
  
  matches <- str_locate_all(x, c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", 
                                                                "1", "2", "3", "4", "5", "6", "7", "8", "9"))
  all <- do.call(rbind, matches)
  start <- all[, 1]
  stop <- all[, 2]
  number_locations <- tibble(start, stop)
  first_number_location <- arrange(number_locations, (start)) %>% slice(1)
  last_number_location <- arrange(number_locations, desc(start)) %>% slice(1)
  
  first_number <- str_sub(x, first_number_location$start[1], first_number_location$stop[1])
  last_number <- str_sub(x, last_number_location$start[1], last_number_location$stop[1])
  
  first_and_last_number <- paste0(first_number, last_number)
  
  return (first_and_last_number)
  
}

numbers <- sapply(calibrations_df$calibrations, find_numbers)

numbers_as_digits <- str_replace_all(numbers, c("one" = "1", "two"="2",
                                      "three" = "3", "four" = "4",
                                      "five" = "5", "six" = "6",
                                      "seven"="7", "eight"="8",
                                      "nine"="9"))

# solution part 2
sum(as.numeric(numbers_as_digits))


