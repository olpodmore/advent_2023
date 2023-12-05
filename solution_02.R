library(tidyverse)

game_records <- read_delim("input_02.txt", delim = ":", col_names = F) 

names(game_records) <- c("game_number", "sets")

game_records_split <- game_records %>%
  mutate(number_red = str_extract_all(sets, "[[:digit:]]+ red")) %>%
  mutate(number_red = str_extract_all(number_red, "[[:digit:]]+")) %>%
  mutate(number_green = str_extract_all(sets, "[[:digit:]]+ green")) %>%
  mutate(number_green = str_extract_all(number_green, "[[:digit:]]+")) %>%
  mutate(number_blue = str_extract_all(sets, "[[:digit:]]+ blue")) %>%
  mutate(number_blue = str_extract_all(number_blue, "[[:digit:]]+")) 

x <- game_records_split$number_red[[1]]

did_cube_colour_exceed_limit <- function (game, limit) {
  
  number_of_colour <- as.numeric(unlist(game))
  exceeded <- any(number_of_colour > limit)
  return(exceeded)
  
}

exceeded_check_red <- lapply(game_records_split$number_red, did_cube_colour_exceed_limit, 12)
not_exceeded_red <- which(exceeded_check_red==FALSE)

exceeded_check_green <- lapply(game_records_split$number_green, did_cube_colour_exceed_limit, 13)
not_exceeded_green <- which(exceeded_check_green==FALSE)

exceeded_check_blue <- lapply(game_records_split$number_blue, did_cube_colour_exceed_limit, 14)
not_exceeded_blue <- which(exceeded_check_blue==FALSE)

possible_games <- intersect(intersect(not_exceeded_red,not_exceeded_green),not_exceeded_blue)

# answer part 1
sum(possible_games)

# part 2

minimum_number_of_cubes <- function (game) {
  
  number_of_colour <- as.numeric(unlist(game))
  minimum_number <- max(number_of_colour)
  return(minimum_number)
  
}

minimum_red <- unlist(lapply(game_records_split$number_red, minimum_number_of_cubes))
minimum_green <- unlist(lapply(game_records_split$number_green, minimum_number_of_cubes))
minimum_blue <- unlist(lapply(game_records_split$number_blue, minimum_number_of_cubes))

powers <- minimum_red*minimum_green*minimum_blue
sum(powers)
