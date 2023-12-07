library(tidyverse)

engine_map <- read_lines("input_03.txt") 

numbers_in_engine_map <- tibble()

for (x in 1:140) {
  
  line <- engine_map[x]
  
  number_locations_matrix <- str_locate_all(line, "[[:digit:]]+")
  number_locations_combined <- do.call(rbind, number_locations_matrix)
  number_start <- number_locations_combined[, 1]
  number_stop <- number_locations_combined[, 2]
  number_locations_df <- tibble(number_start, number_stop) %>%
    mutate(line=x, .before = number_start)
  
  for (y in 1:nrow(number_locations_df)) {
    
    # when characters are at the far right edge of lines -- returns "" and FALSE (e.g. line 9)
    # when characters are at the far left edge of lines -- returns "" and FALSE (e.g. line 42)
    # first line -- returns character(0) when looking above and logical (0) for str_detect which counts as FALSE in any
    # last line -- returns NA when looking below and NA for str_detect which turns any to NA if all FALSE
    
    characters_below <- str_sub(engine_map[x+1], number_locations_df$number_start[y]-1, number_locations_df$number_stop[y]+1)
    characters_above <- str_sub(engine_map[x-1], number_locations_df$number_start[y]-1, number_locations_df$number_stop[y]+1)
    characters_left <- str_sub(engine_map[x], number_locations_df$number_start[y]-1, number_locations_df$number_start[y]-1)
    characters_right <- str_sub(engine_map[x], number_locations_df$number_stop[y]+1, number_locations_df$number_stop[y]+1)
    
    symbol_in_characters_below <- str_detect(characters_below, "[^.0-9]")
    symbol_in_characters_above <- str_detect(characters_above, "[^.0-9]")
    symbol_in_characters_left <- str_detect(characters_left, "[^.0-9]")
    symbol_in_characters_right <- str_detect(characters_right, "[^.0-9]")
    
    adjacent_to_symbol <- any(symbol_in_characters_below,  symbol_in_characters_above,  symbol_in_characters_left, symbol_in_characters_right)
    
    number_locations_df$adjacent_to_symbol[y] <- adjacent_to_symbol
    
  } 
  
  numbers_in_engine_map <- bind_rows(numbers_in_engine_map, number_locations_df)
  
}

numbers_in_engine_map_extracted <- numbers_in_engine_map %>%
  filter(adjacent_to_symbol == TRUE) %>%
  mutate(number = str_sub(engine_map[line], number_start, number_stop))

# answer part 1
sum(as.numeric(numbers_in_engine_map_extracted$number))

# part 2

for (x in 1:140) {
  
  x <- 2
  
  line <- engine_map[x]
  
  gear_locations_matrix <- str_locate_all(line, "[*]")
  gear_locations_combined <- do.call(rbind, gear_locations_matrix)
  gear_start <- gear_locations_combined[, 1]
  gear_stop <- gear_locations_combined[, 2]
  gear_locations_df <- tibble(gear_start, gear_stop) %>%
    mutate(line=x, .before = gear_start)
  
  for (y in 1:nrow(gear_locations_df)) {
    
    y <- 1
    
    # when characters are at the far right edge of lines -- returns "" and FALSE (e.g. line 9)
    # when characters are at the far left edge of lines -- returns "" and FALSE (e.g. line 42)
    # first line -- returns character(0) when looking above and logical (0) for str_detect which counts as FALSE in any
    # last line -- returns NA when looking below and NA for str_detect which turns any to NA if all FALSE
    
    characters_below <- str_sub(engine_map[x+1], gear_locations_df$gear_start[y]-1, gear_locations_df$gear_stop[y]+1)
    characters_above <- str_sub(engine_map[x-1], gear_locations_df$gear_start[y]-1, gear_locations_df$gear_stop[y]+1)
    characters_left <- str_sub(engine_map[x], gear_locations_df$gear_start[y]-1, gear_locations_df$gear_start[y]-1)
    characters_right <- str_sub(engine_map[x], gear_locations_df$gear_stop[y]+1, gear_locations_df$gear_stop[y]+1)
    
    number_in_characters_below <- str_extract(characters_below, "[[:digit:]]+")
    number_in_characters_above <- str_extract(characters_above, "[[:digit:]]+")
    number_in_characters_left <- str_extract(characters_left, "[[:digit:]]+")
    number_in_characters_right <- str_extract(characters_right, "[[:digit:]]+")
    
    adjacent_to_symbol <- any(symbol_in_characters_below,  symbol_in_characters_above,  symbol_in_characters_left, symbol_in_characters_right)
    
    number_locations_df$adjacent_to_symbol[y] <- adjacent_to_symbol
    
  } 
  
  numbers_in_engine_map <- bind_rows(numbers_in_engine_map, number_locations_df)
  
}

