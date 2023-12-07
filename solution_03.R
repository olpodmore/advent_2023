library(tidyverse)

engine_map <- read_lines("input_03.txt") 

# initialise empty tibble to be populated
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


# part 2 ------------------------------------------------------------------

# initialise empty tibble to be populated
gear_locations_df <- tibble()

for (x in 1:140) {
  
  line <- engine_map[x]
  
  number_locations_matrix <- str_locate_all(line, "[[:digit:]]+")
  number_locations_combined <- do.call(rbind, number_locations_matrix)
  number_start <- number_locations_combined[, 1]
  number_stop <- number_locations_combined[, 2]
  number_locations_df <- tibble(number_start, number_stop) %>%
    mutate(line=x, .before = number_start)
  
  gear_locations_temp_line <- tibble()
  
  for (y in 1:nrow(number_locations_df)) {
    
    characters_below <- str_sub(engine_map[x+1], number_locations_df$number_start[y]-1, number_locations_df$number_stop[y]+1)
    characters_above <- str_sub(engine_map[x-1], number_locations_df$number_start[y]-1, number_locations_df$number_stop[y]+1)
    characters_left <- str_sub(engine_map[x], number_locations_df$number_start[y]-1, number_locations_df$number_start[y]-1)
    characters_right <- str_sub(engine_map[x], number_locations_df$number_stop[y]+1, number_locations_df$number_stop[y]+1)
    
    # gears below
    gear_in_characters_below <- str_detect(characters_below, "[*]")
    
    if (is.na(gear_in_characters_below)) {gear_in_characters_below <- FALSE} # for line 140 that has no lines below
    
    if (gear_in_characters_below) { 
      gear_location_relative <- str_locate(characters_below, "[*]")[1] # the start/end value is the same since it's only one character so only extract one
      gear_location_absolute <- number_locations_df$number_start[y] + gear_location_relative -2 # subtract two because the length of characters below is two longer than the number (overlap on either side)
      
      gear_locations_below <- tibble(adjacent_gear_line = x+1, number_line = x, number_start = number_locations_df$number_start[y],
                                    number_stop = number_locations_df$number_stop[y],
                                    adjacent_gear_location = gear_location_absolute) 
    }  else {
      gear_locations_below <- tibble() #if no gears to the left, make an empty tibble
    }
    
    # gears above
    gear_in_characters_above <- str_detect(characters_above, "[*]")
    if (length(gear_in_characters_above)==0) {gear_in_characters_above <- FALSE} # for line 1 that has no lines above
    
    if (gear_in_characters_above) { 
      gear_location_relative <- str_locate(characters_above, "[*]")[1] # the start/end value is the same since it's only one character so only extract one
      gear_location_absolute <- number_locations_df$number_start[y] + gear_location_relative -2 # subtract two because the length of characters below is two longer than the number (overlap on either side)
      
      gear_locations_above <- tibble(adjacent_gear_line = x-1, number_line = x, number_start = number_locations_df$number_start[y],
                                     number_stop = number_locations_df$number_stop[y],
                                     adjacent_gear_location = gear_location_absolute) 
    }  else {
      gear_locations_above <- tibble() #if no gears to the left, make an empty tibble
    }
    
    # gears to the left
    gear_in_characters_left <- str_detect(characters_left, "[*]")
    if (gear_in_characters_left) { 
      gear_locations_left <- tibble(adjacent_gear_line = x, number_line = x, number_start = number_locations_df$number_start[y],
                                          number_stop = number_locations_df$number_stop[y],
                                          adjacent_gear_location = number_locations_df$number_start[y]-1) # the location of the gear will always just be one position to the left
    }  else {
      gear_locations_left <- tibble() #if no gears to the left, make an empty tibble
    }
    
    # gears to the right
    gear_in_characters_right <- str_detect(characters_right, "[*]")
    if (gear_in_characters_right) { 
      gear_locations_right <- tibble(adjacent_gear_line = x, number_line = x, number_start = number_locations_df$number_start[y],
                                          number_stop = number_locations_df$number_stop[y],
                                          adjacent_gear_location = number_locations_df$number_stop[y]+1) # the location of the gear will always just be one position to the right
    } else {
      gear_locations_right <- tibble() #if no gears to the right, make an empty tibble
    }
    
    # binding all gear locations for a single number
    gear_locations_temp_number <- bind_rows(gear_locations_below, gear_locations_above, gear_locations_left, gear_locations_right)
    
    # binding all gear locations for a single line
    gear_locations_temp_line <- bind_rows(gear_locations_temp_line, gear_locations_temp_number)
  } 
  
  # binding all gear locations
  gear_locations_df <- bind_rows(gear_locations_df, gear_locations_temp_line)
  
}

gears_with_2_adjacent_numbers <- gear_locations_df %>%
  distinct() %>% # in case of duplicates
  mutate("absolute_gear_location" = paste0(adjacent_gear_line, "_", adjacent_gear_location)) %>%
  group_by(absolute_gear_location) %>%
  mutate(absolute_gear_location_count=n()) %>%
  filter(absolute_gear_location_count==2) %>%
  mutate(number = str_sub(engine_map[number_line], number_start, number_stop)) %>%
  group_by(absolute_gear_location) %>%
  summarise(gear_ratio = prod(as.numeric(number)))

sum(gears_with_2_adjacent_numbers$gear_ratio)

