library(tidyverse)

scratchcards <- read_delim("input_04.txt", delim = "|", col_names = F)

names(scratchcards) <- c("winning_numbers", "my_numbers") 

scratchcards_clean <- scratchcards %>%
  separate(col=winning_numbers, into = c("game_number", "winning_numbers"), sep=":") %>%
  mutate(winning_numbers = str_trim(winning_numbers, side="both"), my_numbers = str_trim(my_numbers, side="both")) %>%
  mutate(winning_numbers = str_replace_all(winning_numbers, "  ", " "), my_numbers = str_replace_all(my_numbers, "  ", " ")) 

scratchcards_separated <- scratchcards_clean %>%
  separate_rows(winning_numbers, sep= " ") %>%
  separate_rows(my_numbers, sep =" ")

scratchcard_winners <- scratchcards_separated %>%
  group_by(game_number) %>%
  filter(my_numbers == winning_numbers) %>%
  group_by(game_number) %>%
  summarise(number_of_winners = n()) %>%
  mutate(points = 2^(number_of_winners-1))

# answer part 1
sum(scratchcard_winners$points)


# part 2 ------------------------------------------------------------------

# add back in the scratch cards with no winners

scratchcard_win_counts <- scratchcards_clean %>%
  select(game_number) %>%
  left_join(scratchcard_winners, by="game_number") %>%
  mutate("game_number" = as.numeric(str_extract(game_number, "[[:digit:]]+"))) #%>% # reduce to just digit to make it easier to match

scratchcard_win_counts_fixed <- scratchcard_win_counts

for (i in 1:202) {
  
  game_df <- scratchcard_win_counts %>%
    filter(game_number==i) %>%
    group_by(game_number, number_of_winners, points) %>%
    summarise(number_of_cards = n())
  
  number_of_wins <- game_df %>%
    pull(number_of_winners)
  
  number_of_cards <- game_df %>%
    pull()
  
  if (!is.na(number_of_wins)) {
    which_card_copies_won <- (i+1):(i+number_of_wins)
    card_copies <- rep(which_card_copies_won, number_of_cards)
    card_copies_index <- tibble(game_number = card_copies)
    card_copies_to_duplicate <- left_join(card_copies_index, scratchcard_win_counts_fixed) # add number of winners to copied cards
    scratchcard_win_counts <- bind_rows(scratchcard_win_counts, card_copies_to_duplicate) 
  }
  
  
}

