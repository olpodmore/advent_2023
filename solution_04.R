library(tidyverse)

scratchcards <- read_delim("input_04.txt", delim = "|", col_names = F)

names(scratchcards) <- c("winning_numbers", "my_numbers") 

scratchcards_separated <- scratchcards %>%
  separate(col=winning_numbers, into = c("game_number", "winning_numbers"), sep=":") %>%
  mutate(winning_numbers = str_trim(winning_numbers, side="both"), my_numbers = str_trim(my_numbers, side="both")) %>%
  mutate(winning_numbers = str_replace_all(winning_numbers, "  ", " "), my_numbers = str_replace_all(my_numbers, "  ", " ")) %>%
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


# TBC










# 
# colnames_winning_numbers <- c()
# 
# for (i in 1:10) {
#   
#   name <- paste0("winning_number_", i)
#   colnames_winning_numbers <- c(colnames_winning_numbers, name)
#   
# }
# 
# colnames_my_numbers <- c()
# 
# for (i in 1:25) {
#   
#   name <- paste0("my_number_", i)
#   colnames_my_numbers <- c(colnames_my_numbers, name)
#   
# }


# scratchcards_separated <- scratchcards %>%
#   separate(col=winning_numbers, into = c("game_number", "winning_numbers"), sep=":") %>%
#   mutate(winning_numbers = str_trim(winning_numbers, side="both"), my_numbers = str_trim(my_numbers, side="both")) %>%
#   mutate(winning_numbers = str_replace_all(winning_numbers, "  ", " "), my_numbers = str_replace_all(my_numbers, "  ", " ")) %>%
#   separate(col=winning_numbers, into=colnames_winning_numbers, sep = " ", remove=FALSE) %>%
#   separate(col=my_numbers, into=colnames_my_numbers, sep = " ", remove=FALSE) %>%
#   pivot_longer(cols=colnames_winning_numbers, names_to="winning_number_id", values_to="winning_number_value") %>%
#   pivot_longer(cols=colnames_my_numbers, names_to="my_number_id", values_to="my_number_value")
# 
# scratchcard_winners <- scratchcards_separated %>%
#   group_by(game_number) %>%
#   mutate(winning_number = my_number_value %in% winning_number_value)
#   
# 
# test <- tibble(game_number=c(1,1,1,2,2,2), winning=c(3,4,5,6,7,8), mine=c(4,5,6,7,8,3))
# 
# test2 <- test %>%
#   group_by(game_number) %>%
#   mutate(winning_number = mine %in% winning)

