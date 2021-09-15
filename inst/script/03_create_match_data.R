#### 03 Creating Match Data ----


## Setup ----

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

matches <- read_csv(
  "data/vb_matches.csv",
  guess_max = 20000
)
grouped_data <- read_csv("data/player_clusters.csv")

set.seed(16)

## Create Data ----

## Filter where matches were played by the people we have stats for

id_lookup <- grouped_data %>%
  mutate(
    id = stringr::str_c(
      name,
      birthdate,
      sep = " "
    )
  ) %>%
  select(id, .cluster) %>%
  filter(!is.na(id))


matches_sub <- matches %>%
  mutate(
    w_p1_id = stringr::str_c(
      w_player1,
      w_p1_birthdate,
      sep = " "
    ),
    w_p2_id = stringr::str_c(
      w_player2,
      w_p2_birthdate,
      sep = " "
    ),
    l_p1_id = stringr::str_c(
      l_player1,
      l_p1_birthdate,
      sep = " "
    ),
    l_p2_id = stringr::str_c(
      l_player2,
      l_p2_birthdate,
      sep = " "
    )
  ) %>%
  ## Only get games where all 4 players are in grouped_data
  filter(
    w_p1_id %in% id_lookup$id,
    w_p2_id %in% id_lookup$id,
    l_p1_id %in% id_lookup$id,
    l_p2_id %in% id_lookup$id
  )

## Filters it down to about 20,000 observations from 75,000. So quite
## a drop there.

matches_with_group <- matches_sub %>%
  left_join(
    id_lookup %>%
      rename(w_p1_cluster = .cluster),
    by = c("w_p1_id" = "id")
  ) %>%
  left_join(
    id_lookup %>%
      rename(w_p2_cluster = .cluster),
    by = c("w_p2_id" = "id")
  ) %>%
  left_join(
    id_lookup %>%
      rename(l_p1_cluster = .cluster),
    by = c("l_p1_id" = "id")
  ) %>%
  left_join(
    id_lookup %>%
      rename(l_p2_cluster = .cluster),
    by = c("l_p2_id" = "id")
  )

## Explore Data a Little ----

combos <- matches_with_group %>%
  mutate(
    winning_combo = ifelse(
      w_p1_cluster < w_p2_cluster,
      str_c(w_p1_cluster, w_p2_cluster, sep = "-"),
      str_c(w_p2_cluster, w_p1_cluster, sep = "-")
    ),
    losing_combo = ifelse(
      l_p1_cluster < l_p2_cluster,
      str_c(l_p1_cluster, l_p2_cluster, sep = "-"),
      str_c(l_p2_cluster, l_p1_cluster, sep = "-")
    )
  ) %>%
  select(
    w_p1_cluster,
    w_p2_cluster,
    l_p1_cluster,
    l_p2_cluster,
    winning_combo,
    losing_combo,
    score
  )

combos %>%
  count(winning_combo) %>%
  arrange(desc(n))

combos %>%
  count(losing_combo) %>%
  arrange(desc(n))

combos %>%
  count(winning_combo, losing_combo) %>%
  arrange(desc(n)) %>%
  View()

summary <- combos %>%
  count(winning_combo) %>%
  arrange(desc(n)) %>%
  rename(n_won = n) %>%
  left_join(
    combos %>%
      count(losing_combo) %>%
      rename(n_lost = n),
    by = c("winning_combo" = "losing_combo")
  ) %>%
  rename(combo = winning_combo) %>%
  mutate(
    tot_played = n_won + n_lost,
    pct_won = n_won / tot_played,
    freq = tot_played / sum(tot_played)
  )
summary

## Save the Data ----

write_csv(matches_with_group, "data/matches_with_group.csv")
