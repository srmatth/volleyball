#### EDA ----


## Setup ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)


volleyball <- readr::read_csv(
  "data/vb_matches.csv",
  guess_max = 20000
)

colnames(volleyball)


plot(density(volleyball$w_p1_hgt, na.rm = TRUE))
lines(density(volleyball$w_p2_hgt, na.rm = TRUE))
lines(density(volleyball$l_p1_hgt, na.rm = TRUE), col = "red")
lines(density(volleyball$l_p2_hgt, na.rm = TRUE), col = "blue")

## Possibility - use k-means clustering to group specific players
## Application - pairing of players to determine best partnerships



## Some data manipulation ----

long <- volleyball %>%
  mutate(
    across(
      everything(),
      as.character
    )
  ) %>%
  rename(
    w_p1_name = w_player1,
    w_p2_name = w_player2,
    w_p1_rank = w_rank,
    l_p1_name = l_player1,
    l_p2_name = l_player2,
    l_p1_rank = l_rank
  ) %>%
  pivot_longer(
    cols = c(starts_with("w_"), starts_with("l_")),
    names_to = c("win_loss", "player", "variable"),
    names_pattern = "(.)_(..)_(.*)"
  )

## I didn't really like that so let's do it this way
volleyball_new <- volleyball %>%
  mutate(game_id = 1:nrow(.))

player_stats <- volleyball_new %>%
  select(
    game_id,
    circuit,
    tournament,
    match_location = country,
    year,
    date,
    gender,
    match_num,
    name = w_player1,
    birthdate = w_p1_birthdate,
    age = w_p1_age,
    height = w_p1_hgt,
    player_country = w_p1_country,
    team_rank = w_rank,
    score,
    duration,
    bracket,
    round,
    attacks = w_p1_tot_attacks,
    kills = w_p1_tot_kills,
    errors = w_p1_tot_errors,
    hitpct = w_p1_tot_hitpct,
    aces = w_p1_tot_aces,
    serve_errors = w_p1_tot_serve_errors,
    blocks = w_p1_tot_blocks,
    digs = w_p1_tot_digs
  ) %>%
  mutate(win = TRUE) %>%
  bind_rows(
    volleyball_new %>%
      select(
        game_id,
        circuit,
        tournament,
        match_location = country,
        year,
        date,
        gender,
        match_num,
        name = w_player2,
        birthdate = w_p2_birthdate,
        age = w_p2_age,
        height = w_p2_hgt,
        player_country = w_p2_country,
        team_rank = w_rank,
        score,
        duration,
        bracket,
        round,
        attacks = w_p2_tot_attacks,
        kills = w_p2_tot_kills,
        errors = w_p2_tot_errors,
        hitpct = w_p2_tot_hitpct,
        aces = w_p2_tot_aces,
        serve_errors = w_p2_tot_serve_errors,
        blocks = w_p2_tot_blocks,
        digs = w_p2_tot_digs
      ) %>%
      mutate(win = TRUE)
  ) %>%
  bind_rows(
    volleyball_new %>%
      select(
        game_id,
        circuit,
        tournament,
        match_location = country,
        year,
        date,
        gender,
        match_num,
        name = l_player1,
        birthdate = l_p1_birthdate,
        age = l_p1_age,
        height = l_p1_hgt,
        player_country = l_p1_country,
        team_rank = l_rank,
        score,
        duration,
        bracket,
        round,
        attacks = l_p1_tot_attacks,
        kills = l_p1_tot_kills,
        errors = l_p1_tot_errors,
        hitpct = l_p1_tot_hitpct,
        aces = l_p1_tot_aces,
        serve_errors = l_p1_tot_serve_errors,
        blocks = l_p1_tot_blocks,
        digs = l_p1_tot_digs
      ) %>%
      mutate(win = FALSE)
  ) %>%
  bind_rows(
    volleyball_new %>%
      select(
        game_id,
        circuit,
        tournament,
        match_location = country,
        year,
        date,
        gender,
        match_num,
        name = l_player2,
        birthdate = l_p2_birthdate,
        age = l_p2_age,
        height = l_p2_hgt,
        player_country = l_p2_country,
        team_rank = w_rank,
        score,
        duration,
        bracket,
        round,
        attacks = l_p2_tot_attacks,
        kills = l_p2_tot_kills,
        errors = l_p2_tot_errors,
        hitpct = l_p2_tot_hitpct,
        aces = l_p2_tot_aces,
        serve_errors = l_p2_tot_serve_errors,
        blocks = l_p2_tot_blocks,
        digs = l_p2_tot_digs
      ) %>%
      mutate(win = FALSE)
  )

## Get Player-level Stats ----

player_info <- player_stats %>%
  filter(gender == "M") %>%
  group_by(name, birthdate) %>%
  summarize(
    num_games = n(),
    overall_record = mean(win),
    min_age = min(age),
    max_age = max(age),
    # hitpct = mean(hitpct, na.rm = TRUE),
    height = mean(height, na.rm = TRUE),
    attacks = sum(attacks, na.rm = TRUE),
    kills = sum(kills, na.rm = TRUE),
    errors = sum(errors, na.rm = TRUE),
    aces = sum(aces, na.rm = TRUE),
    serve_errors = sum(serve_errors, na.rm = TRUE),
    blocks = sum(blocks, na.rm = TRUE),
    digs = sum(digs, na.rm = TRUE)
  ) %>%
  arrange(desc(num_games)) %>%
  ungroup() %>%
  filter(num_games > 19, attacks > 0, !is.na(height)) %>%
  mutate(
    hit_pct = (kills - errors) / attacks,
    aces_per_game = aces / num_games,
    serve_errors_per_game = serve_errors / num_games,
    blocks_per_game = blocks / num_games,
    digs_per_game = digs / num_games
  ) %>%
  select(
    name,
    birthdate,
    height,
    hit_pct,
    aces_per_game,
    serve_errors_per_game,
    blocks_per_game,
    digs_per_game
  )


## Explore the stats ----

corrplot(cor(player_info %>% select(-name, -birthdate)))

player_info %>%
  ggplot() +
  aes(x = aces_per_game, y = serve_errors_per_game) +
  geom_point() +
  geom_smooth(method = "lm")

player_info %>%
  ggplot() +
  aes(x = aces_per_game, y = digs_per_game) +
  geom_point()

readr::write_csv(player_info, "data/player_info.csv")
