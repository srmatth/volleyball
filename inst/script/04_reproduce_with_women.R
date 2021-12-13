#### 04 Reproduce for Females ----

## Setup ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(readr)
library(tidymodels)
library(purrr)
library(GGally)
library(patchwork)

tidymodels_prefer()


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
  filter(gender == "W") %>%
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
    num_games,
    overall_record,
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

readr::write_csv(player_info, "data/player_info_womens.csv")


player_info <- read_csv("data/player_info_womens.csv")

set.seed(16)

## Preprocess the data ----

mod_dat <- player_info %>%
  select(-name, -birthdate, -num_games, -overall_record) %>%
  mutate(
    across(
      where(is.numeric),
      scale
    )
  )

## Create the clusters ----

clusters <- kmeans(mod_dat, centers = 3)
summary(clusters)
tidy(clusters)


## Some simple visualizations ----

augment(clusters, player_info) %>%
  ggplot(aes(height, aces_per_game, color = .cluster)) +
  geom_point()

augment(clusters, player_info) %>%
  ggplot(aes(hit_pct, aces_per_game, color = .cluster)) +
  geom_point()

augment(clusters, player_info) %>%
  ggplot(aes(digs_per_game, blocks_per_game, color = .cluster)) +
  geom_point()

## Testing different values of k ----

kclusts <- tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(mod_dat, .x)),
    glanced = map(kclust, glance),
  )

kclusts %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")

## There's not really a great elbow, but we are just
## going to use 4 since that's what we did for the men

## Final clustering ----

clusters_final <- kmeans(mod_dat, centers = 4)
player_clusters <- augment(clusters_final, player_info)

write_csv(player_clusters, "data/player_clusters_women.csv")

player_clusters <- read_csv("data/player_clusters_women.csv")

library(extrafont)
loadfonts()
colors <- c("red", "black", "blue", "green")



pairs(
  player_clusters %>%
    select(-name, -birthdate, -.cluster) %>%
    magrittr::set_colnames(
      snakecase::to_title_case(
        colnames(.)
      )
    ),
  col = colors[player_clusters$.cluster],
  pch = 19,
  alpha = 0.5,
  family = "Times New Roman"
)

new_plot <- function(data, mapping, ...) {
  data %>%
    ggplot() +
    geom_histogram(mapping = mapping)
}

player_clusters %>%
  select(-name, -birthdate, -num_games, -overall_record) %>%
  magrittr::set_colnames(
    snakecase::to_title_case(
      colnames(.)
    )
  ) %>%
  ggpairs(
    columns = 1:6,
    ggplot2::aes(
      color = as.factor(Cluster),
      fill = as.factor(Cluster)
    ),
    diag = list(continuous = new_plot),
    upper = list(continuous = wrap("cor", family = "Times New Roman"))
  ) +
  scale_color_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    strip.text.y = element_text(angle = 0)
  )

#### Exploring Clusters ----


player_clusters <- read_csv("data/player_clusters_women.csv")


## Exploration ----

library(stringr)

cluster_sum <- player_clusters %>%
  mutate(
    birth_year = as.numeric(str_sub(birthdate, 1, 4)),
    birth_year = ifelse(
      birth_year > 2020,
      birth_year - 100,
      birth_year
    )
  ) %>%
  group_by(.cluster) %>%
  summarize(
    n = n(),
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    )
  ) %>%
  ungroup()

rect <- cluster_sum %>%
  select(-birth_year, -num_games, -overall_record) %>%
  mutate(
    .cluster = as.factor(.cluster),
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~.x / max(.x)
    )
  ) %>%
  pivot_longer(
    cols = colnames(select(., -.cluster, -n)),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = snakecase::to_title_case(metric)) %>%
  ggplot() +
  aes(x = metric, y = value, fill = .cluster) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_classic() +
  ylab("Scaled Mean Value \n(% of maximum)") +
  xlab("") +
  labs(fill = "Cluster Grouping: ") +
  ggtitle("Comparison of Clusters") +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  guides(
    fill = guide_legend(
      keywidth = unit(0.5, "in"),
      keyheight = unit(0.05, "in"),
      title.vjust = 1
    )
  ) +
  scale_color_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  )

## Donut chart for proportion of players
d1 <- player_clusters %>%
  count(.cluster) %>%
  mutate(
    pct = n / sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n=-1)),
    label_position = (ymin + ymax) / 2,
    label = str_c(
      "Cluster ",
      .cluster,
      "\nPlayers: ",
      n,
      "\nPercent: ",
      round(pct * 100, 2),
      "%"
    )
  ) %>%
  ggplot() +
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = as.factor(.cluster)
  ) +
  geom_rect() +
  geom_label(
    aes(
      x = 3.5,
      y = label_position,
      label = label
    ),
    size = 3,
    fill = "white",
    family = "Times New Roman"
  ) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  ggtitle("Number of Players by Cluster") +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  )

d2 <- player_clusters %>%
  group_by(.cluster) %>%
  summarize(n = sum(num_games)) %>%
  ungroup() %>%
  mutate(
    pct = n / sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n=-1)),
    label_position = (ymin + ymax) / 2,
    label = str_c(
      "Cluster ",
      .cluster,
      "\nMatches Played: ",
      scales::number(n, big.mark = ",", accuracy = 1),
      "\nPercent: ",
      round(pct * 100, 2),
      "%"
    )
  ) %>%
  ggplot() +
  aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 4,
    xmin = 3,
    fill = as.factor(.cluster)
  ) +
  geom_rect() +
  geom_label(
    aes(
      x = 3.5,
      y = label_position,
      label = label
    ),
    size = 3,
    family = "Times New Roman",
    fill = "white"
  ) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  ggtitle("Matches Played by Cluster") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  ) +
  scale_fill_manual(
    values = c(
      "#B6D094",
      "#6A2E35",
      "#D6B59A",
      "#2E2836"
    )
  )

d1
d2
rect


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
grouped_data <- read_csv("data/player_clusters_women.csv")

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

write_csv(matches_with_group, "data/matches_with_group_women.csv")


#### 03a EXPLORE MATCH DATA ----

## Setup ----

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(patchwork)
library(extrafont)
loadfonts()
library(forcats)

matches_with_group <- read_csv("data/matches_with_group_women.csv")

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

get_game_scores <- function(scores) {
  clean_scores <- str_extract_all(scores, "[:digit:]+")
  purrr::map_dfr(
    .x = clean_scores,
    .f = ~{
      data.frame(
        num_matches = length(.x) / 2,
        w_score_1 = as.numeric(.x[1]),
        l_score_1 = as.numeric(.x[2]),
        w_score_2 = as.numeric(.x[3]),
        l_score_2 = as.numeric(.x[4]),
        w_score_3 = as.numeric(.x[5]),
        l_score_3 = as.numeric(.x[6])
      ) %>%
        mutate(
          score_diff_1 = w_score_1 - l_score_1,
          score_diff_2 = w_score_2 - l_score_2,
          score_diff_3 = w_score_3 - l_score_3,
          tot_score_diff = ifelse(
            num_matches == 2,
            score_diff_1 + score_diff_2,
            score_diff_1 + score_diff_2 + score_diff_3
          )
        )
    }
  )
}

combos_w_score <- combos %>%
  bind_cols(get_game_scores(combos$score))


winning_combo_summary <- combos_w_score %>%
  filter(num_matches > 1) %>%
  group_by(winning_combo, losing_combo) %>%
  summarize(
    n = n(),
    num_sweep = sum(num_matches == 2),
    pct_sweep = sum(num_matches == 2) / n(),
    avg_win_margin = mean(tot_score_diff),
    pct_won_match_1 = mean(score_diff_1 > 0),
    pct_won_match_2 = mean(score_diff_2 > 0)
  ) %>%
  filter(n > 100)

record_by_combo <- combos %>%
  count(combo = winning_combo) %>%
  rename(games_won = n) %>%
  left_join(
    combos %>%
      count(combo = losing_combo) %>%
      rename(games_lost = n),
    by = "combo"
  ) %>%
  mutate(
    games_played = games_won + games_lost,
    record = games_won / games_played
  )

plot(record_by_combo$games_played, record_by_combo$record)

record_by_combo %>%
  mutate(
    combo = fct_reorder(combo, record),
    label = str_c(
      "Wins: ", scales::number(games_won, accuracy = 1, big.mark = ","),
      " Losses: ", scales::number(games_lost, accuracy = 1, big.mark = ",")
    )
  ) %>%
  ggplot() +
  aes(y = combo, x = record, fill = games_played) +
  geom_bar(stat = "identity") +
  geom_label(
    aes(x = record / 2, y = combo, label = label),
    fill = "#ffffff",
    family = "Times New Roman"
  ) +
  xlab("Overall Win Percentage") +
  ylab("Partner\nGroup\nCombination") +
  labs(fill = "Number of Games\nPlayed") +
  ggtitle("Record by Partner Group Combination", "Colored by Number of Games Played") +
  scale_x_continuous(
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = c(0.85, 0.15),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) +
  scale_fill_gradient(low = "#F0DCE5", high = "#2e2836") +
  guides(
    fill = guide_colorbar(
      title.hjust = 0.5,
      direction = "horizontal",
      title.position = "top"
    )
  )


## Look at just the 1-2 Combination ----

combos_w_score %>%
  filter(winning_combo == "1-2" | losing_combo == "1-2") %>%
  count(winning_combo, losing_combo)

win_33 <- combos_w_score %>%
  filter(
    winning_combo == "1-2",
    losing_combo != "1-2",
    num_matches > 1
  ) %>%
  group_by(combo = losing_combo) %>%
  summarize(
    n_won = n(),
    score_diff_won = sum(tot_score_diff),
    sweep_won = sum(num_matches == 2)
  )

loss_33 <- combos_w_score %>%
  filter(
    losing_combo == "1-2",
    winning_combo != "1-2",
    num_matches > 1
  ) %>%
  group_by(combo = winning_combo) %>%
  summarize(
    n_lost = n(),
    score_diff_loss = sum(tot_score_diff),
    sweep_loss = sum(num_matches == 2)
  ) %>%
  mutate(score_diff_loss = -score_diff_loss)

summary_33 <- win_33 %>%
  left_join(loss_33, by = "combo") %>%
  mutate(
    tot_games = n_won + n_lost,
    record = n_won / tot_games,
    tot_score_diff = score_diff_won + score_diff_loss,
    avg_score_diff = tot_score_diff / tot_games,
    tot_sweep = sweep_won + sweep_loss,
    pct_sweep = tot_sweep / tot_games
  ) %>%
  select(
    combo,
    tot_games,
    record,
    avg_score_diff,
    pct_sweep
  )





