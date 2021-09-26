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

matches_with_group <- read_csv("data/matches_with_group.csv")

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


## Look at just the 3-3 Combination ----

combos_w_score %>%
  filter(winning_combo == "3-3" | losing_combo == "3-3") %>%
  count(winning_combo, losing_combo)

win_33 <- combos_w_score %>%
  filter(
    winning_combo == "3-3",
    losing_combo != "3-3",
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
    losing_combo == "3-3",
    winning_combo != "3-3",
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


