#### 02a EXPLORE CLUSTERS ----

## Setup ----

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(patchwork)
library(extrafont)
loadfonts()

player_clusters <- read_csv("data/player_clusters.csv")


## Exploration ----

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

