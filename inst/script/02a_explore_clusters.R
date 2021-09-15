#### 02a EXPLORE CLUSTERS ----

## Setup ----

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)

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
  )


