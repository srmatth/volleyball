#### 02 CLUSTER PLAYERS ----

## This Rscript will be where I use k-means clustering to assign
## groups to the different players. These groups will be used in
## future analyses to determine which kinds of players work
## well together

## This code follows the structure in this blog post
## https://juliasilge.com/blog/kmeans-employment/

## Setup ----

library(dplyr)
library(readr)
library(tidymodels)
library(purrr)
library(ggplot2)
library(GGally)

tidymodels_prefer()

player_info <- read_csv("data/player_info.csv")

set.seed(16)

## Preprocess the data ----

mod_dat <- player_info %>%
  select(-name, -birthdate) %>%
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

## There seems to be a pretty good elbow at 4,
## so let's use that many clusters

## Final clustering ----

clusters_final <- kmeans(mod_dat, centers = 4)
player_clusters <- augment(clusters_final, player_info)

write_csv(player_clusters, "data/player_clusters.csv")

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

player_clusters %>%
  select(-name, -birthdate) %>%
  magrittr::set_colnames(
    snakecase::to_title_case(
      colnames(.)
    )
  ) %>%
  ggpairs(columns = 1:6, aes(color = Cluster)) +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    strip.text.y = element_text(angle = 0)
  )


