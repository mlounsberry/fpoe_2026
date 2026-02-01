# We will use this script to create the individual punter 

library(tidyverse)
library(nflreadr)
library(tidymodels)
library(lme4)

pbp <- nflreadr::load_pbp(1999:2025)

# Start by building expected return yards

returns <- pbp %>%
  filter(play_type == 'punt',
         !is.na(punt_returner_player_id),
         punt_fair_catch == 0)

# Filtering out any returns above 75th percentile and below 25th percentile

q1_return <- quantile(returns$return_yards, c(0.25))
q3_return <- quantile(returns$return_yards, c(0.75))

iqr_return <- q3_return - q1_return

lower <- q1_return - 1.5 * iqr_return
upper <- q3_return + 1.5 * iqr_return

returns <- returns %>%
  filter(return_yards > lower & return_yards < upper)

# Mixed effects

expected_return_fit <- lmer(
  return_yards ~ kick_distance + yardline_100 +
    (1 | punter_player_id) +
    (1 | punt_returner_player_id),
  data = returns,
  REML = FALSE)

returns <- returns %>%
  mutate(
    expected_return_yards = predict(expected_return_fit, returns, allow.new.levels = TRUE),
    expected_return_yards = case_when(
      punt_fair_catch == 1 ~ 0,
      is.na(punt_returner_player_id) ~ 0,
      TRUE ~ expected_return_yards))

rmse <- sqrt(mean((returns$return_yards - returns$expected_return_yards)^2))
rmse

# xgboost

# xgb_spec <- boost_tree(
#   trees = 500,
#   tree_depth = 6,
#   learn_rate = 0.05,
#   loss_reduction = 0,
#   sample_size = 1,
#   mtry = 2) %>%
#   set_engine("xgboost") %>%
#   set_mode("regression")
# 
# xgb_recipe <- recipe(
#   return_yards ~ kick_distance + yardline_100,
#   data = returns)
# 
# xgb_workflow <- workflow() %>%
#   add_model(xgb_spec) %>%
#   add_recipe(xgb_recipe)
# 
# exp_return_yds <- fit(xgb_workflow, data = returns)
# 
# returns <- returns %>%
#   mutate(
#     expected_return_yards = predict(exp_return_yds, returns)$.pred)







punts <- pbp %>%
  filter(!is.na(play_type)) %>%
  group_by(game_id) %>% 
  mutate(half_change = ifelse(lead(game_half) != game_half, 1, 0)) %>%
  ungroup() %>%
  filter(half_change == 0) %>%
  group_by(game_id) %>%
  mutate(next_yardline = lead(yardline_100)) %>%
  ungroup() %>%
  filter(play_type == 'punt',
         punt_blocked == 0,
         kick_distance >= 20)








pbp %>%
  filter(play_type == 'punt') %>%
  mutate(next_yardline_100 = 100 - (yardline_100 - kick_distance + xYards)

# Modeling

