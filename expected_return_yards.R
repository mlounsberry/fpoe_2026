library(dplyr)
library(lme4)

# ------------------------------------------------------------
# 1. Train / test split by season
# ------------------------------------------------------------

latest_season <- max(pbp$season, na.rm = TRUE)

pbp <- pbp %>%
  mutate(split = if_else(season < latest_season, "train", "test"))

# ------------------------------------------------------------
# 2. Stage 1 dataset: all punts (train + test)
# ------------------------------------------------------------

returns_stage1 <- pbp %>%
  filter(play_type == "punt") %>%
  mutate(
    punter_player_id = factor(punter_player_id),
    punt_returner_player_id = factor(punt_returner_player_id),
    returned = if_else(
      punt_fair_catch == 0 & !is.na(punt_returner_player_id), 1L, 0L))

# ------------------------------------------------------------
# 3. Stage 1 model: probability of a return
# ------------------------------------------------------------

return_prob_fit <- glmer(
  returned ~ kick_distance + yardline_100 + (1 | punter_player_id),
  data = returns_stage1 %>% filter(split == "train"),
  family = binomial(link = "logit"))

# ------------------------------------------------------------
# 4. Stage 2 dataset: only actual returns
# ------------------------------------------------------------

returns_stage2 <- returns_stage1 %>%
  filter(split == "train", returned == 1)

# ------------------------------------------------------------
# 5. Stage 2 model: expected yards GIVEN a return
# ------------------------------------------------------------

return_yards_fit <- lmer(
  return_yards ~ kick_distance + yardline_100 +
    (1 | punter_player_id) +
    (1 | punt_returner_player_id),
  data = returns_stage2,
  REML = FALSE)

# ------------------------------------------------------------
# 6. Apply models to TEST season
# ------------------------------------------------------------

returns_test <- returns_stage1 %>%
  filter(split == "test") %>%
  select(
    game_id,
    play_id,
    kick_distance,
    yardline_100,
    returned,
    punter_player_id,
    punt_returner_player_id,
    return_yards) %>%
  mutate(
    # Probability of a return
    prob_return = predict(
      return_prob_fit,
      newdata = .,
      type = "response",
      allow.new.levels = TRUE),
    # Expected yards conditional on a return
    expected_yards_given_return = if_else(
      returned == 1,
      predict(
        return_yards_fit,
        newdata = .,
        allow.new.levels = TRUE), 0),
    
    # Final two-stage expectation
    expected_return_yards = prob_return * expected_yards_given_return
  )

# ------------------------------------------------------------
# 7. RMSE on TEST set only
# ------------------------------------------------------------

rmse_two_stage_test <- sqrt(
  mean((returns_test$return_yards - returns_test$expected_return_yards)^2, na.rm = TRUE))

rmse_two_stage_test

# Saving models