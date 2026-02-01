library(dplyr)
library(lme4)

# First we're going to start by creating a dataframe of punts for us to
# train our models on.

latest_season <- max(pbp$season, na.rm = TRUE)

pbp <- pbp %>%
  mutate(split = if_else(season < latest_season, "train", "test"))

returns_stage1 <- pbp %>%
  filter(play_type == "punt") %>%
  mutate(
    
    # We're turning the punter_player_id and punt_returner_player_id into factors
    # to be used in the mixed effect models below
    
    punter_player_id = factor(punter_player_id),
    punt_returner_player_id = factor(punt_returner_player_id),
    
    # Identifying what punts are actually returned
    
    returned = ifelse(punt_fair_catch == 0 & !is.na(punt_returner_player_id), 1, 0))

    # Question: Why can't we just say if return_yards == 0 there was no return?
    # Answer: nflreadr has the yards for all punts set to 0, so if there was a return
    # for 0 yards and a punt wasn't returned at all they are both 0.


# Fitting a model to give the probability of a return based on how far the kick
# was and where it took place from. We're also controlling for who the punter is
# since how they punt (spin, hang time, etc.) can affect if it's returned.

return_prob_fit <- glmer(
  returned ~ kick_distance + yardline_100 +
    (1 | punter_player_id),
  data = returns_stage1,
  family = binomial(link = "logit"))

# Now we're creating our second model, a model that says given there is a return
# how many return yards should be expected?

# Creating a dataframe of just returns

returns_stage2 <- returns_stage1 %>%
  filter(returned == 1)

# Training our model to predict the amount of return yards on a play given
# the kick_distance, the yardline_100 the punt takes place from, and controls
# for the punter is and who the returner is.

return_yards_fit <- lmer(
  return_yards ~ kick_distance + yardline_100 +
    (1 | punter_player_id) +
    (1 | punt_returner_player_id),
  data = returns_stage2,
  REML = FALSE)

# Creating new dataframe with both our predictions included

returns <- returns_stage1 %>%
  select(game_id,
         play_id,
         desc,
         kick_distance,
         yardline_100,
         returned,
         punter_player_id,
         punt_returner_player_id,
         return_yards)

# Creating a dataframe of just returned punts to create expected yards
# given a return

exp_yds_df <- returns %>%
  filter(returned == 1) %>%
  mutate(expected_yards_given_return = predict(
    return_yards_fit,
    newdata = .,
    allow.new.levels = TRUE))

# Adding the probability of a return to our returns df and then joining on
# the expected yards given there was a return

returns <- returns %>%
  mutate(
    prob_return = predict(
      return_prob_fit,
      newdata = .,
      type = "response",
      allow.new.levels = TRUE)) %>%
  left_join(exp_yds_df %>% select(game_id, play_id, expected_yards_given_return),
            by = c("game_id", "play_id")) %>%
  mutate(expected_yards_given_return = coalesce(expected_yards_given_return, 0),
         expected_return_yards = prob_return * expected_yards_given_return)
  
# Calculating rmse

rmse_two_stage <- sqrt(
  mean(
    (returns$return_yards -
       returns$expected_return_yards)^2,
    na.rm = TRUE
  )
)

rmse_two_stage

# 8.052479







