set.seed(1234)
# Load in libraries
library(tidyverse)
library(xgboost)
library(vip)
library(gt)
library(gtExtras)
# Create a vector for the specific QBs
qbs <- c("Jayden Daniels", "Drake Maye", "Caleb Williams", "Bo Nix", "Michael Penix Jr.", "J.J. McCarthy")
# Read in the data
nfl_data <- read_csv("nfl_data.csv")
cfb <- read_csv("cfb_data.csv")
cfb_clusers <- read_csv("best_perf_clusters_cfb.csv") %>%
  rename("team" = ...1) %>%
  select(team, cluster)
# Create a nfl_success df
nfl_success <- nfl_data %>%
  mutate( # Create "points" variable based on passer/rusher pts
    points = ifelse(PasserPoints == "NULL", RusherPoints, PasserPoints),
    points = as.numeric(points)
  ) %>%
  filter(points != "NULL") %>% # Remove null observations
  select(Season, Player, UniversalPlayerId, points) %>% 
  group_by(UniversalPlayerId, Season, Player) %>%
  summarise( # Get number of plays and points
    plays = n(),
    total_points = sum(points),
    season = 1
  ) %>%
  group_by(Player) %>%
  mutate(seasons = cumsum(season)) %>% # Get player season num
  ungroup() %>% # Remove constraints
  select(UniversalPlayerId, Player, Season, total_points, plays, seasons)
# Aggregate players cfb stats
cfb_new <- cfb %>%
  mutate(team = paste(Season, OffTeamName)) %>% # For joining cfb_clusters
  left_join(cfb_clusters, by = c("team")) %>%
  rename(scheme = cluster) %>%
  mutate( # Make columns numeric
   Player = ifelse(Player == "Gardner Minshew II", "Gardner Minshew", Player),
   points = ifelse(PasserPoints == "NULL", RusherPoints, PasserPoints),
   points = as.numeric(points),
   points = ifelse(is.na(points), 0, points),
   AnyMotion = as.numeric(AnyMotion),
   JetMotion = as.numeric(JetMotion),
   motion = ifelse(AnyMotion == 1 | JetMotion == 1, 1, 0),
   motion = ifelse(is.na(motion), 0, motion),
   ThrowDepth = as.numeric(ThrowDepth),
   ThrowDepth = ifelse(is.na(ThrowDepth), 0, ThrowDepth),
   AirYards = as.numeric(AirYards),
   AirYards = ifelse(is.na(AirYards), 0, AirYards),
   # Create short/long pass criteria vars
   short_pass = ifelse(ThrowDepth < 10, 1, 0),
   long_pass = ifelse(ThrowType %in% c("Level 2", "Level 3"), 1, 0),
   pass_protection = ifelse(BlockingScheme == "Pass Protection", 1, 0),
   PassingYards = as.numeric(PassingYards),
   PassingYards = ifelse(is.na(PassingYards), 0, PassingYards),
   PassingTouchdown = as.numeric(PassingTouchdown),
   PassingTouchdown = ifelse(is.na(PassingTouchdown), 0, PassingTouchdown),
   RushingYards = as.numeric(RushingYards),
   RushingYards = ifelse(is.na(RushingYards), 0, RushingYards),
   RushingTouchdowns = as.numeric(RushingTouchdowns),
   RushingTouchdowns = ifelse(is.na(RushingTouchdowns), 0, RushingTouchdowns),
   Hurried = as.numeric(Hurried),
   Hurried = ifelse(is.na(Hurried), 0, Hurried),
   Hit = as.numeric(Hit),
   Hit = ifelse(is.na(Hit), 0, Hit),
   Sacked = as.numeric(Sacked),
   Sacked = ifelse(is.na(Sacked), 0, Sacked),
   PassingAttempt = as.numeric(PassingAttempt),
   PassingAttempt = ifelse(is.na(PassingAttempt), 0, PassingAttempt),
   Completion = as.numeric(Completion),
   Completion = ifelse(is.na(Completion), 0, Completion),
   OnTargetThrow = as.numeric(OnTargetThrow),
   OnTargetThrow = ifelse(is.na(OnTargetThrow), 0, OnTargetThrow),
   Interception = as.numeric(Interception),
   Interception = ifelse(is.na(Interception), 0, Interception),
   DroppedInterceptions = as.numeric(DroppedInterceptions),
   DroppedInterceptions = ifelse(is.na(DroppedInterceptions), 0, DroppedInterceptions),
   Carries = as.numeric(Carries),
   Carries = ifelse(is.na(Carries), 0, Carries),
   RushingFumbles = as.numeric(RushingFumbles),
   RushingFumbles = ifelse(is.na(RushingFumbles), 0, RushingFumbles),
   EPA = as.numeric(EPA),
   EPA = ifelse(is.na(EPA), 0, EPA)
      ) %>%
  na.omit() # Remove all NAs

# Get a vector for all the nfl_ids
nfl_ids <- as.vector(unique(nfl_success$UniversalPlayerId))
# Create model df
scheme_model_data <- cfb_new %>%
  filter(!UniversalPlayerId %in% nfl_ids & !Player %in% qbs) %>% # Filter observations w/ the NFL (& 6 QBs) QBs in the df
  select(scheme, PlayAction, motion, RPO, short_pass, long_pass, pass_protection, OL) %>%
  mutate(scheme = scheme - 1) # Change scheme variable for classification

# Create train/test data sets
ind <- sample(1:nrow(scheme_model_data), .75 * nrow(scheme_model_data))
train <- scheme_model_data %>%
  dplyr::slice(ind) %>%
  select(-scheme) %>%
  as.matrix() %>%
  xgb.DMatrix(label = scheme_model_data$scheme[ind])
test <- scheme_model_data %>%
  dplyr::slice(-ind) %>%
  select(-scheme) %>%
  as.matrix() %>%
  xgb.DMatrix(label = scheme_model_data$scheme[-ind])

# Build scheme classification model
scheme_class <- xgb.train(
  params = list(
    num_class = 3,
    objective = "multi:softmax",
    eval_metric = "merror"
  ),
  data = train,
  nrounds = 500,
  early_stopping_rounds = 100,
  watchlist = list(
    train = train,
    test = test
  )
)

# Predict for both nfl qbs cfb schemes
nfl_qbs <- cfb_new %>%
  filter(UniversalPlayerId %in% nfl_ids)
predicting_nfl_cfb_schemes <- nfl_qbs %>%
  select(scheme, PlayAction, motion, RPO, short_pass, long_pass, pass_protection, OL) %>%
  mutate(scheme = scheme - 1)
nfl_scheme_matrix <- predicting_nfl_cfb_schemes %>%
  select(-scheme) %>%
  as.matrix() %>%
  xgb.DMatrix(label = predicting_nfl_cfb_schemes$scheme)
spec_qbs <- cfb_new %>%
  filter(Player %in% qbs)
# Predict for 6 qbs cfb schemes
predicting_cfb_schemes <- spec_qbs %>%
  select(scheme, PlayAction, motion, RPO, short_pass, long_pass, pass_protection, OL) %>%
  mutate(scheme = scheme - 1)
cfb_scheme_matrix <- predicting_cfb_schemes %>%
  select(-scheme) %>%
  as.matrix() %>%
  xgb.DMatrix(label = predicting_cfb_schemes$scheme)
# Make predictions
nfl_scheme_cfb_preds <- predict(scheme_class, nfl_scheme_matrix)
spec_scheme_cfb_preds <- predict(scheme_class, cfb_scheme_matrix)
# Aggregate scheme data
nfl_df <- cbind(nfl_qbs, scheme_num = nfl_scheme_cfb_preds + 1)
spec_df <- cbind(spec_qbs, scheme_num = spec_scheme_cfb_preds + 1)
# Aggregate nfl qb's college data based on playerid, player, and scheme
cfb_agg <- nfl_df %>%
  group_by(UniversalPlayerId, Player, scheme_num) %>%
  summarise(
    plays_college = n(),
    passing_att = sum(PassingAttempt),
    p_ypp = sum(PassingYards)/passing_att,
    passing_tds = sum(PassingTouchdown),
    ad_pp = sum(ThrowDepth)/passing_att,
    ay_pp = sum(AirYards)/passing_att, 
    cp = sum(Completion)/passing_att,
    ott_pct = sum(OnTargetThrow)/passing_att,
    ints = sum(as.numeric(Interception)),
    dropped_ints = sum(DroppedInterceptions),
    cfb_ppp = sum(points)/plays_college
  ) %>%
  ungroup() %>%
  select(-c(plays_college, passing_att))
joined <- nfl_success %>%
  left_join(cfb_agg, by = c("UniversalPlayerId", "Player")) %>%
  na.omit() %>%
  mutate(nfl_ppp = total_points/plays) # Create response variable for model

# Create prediction model
# Create train/test sets
ind2 <- sample(1:nrow(joined), .75 * nrow(joined))
train2 <- joined %>%
  dplyr::slice(ind2) %>%
  select(-c(UniversalPlayerId, Player, Season, total_points, plays, nfl_ppp)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = joined$nfl_ppp[ind2])
test2 <- joined %>%
  dplyr::slice(-ind2) %>%
  select(-c(UniversalPlayerId, Player, Season, total_points, plays, nfl_ppp)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = joined$nfl_ppp[-ind2])

# Create points_model
points_model <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    eval_metric = "rmse"
  ),
  data = train2,
  nrounds = 500,
  early_stopping_rounds = 100,
  watchlist = list(
    train = train2,
    test = test2
  )
)

# Aggregate 6 QBs data
qbs_agg <- spec_df %>%
  group_by(UniversalPlayerId, Player, scheme_num) %>%
  summarise(
    plays_college = n(),
    passing_att = sum(PassingAttempt),
    p_ypp = sum(PassingYards)/passing_att,
    passing_tds = sum(PassingTouchdown),
    ad_pp = sum(ThrowDepth)/passing_att,
    ay_pp = sum(AirYards)/passing_att,
    cp = sum(Completion)/passing_att,
    ott_pct = sum(OnTargetThrow)/passing_att,
    ints = sum(as.numeric(Interception)),
    dropped_ints = sum(DroppedInterceptions),
    cfb_ppp = sum(points)/plays_college
  ) %>%
  ungroup()
# Repeat each observation 3 times
qbs_agg <- qbs_agg[rep(row.names(qbs_agg), each = 3), ] 
# Create a data set to make 3 year predictions
qbs_agg <- qbs_agg %>%
  mutate(new_col = 1) %>% # Create a column that represents season
  group_by(UniversalPlayerId, Player, scheme_num) %>% # Group by id, player, scheme
  mutate(seasons = cumsum(new_col)) %>% # Create a column that represents year in the league
  ungroup() %>%
  select(Player, seasons, scheme_num, p_ypp, passing_tds, ad_pp, ay_pp, cp, ott_pct, ints, dropped_ints, cfb_ppp)
# Create matrix to make predictions
special_pred_matrix <- qbs_agg %>%
  select(-Player) %>%
  as.matrix() %>%
  xgb.DMatrix()
# Make and aggregate predictions for visuals
preds <- predict(points_model, special_pred_matrix)
final_df <- cbind(qbs_agg, preds)
final_df$seasons <- as.character(final_df$seasons)
# Reshape the data to wide format
data_wide <- pivot_wider(final_df, names_from = seasons, values_from = c(preds))
final_df <- data_wide