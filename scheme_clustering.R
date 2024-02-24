# Libraries
library(tidyverse)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(mclust)
# Read in data
nfl <- read.csv("nfl_data.csv")
cfb <- read.csv("cfb_data.csv")
# Find data distribution
cfb %>% 
  filter(!ThrowType %in% c("Hail Mary", "Flea Flicker", "Normal",
                           "Touch", "Sidearm", "NULL")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(level_1 = sum(ThrowType == "Level 1"),
            level_2 = sum(ThrowType == "Level 2"),
            level_3 = sum(ThrowType == "Level 3"),
            shovel = sum(ThrowType == "Shovel"),
            n_throw_types_obs = n()) %>% 
  ggplot(aes(x = level_1)) + 
  geom_histogram()
# Find target type distribution
cfb %>% 
  filter(!GeneralTargetType %in% c("NULL", "Other")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(screen = sum(GeneralTargetType == "Screen"),
            out = sum(GeneralTargetType == "Out"),
            corner = sum(GeneralTargetType == "Corner"),
            cross = sum(GeneralTargetType == "Cross"),
            flat = sum(GeneralTargetType == "Flat"),
            hook = sum(GeneralTargetType == "Hook"),
            vertical = sum(GeneralTargetType == "Vertical"),
            post = sum(GeneralTargetType == "Post"),
            shallow = sum(GeneralTargetType == "Shallow"),
            comeback = sum(GeneralTargetType == "Comeback")) %>% 
  ggplot(aes(x = shallow)) + 
  geom_histogram()
# Find run type distribution
runtypes <- unique(cfb$RunType)
map(runtypes, function(x) cfb %>% 
      group_by(Season, OffTeamName) %>% 
      filter(RunType == x, .preserve = T) %>% 
      summarise(RunType = n()) %>% 
      ggplot(aes(x = RunType)) + 
      geom_histogram() +
      labs(title = x))
# Get motion, pa, rpo, pass misdirection, short pass, throw type, blocking scheme frequencies (w/ cfb data)
cfb %>% 
  mutate(AnyMotion = as.numeric(AnyMotion),
         JetMotion = as.numeric(JetMotion),
         motion = case_when(AnyMotion == 1 |
                              JetMotion == 1 ~ 1,
                            is.na(AnyMotion) == T |
                              is.na(JetMotion) == T ~ NA,
                            .default = 0)) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(motion_pct = sum(motion) / n(),
            n_motion_obs = n()) %>% 
  ungroup() -> df1
cfb %>% 
  mutate(PlayAction = as.numeric(PlayAction)) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(play_action_pct = sum(PlayAction / n()),
            n_playaction_obs = n()) -> df2
cfb %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(rpo_pct = sum(RPO) / n(),
            n_rpo_obs = n()) -> df3
cfb %>% 
  mutate(pass_misdirection = case_when(
    QBatPassType %in% c("Shuffling", "Planted") ~ 0,
    QBatPassType %in% c("Moving", "Jump Pass") &
      Hurried == "0" ~ 1,
    .default = NA
  )) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(pass_misdirection_pct = sum(pass_misdirection) / n(),
            n_pass_misdirection_obs = n()) -> df4
short_passes <- c("Comeback", "Flat", "Hook", "Cut", "Screen", "Shallow")
cfb %>% 
  mutate(short_pass = case_when(
    GeneralTargetType == "NULL" ~ NA,
    GeneralTargetType %in% short_passes ~ 1,
    .default = 0
  )) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(short_pass_pct = sum(short_pass) / n(),
            n_short_pass_obs = n()) -> df5
cfb %>% 
  filter(!ThrowType %in% c("Hail Mary", "Flea Flicker", "Normal",
                           "Touch", "Sidearm", "Shovel", "NULL")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(level_1_pct = sum(ThrowType == "Level 1") / n(),
            level_2_pct = sum(ThrowType == "Level 2") / n(),
            level_3_pct = sum(ThrowType == "Level 3") / n(),
            n_throw_types_obs = n()) -> df6
cfb %>% 
  filter(!BlockingScheme %in% c("NULL", "No Video")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(pass_prot_pct = 
              sum(BlockingScheme == "Pass Protection") / n(),
            screen_pct = sum(BlockingScheme == "Screen") / n(),
            n_blocking_obs = n()) -> df7
# Combined frequencies
dfs <- list(df1, df2, df3, df4, df5, df6, df7)
# Merge dfs, create df vars, store into "cluster_data"
merge_columns <- c("Season", "OffTeamName")
Reduce(function(x, y) merge(x, y, by = merge_columns), 
       dfs) %>% 
  rename("Team" = "OffTeamName") %>% 
  rowwise() %>%
  mutate(row_name = paste(Season, Team, sep = " ")) %>%
  ungroup() %>%
  column_to_rownames(var = "row_name") -> cluster_data
# Repeat process w/ nfl data
nfl %>% 
  mutate(AnyMotion = as.numeric(AnyMotion),
         JetMotion = as.numeric(JetMotion),
         motion = case_when(AnyMotion == 1 |
                              JetMotion == 1 ~ 1,
                            is.na(AnyMotion) == T |
                              is.na(JetMotion) == T ~ NA,
                            .default = 0)) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(motion_pct = sum(motion) / n(),
            n_motion_obs = n()) %>% 
  ungroup() -> df1
nfl %>% 
  mutate(PlayAction = as.numeric(PlayAction)) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(play_action_pct = sum(PlayAction / n()),
            n_playaction_obs = n()) -> df2

nfl %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(rpo_pct = sum(RPO) / n(),
            n_rpo_obs = n()) -> df3
nfl %>% 
  mutate(pass_misdirection = case_when(
    QBatPassType %in% c("Shuffling", "Planted") ~ 0,
    QBatPassType %in% c("Moving", "Jump Pass") &
      Hurried == "0" ~ 1,
    .default = NA
  )) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(pass_misdirection_pct = sum(pass_misdirection) / n(),
            n_pass_misdirection_obs = n()) -> df4
short_passes <- c("Comeback", "Flat", "Hook", "Cut", "Screen", "Shallow")
nfl %>% 
  mutate(short_pass = case_when(
    GeneralTargetType == "NULL" ~ NA,
    GeneralTargetType %in% short_passes ~ 1,
    .default = 0
  )) %>% 
  drop_na() %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(short_pass_pct = sum(short_pass) / n(),
            n_short_pass_obs = n()) -> df5
nfl %>% 
  filter(!ThrowType %in% c("Hail Mary", "Flea Flicker", "Normal",
                           "Touch", "Sidearm", "Shovel", "NULL")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(level_1_pct = sum(ThrowType == "Level 1") / n(),
            level_2_pct = sum(ThrowType == "Level 2") / n(),
            level_3_pct = sum(ThrowType == "Level 3") / n(),
            n_throw_types_obs = n()) -> df6
nfl %>% 
  filter(!BlockingScheme %in% c("NULL", "No Video")) %>% 
  group_by(Season, OffTeamName) %>% 
  summarise(pass_prot_pct = 
              sum(BlockingScheme == "Pass Protection") / n(),
            screen_pct = sum(BlockingScheme == "Screen") / n(),
            n_blocking_obs = n()) -> df7
dfs <- list(df1, df2, df3, df4, df5, df6, df7)
merge_columns <- c("Season", "OffTeamName")
Reduce(function(x, y) merge(x, y, by = merge_columns), 
       dfs) %>% 
  rename("Team" = "OffTeamName") %>% 
  rowwise() %>%
  mutate(row_name = paste(Season, Team, sep = " ")) %>%
  ungroup() %>%
  column_to_rownames(var = "row_name") -> nfl_data
# Set predictors
predictors <- names(cluster_data %>% 
                      select(!contains("_obs"), -Season, -Team))
# Create sample filter
min_sample <- 50
obs_cols <- names(cluster_data)[grep("_obs", names(cluster_data))]
cluster_data %>% 
  bind_rows(nfl_data) %>% 
  filter(n_motion_obs >= 50,
         n_playaction_obs >= 50,
         n_rpo_obs >= 50,
         n_pass_misdirection_obs >= 50,
         n_short_pass_obs >= 50,
         n_throw_types_obs >= 50,
         n_blocking_obs >= 50) %>%
  #mutate(across(.cols = predictors, ~ntile(., 100))) %>% 
  select(all_of(predictors)) -> model_data
# Correlation plot
model_data %>% 
  scale() %>% 
  cor() %>% 
  ggcorrplot()
# PCA
model_data %>% 
  prcomp(center = T, scale. = T) %>% 
  .[["x"]] -> pca_data
# Scaled data
model_data %>% 
  scale() -> scaled_data
# Optimal clusters w/ kmeans
fviz_nbclust(x = scaled_data,
             FUNcluster = stats::kmeans, 
             method = "gap_stat", # silhouette or gap_stat
             k.max = 10, 
             verbose = T)
# Optimal clusters w/ hcut
fviz_nbclust(x = scaled_data,
             FUNcluster = hcut, 
             method = "gap_stat", # silhouette or gap_stat
             k.max = 10, 
             verbose = T)
# Optimal clusters w/ kmeans + PCA
fviz_nbclust(x = pca_data,
             FUNcluster = stats::kmeans, 
             method = "gap_stat", # silhouette or gap_stat
             k.max = 10, 
             verbose = T)
# Set a number of clusters
G <- 3
# Create kmeans and gmm
kmeans <- kmeans(scaled_data, G)
gmm <- Mclust(scaled_data, G)
gmm_clusters <- data.frame(model_data, 
                           cluster = gmm[["classification"]], 
                           uncertainty = gmm[["uncertainty"]])
kmeans_clusters <- data.frame(model_data,
                              cluster = kmeans[["cluster"]])
# Visualize cluster_data
cluster_data %>% 
  ggplot(aes(pass_prot_pct)) +
  geom_density()
# Visualize cluster results
fviz_pca(model_data %>% 
           prcomp(center = T, scale. = T))
# Chronback alpha function (courtesy of: https://blogs.fangraphs.com/a-new-way-to-look-at-sample-size/)
cb_alpha <- function(df, K = ncol(df)) {
  # Initialize empty vectors
  wide.vector <- c()
  tall.vector <- c()
  
  # Loop over columns to calculate variances
  for (j in 1:ncol(df[,1:K])) {
    tall.vector <- c(tall.vector, var(df[,j]))
  }
  
  # Loop over rows to calculate sums
  for (i in 1:nrow(df[,1:K])) {
    wide.vector <- c(wide.vector, sum(data.matrix(df[,1:K])[i,]))
  }
  
  # Calculate alpha coefficient
  alpha <- K / (K - 1) * (1 - sum(tall.vector) / var(wide.vector))
  
  # Calculate mean and standard deviation
  X_bar <- mean(wide.vector)
  sd <- sqrt(var(wide.vector))
  
  # Return results as a list
  return(list(alpha = alpha, 
              X_bar = X_bar, 
              K = K, 
              sd = sd, 
              within = tall.vector, 
              between = wide.vector))
}

# Obtain minimum sample size from cluster_data
min_samp_size <- cluster_data %>% 
  arrange(n_motion_obs) %>% 
  select(n_motion_obs) %>% 
  unlist()

# Generate sample list
sample.list <- seq(from = min_samp_size %>% min(), 
                   to = min_samp_size %>% 
                     quantile(probs = seq(0.1, 1, 0.1)) %>% 
                     .[[1]] %>% 
                     ceiling(), 
                   by = 1)

# Define motion_parse function
motion_parse <- function(df, K, random = TRUE) {
  if (random) {
    return(stat_random(df, K)$motion_pct[1:K])
  } else {
    return(df$motion_pct[1:K])
  }
}

# Initialize list of parsing functions
FUN.list <- list(list(motion_parse, 'motion_pct'))

# Loop over sample list and parsing functions
for (i in sample.list) {
  for (j in FUN.list) {
    player.year.matrix <- matrix_parse(df.prep, i, j[[1]], Random) 
    # get rid of the matrix_parse i
    alpha.list <- FG_alpha(player.year.matrix)
    out.list <- data.frame(denom = i, 
                           type = 'Snap', 
                           stat = j[[2]],
                           alpha = alpha.list$alpha, 
                           sample_mean = alpha.list$X_bar, 
                           sample_sd = alpha.list$sd)
    out.df <- rbind(out.df, out.list)
  }
}

# Calculate CB_alpha for filtered cluster_data
cb_alpha(cluster_data %>% 
           filter(n_motion_obs > min_samp_size[1]) %>% 
           select(!contains("_obs"), -Season, -Team))