# Training data to find which stats are important for probability of winning
# Used the 2021 tournament
example_results <- read.csv("2021-Results.csv")
dataset_2021 <- subset(dataset, YEAR == 2021)

# Merge the tournament results with team information for Team1 and Team2
matchup_stats <- merge(example_results, dataset_2021, by.x = "Team1", by.y = "TEAM", suffixes = c("", ".winner"))
matchup_stats <- merge(matchup_stats, dataset_2021, by.x = "Team2", by.y = "TEAM", suffixes = c("", ".loser"))

# Remove useless columns
matchup_stats <- subset(matchup_stats, select = -c(Year, YEAR, YEAR.loser, Round, ROUND, ROUND.loser))

# Found the stats of winners and losers of each matchup
numeric_columns <- sapply(matchup_stats, is.numeric)
matchup_stats <- matchup_stats[, numeric_columns]
stats_columns <- colnames(matchup_stats)[grep("\\.loser", colnames(matchup_stats))]

# Found the difference between the stats of winners and losers
for (stat in stats_columns) {
  stat_name <- gsub("\\.loser", "", stat)
  diff_column <- paste0(stat_name, "_diff")
  matchup_stats[[diff_column]] <- matchup_stats[[stat_name]] - matchup_stats[[stat]]
}

stat_diff <- matchup_stats[, grep("_diff", colnames(matchup_stats))]

# Found which of those differences are greater than 0
counts_all <- numeric()
counts_greater_than_0 <- numeric()

# Loop through each column
for (col in names(stat_diff)) {
  counts_all <- c(counts_all, length(stat_diff[[col]]))
  counts_greater_than_0 <- c(counts_greater_than_0, sum(stat_diff[[col]] > 0))
}

# Display the results
result <- data.frame(
  Column_Name = names(stat_diff),
  Count_All = counts_all,
  Count_Greater_Than_0 = counts_greater_than_0
)

# From there is where we found the coefficients, which correspond to the "weight"
# of each stat in the probability of winning

matchup_prob <- function(teamdata_matchup, n_sim = 10000, mean_diff = 57.049, sd_diff = 103.68) {
  coefficients <- c(0.716, 0.466, 0.7, 6.616, 0.716, 0.716, 0.583, 0, 0.533, 0.55, 0.566, 0.583, 0.516, 0.483, 0.483, 0.516, 0.566, 0.566, 0.433, 0.433, 0.583, 0.466, 0.566, 0.483, 0.566, 0.466, 0.516, 0.416, 0.483, 0.45, 0.583, 0.4, 0.366, 0.633, 0.483, 0.533, 0.516, 0.4, 0.683, 0.55, 0.45, 0.65, 0.533)

  power1 <- colSums(teamdata_matchup$list_data$team1_year1[, -c(1:5)] * coefficients)
  power2 <- colSums(teamdata_matchup$list_data$team2_year2[, -c(1:5)] * coefficients)

  power1 <- sum(power1)
  power2 <- sum(power2)

  simulated_diffs <- rnorm(n_sim, mean = mean_diff, sd = sd_diff)

  if (power1 - power2 > 0) {
    team1_wins <- sum(simulated_diffs >= (power1 - power2))
    team2_wins <- sum(simulated_diffs < (power1 - power2))
  } else {
    team1_wins <- sum(simulated_diffs > (power2 - power1))
    team2_wins <- sum(simulated_diffs <= (power2 - power1))
  }
  if (power1 - power2 > 0) {
  team1_prob <- team1_wins / n_sim
  team2_prob <- team2_wins / n_sim
  } else {
    team1_prob <- 1 - (team1_wins / n_sim)
    team2_prob <- 1 - (team2_wins / n_sim)
  }

  return(list(Team1_Power = power1, Team2_Power = power2, Team1_Probability = team1_prob, Team2_Probability = team2_prob))
}
