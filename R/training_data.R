# Training data to find which stats are important for probability of winning
# Used the 2021 tournament results
# This is specifically for matchup_function.R
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
