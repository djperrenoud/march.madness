#' Matchup Probability Function
#'
#' Computes power ratings for two selected teams based on their statistics,
#' and simulates the probability of each team winning using a normal distribution.
#'
#' @importFrom stats rnorm runif setNames
#'
#' @param teamdata_matchup An S3 object of class 'teamdata_matchup' containing
#' the subsetted data of two teams in the selected years.
#'
#' @param n_sim The number of simulations to run. Default is 10,000.
#'
#' @param mean_diff The mean of the normal distribution. Default is 57.049.
#'
#' @param sd_diff The standard deviation of the normal distribution. Default is 103.68.
#'
#' @return A named list containing the power indices of each team,
#' the probability of each team winning, and a predicted winner based on a weighted coin flip.
#'
#' @export
matchup_prob <- function(teamdata_matchup, n_sim = 10000, mean_diff = 57.049, sd_diff = 103.68) {

  # Ensure the input is a valid teamdata_matchup object
  if (!inherits(teamdata_matchup, "teamdata_matchup")) {
    stop("Input must be of class 'teamdata_matchup'.")
  }

  # Extract team data
  team1_data <- teamdata_matchup$list_data$team1_year1
  team2_data <- teamdata_matchup$list_data$team2_year2

  # Ensure both teams have data
  if (nrow(team1_data) == 0 || nrow(team2_data) == 0) {
    stop("One or both teams do not have data.")
  }

  # Define weights for each statistic (formerly `coefficients`)
  stat_weights <- c(0.716, 0.466, 0.7, 6.616, 0.716, 0.716, 0.583, 0, 0.533,
                    0.55, 0.566, 0.583, 0.516, 0.483, 0.483, 0.516, 0.566,
                    0.566, 0.433, 0.433, 0.583, 0.466, 0.566, 0.483, 0.566,
                    0.466, 0.516, 0.416, 0.483, 0.45, 0.583, 0.4, 0.366, 0.633,
                    0.483, 0.533, 0.516, 0.4, 0.683, 0.55, 0.45, 0.65, 0.533)

  # Select only numeric columns
  numeric_cols <- sapply(team1_data, is.numeric)
  team1_stats <- team1_data[, numeric_cols, drop = FALSE]
  team2_stats <- team2_data[, numeric_cols, drop = FALSE]

  # Remove unnecessary numeric columns (e.g., YEAR, CONF.ID, etc.)
  if (ncol(team1_stats) > length(stat_weights)) {
    team1_stats <- team1_stats[, 1:length(stat_weights), drop = FALSE]
    team2_stats <- team2_stats[, 1:length(stat_weights), drop = FALSE]
  }

  # Ensure column count matches `stat_weights`
  if (ncol(team1_stats) != length(stat_weights)) {
    stop("Mismatch between number of numeric columns and the number of weights.")
  }

  # Compute power index for each team
  power1 <- sum(colSums(team1_stats * stat_weights))
  power2 <- sum(colSums(team2_stats * stat_weights))

  # Simulate power differences
  simulated_diffs <- rnorm(n_sim, mean = mean_diff, sd = sd_diff)

  # Compute probabilities
  diff <- power1 - power2
  team1_wins <- sum(simulated_diffs <= diff)
  team2_wins <- sum(simulated_diffs > diff)

  team1_prob <- team1_wins / n_sim
  team2_prob <- team2_wins / n_sim

  # Extract team names and years
  team1_name <- team1_data$TEAM[1]
  team1_year <- team1_data$YEAR[1]
  team2_name <- team2_data$TEAM[1]
  team2_year <- team2_data$YEAR[1]

  # Weighted coin flip to predict winner
  predicted_winner <- ifelse(runif(1) < team1_prob,
                             paste(team1_name, team1_year),
                             paste(team2_name, team2_year))

  # Format the output
  results <- setNames(
    list(
      round(power1, 2),
      round(power2, 2),
      paste0(round(team1_prob * 100, 2), "%"),
      paste0(round(team2_prob * 100, 2), "%"),
      predicted_winner
    ),
    c(
      paste(team1_name, team1_year, "Power Index"),
      paste(team2_name, team2_year, "Power Index"),
      paste("Probability of", team1_name, team1_year, "Win"),
      paste("Probability of", team2_name, team2_year, "Win"),
      "Prediction"
    )
  )

  return(results)
}
