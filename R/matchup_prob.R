#' Matchup Probability Function
#'.
#' Gives two selectd teams a "power" rating based on their statistics and
#' uses this and a normal distribution to simulate
#' the probability of each team winning
#'
#' @param teamdata_matchup A custom S3 object of class 'teamdata_matchup'
#' containing the subsetted data of two teams in the selected years.
#'
#' @return The "power" index of each team as well as the probability of each
#' team winning. Also a weighted coin flip if you want to select a winner from
#' the probabilities.
#'
#' @examples selected_matchup <- matchup_select("Purdue", 2024, "Utah St.", 2024)
#' matchup_prob(selected_matchup)
#'
#' @export
matchup_prob <- function(teamdata_matchup, n_sim = 10000, mean_diff = 57.049, sd_diff = 103.68) {
  # Coefficients found from training_data.R
  coefficients <- c(0.716, 0.466, 0.7, 6.616, 0.716, 0.716, 0.583, 0, 0.533, 0.55, 0.566, 0.583, 0.516, 0.483, 0.483, 0.516, 0.566, 0.566, 0.433, 0.433, 0.583, 0.466, 0.566, 0.483, 0.566, 0.466, 0.516, 0.416, 0.483, 0.45, 0.583, 0.4, 0.366, 0.633, 0.483, 0.533, 0.516, 0.4, 0.683, 0.55, 0.45, 0.65, 0.533)

  # Calculate the power index for each team
  power1 <- colSums(teamdata_matchup$list_data$team1_year1[, -c(1:5)] * coefficients)
  power2 <- colSums(teamdata_matchup$list_data$team2_year2[, -c(1:5)] * coefficients)

  power1 <- sum(power1)
  power2 <- sum(power2)

  # Simulate the difference in power index
  simulated_diffs <- rnorm(n_sim, mean = mean_diff, sd = sd_diff)

  # Calculate the probability of each team winning
  if (power1 - power2 > 0) {
    team1_wins <- sum(simulated_diffs <= (power1 - power2))
    team2_wins <- sum(simulated_diffs > (power1 - power2))
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

  # Weighted coin flip to select a winner
  if (runif(1) < team1_prob) {
    prediction <- "Team 1"
  } else {
    prediction <- "Team 2"
  }

  # Format the results
  formatted_results <- list(
    Team1_Power_Index = round(power1, 2),
    Team2_Power_Index = round(power2, 2),
    Probability_of_Team1_Win = paste0(round(team1_prob * 100, 2), "%"),
    Probability_of_Team2_Win = paste0(round(team2_prob * 100, 2), "%"),
    Prediction = prediction
  )

  return(formatted_results)
}
