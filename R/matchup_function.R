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
