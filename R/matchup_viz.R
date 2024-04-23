#' Matchup Vis
#'
#' This function takes two teams and compares four important statistics
#' (Efficiency Field Goals, Free Throw Rate, Offensive Rebounds, and Turnover
#' Rate) to give insights on which team is better. The output of the function is
#' a bar chart showing side by side percentages for each team's metrics.
#'
#' @importFrom "ggplot2" "ggplot" "geom_bar" "aes" "theme_minimal" "theme" "labs" "element_text"
#'
#' @param teamdata_matchup A list of two dataframes, each containing the statistics for that team and year
#'
#' @return The bar chart described above.
#'
#' @export
matchup_chart <- function(teamdata_matchup) {

  team1_statistics <- teamdata_matchup$list_data$team1_year1[, c(16, 18, 20, 22)]
  team2_statistics <- teamdata_matchup$list_data$team2_year2[, c(16, 18, 20, 22)]

  #Creates a dataframe storing the EFG, FTR, TOV, and OREB statistics, for the
  #two teams picked with the team_select function.
  combined_stats <- rbind(data.frame(Team = "Team1", team1_statistics),
                          data.frame(Team = "Team2", team2_statistics))

  combined_stats_long <- tidyr::gather(combined_stats, "Statistic", 'Value', 'EFG.', 'FTR',
                                       'TOV.', 'OREB.')
  #Creates a side-by-side bar chart with blue and red columns comparing
  #statistics of interest for two teams.
  ggplot(combined_stats_long, aes(x = Statistic, y = Value, fill = Team)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Comparison of Team Statistics",
         x = "Statistic",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
