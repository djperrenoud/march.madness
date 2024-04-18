#' Match-up Select
#'
#' Creates a custom S3 object representing a list of data
#' for two specific teams and years.
#'
#' @param team1 A character string representing the  first team name.
#'
#' @param year1 An integer representing the first year.
#'
#' @param team2 A character string representing the second team name.
#'
#' @param year2 An integer representing the second year.
#'
#'
#' @return A custom S3 object of class 'teamdata_matchup' containing a
#' list of the subsetted data.
#'
#' @examples selected_matchup <- matchup_select("Baylor", 2023, "Purdue", 2023)
#' print(selected_matchup)
#'
#' @export
matchup_select <- function(team1, year1, team2, year2) {
  # Load the dataset
  dataset <- read.csv("initial_clean.csv")

  # Subset the dataset based on the provided team and year
  # NOTE: We need to flag them somehow if they type the team name wrong
  subset_data1 <- dataset[as.character(dataset$TEAM) == team1 & dataset$YEAR == year1, ]

  subset_data2 <- dataset[as.character(dataset$TEAM) == team2 & dataset$YEAR == year2, ]

  # Return a list with two elements: subsetted data for team1 and year1, and subsetted data for team2 and year2
  return(new_teamdata_matchup(list(team1_year1 = subset_data1, team2_year2 = subset_data2)))
}
