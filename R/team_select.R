#' Team Select
#'
#' Creates a custom S3 object representing a subset of data
#' for a specific team and year.
#'
#' @param team A character string representing the team name.
#'
#' @param year An integer representing the year.

#' @return A custom S3 object of class 'teamdata' containing the subsetted data.
#'
#' @examples selected_team <- team_select("Baylor", 2024)
#' print(selected_team)
#'
#' @export
team_select <- function(team, year) {
  # Load the dataset
  dataset <- read.csv("data-raw/dataset.csv")

  # Subset the dataset based on the provided team and year
  # NOTE: We need to flag them somehow if they type the team name wrong
  subset_data <- dataset[as.character(dataset$TEAM) ==
    team & dataset$YEAR == year, ]

  # Return the subsetted data
  # Also this won't be as messy bc we clean our data to only fit what we need
  return(new_teamdata(subset_data))
}




#' Note we could also create a team_matchup function that could be used
#' to compare two teams in whatever metric we want. This could be another
#' custom S3 object we do OR we could just throw two of them into each function.
