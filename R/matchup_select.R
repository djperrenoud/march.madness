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
#' @export
matchup_select <- function(team1, year1, team2, year2) {
  # Load the dataset
  dataset <- read.csv("data/initial_clean.csv")

  # Process each team and year using a nested function
  process_team_year <- function(team, year) {
    formatted_team <- format_team_name(team)

    # Check if the year is valid
    if (year < 2008) {
      stop("Error: Data only goes back as far as 2008.")
    }

    # Check if the formatted team name exists in the dataset
    if (!formatted_team %in% as.character(dataset$TEAM)) {
      stop("Error: Team name not found. Please check spelling.")
    }

    # Check if the selected year is available for the team
    available_years <- unique(dataset$YEAR[dataset$TEAM == formatted_team])
    if (!year %in% available_years) {
      cat(sprintf("The selected year %s is not available for %s.\n", year, formatted_team))
      cat("Please select an available year or choose 'Exit' to cancel:\n")
      available_years <- c(available_years, "Exit")
      choice <- menu(available_years, title = "Available Years")

      if (choice == length(available_years)) {  # User selects 'Exit'
        cat("Exiting selection.\n")
        return(NULL)
      } else {
        year <- as.integer(available_years[choice])
      }
    }

    return(list(team = formatted_team, year = year))
  }

  # Process each team and year
  result1 <- process_team_year(team1, as.integer(year1))
  if (is.null(result1)) return(NULL)  # Exit if user chose to cancel

  result2 <- process_team_year(team2, as.integer(year2))
  if (is.null(result2)) return(NULL)  # Exit if user chose to cancel

  # Subset data for each team and year
  subset_data1 <- dataset[dataset$TEAM == result1$team & dataset$YEAR == result1$year, ]
  subset_data2 <- dataset[dataset$TEAM == result2$team & dataset$YEAR == result2$year, ]

  # Return a custom S3 object with the data
  return(new_teamdata_matchup(list(team1_year1 = subset_data1, team2_year2 = subset_data2)))
}

