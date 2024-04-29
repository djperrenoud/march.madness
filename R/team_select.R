#' Team Select
#'
#' Creates a custom S3 object representing a subset of data
#' for a specific team and year.
#'
#' @importFrom "utils" "read.csv" "menu"
#'
#' @param team A character string representing the team name.
#'
#' @param year An integer representing the year.

#' @return A custom S3 object of class 'teamdata' containing the subsetted data.
#'
#'
#' @export
team_select <- function(team, year) {
  # Load the dataset
  dataset <- read.csv("data/initial_clean.csv")

  # Format the team name input
  formatted_team <- format_team_name(team)

  # Check if the year is valid
  if (year < 2008) {
    stop("Error: Data only goes back as far as 2008.")
  }

  # Check if the formatted team name exists in the dataset
  if (!formatted_team %in% as.character(dataset$TEAM)) {
    stop("Error: Team name not found. Please check spelling.
         Note: Package will only correct capitalization erros and will
         automatically abbreviate state to St.")
  }

  # Check if the selected year is available for the team
  available_years <- unique(dataset$YEAR[dataset$TEAM == formatted_team])
  if (!year %in% available_years) {
    cat("The selected year is not available for", formatted_team, "\n")
    cat("Please select an available year or choose 'Exit' to cancel:\n")
    # Adding an 'Exit' option to the menu
    available_years <- c(available_years, "Exit")
    choice <- menu(available_years, title = "Available Years")

    # Check if user chooses to exit
    if (choice == length(available_years)) {
      cat("Exiting selection.\n")
      return(NULL)
    } else {
      year <- available_years[choice]
    }
  }

  # Subset the dataset based on the provided team and year
  subset_data <- dataset[as.character(dataset$TEAM) ==
    formatted_team & dataset$YEAR == year, ]

  # Return the subsetted data
  return(new_teamdata(subset_data))
}
