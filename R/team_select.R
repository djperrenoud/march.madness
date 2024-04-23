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
  dataset <- read.csv("initial_clean.csv")

  # Format the team name input
  format_team_name <- function(team) {
    # Normalize space and convert to lowercase to handle case variations
    team <- trimws(team) # Remove any leading/trailing white space
    words <- strsplit(team, " ")[[1]]
    words <- tolower(words)  # Lowercase all to handle variations

    # Handle specific cases
    if (length(words) >= 2) {
      if (words[1] == "loyola" && (words[2] == "md" || words[2] == "maryland")) {
        return("Loyola MD")
      } else if (words[1] == "liu" && words[2] == "brooklyn") {
        return("LIU Brooklyn")
      } else if (words[1] == "ut" && words[2] == "arlington") {
        return("UT Arlington")
      } else if (words[1] == "mount" && (words[2] == "saint" || words[2] == 'st' || words[2] == 'st.') && (words[3] == "marys" || words[3] == "mary's")) {
        return("Mount St. Mary's")
      } else if (words[1] == "uc" && words[2] == "davis") {
        return("UC Davis")
      } else if (words[1] == "uc" && words[2] == "irvine") {
        return("UC Irvine")
      } else if (words[1] == "uc" && words[2] == "santa" && words[3] == "barbara") {
        return("UC Santa Barbara")
      } else if (words[1] == "miami" && (words[2] == fl || words[2] == florida)) {
        return("Miami FL")
      }
    }

    # Handle Specific Four Letter Abbreviations
    if (nchar(words[1]) == 4 && length(words) == 1){
      if (words[1] == "unlv"){
        return("UNLV")
      } else if (words[1] == "utep") {
        return("UTEP")
      } else if (words[1] == "utsa") {
        return("UTSA")
      }
    }

    # Handle three-letter school names
    if (nchar(words[1]) == 3 && length(words) == 1) {
      return(toupper(words[1]))
    }

    # Handle the Miami case
    if (length(words) == 1 && words[1] == "miami") {
      warning("Note: 'Miami Ohio' is not included in the dataset. Referring to 'Miami FL'.")
      return("Miami FL")
    }

    # Apply general capitalization rules
    words <- ifelse(words == "of", "of",
                    ifelse(words == "state", "St.",
                           ifelse(nchar(words) >= 3,
                                  sub("^(.)", "\\U\\1", words, perl = TRUE),
                                  sub("st$", "St.", words))))

    return(paste(words, collapse = " "))
  }

  formatted_team <- format_team_name(team)

  # Check if the year is valid
  if (year < 2008) {
    stop("Error: Data only goes back as far as 2008.")
  }

  # Check if the formatted team name exists in the dataset
  if (!formatted_team %in% as.character(dataset$TEAM)) {
    stop("Error: Team name not found. Please check spelling. Note: Package will only correct capitalization erros and will automatically abbreviate state to St.")
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
