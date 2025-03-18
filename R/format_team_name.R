#' Format Team Name
#'
#' Formats the team name input to correct common variations and standardize names.
#'
#' @param team A character string representing the team name as inputted by the user.
#'
#' @return Properly formatted team name matching the dataset.
#'
#' @examples
#' formatted_team <- format_team_name("utah st")
#' print(formatted_team)
#'
#' @export
format_team_name <- function(team) {
  # Normalize space and convert to lowercase to handle case variations
  team <- trimws(team)  # Remove leading/trailing spaces
  words <- strsplit(team, " ")[[1]]
  words <- tolower(words)  # Convert to lowercase for consistency

  # Handle specific cases
  special_cases <- list(
    "loyola md" = "Loyola MD",
    "loyola maryland" = "Loyola MD",
    "liu brooklyn" = "LIU Brooklyn",
    "ut arlington" = "UT Arlington",
    "unc asheville" = "UNC Asheville",
    "mcneese" = "McNeese St.",
    "mcneese state" = "McNeese St.",
    "uc davis" = "UC Davis",
    "uc irvine" = "UC Irvine",
    "uc santa barbara" = "UC Santa Barbara",
    "uc san diego" = "UC San Diego",
    "miami fl" = "Miami FL",
    "miami florida" = "Miami FL"
  )

  formatted_team <- special_cases[[tolower(team)]]
  if (!is.null(formatted_team)) return(formatted_team)

  # Handle 'Saint' Schools
  if (length(words) >= 2) {
    if (words[1] == "mount" && words[2] %in% c("saint", "st", "st.") && words[3] %in% c("marys", "mary's")) {
      return("Mount St. Mary's")
    } else if (words[1] %in% c("saint", "st", "st.")) {
      saint_schools <- c("joseph's" = "Saint Joseph's", "josephs" = "Saint Joseph's",
                         "louis" = "Saint Louis", "marys" = "Saint Mary's", "mary's" = "Saint Mary's",
                         "peters" = "Saint Peter's", "peter's" = "Saint Peter's")
      if (!is.null(saint_schools[[words[2]]])) {
        return(saint_schools[[words[2]]])
      }
    }
  }

  # Handle A&M and A&T Schools
  if (length(words) >= 2) {
    if (length(words) >= 3 && words[1] == "north" && words[2] == "carolina" && words[3] == "a&t") {
      return("North Carolina A&T")
    } else if (length(words) >= 3 && words[1] == "prairie" && words[2] == "view" && words[3] == "a&m") {
      return("Prairie View A&M")
    } else if (length(words) >= 4 && words[1] == "texas" && words[2] == "a&m" && words[3] == "corpus" && words[4] == "christi") {
      return("Texas A&M Corpus Christi")
    } else if (words[1] == "texas" && words[2] == "a&m") {
      return("Texas A&M")
    }
  }

  # Handle Specific Four-Letter Abbreviations
  four_letter_abbreviations <- c("unlv", "utep", "utsa", "umbc", "ucla")
  if (length(words) == 1 && words[1] %in% four_letter_abbreviations) {
    return(toupper(words[1]))
  }

  # Handle three-letter school names (e.g., "BYU")
  if (length(words) == 1 && nchar(words[1]) == 3) {
    return(toupper(words[1]))
  }

  # Handle the Miami case
  if (length(words) == 1 && words[1] == "miami") {
    warning("Note: 'Miami Ohio' is not included in the dataset. Referring to 'Miami FL'.")
    return("Miami FL")
  }

  # Apply general capitalization rules
  words <- sapply(words, function(word) {
    if (word == "of") return("of")  # Keep "of" lowercase
    if (word == "state") return("St.")  # Convert "state" to "St."
    return(tools::toTitleCase(word))  # Capitalize each word
  })

  return(paste(words, collapse = " "))
}
