#' When cleaning our data, we had columns with statistical values such as
#' PPG which were followed by columns such as PPG Rank. We only wanted to
#' keep the actual values as we deemed the ranks unimportant when we had the
#' actual values themselves.
#'
#' Also note that we kept every single statistical value from our dataset
#' BECAUSE it was important to find the weight of importance of each of them.
#' If a stat was useless that was made up for in our weights so we decided to
#' include all of the stats we had available and just git rid of our
#' unnecessary rank columns.

usethis::use_data(DATASET, overwrite = TRUE)

#Import csv as dataframe.
raw_data <- read.csv("data-raw/KenPom Barttorvik.csv")

#Subset data to only include columns holding stats of interest to our analysis.
clean_data <- raw_data[, c(1, 2, 8, 9, 10, 13, 17, 21, 25, 26, 27, 29:64)]

#Export the dataframe as a new csv file.
write.csv(clean_data, file = "initial_clean.csv", row.names = FALSE)
