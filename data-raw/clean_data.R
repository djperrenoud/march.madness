#Import csv as dataframe.
raw_data <- read.csv("data-raw/dataset.csv")

#Subset data to only include columns holding stats of interest to our analysis.
clean_data <- raw_data[, c(1,2,8,9,10,13,17,21,25,26,27,29:64)]

#Export the dataframe as a new csv file.
write.csv(clean_data, file = "initial_clean.csv", row.names = FALSE)
