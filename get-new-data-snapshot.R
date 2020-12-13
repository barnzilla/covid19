# Load dependencies
library(cansim); library(dplyr); library(stringr); library(tidyr)

# Helper functions
# Wrangle the raw data
wrangle_data <- function(d) {
  # Reshape data from long to wide format
  d_wide <- spread(d %>% select("Case identifier number", "Case information", VALUE, REF_DATE), "Case information", VALUE)

  # Add leading zeros to case identifier number
  d_wide$`Case identifier number` <- str_pad(d_wide$`Case identifier number`, width = nchar(max(as.numeric(d$`Case identifier number`))), pad = "0")

  # Identify select vectors
  vectors_to_factor <- c("Age group", "Gender", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital status", "Recovered", "Death")

  # Restructure as factors
  d_wide[vectors_to_factor] <- lapply(d_wide[vectors_to_factor], factor)

  # Add semantic labels
  d_wide$`Age group` <- revalue(d_wide$`Age group`, c("1" = "0-19", "2" = "20-29", "3" = "30-39", "4" = "40-49", "5" = "50-59", "6" = "60-69", "7" = "70-79", "8" = "80+", "99" = "Not stated"), warn_missing = FALSE)
  d_wide$Gender <- revalue(d_wide$Gender, c("1" = "Male", "2" = "Female", "3" = "Non-binary", "7" = "Non-binary", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$Region <- revalue(d_wide$Region, c("1" = "Atlantic", "2" = "Quebec", "3" = "Ontario and Nunavut", "4" = "Prairies and the Northwest Territories", "5" = "British Columbia and Yukon"), warn_missing = FALSE)
  d_wide$Occupation <- revalue(d_wide$Occupation, c("1" = "Health care worker", "2" = "School or daycare worker/attendee", "3" = "Long term care resident", "4" = "Other", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$Asymptomatic <- revalue(d_wide$Asymptomatic, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$Transmission <- revalue(d_wide$Transmission, c("1" = "Domestic acquisition", "2" = "International travel", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$`Hospital status` <- revalue(d_wide$`Hospital status`, c("1" = "Hospitalized and in intensive care unit", "2" = "Hospitalized, but not in intensive care unit", "3" = "Not hospitalized", "9" = "Not stated/unknown"), warn_missing = FALSE)
  d_wide$Recovered <- revalue(d_wide$Recovered, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$Death <- revalue(d_wide$Death, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)

  # Add day (select first day of the week since not given), month and reference year vectors together and structure as a date object
  d_wide$`Episode date` <- as.Date(paste(d_wide$REF_DATE, str_pad(d_wide$`Episode week`, width = 2, pad = 0), 1, sep = "-"), "%Y-%U-%u")

  # Change format to %d-%b-%y
  d_wide$`Episode date` <- strftime(d_wide$`Episode date`, format = "%d-%b-%y")

  # Remove unwanted vectors from data
  d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", "Region", "Occupation", Asymptomatic, Transmission, "Hospital status", Recovered, Death)

  # Rename vectors
  names(d_wide) <- c("CaseID", "Episode Date", "Gender", "Age Group", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital Status", "Recovered", "Death")

  # Order data by case ids in ascending order
  d_wide <- d_wide %>% arrange(CaseID)

  return(d_wide)
}

# Import CANSIM data
d <- get_cansim("13-10-0781-01", refresh = TRUE)

# Wrangle data
new_snapshot <- wrangle_data(d)

# Convert `Episode Date` to date object
new_snapshot$`Episode Date` <- as.Date(new_snapshot$`Episode Date`, "%d-%b-%y")

# Create aggregate data
aggregate_data <- aggregate(
  new_snapshot$CaseID,
  list(
    new_snapshot$`Episode Date`,
    new_snapshot$`Age Group`,
    new_snapshot$Gender,
    new_snapshot$Region,
    new_snapshot$Occupation,
    new_snapshot$`Hospital Status`,
    new_snapshot$Death,
    new_snapshot$Transmission
  ),
  length
)

# Update names in aggregate_data
names(aggregate_data) <- c(
  "Episode Date",
  "Age Group",
  "Gender",
  "Region",
  "Occupation",
  "Hospital Status",
  "Death",
  "Transmission",
  "Counts"
)

# Convert factors to characters
aggregate_data[-c(1, ncol(aggregate_data))] <- lapply(aggregate_data[-c(1, ncol(aggregate_data))], as.character)

# Export data
saveRDS(aggregate_data, paste0("c:/users/joelb/onedrive/github/covid19/data/aggregate-data-", Sys.Date(), ".Rdata"), compress = "xz")
