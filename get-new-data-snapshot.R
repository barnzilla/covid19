# Load dependencies
library(cansim); library(plyr); library(dplyr); library(stringr); library(readr); library(tidyr)

# Helper functions
# Wrangle the raw data
wrangle_data <- function(d) {
  d_wide <- d %>% filter(`Episode week` != 99)

  # Add leading zeros to Case Identifier Number
  d_wide$`Case identifier number` <- str_pad(d_wide$`Case identifier number`, width = nchar(max(as.numeric(d$`Case identifier number`))), pad = "0")

  # Identify select vectors
  #vectors_to_factor <- c("Age group", "Gender", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital status", "Recovered", "Death")
  vectors_to_factor <- c("Age group", "Gender", "Region", "Occupation", "Asymptomatic", "Transmission", "Hospital status", "Death")
  
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
  #d_wide$Recovered <- revalue(d_wide$Recovered, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)
  d_wide$Death <- revalue(d_wide$Death, c("1" = "Yes", "2" = "No", "9" = "Not stated"), warn_missing = FALSE)

  # Add day (select first day of the week since not given), month and reference year vectors together and structure as a date object
  d_wide$`Episode date` <- as.Date(paste(paste0("20", d_wide$`Episode year`), str_pad(d_wide$`Episode week`, width = 2, pad = 0), 1, sep = "-"), "%Y-%U-%u")

  # Change format to %d-%b-%y
  #d_wide$`Episode date` <- strftime(d_wide$`Episode date`, format = "%d-%b-%y")

  # Remove unwanted vectors from data
  #d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", "Region", "Occupation", Asymptomatic, Transmission, "Hospital status", Recovered, Death)
  d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", "Region", "Occupation", Asymptomatic, Transmission, "Hospital status", Death)

  # Order data by case ids in ascending order
  d_wide <- d_wide %>% arrange(`Case identifier number`)

  return(d_wide)
}

# Set working directory
setwd("c:/users/joelb/onedrive/github/covid19")

# Import data
d <- read_csv(paste0(getwd(), "/raw-data/", sort(list.files(paste0(getwd(), "/raw-data"), pattern = ".csv"), decreasing = TRUE)[1]))

# Change vector names
lookup <- tibble(
  short = c(
    "COV_ID",
    "COV_REG",
    "COV_EW",
    "COV_EWG",
    "COV_EY",
    "COV_GDR",
    "COV_AGR",
    "COV_OCC",
    "COV_ASM",
    "COV_OW",
    "COV_OY",
    "COV_HSP",
    "COV_RSV",
    "COV_RW",
    "COV_RY",
    "COV_DTH",
    "COV_TRM"
  ),
  long = c(
    "Case identifier number",
    "Region",
    "Episode week",
    "Episode week group",
    "Episode year",
    "Gender",
    "Age group",
    "Occupation",
    "Asymptomatic",
    "Onset week of symptoms",
    "Onset year of symptoms",
    "Hospital status",
    "Recovered",
    "Resolution week",
    "Resolution year",
    "Death",
    "Transmission"
  )
)
names(d) <- sapply(names(d), function(x) {
  if(x %in% lookup$short) {
    output <- lookup$long[lookup$short == x]
  } else {
    output <- x
  }
})

# Wrangle data
new_snapshot <- wrangle_data(d)

# Convert `Episode Date` to date object
new_snapshot$`Episode date` <- as.Date(new_snapshot$`Episode date`, "%d-%b-%y")

# Create aggregate data
aggregate_data <- aggregate(
  new_snapshot$`Case identifier number`,
  list(
    new_snapshot$`Episode date`,
    new_snapshot$`Age group`,
    new_snapshot$Gender,
    new_snapshot$Region,
    new_snapshot$Occupation,
    new_snapshot$`Hospital status`,
    new_snapshot$Death,
    new_snapshot$Transmission
  ),
  length
)

# Update names in aggregate_data
names(aggregate_data) <- c(
  "Episode date",
  "Age group",
  "Gender",
  "Region",
  "Occupation",
  "Hospital status",
  "Death",
  "Transmission",
  "Counts"
)

# Convert factors to characters
aggregate_data[-c(1, ncol(aggregate_data))] <- lapply(aggregate_data[-c(1, ncol(aggregate_data))], as.character)

# Export data
saveRDS(aggregate_data, paste0("c:/users/joelb/onedrive/github/covid19/data/aggregate-data-", Sys.Date() ,".Rdata"), compress = "xz")
