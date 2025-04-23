# Load libraries
library(arules)
library(arulesViz)
library(readr)
library(stringr)

# Read the data
flights <- read.csv("Data_Train.csv")

# Fix route extraction function
# Create a safer function to extract first and last airports from routes
extract_airports <- function(route_str) {
  # Split by arrow symbol (both → and possible plain -> arrows)
  parts <- unlist(strsplit(as.character(route_str), "→|->"))
  # Trim whitespace
  parts <- trimws(parts)
  # Return first and last non-empty parts
  parts <- parts[parts != ""]
  if(length(parts) > 0) {
    return(list(first = parts[1], last = parts[length(parts)]))
  } else {
    return(list(first = NA, last = NA))
  }
}

# Apply the extraction function to each route
airport_info <- lapply(flights$Route, extract_airports)

# Add the extracted information to the flights dataframe
flights$source_airport <- sapply(airport_info, function(x) x$first)
flights$destination_airport <- sapply(airport_info, function(x) x$last)

# Extract all intermediate airports from the route
extract_all_airports <- function(route_str) {
  # Split by arrow symbol
  parts <- unlist(strsplit(as.character(route_str), "→|->"))
  # Trim whitespace
  parts <- trimws(parts)
  return(parts[parts != ""])
}

# Create a new column with all airports in each route
flights$all_airports <- sapply(flights$Route, 
                               function(x) paste(extract_all_airports(x), collapse=","))

# Verify the extraction worked
head(data.frame(
  Route = flights$Route,
  Source = flights$source_airport,
  Destination = flights$destination_airport,
  All_Airports = flights$all_airports
))

# Create the transaction dataset for Apriori - focusing only on airports
flight_subset <- data.frame(
  Airline = flights$Airline,
  Source_Airport = flights$source_airport,
  Destination_Airport = flights$destination_airport,
  Total_Stops = flights$Total_Stops,
  Price_Category = cut(flights$Price, 
                       breaks = c(0, 5000, 10000, 15000, Inf),
                       labels = c("Low", "Medium", "High", "Very High"))
)

# Convert to transactions format
flight_transactions <- as(flight_subset, "transactions")

# Apply Apriori algorithm with focus on route patterns
flight_rules <- apriori(flight_transactions, 
                        parameter = list(supp = 0.1, conf = 0.6, minlen = 2))

# Sort rules by support (high to low)
flight_rules_sorted_by_support <- sort(flight_rules, by = "support", decreasing = TRUE)

# Inspect the top rules by support
cat("\n\n--- Top Rules by Support ---\n")
inspect(head(flight_rules_sorted_by_support, 20))

# Find rules where source airport predicts destination airport, sorted by support
airport_rules <- subset(flight_rules, 
                        lhs %pin% "Source_Airport" & 
                          rhs %pin% "Destination_Airport")
airport_rules_by_support <- sort(airport_rules, by = "support", decreasing = TRUE)
cat("\n\n--- Top Airport Rules by Support ---\n")
inspect(head(airport_rules_by_support, 10))

# Find rules about specific source airports and their common destinations
# Sort by support
blr_rules <- subset(flight_rules, lhs %pin% "Source_Airport=BLR")
blr_rules_by_support <- sort(blr_rules, by = "support", decreasing = TRUE)
cat("\n\n--- Top BLR Rules by Support ---\n")
inspect(head(blr_rules_by_support, 10))

