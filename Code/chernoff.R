data <- read.csv('flights.csv')

# Load required libraries
library(aplpack)  # For Chernoff faces
library(dplyr)    # For data manipulation

# Assuming data is already loaded in 'data' variable
# Calculate metrics by airline including AIR_TIME
airline_metrics <- data %>%
  group_by(AIRLINE) %>%
  summarize(
    avg_departure_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
    avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
    avg_air_time = mean(AIR_TIME, na.rm = TRUE),
    pct_cancelled = 100 * sum(CANCELLED, na.rm = TRUE) / n(),
    pct_diverted = 100 * sum(DIVERTED, na.rm = TRUE) / n()
  )

# Print the metrics table
print(airline_metrics)

# Normalize the data for Chernoff faces
# This helps ensure the face features are appropriately scaled
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Create a data frame with normalized metrics
# Note: For delays, we invert normalization so that longer delays = more negative face features
faces_data <- airline_metrics %>%
  mutate(
    norm_dep_delay = 1 - normalize(avg_departure_delay),
    norm_arr_delay = 1 - normalize(avg_arrival_delay),
    norm_air_time = normalize(avg_air_time),  # longer flights = larger feature
    norm_cancelled = 1 - normalize(pct_cancelled),
    norm_diverted = 1 - normalize(pct_diverted)
  ) %>%
  select(AIRLINE, norm_dep_delay, norm_arr_delay, norm_air_time, norm_cancelled, norm_diverted)

# Create Chernoff faces
# Set up a meaningful mapping of metrics to facial features:
# - departure delay: mouth shape (higher = happier)
# - arrival delay: eye size (higher = larger eyes)
# - air time: nose width (higher = wider nose)
# - cancelled flights: face shape (higher = rounder face)
# - diverted flights: eyebrow slant (higher = more raised eyebrows)

# Create the faces

faces(
  faces_data[, 2:6],  # Include all metrics
  labels = faces_data$AIRLINE,
  face.type = 0,
  main = "Airline Performance Comparison using Chernoff Faces",
  cex = 1.2
)
dev.off()

# Add a plot with explanation of what each facial feature represents
legend_data <- data.frame(
  feature = c("Mouth shape", "Eye size", "Nose width", "Face shape", "Eyebrow slant"),
  meaning = c("Departure delay", "Arrival delay", "Average Air Time", "Cancelled flights", "Diverted flights")
)

print("Facial Feature Mapping:")
print(legend_data)





