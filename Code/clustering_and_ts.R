# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

# Step 1: Load Data
data <- read.csv("Data_Train.csv")

# Function to convert duration to minutes
convert_duration_to_minutes <- function(duration) {
  # Add "0m" or "0h" if missing
  duration <- gsub("h$", "h 0m", duration)
  duration <- gsub("m$", "0h m", duration)
  
  # Extract hours and minutes
  hours <- as.numeric(gsub("h.*", "", duration))
  mins <- as.numeric(gsub(".*h |m", "", duration))
  
  # Handle NA safely
  hours[is.na(hours)] <- 0
  mins[is.na(mins)] <- 0
  
  # Total minutes
  return(hours * 60 + mins)
}

# Apply function
data$Duration_Minutes <- convert_duration_to_minutes(data$Duration)

# Check result
head(data$Duration_Minutes)
head(data)


# Step 3: Convert Total_Stops to numeric
data$Stops_num <- case_when(
  data$Total_Stops == "non-stop" ~ 0,
  data$Total_Stops == "1 stop" ~ 1,
  data$Total_Stops == "2 stops" ~ 2,
  data$Total_Stops == "3 stops" ~ 3,
  data$Total_Stops == "4 stops" ~ 4,
  TRUE ~ NA_real_
)


head(data)


unique(data$Airline)

# Step 5: Get top 5 Source-Destination combinations
top_routes <- data %>%
  group_by(Source, Destination) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  slice_head(n = 3)


# --- Select Route 1 ---
src1 <- top_routes$Source[1]
dest1 <- top_routes$Destination[1]

route1_data <- data %>% filter(Source == src1, Destination == dest1)

# --- No airline filtering, include all airlines
summary_data <- route1_data %>%
  group_by(Airline) %>%
  summarise(
    Price = mean(Price, na.rm = TRUE),
    Duration = mean(Duration_Minutes, na.rm = TRUE),
    Stops = mean(Stops_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

print(summary_data)

df <- as.data.frame(summary_data)
rownames(df) <- df$Airline
df <- df[, -1]  # Remove Airline column

# --- K-Means Clustering (Price & Duration) ---
k_data <- scale(df[, c("Price", "Duration")])
set.seed(42)
kfit <- kmeans(k_data, centers = 3, nstart = 25)

fviz_cluster(kfit, data = k_data, geom = "point",
             labelsize = 5,
             main = paste("K-Means Clustering:", src1, "to", dest1)) +
  geom_text(label = rownames(k_data), vjust = -1, size = 4)

# --- Hierarchical Clustering (Stops) ---
h_data <- scale(df[, "Stops", drop = FALSE])
dist_mat <- dist(h_data)
hc <- hclust(dist_mat, method = "ward.D2")

# प्लॉट बनाएँ - मुख्य शीर्षक में स्पष्ट करें
plot(hc, labels = rownames(h_data),
     main = paste("Hierarchical Clustering based on Stops:", src1, "to", dest1),
     sub = "Clusters formed using number of stops")

# आयताकार सीमाएँ दिखाएँ
rect.hclust(hc, k = 3, border = 2:4)


# --- Select Route 2 ---
src2 <- top_routes$Source[2]
dest2 <- top_routes$Destination[2]
route2_data <- data %>% filter(Source == src2, Destination == dest2)

# --- No airline filtering, include all airlines
summary_data2 <- route2_data %>%
  group_by(Airline) %>%
  summarise(
    Price = mean(Price, na.rm = TRUE),
    Duration = mean(Duration_Minutes, na.rm = TRUE),
    Stops = mean(Stops_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()
print(summary_data2)

df2 <- as.data.frame(summary_data2)
rownames(df2) <- df2$Airline
df2 <- df2[, -1]  # Remove Airline column

# --- K-Means Clustering (Price & Duration) ---
k_data2 <- scale(df2[, c("Price", "Duration")])
set.seed(42)
kfit2 <- kmeans(k_data2, centers = 3, nstart = 25)
fviz_cluster(kfit2, data = k_data2, geom = "point",
             labelsize = 5,
             main = paste("K-Means Clustering:", src2, "to", dest2)) +
  geom_text(label = rownames(k_data2), vjust = -1, size = 4)

# --- Hierarchical Clustering (Stops) ---
h_data2 <- scale(df2[, "Stops", drop = FALSE])
dist_mat2 <- dist(h_data2)
hc2 <- hclust(dist_mat2, method = "ward.D2")

# Plot with clear title
plot(hc2, labels = rownames(h_data2),
     main = paste("Hierarchical Clustering based on Stops:", src2, "to", dest2),
     sub = "Clusters formed using number of stops")

# Add rectangle borders
rect.hclust(hc2, k = 3, border = 2:4)





# Create a new column for meal info
data$Meal_Provided <- ifelse(data$Additional_Info == "In-flight meal not included", 0, 1)

# Aggregate average values by Airline & Meal_Provided
summary_meal <- data %>%
  group_by(Airline, Meal_Provided) %>%
  summarise(
    Price = mean(Price, na.rm = TRUE),
    Duration = mean(Duration_Minutes, na.rm = TRUE),
    Stops = mean(Stops_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

# Set rownames and scale features
df_meal <- as.data.frame(summary_meal)
rownames(df_meal) <- paste(df_meal$Airline, df_meal$Meal_Provided, sep = "_")
df_meal <- df_meal[, c("Price", "Duration", "Stops")]
df_meal_scaled <- scale(df_meal)

# Hierarchical Clustering
dist_mat <- dist(df_meal_scaled)
hc <- hclust(dist_mat, method = "ward.D2")

# Plot
plot(hc, main = "Hierarchical Clustering based on Meal Info")
rect.hclust(hc, k = 2, border = 2:3)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(zoo)
library(tsibble)
library(feasts)
library(forecast)

# Read data
df <- read_csv('/Users/sayantabiswas/Downloads/Delay Airlines.csv',
               show_col_types = FALSE)
head(df)

# Summary statistics
summary(df)
glimpse(df)

# Missing values
colSums(is.na(df))

# Numeric features
numeric_features <- c('arr_flights', 'arr_del15', 'carrier_ct', 'weather_ct', 'nas_ct',
                      'security_ct', 'late_aircraft_ct', 'arr_cancelled', 'arr_diverted',
                      'arr_delay', 'carrier_delay', 'weather_delay', 'nas_delay',
                      'security_delay', 'late_aircraft_delay')

# Histograms
for (feature in numeric_features) {
  p <- ggplot(df, aes_string(x = feature)) +
    geom_density(color = "#ff4b5c", size = 1.2) +
    ggtitle(paste("Distribution of", feature)) +
    labs(x = feature, y = "Density") +
    coord_cartesian(xlim = c(0, 1000)) +  # Limit x-axis
    theme_few() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11)
    )
  print(p)
}

# Categorical features
categorical_features <- c('carrier', 'carrier_name', 'airport', 'airport_name')

for (feature in categorical_features) {
  top_categories <- df %>%
    count(!!sym(feature), sort = TRUE) %>%
    top_n(15, n)
  
  ggplot(top_categories, aes(x = reorder(!!sym(feature), -n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    ggtitle(paste("Top 15", feature, "Counts")) +
    theme_minimal() +
    labs(x = feature, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
  print(p)
}

# Create 'date' column
df <- df %>%
  mutate(date = make_date(year, month, 1)) %>%
  arrange(date)

# Set index (in R we keep it as a regular column)
df <- df %>% replace_na(list(arr_flights = median(df$arr_flights, na.rm = TRUE)))

# Time series trend
ggplot(df, aes(x = date, y = arr_flights)) +
  geom_line(color = "blue") +
  geom_point(size = 0.8) +
  ggtitle("Trend of Arrival Flights Over Time") +
  theme_minimal() +
  labs(x = "Time", y = "Number of Arrival Flights") -> p
print(p)

# Yearly aggregation
yearly_df <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(arr_flights = sum(arr_flights, na.rm = TRUE))

ggplot(yearly_df, aes(x = year, y = arr_flights)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 1) +
  ggtitle("Yearly Sum of Arrival Flights Over Time") +
  theme_minimal() +
  labs(x = "Year", y = "Number of Arrival Flights") -> p
print(p)

# Time series decomposition
ts_data <- ts(df$arr_flights, start = c(min(df$year), min(df$month)), frequency = 12)
decomp <- decompose(ts_data, type = "additive")
plot(decomp)

