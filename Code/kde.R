# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Assuming 'data' is already available in the environment

data <- read.csv('flights.csv')

# Set negative delay times to 0
data$DEPARTURE_DELAY[data$DEPARTURE_DELAY < 0] <- 0

data <- data %>% filter(data$DEPARTURE_DELAY != 0)

# Count occurrences of each airline
airline_counts <- data %>%
  count(AIRLINE) %>%
  arrange(desc(n))

# Select top 9 airlines by occurrence
top_airlines <- airline_counts %>%
  head(9) %>%
  pull(AIRLINE)

# Filter data to only include top airlines
top_airline_data <- data %>%
  filter(AIRLINE %in% top_airlines)

# Define x-axis scale limit (0-300 minutes) and interval (50 minutes)
x_limit <- 300
x_breaks <- seq(0, x_limit, by = 50)

# Define a color palette with 9 distinct colors
colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
            "#FFFF33", "#A65628", "#F781BF", "#999999")

# Calculate the maximum y-value across all density estimates first
max_density <- 0
for(airline in top_airlines) {
  airline_data <- data %>% filter(AIRLINE == airline)
  
  if(nrow(airline_data) >= 2) {
    # Calculate density without plotting
    dens <- density(airline_data$DEPARTURE_DELAY, from = 0, to = x_limit, kernel = "gaussian")
    curr_max <- max(dens$y)
    
    # Update the overall maximum if needed
    if(curr_max > max_density) {
      max_density <- curr_max
    }
  }
}

# Create a list to hold the individual plots with the same y-axis scale
plot_list <- list()

# Generate a density plot for each of the top airlines with the same x and y-axis scales
for(i in 1:length(top_airlines)) {
  airline <- top_airlines[i]
  airline_data <- data %>% filter(AIRLINE == airline)
  
  if(nrow(airline_data) >= 2) {
    plot <- ggplot(airline_data, aes(x = DEPARTURE_DELAY)) +
      geom_density(fill = colors[i], color = "black", alpha = 0.7, kernel = "gaussian") +
      labs(title = paste("Airline:", airline),
           x = "Departure Delay (minutes)",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(size = 10, face = "bold"),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 7)) +
      # Set x-axis limits from 0 to 300
      coord_cartesian(xlim = c(0, x_limit), ylim = c(0, max_density)) +
      # Add breaks every 50 minutes
      scale_x_continuous(breaks = x_breaks)
    
    plot_list[[i]] <- plot
  } else {
    # Create an empty plot with a message if not enough data
    plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient data for", airline)) +
      theme_void() +
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    plot_list[[i]] <- plot
  }
}

# Calculate how many rows we need (if we have fewer than 9 airlines)
num_rows <- ceiling(length(plot_list) / 3)

# Arrange the plots in a grid
grid_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 3,
  nrow = num_rows,
  top = "Kernel Density Estimates of Departure Delays (0-300 Minutes)"
)

# Display the grid plot
print(grid_plot)

