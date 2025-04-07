library(ggplot2)
library(ggforce)  # For geom_circle
library(dplyr)
library(patchwork)  # For arranging plots side by side

# Function to create a basic entropy target visualization
create_entropy_target <- function(points_data, entropy_value, title) {
  # Create base target
  p <- ggplot() +
    # Create target rings
    geom_circle(aes(x0 = 0, y0 = 0, r = 150), fill = "white", color = "black") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 125), fill = "white", color = "black") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 100), fill = "white", color = "black") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 75), fill = "white", color = "black") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 50), fill = "black", color = "white") +
    geom_circle(aes(x0 = 0, y0 = 0, r = 25), fill = "black", color = "white") +
    
    # Add bullet holes
    geom_point(data = points_data, aes(x = x, y = y), 
               shape = 21, size = 5, fill = "black", color = "white", stroke = 1.5) +
    
    # Add title and entropy value
    labs(title = title, subtitle = paste("Entropy =", round(entropy_value, 2))) +
    
    # Ensure circular appearance and remove unnecessary elements
    coord_fixed() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      plot.margin = margin(30, 10, 30, 10)
    ) +
    
    # Set reasonable limits
    xlim(-160, 160) + 
    ylim(-160, 160)
  
  return(p)
}

# Example usage:
# Create sample data for high entropy (dispersed points)
high_entropy_data <- data.frame(
  x = runif(20, -150, 150),
  y = runif(20, -150, 150)
)

# Create sample data for low entropy (clustered points)
low_entropy_data <- data.frame(
  x = rnorm(15, 0, 25),
  y = rnorm(15, 0, 25)
)

# Create visualizations
high_plot <- create_entropy_target(high_entropy_data, 0.72, "Geographic Regions")
low_plot <- create_entropy_target(low_entropy_data, 0.35, "Income Groups")

# Display side by side
high_plot + low_plot + plot_layout(ncol = 2)
