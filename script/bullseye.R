library(ggplot2)
library(ggforce)
library(dplyr)
library(rlang)

bullseye <- function(entropy_value,
                     n_points = 10,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL,
                     seed = NULL) {
  
  if (!is.numeric(entropy_value) || entropy_value < 0 || entropy_value > 1) {
    stop("Entropy value must be a number between 0 and 1.")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # Generate points
  points <- tibble(
    angle = runif(n_points, 0, 2 * pi),
    radius = sqrt(runif(n_points, 0, 1)) * entropy_value^0.5,  # more spread at high entropy
    x = radius * cos(angle),
    y = radius * sin(angle)
  )
  
  # Create rings
  rings <- tibble(
    r = seq(0.2, 1, by = 0.2),
    fill = rep(c("white", "gray90"), length.out = 5)
  )
  
  # Plot
  ggplot() +
    geom_circle(data = rings, aes(x0 = 0, y0 = 0, r = r, fill = I(fill)),
                color = "gray60", linewidth = 0.3, show.legend = FALSE) +
    geom_point(data = points, aes(x = x, y = y),
               shape = 21, fill = "black", color = "white", stroke = 0.3, size = 2.5) +
    coord_fixed() +
    labs(
      title = title %||% "Entropy Visualization",
      subtitle = subtitle %||% paste0("Dissonance: ", round(entropy_value, 2)),
      caption = caption
    ) +
    theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 8, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    )
}
