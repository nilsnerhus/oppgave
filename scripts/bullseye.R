#' @title Create a bullseye visualization of dominance
#' @description Generates a bullseye/shooting target visualization where points 
#'   are distributed based on a dominance value. Higher dominance values (closer to 1)
#'   result in points clustered toward the center, while lower values (closer to 0)
#'   result in points scattered throughout the target.
#'
#' @param dominance_value Numeric value between 0 and 1 representing the dominance measure
#' @param n_points Number of points to plot (default: 10)
#' @param title Optional title for the plot
#' @param subtitle Optional subtitle (dominance value will be shown if not provided)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A ggplot object with the bullseye visualization
#'
#' @examples
#' # Create bullseye with high dominance (clustered points)
#' bullseye(0.85, title = "Income Groups")
#' 
#' # Create bullseye with low dominance (scattered points)
#' bullseye(0.3, title = "Geographic Regions")

bullseye <- function(
    dominance_value,
    n_points = 10,
    title = NULL,
    subtitle = NULL,
    seed = 1234
) {
  
  # Validate input
  if (!is.numeric(dominance_value) || dominance_value < 0 || dominance_value > 1) {
    stop("Dominance value must be a number between 0 and 1.")
  }
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Import required packages if needed
  `%>%` <- magrittr::`%>%`
  
  # Create rings for the bullseye
  # 10 total levels: 9 rings plus center circle
  rings <- tibble::tibble(
    r = seq(0.1, 1, length.out = 10),
    # Rings 7-10 (inner) are black, rings 1-6 (outer) are white
    fill = ifelse(r >= 0.45, "white", "black"),
    # All rings have outlines
    color = "black",
    # Inverse outline color for black rings
    outline = ifelse(r >= 0.45, "black", "white")
  )
  
  # Define parameters for the logistic transition function
  midpoint <- 0.7  # Transition centered at dominance=0.4
  steepness <- 12  # Fairly steep transition
  
  # Generate points based on dominance value
  points <- tibble::tibble(
    # Assign sectors for the points
    sector = ceiling(seq(1, n_points) / n_points * 8),
    
    # Calculate sector-based angles
    # When dominance is low, points stay more within their sector
    # When dominance is high, sector boundaries matter less
    sector_angle = sector * 2 * pi / 8,
    angle_variance = dominance_value * 2 * pi + (1 - dominance_value) * pi/4,
    angle = sector_angle + runif(n_points, -angle_variance/2, angle_variance/2),
    
    # Calculate alpha using logistic function for better transition control
    # This creates more distinction between 0.2-0.3 but similar clustering for 0.4-0.6
    alpha = 1 + 19 / (1 + exp(-steepness * (dominance_value - midpoint))),
    beta = 1.2,  # Slightly lower beta for more spread at low dominance
    
    # Generate radii from beta distribution
    radius = rbeta(n_points, shape1 = beta, shape2 = alpha),
    
    # Convert to Cartesian coordinates
    x = radius * cos(angle),
    y = radius * sin(angle)
  )
  
  # Create the plot
  ggplot2::ggplot() +
    # Draw the rings from outside to inside (reverse order)
    ggforce::geom_circle(
      data = rings %>% dplyr::arrange(desc(r)), 
      ggplot2::aes(
        x0 = 0, y0 = 0, r = r, 
        color = I(outline),
        fill = I(fill)
      ),
      linewidth = 0.3
    ) +
    # Add the points
    ggplot2::geom_point(
      data = points, 
      ggplot2::aes(x = x, y = y),
      shape = 21, 
      fill = "black", 
      color = "white", 
      stroke = 0.3, 
      size = 5
    ) +
    # Ensure circular appearance
    ggplot2::coord_fixed() +
    # Set plot labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle %||% paste0("Dominance: ", round(dominance_value, 2))
    ) +
    # Remove axes and background
    ggplot2::theme_void(base_family = "sans") +
    # Style text elements
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}