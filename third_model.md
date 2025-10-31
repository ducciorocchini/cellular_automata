Perfect 🌿 — let’s build a **single clean version** of your model that:

✅ Generates a **fractal landscape** (Perlin noise)
✅ Computes **slope** (terrain steepness)
✅ Derives a **spatial growth probability map** based on terrain + slope
✅ Runs the **forest spread simulation**
✅ Colors vegetation by **growth probability (not elevation)**
✅ Returns plots for terrain, slope, probability, forest evolution, and vegetation cover

---

## 🌲 Full Model: `cellular.forest.landscape.probability()`

```r
cellular.forest.landscape.probability <- function(num_iterations = 50, plot_interval = 10,
                                                  n_rows = 100, n_cols = 100,
                                                  frequency = 0.05, octaves = 5,
                                                  base_growth = 0.1, death_prob = 0.02,
                                                  seed = NULL) {
  # --- 1. Load packages ---
  library(ggplot2)
  library(patchwork)
  library(ambient)
  library(tidyverse)
  library(viridis)
  
  # --- 2. Generate fractal terrain ---
  if (!is.null(seed)) set.seed(seed)
  terrain <- noise_perlin(c(n_rows, n_cols), frequency = frequency, octaves = octaves)
  terrain <- (terrain - min(terrain)) / (max(terrain) - min(terrain))  # normalize to [0,1]
  
  # --- 3. Compute slope (roughness) ---
  get_slope <- function(mat) {
    slope <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
    for (i in 2:(nrow(mat) - 1)) {
      for (j in 2:(ncol(mat) - 1)) {
        dx <- (mat[i, j + 1] - mat[i, j - 1]) / 2
        dy <- (mat[i + 1, j] - mat[i - 1, j]) / 2
        slope[i, j] <- sqrt(dx^2 + dy^2)
      }
    }
    slope <- (slope - min(slope)) / (max(slope) - min(slope))
    return(slope)
  }
  slope <- get_slope(terrain)
  
  # --- 4. Compute spatial growth probability map ---
  growth_prob_map <- base_growth * (1 - terrain) * (1 - slope)
  
  # --- 5. Initialize vegetation grid ---
  grid <- matrix(0, nrow = n_rows, ncol = n_cols)
  grid[sample(1:(n_rows * n_cols), size = 200)] <- 1  # initial vegetation
  
  # --- 6. Helper functions ---
  get_neighbors <- function(row, col, grid) {
    neighbors <- c()
    for (i in -1:1) {
      for (j in -1:1) {
        if (!(i == 0 & j == 0)) {
          r <- row + i
          c <- col + j
          if (r > 0 & r <= nrow(grid) & c > 0 & c <= ncol(grid)) {
            neighbors <- c(neighbors, grid[r, c])
          }
        }
      }
    }
    return(neighbors)
  }
  
  update_grid <- function(grid, prob_map) {
    new_grid <- grid
    for (row in 1:nrow(grid)) {
      for (col in 1:ncol(grid)) {
        # Growth depends on neighbors and local probability
        if (grid[row, col] == 0) {
          neighbors <- get_neighbors(row, col, grid)
          if (sum(neighbors) > 0) {
            if (runif(1) < prob_map[row, col]) {
              new_grid[row, col] <- 1
            }
          }
        }
        # Death
        if (grid[row, col] == 1 && runif(1) < death_prob) {
          new_grid[row, col] <- 0
        }
      }
    }
    return(new_grid)
  }
  
  # --- 7. Plot functions ---
  plot_raster <- function(mat, title, legend_label) {
    df <- expand.grid(x = 1:ncol(mat), y = 1:nrow(mat))
    df$value <- as.vector(mat)
    ggplot(df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_viridis(option = "C", direction = -1) +
      labs(title = title, fill = legend_label) +
      coord_equal() +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  }
  
  plot_grid <- function(grid, prob_map, title = "") {
    df <- expand.grid(x = 1:ncol(grid), y = 1:nrow(grid))
    df$value <- as.vector(grid)
    df$prob <- as.vector(prob_map)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_raster(aes(fill = ifelse(value == 1, prob, NA))) +
      scale_fill_viridis(option = "C", na.value = "white", direction = -1) +
      labs(title = title, fill = "Growth Probability (vegetated)") +
      coord_equal() +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  }
  
  # --- 8. Run simulation ---
  plots <- list()
  cover <- numeric(num_iterations + 1)
  cover[1] <- sum(grid) / (n_rows * n_cols)
  
  plots[[1]] <- plot_grid(grid, growth_prob_map, "Iteration 0")
  
  for (i in 1:num_iterations) {
    grid <- update_grid(grid, growth_prob_map)
    cover[i + 1] <- sum(grid) / (n_rows * n_cols)
    
    if (i %% plot_interval == 0) {
      plots[[length(plots) + 1]] <- plot_grid(grid, growth_prob_map, paste("Iteration", i))
    }
  }
  
  final_plot <- wrap_plots(plots, ncol = 3)
  
  # --- 9. Vegetation cover time series ---
  cover_df <- tibble(
    iteration = 0:num_iterations,
    vegetation_cover = cover * 100
  )
  
  cover_plot <- ggplot(cover_df, aes(x = iteration, y = vegetation_cover)) +
    geom_line(color = "forestgreen", linewidth = 1.2) +
    geom_point(color = "darkgreen") +
    theme_minimal() +
    labs(
      title = "Vegetation Cover Over Time",
      x = "Iteration",
      y = "Vegetation Cover (%)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # --- 10. Diagnostic maps ---
  terrain_plot <- plot_raster(terrain, "Fractal Terrain (Elevation)", "Elevation")
  slope_plot <- plot_raster(slope, "Slope (Steepness)", "Slope")
  prob_plot <- plot_raster(growth_prob_map, "Probability of Vegetation Growth", "Growth Probability")
  
  # --- 11. Return results ---
  return(list(
    terrain_plot = terrain_plot,
    slope_plot = slope_plot,
    probability_plot = prob_plot,
    forest_evolution = final_plot,
    cover_plot = cover_plot,
    cover_data = cover_df
  ))
}
```

---

## 🧪 Example Run

```r
result <- cellular.forest.landscape.probability(
  num_iterations = 100,
  plot_interval = 20,
  frequency = 0.03,
  octaves = 5,
  seed = 123
)

# View the generated maps and results
result$terrain_plot        # Terrain (elevation)
result$slope_plot          # Slope (steepness)
result$probability_plot    # Growth probability map
result$forest_evolution    # Forest spread colored by probability
result$cover_plot          # Vegetation cover dynamics
head(result$cover_data)    # Numeric data for analysis
```

---

## 🌿 How to Interpret

| Map                  | Meaning                                                                         |
| -------------------- | ------------------------------------------------------------------------------- |
| **Terrain**          | The fractal landscape (elevation pattern)                                       |
| **Slope**            | Terrain steepness (roughness)                                                   |
| **Probability**      | Spatial map of growth probability = base_growth × (1 - elevation) × (1 - slope) |
| **Forest Evolution** | Vegetated cells in white areas, colored by local growth probability             |
| **Cover Plot**       |                                                                                 |
