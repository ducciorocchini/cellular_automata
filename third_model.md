Excellent 👍 — adding **vegetation cover tracking** will let you analyze how the forest reaches equilibrium or fluctuates through time across your fractal landscape.

Below is the improved version of the integrated model, which now:

* Returns **both** the final visualization and
* A **data frame of vegetation cover (%) over iterations**

---

## 🌳 `cellular.forest.landscape()` – with Vegetation Cover Tracking

```r
cellular.forest.landscape <- function(num_iterations = 50, plot_interval = 10,
                                      n_rows = 100, n_cols = 100,
                                      frequency = 0.05, octaves = 5,
                                      base_growth = 0.1, death_prob = 0.02,
                                      seed = NULL) {
  # --- 1. Load libraries ---
  library(ggplot2)
  library(patchwork)
  library(ambient)
  library(tidyverse)
  library(viridis)
  
  # --- 2. Generate fractal landscape ---
  if (!is.null(seed)) set.seed(seed)
  terrain <- noise_perlin(c(n_rows, n_cols), frequency = frequency, octaves = octaves)
  
  # Normalize terrain to [0, 1]
  terrain <- (terrain - min(terrain)) / (max(terrain) - min(terrain))
  
  # --- 3. Initialize forest grid ---
  grid <- matrix(0, nrow = n_rows, ncol = n_cols)
  grid[sample(1:(n_rows * n_cols), size = 200)] <- 1  # Start with 200 vegetated cells
  
  # --- 4. Helper functions ---
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
  
  update_grid <- function(grid, terrain) {
    new_grid <- grid
    for (row in 1:nrow(grid)) {
      for (col in 1:ncol(grid)) {
        # Growth depends on neighbors and terrain
        if (grid[row, col] == 0) {
          neighbors <- get_neighbors(row, col, grid)
          if (sum(neighbors) > 0) {
            terrain_factor <- 1 - terrain[row, col]  # low terrain = high growth
            growth_prob <- base_growth * terrain_factor
            if (runif(1) < growth_prob) {
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
  
  plot_grid <- function(grid, terrain, title = "") {
    df <- expand.grid(x = 1:ncol(grid), y = 1:nrow(grid))
    df$value <- as.vector(grid)
    df$terrain <- as.vector(terrain)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_raster(aes(fill = ifelse(value == 1, terrain, NA))) +
      scale_fill_viridis(option = "D", na.value = "white", direction = -1) +
      labs(title = title, fill = "Elevation (vegetated)") +
      coord_equal() +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  }
  
  # --- 5. Run the simulation ---
  plots <- list()
  cover <- numeric(num_iterations + 1)
  cover[1] <- sum(grid) / (n_rows * n_cols)
  
  plots[[1]] <- plot_grid(grid, terrain, "Iteration 0")
  
  for (i in 1:num_iterations) {
    grid <- update_grid(grid, terrain)
    cover[i + 1] <- sum(grid) / (n_rows * n_cols)
    
    if (i %% plot_interval == 0) {
      plots[[length(plots) + 1]] <- plot_grid(grid, terrain, paste("Iteration", i))
    }
  }
  
  final_plot <- wrap_plots(plots, ncol = 3)
  
  # --- 6. Create vegetation cover data frame ---
  cover_df <- tibble(
    iteration = 0:num_iterations,
    vegetation_cover = cover * 100  # convert to %
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
  
  # --- 7. Return both outputs ---
  return(list(
    landscape_plots = final_plot,
    cover_plot = cover_plot,
    cover_data = cover_df
  ))
}
```

---

### 🧪 Example Use

```r
result <- cellular.forest.landscape(
  num_iterations = 100,
  plot_interval = 20,
  frequency = 0.03,
  octaves = 5,
  seed = 123
)

# Display spatial evolution
result$landscape_plots

# Display vegetation cover through time
result$cover_plot

# Inspect data
head(result$cover_data)
```

---

### 🌿 What You’ll Get

1. **`result$landscape_plots`** → shows the spatial pattern of forest spread across the fractal landscape.
2. **`result$cover_plot`** → a line graph showing vegetation cover (%) through iterations.
3. **`result$cover_data`** → a data frame (`iteration`, `vegetation_cover`) for analysis or export.
