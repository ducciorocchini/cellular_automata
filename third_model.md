Perfect — this is where things get interesting 🌿🏔️

Let’s combine your two ideas:

* **The fractal landscape model** (`cellular.landscape`) gives us a heterogeneous environment (e.g., “elevation” or “soil moisture”).
* **The forest spread model** (`cellular.forest`) simulates vegetation growth and death.

Now we’ll **force the forest dynamics to depend on the landscape**, so growth probability varies with the terrain.
For instance, we can assume:

* **Low elevation (valleys)** → higher vegetation growth probability.
* **High elevation (mountain tops)** → lower vegetation growth probability.

---

## 🌲 Combined Model: Forest Spread on a Fractal Landscape

Here’s the integrated model function:

```r
cellular.forest.landscape <- function(num_iterations = 50, plot_interval = 10,
                                      n_rows = 100, n_cols = 100,
                                      frequency = 0.05, octaves = 5,
                                      base_growth = 0.1, death_prob = 0.02,
                                      seed = NULL) {
  # Load libraries
  library(ggplot2)
  library(patchwork)
  library(ambient)
  library(tidyverse)
  library(viridis)
  
  # --- 1. Generate fractal landscape ---
  if (!is.null(seed)) set.seed(seed)
  terrain <- noise_perlin(c(n_rows, n_cols), frequency = frequency, octaves = octaves)
  
  # Normalize terrain to [0, 1] for easier interpretation (0 = low, 1 = high)
  terrain <- (terrain - min(terrain)) / (max(terrain) - min(terrain))
  
  # --- 2. Initialize forest grid ---
  grid <- matrix(0, nrow = n_rows, ncol = n_cols)
  grid[sample(1:(n_rows * n_cols), size = 200)] <- 1  # start with 200 vegetated cells
  
  # --- 3. Helper functions ---
  
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
        # Growth (depends on neighbors and terrain)
        if (grid[row, col] == 0) {
          neighbors <- get_neighbors(row, col, grid)
          if (sum(neighbors) > 0) {
            # Growth probability decreases with elevation
            terrain_factor <- 1 - terrain[row, col]
            growth_prob <- base_growth * terrain_factor
            if (runif(1) < growth_prob) {
              new_grid[row, col] <- 1
            }
          }
        }
        # Death (constant probability)
        if (grid[row, col] == 1) {
          if (runif(1) < death_prob) {
            new_grid[row, col] <- 0
          }
        }
      }
    }
    return(new_grid)
  }
  
  # --- 4. Plotting function ---
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
  plots[[1]] <- plot_grid(grid, terrain, "Iteration 0")
  
  for (i in 1:num_iterations) {
    grid <- update_grid(grid, terrain)
    if (i %% plot_interval == 0) {
      plots[[length(plots) + 1]] <- plot_grid(grid, terrain, paste("Iteration", i))
    }
  }
  
  # --- 6. Combine results ---
  final_plot <- wrap_plots(plots, ncol = 3)
  final_plot
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
result
```

You’ll see:

* The **fractal terrain** underneath (color = elevation).
* Vegetation grows preferentially in **low-lying regions (valleys)** where `terrain` is low (growth probability higher).
* Over time, vegetation spreads but remains structured by the topography.

---

### 🌍 Model Logic Summary

| Process   | Description                             | Depends on                 |
| --------- | --------------------------------------- | -------------------------- |
| Growth    | Empty cells next to vegetation can grow | `terrain` (lower = faster) |
| Death     | Random 2% mortality                     | constant                   |
| Landscape | Perlin fractal noise                    | `frequency`, `octaves`     |

---

### 💡 Optional Extensions

If you’d like to expand:

1. **Make growth depend on both neighbors and terrain moisture** (e.g., `terrain_factor^2`).
2. **Track vegetation cover** through time and plot the curve.
3. **Return both the final grid and terrain matrix** for post-analysis.

---

Would you like me to modify this version so it **returns both the plots and a time series of total vegetation cover** (so you can analyze how vegetation stabilizes across the terrain)?
