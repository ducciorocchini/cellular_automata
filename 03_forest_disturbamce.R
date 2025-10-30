library(ambient)
library(tidyverse)
library(viridis)

# --- Parameters ---
size <- 100
timesteps <- 80
base_colonization <- 0.3
base_disturb <- 0.01

# --- Terrain and suitability ---
terrain <- noise_perlin(c(size, size), frequency = 0.05, octaves = 5)
suitability <- (terrain - min(terrain)) / (max(terrain) - min(terrain))

# --- Initialize forest states ---
set.seed(42)
forest <- matrix(0, size, size)
forest[sample(1:(size^2), 200)] <- 1  # initial seedlings

# --- Climate forcing functions ---
temperature <- seq(0, 1, length.out = timesteps)              # warming trend
moisture <- 0.5 + 0.5 * sin(seq(0, 4 * pi, length.out = timesteps))  # periodic oscillation

# --- Helper: neighborhood function ---
neighbors <- function(mat, i, j) {
  rows <- max(1, i-1):min(size, i+1)
  cols <- max(1, j-1):min(size, j+1)
  mat[rows, cols]
}

# --- Update function with climate dependence ---
update_forest <- function(forest, suitability, temp, moist) {
  new_forest <- forest
  for (i in 1:size) {
    for (j in 1:size) {
      state <- forest[i, j]
      neigh <- neighbors(forest, i, j)
      forested_neigh <- any(neigh > 0)
      local_suit <- suitability[i, j]
      
      # Climate-modulated parameters
      p_colonize <- base_colonization * local_suit * (1 - 0.6 * temp) * (0.5 + 0.5 * moist)
      p_disturb <- base_disturb * (1 + 3 * temp) * (1 - 0.5 * moist)
      
      if (state == 0 && forested_neigh && runif(1) < p_colonize) {
        new_forest[i, j] <- 1
      } else if (state == 1) {
        new_forest[i, j] <- 2
      } else if (state == 2 && runif(1) < p_disturb) {
        new_forest[i, j] <- 0
      }
    }
  }
  new_forest
}

# --- Simulation ---
snapshots <- list()
for (t in 1:timesteps) {
  forest <- update_forest(forest, suitability, temperature[t], moisture[t])
  if (t %% 20 == 0) {
    df <- expand.grid(x = 1:size, y = 1:size)
    df$state <- as.vector(forest)
    snapshots[[length(snapshots) + 1]] <- ggplot(df, aes(x, y, fill = factor(state))) +
      geom_tile() +
      scale_fill_viridis_d(option = "E", labels = c("Empty", "Young", "Mature")) +
      coord_equal() +
      labs(title = paste("Climate-Driven Forest Dynamics — Step", t),
           fill = "State") +
      theme_void()
  }
}
# Example output
print(snapshots[[length(snapshots)]])
