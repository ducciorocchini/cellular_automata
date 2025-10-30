library(ambient)
library(tidyverse)
library(viridis)

# --- Parameters ---
size <- 100
timesteps <- 50
p_disturb <- 0.01  # disturbance probability

# --- Load or regenerate terrain ---
terrain <- noise_perlin(c(size, size), frequency = 0.05, octaves = 5)
suitability <- (terrain - min(terrain)) / (max(terrain) - min(terrain))  # normalize 0-1

# --- Initialize forest states ---
set.seed(123)
forest <- matrix(0, size, size)
forest[sample(1:(size^2), 100)] <- 1  # small number of initial colonizations

# --- Neighborhood function (Moore) ---
neighbors <- function(mat, i, j) {
  rows <- max(1, i-1):min(size, i+1)
  cols <- max(1, j-1):min(size, j+1)
  mat[rows, cols]
}

# --- Update rule ---
update_forest <- function(forest, suitability) {
  new_forest <- forest
  for (i in 1:size) {
    for (j in 1:size) {
      state <- forest[i, j]
      local_suit <- suitability[i, j]
      neigh <- neighbors(forest, i, j)
      forested_neigh <- any(neigh > 0)
      
      if (state == 0 && forested_neigh && runif(1) < local_suit * 0.3) {
        new_forest[i, j] <- 1  # colonization
      } else if (state == 1) {
        new_forest[i, j] <- 2  # succession
      } else if (state == 2 && runif(1) < p_disturb) {
        new_forest[i, j] <- 0  # disturbance
      }
    }
  }
  new_forest
}

# --- Run simulation ---
plots <- list()
for (t in 1:timesteps) {
  forest <- update_forest(forest, suitability)
  if (t %% 10 == 0) {
    df <- expand.grid(x = 1:size, y = 1:size)
    df$state <- as.vector(forest)
    plots[[length(plots) + 1]] <- ggplot(df, aes(x, y, fill = factor(state))) +
      geom_tile() +
      scale_fill_viridis_d(option = "E", labels = c("Empty", "Young", "Mature")) +
      coord_equal() +
      labs(title = paste("Forest Dynamics — Time step", t), fill = "State") +
      theme_void()
  }
}
# Example: Display last snapshot
print(plots[[length(plots)]])
