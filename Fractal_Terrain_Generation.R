# Load packages
library(ambient)
library(tidyverse)
library(viridis)

# Generate fractal noise
terrain <- noise_perlin(c(100, 100), frequency = 0.05, octaves = 5)

# Convert matrix to tidy format
terrain_df <- as.data.frame(terrain) |>
  rownames_to_column("y") |>
  pivot_longer(-y, names_to = "x", values_to = "value") |>
  mutate(
    x = as.numeric(str_remove(x, "V")),
    y = as.numeric(y)
  )

# Plot with reversed viridis legend
ggplot(terrain_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis(option = "D", direction = -1) +  # Reverses the legend colors
  coord_equal() +
  labs(
    title = "Fractal Landscape via Perlin Noise",
    x = NULL,
    y = NULL,
    fill = "Height"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
