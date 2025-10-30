cellular.forest <- function(num_iterations = 50, plot_interval = 10){
  
  # Load necessary libraries
  library(ggplot2)
  library(patchwork)

  # Set up the environment (grid)
  n_rows <- 50  # Number of rows in the grid
  n_cols <- 50  # Number of columns in the grid
  grid <- matrix(0, nrow = n_rows, ncol = n_cols)  # Create an empty grid

  # Function to initialize the grid with random vegetation
  initialize_grid <- function(n_rows, n_cols) {
    grid <- matrix(0, nrow = n_rows, ncol = n_cols)  # Empty grid
    set.seed(NULL)  # Remove the fixed seed to introduce randomness
    grid[sample(1:(n_rows * n_cols), size = 100)] <- 1  # Random vegetation spread
    return(grid)
  }

  # Function to get neighbors of a given cell
  get_neighbors <- function(row, col, grid) {
    neighbors <- c()
    for (i in -1:1) {
      for (j in -1:1) {
        if (!(i == 0 & j == 0)) {  # Don't include the cell itself
          neighbor_row <- row + i
          neighbor_col <- col + j
          if (neighbor_row > 0 & neighbor_row <= nrow(grid) & neighbor_col > 0 & neighbor_col <= ncol(grid)) {
            neighbors <- c(neighbors, grid[neighbor_row, neighbor_col])
          }
        }
      }
    }
    return(neighbors)
  }

  # Function to update the grid
  update_grid <- function(grid) {
    new_grid <- grid
    for (row in 1:nrow(grid)) {
      for (col in 1:ncol(grid)) {
        # Check if a cell is empty (0) and if it should grow vegetation
        if (grid[row, col] == 0) {
          neighbors <- get_neighbors(row, col, grid)
          if (sum(neighbors) > 0) {  # If there's at least one neighbor with vegetation
            if (runif(1) < 0.1) {  # 10% chance of vegetation growth
              new_grid[row, col] <- 1
            }
          }
        }
        # If a cell has vegetation, it can be removed (simulating environmental stress)
        if (grid[row, col] == 1) {
          if (runif(1) < 0.02) {  # 2% chance of vegetation dying
            new_grid[row, col] <- 0
          }
        }
      }
    }
    return(new_grid)
  }

  # Function to visualize the grid
  plot_grid <- function(grid, title = "Vegetation Growth") {
    df <- expand.grid(x = 1:ncol(grid), y = 1:nrow(grid))
    df$value <- as.vector(grid)
    
    ggplot(df, aes(x = x, y = y, fill = factor(value))) +
      geom_tile() +
      scale_fill_manual(values = c("white", "green")) +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(title = title, fill = "Vegetation") +
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Run the model for the specified number of iterations and store plots
  plots <- list()

  # Initialize grid randomly
  grid <- initialize_grid(n_rows, n_cols)

  # Initial state plot
  plots[[1]] <- plot_grid(grid, "Iteration 0")

  # Store plots for every 'plot_interval' iterations
  for (i in 1:num_iterations) {
    grid <- update_grid(grid)
    
    # Store the plot every 'plot_interval' iterations
    if (i %% plot_interval == 0) {
      plots[[length(plots) + 1]] <- plot_grid(grid, paste("Iteration", i))
    }
  }

  # Arrange the plots in a multi-frame layout with 3 plots per row
  final_plot <- wrap_plots(plots, ncol = 3)  # Display in 3 columns
  final_plot
}
