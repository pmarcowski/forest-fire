# Title: Simulate and visualize forest fire dynamics
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-02-03
# Copyright (c) 2024 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script simulates the spread of fire through a forest, representing
# a simplified model of wildfire behavior. The forest is modeled as a grid where
# each cell can be in one of four states: empty, tree, burning, or burned down.
# Trees grow at a specified rate in empty cells, and fire spreads from burning trees
# to adjacent trees with a certain probability. Over time, burning trees transition
# to a burned-down state, and then to empty, mimicking the cycle of forest regeneration
# and destruction. The simulation visualizes the dynamic process of forest fires,
# including the ignition, spread, and aftermath of fires over a defined number of steps.
# Parameters such as tree growth rate, fire spread chance, and the initial density
# of trees can be adjusted to explore different fire dynamics.

# Initialize parameters
grid_size <- 100 # grid size
tree_growth_rate <- 0 # probability of tree growth in empty cell
fire_spread_chance <- 1 # probability of fire spreading to adjacent trees
initial_trees_density <- .6 # initial percentage of the grid covered with trees
tlength <- 500 # total number of simulation steps
color_map <- c("white", "green", "green4", "red", "black") # color map for visualizing forest state

# Initialize the grid
# 0 = empty cell
# 1 = green tree
# 2 = dark green tree
# 2 = burning tree
# 4 = burned down tree
forest_grid <- matrix(
  sample(
    0:4, grid_size^2,
    replace = TRUE,
    prob = c(1 - initial_trees_density, initial_trees_density * .5, initial_trees_density * .5, 0, 0)
  ),
  nrow = grid_size, ncol = grid_size
)

simulate_forest_fire <- function(tlength) {
  # Simulates and visualizes forest fire dynamics.
  # Updates forest state grid based on the current grid configuration.
  # Applies rules of tree growth, fire spread, and the aftermath
  # of a burn to each cell in the grid.
  #
  # Args:
  #   grid: A matrix representing the current state of the forest.
  #
  # Returns:
  #   A matrix representing the updated state of the forest after applying
  #   growth and fire spread rules.

  .update_forest <- function(grid) {
    # Internal function for updating forest state grid
    new_grid <- grid
    fire_spread <- FALSE # flag to indicate if fire has spread

    for (row in 1:nrow(grid)) {
      for (col in 1:ncol(grid)) {
        neighbors <- c(
          grid[row, max(col - 1, 1)],
          grid[max(row - 1, 1), col],
          grid[row, min(col + 1, ncol(grid))],
          grid[min(row + 1, nrow(grid)), col]
        )

        # Handle growth and fire spread for green and dark green trees
        if (grid[row, col] == 1 || grid[row, col] == 2) {
          if (any(neighbors == 3) && runif(1) < fire_spread_chance) { # check for burning neighbors
            new_grid[row, col] <- 3 # tree catches fire
            fire_spread <- TRUE
          }
        } else if (grid[row, col] == 0 && runif(1) < tree_growth_rate) {
          new_grid[row, col] <- sample(1:2, 1, TRUE) # new tree grows, green or dark green
        } else if (grid[row, col] == 3) {
          new_grid[row, col] <- 4 # tree burns down
        } else if (grid[row, col] == 4) {
          new_grid[row, col] <- 0 # burned down tree becomes empty
        }
      }
    }
    return(list(new_grid, fire_spread))
  }

  for (t in 1:tlength) {
    # Initiate fire at a random tree in the first step
    if (t == 1) {
      forest_grid[sample(1:grid_size, 1), sample(1:grid_size, 1)] <- 3
    }

    # Get current forest state
    forest_state <- .update_forest(forest_grid)
    forest_grid <- forest_state[[1]]
    fire_spread <- forest_state[[2]]

    if (!fire_spread) { # if fire did not spread, stop the simulation
      message("Simulation stopped early. Fire died down at time: ", t)
      break
    }

    # Visualize current forest state
    par(mfrow = c(1, 1), pty = "s")
    image(1:grid_size, 1:grid_size, t(forest_grid), col = color_map, axes = FALSE, xlab = "", ylab = "")
    box()
    title("Forest Fire Simulation")
    mtext(paste("Time:", t, "/", tlength), side = 1, line = 1)

    Sys.sleep(.1) # pause for animation effect

    # Message simulation complete
    if (t == tlength) {
      message("Simulation complete after specified time:", tlenght)
    }
  }
}

# Run simulation for specified time
simulate_forest_fire(tlength)
