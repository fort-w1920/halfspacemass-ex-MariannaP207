#### Algorithm 1 
# Train halfspace mass

train_halfspace_mass <- function(
           data,                # data set to train on
           n_halfspace = 2000,  # number of simulated halfspaces
           subsample = 5,       # subsample of data set; D(i) in Chen et al.
           scope = 1,           # size parameter; lambda in Chen et al.
           seed) {
    
    data_dim <- dim(data)[2]
    
    # create empty matrices and vectors which will be filled with output data
    directions <- matrix(nrow = n_halfspace, ncol = data_dim)
    s_i_vector <- 
          left <-
         right <- vector(mode = "numeric", length = n_halfspace)

    set.seed(seed)
    
    for (i in seq_len(n_halfspace)) {
    
    direction <- generate_direction(data_dim) # random direction
    sampled_data <- get_subsample(data, subsample) 
    projected_subsample <- project_data(direction, sampled_data) # projection
    s_i <- get_s_i(projected_subsample, scope)
    fraction_left <- sum(projected_subsample < s_i) / subsample 
    fraction_right <- sum(projected_subsample >= s_i) / subsample
 
    directions[i, ] <- direction
    s_i_vector[i] <- s_i
    left[i] <- fraction_left
    right[i] <- fraction_right
    
    }

    HM_list <- list(
      "directions" = directions,
      "s_i_points" = s_i_vector,
      "left" = left,
      "right" = right
    )
    HM_list
  }

# generate random direction by random sampling from standard normal distribution
generate_direction <- function(dimension) {
  direction <- rnorm(dimension) 
  normalized_direction <- direction / sqrt(sum(direction ^ 2))
  normalized_direction
}

# get subset of data of size <subsample> without replacement
get_subsample <- function(data, subsample) {
  sampled_rows <- sample(nrow(data), subsample, replace = FALSE)
  data[sampled_rows, , drop = FALSE]
}

# project given subsample on the direction vector
project_data <- function(direction, sampled_data) {
  projected_data <- as.matrix(sampled_data) %*% direction
  projected_data
}

# calculate critical points of projected subsample
# and randomly select s(i) based on these critical points and scope
get_s_i <- function(projected_subsample, scope) {
  # critical points
  max_point <- max(projected_subsample)
  min_point <- min(projected_subsample)
  mid_point <- (max_point + min_point) / 2
  # random s(i)
  span <- max_point - min_point
  deviation <- (scope * span) / 2
  left_bound <- mid_point - deviation
  right_bound <- mid_point + deviation
  s_i <- runif(1, min = left_bound, max = right_bound)
  s_i
}
