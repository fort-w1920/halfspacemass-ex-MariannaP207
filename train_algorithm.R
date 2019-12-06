#### Algorithm 1
# Train halfspace mass

train_depth <- function(
  data,              # data set to train on
  n_halfspace = 1L,  # number of simulated halfspaces; t in Chen et al.
  subsample = 1L,    # subsample fraction of data set; D(i) in Chen et al.
  scope = 1L,        # size parameter; lambda in Chen et al.
  seed = 1) {
  check_inputs(data, n_halfspace, subsample, scope, seed)
  
  data_dim <- dim(data)[2]
  n_subsample <- ceiling(nrow(data) * subsample)
  
  # create empty matrix for generated random directions and projected data
  directions <- matrix(nrow = n_halfspace, ncol = data_dim)
  projections <- matrix(nrow = n_halfspace, ncol = n_subsample)
  # create empty vectors for critical points on the generated directions
  split_vector <- left <- right <-
    vector(mode = "numeric", length = n_halfspace)
  
  set.seed(seed)
  
  for (i in seq_len(n_halfspace)) {
    direction <- generate_direction(data_dim)
    sampled_data <- get_subsample(data, n_subsample)
    projected_subsample <- project_data(direction, sampled_data)
    split <- get_split(projected_subsample, scope)
    fraction_left <- sum(projected_subsample < split) / n_subsample
    fraction_right <- sum(projected_subsample >= split) / n_subsample
    
    directions[i, ] <- direction
    projections[i, ] <- projected_subsample
    split_vector[i] <- split
    left[i] <- fraction_left
    right[i] <- fraction_right
  }
  
  HM_list <- list(
    "directions" = directions,
    "projections" = projections,
    "split_points" = split_vector,
    "left_weights" = left,
    "right_weights" = right
  )
  HM_list
}

check_inputs <- function(data, n_halfspace, subsample, scope, seed) {
  library(checkmate)
  assert(check_data_frame(data,
                          types = c("numeric", "integer"),
                          any.missing = FALSE, min.rows = 1, min.cols = 1),
         check_matrix(data,
                          mode = c("numeric", "integerish"),
                          any.missing = FALSE, min.rows = 1, min.cols = 1),
         combine = "or")
  assert_count(n_halfspace, positive = TRUE)
  assert_numeric(subsample, lower = .Machine$double.eps, 
                 upper = 1, len = 1, any.missing = FALSE)
  assert_numeric(scope, lower = 1)
  assert_count(seed, positive = TRUE)
}


# generate direction by random sampling from standard normal distribution
# normalize the direction for the next projection on it
generate_direction <- function(dimension) {
  direction <- rnorm(dimension)
  normalized_direction <- direction / sqrt(sum(direction^2))
  normalized_direction
}

# get fraction of data of size <subsample> without replacement
get_subsample <- function(data, n_subsample) {
  sampled_rows <- sample(nrow(data), n_subsample, replace = FALSE)
  data[sampled_rows, , drop = FALSE]
}

# project given subsample data on the direction vector
project_data <- function(direction, data) {
  #checkmate::assert_true(all.equal(sum(direction^2), 1L, 
  #                                 tolerance = .Machine$double.eps))
  projected_data <- as.matrix(data) %*% direction
  projected_data
}

# calculate critical points of projected subsample
# and randomly select s(i) based on these critical points and scope
get_split <- function(projected_subsample, scope) {
  # calculate critical points
  max_point <- max(projected_subsample)
  min_point <- min(projected_subsample)
  mid_point <- (max_point + min_point) / 2
  # calculate and return a random split point
  span <- max_point - min_point
  left_bound <- mid_point - scope * span * 0.5
  right_bound <- mid_point + scope * span * 0.5
  split <- runif(1, min = left_bound, max = right_bound)
  split
}
