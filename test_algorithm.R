#### Algorithm 2
# Test algorithm for query points x

evaluate_depth <- function(data, 
                           halfspaces, 
                           metric = c("mass", "depth")) {
  
  metric <- match.arg(metric, c("mass", "depth"))
  checkmate::assert_character(metric, len = 1)
  
  # extract necessary information from the <halfspaces> object
  n_halfspaces <- dim(halfspaces$directions)[1]
  directions <- halfspaces$directions
  trained_projections <- halfspaces$projections
  split_points <- halfspaces$split_points
  left_weights <- halfspaces$left_weights
  right_weights <- halfspaces$right_weights
  
  if (metric == "mass") {
    hm_new <- get_halfspace_mass(data, n_halfspaces, 
                                 directions, split_points,
                                 left_weights, right_weights)
    return(hm_new)
  } 
  
  if (metric == "depth") {
    hm_new <- rep(Inf, dim(data)[1])
    for (i in seq_len(n_halfspaces)) {
      # project all new points on a random direction
      data_projected <- project_data(directions[i, ], data)
      # take one projection which was trained before
      projection_i <- trained_projections[i, ]
      # update halfspace mass every time when less points on each side of 
      # the projection are found
      hm_new <- get_tukey_depth(hm_new, data_projected, projection_i)
    }
    return(hm_new)
  }
}

get_halfspace_mass <- function(data, n_halfspaces,
                               directions, split_points,
                               left_weights, right_weights) {
  hm_new <- rep(0, dim(data)[1])
  for (i in seq_len(n_halfspaces)) {
    data_projected <- project_data(directions[i, ], data)
    # for each point in data set
    # define on which side from the current split point it lies
    on_left <- (data_projected < split_points[i])
    # add left weights if data point is on the left from the split point
    hm_new[on_left] <- hm_new[on_left] + left_weights[i]
    # otherwise add right weights
    hm_new[!on_left] <- hm_new[!on_left] + right_weights[i]
  }
  hm_new / n_halfspaces
}


get_tukey_depth <- function(hm_new, data_projected, projection_i) {
  for (k in seq_len(length(data_projected))) {
    # find minimum of new data points which are on each side of the projection
    min_points <- sum(projection_i > data_projected[k])
    hm_new[k] <- min(hm_new[k], min_points)
  }
  hm_new
}
