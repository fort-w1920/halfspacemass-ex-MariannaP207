#### Algorithm 2
# Test algorithm for any query point x

evaluate_depth <- function(data, 
                           halfspaces, 
                           metric = c("mass", "depth")) {
  
  metric <- match.arg(metric, c("mass", "depth"))
  hm_new <- rep(0, dim(data)[1])
  n_halfspaces <- dim(halfspaces$directions)[1]
  
  # extract necessary information from <halfspaces> object
  directions <- halfspaces$directions
  split_points <- halfspaces$split_points
  left_weights <- halfspaces$left_weights
  right_weights <- halfspaces$right_weights
  
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
