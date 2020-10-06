brute_force_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x)==2)
  stopifnot(any(x[,c("v","w")] > 0))
  
  # Creating necessary vectors
  value <- 0
  value_temp <- c()
  value_temp[1] <- 0
  elements <- c()
  elements_temp <- c()
  elements_temp[1] <- 0
  weight <- 0
  
  # use the combn function to calculate permutations
  #smallest permutations should start at element 2
  i <- 2
  
  while(i <= nrow(x)){
    #get all combinations for weights and values in matrix of 2 rows
    weight_comb <- combn(x[,"w"], i)
    value_comb <- combn(x[,"v"], i)
    #sum all columns to get the weight and value sums
    #returns vectors
    sum_weight_comb <- colSums(weight_comb)
    sum_value_comb <- colSums(value_comb)
    
    #find indices of correct weight combinations
    indices <- which(sum_weight_comb <= W)
    
    #finds and saves the max value and elements
    value <- max(sum_value_comb[indices])
    index_max <- which.max(sum_value_comb[indices])
    
    elements_weight_temp <- weight_comb[, index_max]
    
    for(j in 1:i){
      elements[j] <- which(x[,"w"] == elements_weight_temp[j])
      j <- j + 1
    }
    i <- i + 1
  }
  
  
  result <- list("value" = value, "elements" = elements)
  return(result)
  
}