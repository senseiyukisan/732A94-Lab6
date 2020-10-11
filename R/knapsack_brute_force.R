brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot(is.numeric(W))
  if(!"w" %in% colnames(x))
  {
    stop("Missing column w in input data");
  }
  if(!"v" %in% colnames(x))
  {
    stop("Missing column v in input data");
  }
  values = x$v
  weights = x$w

  index_combs = list()
  weight_combs = list()
  value_combs = list()
  
  if(parallel == FALSE){
    # Iterate over each possible number of element combinations (1-n) and fill list
    for(i in 1:nrow(x)){
      # Using paste function to sum index strings together
      index_combs[[i]] = combn(rownames(x), i, paste, collapse=",")
      # Using sum function to sum weights and values
      weight_combs[[i]] = combn(weights, i, sum)
      value_combs[[i]] = combn(values, i, sum)
    }
    
  }else{
      cores <- parallel::detectCores()
      cl <- parallel::makeCluster(cores, type = "PSOCK")
      parallel::clusterExport(cl=cl, varlist=c("x"), envir=environment())
      index_combs <- parallel::parLapply(cl, 1:length(x$v), function(i){
        combn(rownames(x), m = i, paste, collapse=",")
      })
      weight_combs <- parallel::parLapply(cl, 1:length(x$w),function(i){
        combn(weights, m = i, sum) 
      })
      value_combs <- parallel::parLapply(cl, 1:length(x$v), function(i){
        combn(values, m = i, sum) 
      })
      parallel::stopCluster(cl)
  }
  
  # Transform list to vector for index, weight and value combinations
  index_combs_v = unlist(index_combs)
  weight_combs_v = unlist(weight_combs)
  value_combs_v = unlist(value_combs)
  
  # Create index mask to figure out which weights are below threshold
  mask_weight = which(weight_combs_v <= W)
  filtered_value_combs = value_combs_v[mask_weight]
  value = max(filtered_value_combs)
  
  # Create index mask to figure out which value combination is the optimum
  # Unlist and transform to create valid output format for elements
  mask_values = which(value_combs_v == value)
  elements = as.numeric(unlist(strsplit(index_combs_v[mask_values],",")))  
  
  
  return(list(value=round(value), elements=elements))
}  
