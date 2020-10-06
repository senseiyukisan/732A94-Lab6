# Greedy knapsack

greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x)==2)
  stopifnot(any(x[,c("v","w")] > 0))
  
  # Add specific volume vector
  x$nu <- x[["v"]]/x[["w"]]
  x <- x[order(x$nu, decreasing = TRUE), ]
  
  # Creating necessary vectors
  value <- 0
  elements <- c()
  weight <- 0
  
  #counter
  i <- 1
  
  #knapsack_greedy
  while(weight + x[i,"w"] < W){
    weight <- weight + x[i, "w"]
    value <- value + x[i, "v"]
    elements[i] <- as.numeric(rownames(x[i,]))
    i <- i+1
  }
  result <- list("value" = value, "elements" = elements)
  return(result)
}