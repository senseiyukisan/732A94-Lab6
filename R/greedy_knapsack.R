#' @title Greedy Knapsack
#' @description Implementation of knapsack problem using greedy algorithm.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' 
#' @param x data.frame 
#' @param W integer (maximum weight fitting in the knapsack)
#' 
#' @return Returns a list object containing value and elements information.
#' @export
greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x)==2)
  stopifnot(any(x[,c("v","w")] > 0))
  if(W <= 0)
  {
    stop("W must be positive numeric!");
  }
  
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