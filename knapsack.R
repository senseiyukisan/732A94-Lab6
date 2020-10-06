set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_brute_force <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(ncol==2)
  stopifnot(any(x[,c("v","w")] > 0))
  
  # use the combn function to calculate permutations
  
}

# Dynamic programming

# // Input:
#   // Values (stored in array v)
# // Weights (stored in array w)
# // Number of distinct items (n)
# // Knapsack capacity (W)
# // NOTE: The array "v" and array "w" are assumed to store all relevant values starting at index 1.
# 
# for j from 0 to W do:
#   m[0, j] := 0
# 
# for i from 1 to n do:
#   for j from 0 to W do:
#   if w[i] > j then:
#   m[i, j] := m[i-1, j]
# else:
#   m[i, j] := max(m[i-1, j], m[i-1, j-w[i]] + v[i])




# Greedy knapsack

greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x)==2)
  stopifnot(any(x[,c("v","w")] > 0))
  
  # Add specific volume vector
  x$nu <- x[["v"]]/x[["w"]]
  x <- x[order(x$nu), ]
  
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