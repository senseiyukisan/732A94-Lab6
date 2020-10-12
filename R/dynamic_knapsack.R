#' @title Dynamic Programming Knapsack
#' @description Implementation of knapsack problem using dynamic programming.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' 
#' @param x data.frame 
#' @param W integer (maximum weight fitting in the knapsack)
#' 
#' @return Returns a list object containing value and elements information.
#' @export
knapsack_dynamic <- function(x, W) {
  stopifnot(is.numeric(W))
  if(!"w" %in% colnames(x))
  {
    stop("Missing column w in input data");
  }
  if(!"v" %in% colnames(x))
  {
    stop("Missing column v in input data");
  }
  if(W <= 0)
  {
    stop("W must be positive numeric!");
  }
  
  values = x$v
  weights = x$w
  m = matrix(0, nrow=nrow(x)+1, ncol=W+1)
  
  for (i in 1:nrow(x)) {
    for (j in 1:W+1) {
      if (weights[i] > j) {
        m[i+1, j] = m[i, j]
      }
      else {
        m[i+1, j] = max(m[i,j], m[i, j-weights[i]] + values[i])
      }
    }
  }
  
  output_list = list("value"=0, "elements"=c())
  output_list[["value"]]= round(m[nrow(m),ncol(m)])
  
  items = c()
  row_num = nrow(x)+1
  col_num = W+1
  while(row_num > 1){
    if(m[row_num-1, col_num] < m[row_num, col_num] ){
      items = append(items, row_num-1)
      row_num = row_num - 1
      col_num = col_num - weights[row_num]
    }else{
      row_num = row_num - 1
    }
  }
  
  output_list[["elements"]] = sort(items, decreasing=FALSE)
  
  return(output_list)
}

# test_speed <- function(number_objects) {
#   start_time = Sys.time()
#   knapsack_dynamic(x = knapsack_objects[1:number_objects,], W = 3500)
#   end_time = Sys.time()
#   time_passed = end_time-start_time
#   cat("Calculation took ", time_passed, " seconds.\n")
#   return(time_passed)
# }
# 
# speed_vector = c()
# for (i in 1:10) {
#   speed_vector = c(speed_vector, test_speed(500))
# }
# print(mean(speed_vector))