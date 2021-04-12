
#' Count valid data
#'
#' @param x a numeric vector
#'
#' @return total non-missing values of x.
#'
nvalid <- function(x) {
  # if(all(is.na(x))) return(0)
  sum(!is.na(x))
}


mean_wise <- function(x){
  if(all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}