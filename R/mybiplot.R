#' @title mybiplot
#'
#' @description a function which assigns each value in a vector as "in", "outu", or "outl" based on thier distance from the mean
#'
#' @param x a numeric vector
#' @param k the number of standard deviations to compare with
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{mybiplot(x = mpg, k=1)}
mybiplot <- function(x = double(), k = double()){

  stopifnot(is.double(x))
  stopifnot(is.double(k))

  k <- c(k)
  z <- scale(x)

  y <- c()

  for (i in z) {
    for (k in k) {
      if(i >= -k & i <= k){
        y <- append(y, "in")
      }
      else{
        if(i > k){
          y <- append(y, "outu")
        }
        else{
          y <- append(y, "outl")
        }
      }
    }
  }
  inout <- matrix(y, nrow = length(x), ncol = 1)

  l <- list(x = x, y = inout, k = k)
  structure(.Data =l, class = "math4753biplot")
}

