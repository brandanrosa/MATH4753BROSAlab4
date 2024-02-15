#' @title summary.math4753biplot
#'
#' @description produces a summary which includes mean, median, and IQR
#'
#' @param ... additional options
#' @param object a numeric vector
#'
#' @importFrom dplyr n group_by summarize %>%
#' @importFrom stats median IQR
#' @importFrom rlang .data
#'
#' @return a summary of mean, median, IQR, n grouped by their .data$inout status
#' @export summary.math4753biplot
#'
#' @export
#'
#' @examples
#' \dontrun{summarymath4753biplot(x = mpg)}
summary.math4753biplot <- function(object, ...){
  x <- NULL
  inout <- NULL

  df <- data.frame(x = x$x, inout = x$y, ...)

  s <- df %>%
    group_by(inout) %>%
    summarize(mean = mean(x), median = median(x), IQR = IQR(x), n = n())

  print(s)
}

