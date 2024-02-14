#' @title summary.math4753biplot
#'
#' @description produces a summary which includes mean, median, and IQR
#'
#' @param ... additional options
#' @param x a numeric vector
#'
#' @return a summary of mean, median, IQR, n grouped by their .data$inout status
#' @importFrom dplyr n group_by summarize
#' @importFrom stats median IQR
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{summarymath4753biplot(x = mpg)}
#' @export
summary.math4753biplot <- function(x, ...){

  df <- data.frame(x = x$x, inout = x$y)

  s <- df %>%
    group_by(.data$inout) %>%
    summarize(mean = mean(x), median = median(x), IQR = IQR(x), n = n())

  print(s)
}

