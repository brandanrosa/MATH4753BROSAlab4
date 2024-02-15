#' @title print.math4753biplot
#'
#' @description takes a numeric vector and plots a histogram using ggploit2
#'
#' @param x a numeric vector
#' @param ... passes extra options into the function
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#' @return a histogram
#' @export print.math4753biplot
#'
#' @export
#'
#' @examples
#' \dontrun{print.math4753biplot(x = mpg)}
print.math4753biplot <- function(x, ...){
  inout <- NULL

  df <- data.frame(x = x$x, inout = x$y, ...)

  p <- df %>%
    ggplot(aes(x = x, fill = inout)) +
    geom_histogram(binwidth = 0.4) +
    labs(title = paste0("Biplot,", " k=", x$k))

  print(p)
}



