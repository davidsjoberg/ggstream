library(KernSmooth)
library(tidyverse)
set.seed(sample(1:100000, 1))
x <- 1:10
y <- sample(1:100, 10, replace = T)

make_smooth <- function(x, y, force_to_zero = FALSE, bw = NULL, spans_x_extra = 6){
  .x <- x[!is.na(x)]
  .y <- y[!is.na(y)]
  min_x <- min(.x)
  max_x <- min(.y)
  if(is.null(bw)) {
    bwidth = KernSmooth::dpill(.x,.y)
  }
  
  if(force_to_zero){
      times <- 1:spans_x_extra
    for(t in times) {
      .x <- c(min_x - t * bwidth, .x, max_x + t * bwidth)
      .y <- c(0, .y, 0)
    }
  }
  fit <- KernSmooth::locpoly(.x, .y, bandwidth = bwidth, gridsize = 10000)
  filter_vector <- !(fit$y == 0 & (fit$x < min_x | fit$x > max_x))
  fit$x <- fit$x[filter_vector]
  fit$y <- fit$y[filter_vector]
  
  tibble(x = fit$x, y = fit$y)
}

plt_df <- make_smooth(x, y, force_to_zero = T)

ggplot(plt_df,
       aes(x = x,
           y = y)) +
  geom_line() +
  geom_point(data = tibble(x, y))
