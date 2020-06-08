calc_y_offsets <- function(df) {

  df$ymin <- cumsum(-1 * c(-sum(df$y) / 2, df$y))[-1]

  df$ymax <- df$ymin + df$y

  return(df)

}

extend_data <- function(df) {

  df <- df[order(df$x), ]

  data.frame( x = c(df$x, rev(df$x)),
              y = c(df$ymin, rev(df$ymax)),
              group = unique(df$group))

}

points_on_line <- function(.x, x1, y1, x2, y2) {
  .slope <- (y2 - y1) / (x2 - x1)
  .intercept <- y2 - .slope * x2
  .intercept + .x * .slope
}


build_range <-  function(x, inside_range) {

  xlr <- inside_range[inside_range >= x[[1]] & inside_range <= x[[3]]]

  points <- points_on_line(xlr, x[[1]], x[[2]], x[[3]], x[[4]])

  data.frame(x = xlr,
             y = points)

}
