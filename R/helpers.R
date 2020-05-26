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
