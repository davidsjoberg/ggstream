calc_y_offsets <- function(df) {

  df$ymin <- cumsum(-1 * c(-sum(df$y) / 2, df$y))[-1]

  df$ymax <- df$ymin + df$y

  return(df)

}
