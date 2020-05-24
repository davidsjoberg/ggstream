#
# .df <- tibble(x = mtcars$drat,
#               y = mtcars$qsec)
# w <- .df$y / sum(.df$y)
# m <- stats::density(.df$x, weights = w, n = 5000, bw = .3)
# df <- dplyr::tibble(x = m$x,
#                     y = m$y)
#
# # Unnormalize density so that height matches true data relative size
# group_min_x <- min(.df$x, na.rm = T)
# group_max_x <- max(.df$x, na.rm = T)
# group_average_y <- mean(.df$y)
# mulitplier <- abs(group_max_x - group_min_x) * group_average_y
# df$y <- df$y * mulitplier
#
# ggplot() +
#   geom_point(data = .df, aes(x, y)) +
#   geom_line(data = df, aes(x, y), color = "red")
