# @title make_smooth_density
#
# Takes points and turns them into a density line.
#
# @param .df a data frame that must contain x and y
# @param bw bandwidth of kernal density
# @param n_grid number of x points that should be calculated. The higher the more smooth plot.
# @param min_x minimum x value of all groups
# @param max_x maximum x value of all groups
#
# @return a data frame
make_smooth_density <- function(df, bw = bw, n_grid = n_grid, min_x, max_x) {

  group <- df$group[1]

  group_min_x <- min(df$x, na.rm = T)

  group_max_x <- max(df$x, na.rm = T)

  range_dist <- max_x - min_x

  group_average_y <- mean(df$y, na.rm = TRUE)

  df <-  df[stats::complete.cases(df), ]

  bwidth <- bw

  w <- df$y / sum(df$y)

  m <- stats::density(df$x, weights = w, from = min_x - range_dist, to = max_x + range_dist, n = n_grid, bw = bwidth)

  df <- data.frame(x = m$x,
                   y = m$y)

  df <- df[(df$x <= max_x & df$x >= min_x) | df$y > 1/10000 * max(df$y), ]

  # Un-normalize density so that height matches true data relative size

  mulitplier <- abs(group_max_x - group_min_x) * group_average_y

  df$y <- df$y * mulitplier

  data.frame(x = df$x,
             y = df$y,
             group = group)
}

# @title make_smooth_loess
#
# Takes points and turns them into a LOESS-estimated line.
#
# @param df a data frame that must contain x and y
# @param bw bandwidth of kernal density
# @param n_grid number of x points that should be calculated. The higher the more smooth plot.
# @param min_x minimum x value of all groups
# @param max_x maximum x value of all groups
#
# @return a data frame
# @export
make_smooth_loess <- function(df, bw = bw, n_grid = n_grid, min_x, max_x){

  group <- df$group[[1]]

  df <-  df[stats::complete.cases(df), ]

  bwidth <- bw

  m <- stats::loess(y ~ x, df, span = bw)

  x_range <- seq(min_x, max_x, length.out = n_grid)

  df <- data.frame(x = x_range,
                   y = stats::predict(m, x_range))

  df$y <- ifelse(df$y < 0, 0, df$y)

  df <- df[stats::complete.cases(df), ]

  df$group <- group

  df
}


# @title make_connect_dots
#
# Returns n number of points from data that perfectly fits data.
#
# @param df a data frame that must contain x and y
# @param n_grid number of x points that should be calculated. The higher the more smooth plot.
# @param min_x minimum x value of all groups
# @param max_x maximum x value of all groups
#
# @return a data frame
#
# @export
make_connect_dots <- function(df, n_grid = n_grid, min_x, max_x, ...){

  new_y <- sapply(split(df, list(df$x, df$group)), function(x) mean(x$y))

  df <- unique(df[c("x", "group")])

  df <- df[order(df$x),]

  df$y <- new_y

  group <- df$group[[1]]

  df <-  df[stats::complete.cases(df), ]

  range_x <- seq(min_x, max_x, length.out = n_grid)

  steps <- df$x[-1]

  outside_local_range <- range_x[range_x < min(df$x) | range_x > max(df$x)]

  inside_local_range <- range_x[range_x >= min(df$x) & range_x <= max(df$x)]

  df <- data.frame(xlag = c(NA, utils::head(df$x, -1)),
             ylag = c(NA, utils::head(df$y, -1)),
             x = df$x,
             y = df$y)

  df <-  df[stats::complete.cases(df), ]

  inrange <- apply(df, 1, build_range, inside_range = inside_local_range)

  inrange_out <- do.call(rbind, inrange)

  out <- rbind(inrange_out,
    data.frame(x = outside_local_range,
             y = rep(0, length(outside_local_range))))

   out$group <- group

   out[order(out$x),]

}



# @title stack_densities
#
# Takes density lines of equal x range and stack them on top of each other symmetrically aournd zero.
#
# @param data a data frame
# @param bw bandwidth of kernal density
# @param n_grid number of x points that should be calculated. The higher the more smooth plot.
# @param center_fun a function that returns the y center for each possible x in range of x.
# @param method which estimation should be applied. Default is LOESS.
#
# @return a data frame
#
#
# @export
stack_densities <- function(data, bw = bw, n_grid = n_grid, center_fun = center_fun, method = method){

  if (is.null(center_fun)) {
    center_fun <- function(x) {
      return(0)
    }
  }

  fun <- switch(method,
         density = make_smooth_density,
         loess = make_smooth_loess,
         raw = make_connect_dots)

  x_range <- range(data$x)

  list <- lapply(split(data, data$group), fun, bw = bw, n_grid = n_grid, min_x = x_range[1], max_x = x_range[2])

  data <- do.call(rbind, list)

  data$group_tmp <- as.numeric(factor(data$group))

  data <- data[order(data$x, data$group_tmp),]

  data_list <- lapply(split(data, data$x), calc_y_offsets)

  data <- do.call(rbind, data_list)

  data$ymin <- data$ymin + center_fun(data$x)

  data$ymax <- data$ymax + center_fun(data$x)

  data <- lapply(split(data, data$group), extend_data)

  do.call(rbind, data)

}



StatStreamDensity <- ggplot2::ggproto(
  "StatStreamDensity",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  extra_params = c("bw", "n_grid", "na.rm", "center_fun", "method"),

  setup_data = function(data, params) {

    panels <- unique(data$PANEL)

    per_panel <- lapply(
      split(data, panels),
        stack_densities,
        bw = params$bw,
        n_grid = params$n_grid,
        center_fun = params$center_fun,
        method = params$method
      )

    per_panel <-
      lapply(seq_along(per_panel), function(x) {
           data.frame(per_panel[[x]], PANEL = panels[x])
      })

    per_panel <- do.call(rbind, per_panel)

    per_panel$PANEL <- factor(per_panel$PANEL)

    chars <- unique(data[!names(data) %in% c("x", "y")])

    chars$id  <- 1:nrow(chars)

    per_panel$p_id <- 1:nrow(per_panel)

    out <- merge(chars, per_panel, by = c("group", "PANEL"), all.x = FALSE)

    out <- out[order(out$id, out$p_id), ]

    out <- out[!names(out) %in% c("id", "p_id")]

    rownames(out) <- NULL

    out

  },

  compute_group = function(data, scales) {
    data
  }
)

#' @title geom_stream
#'
#' stat to compute `geom_stream`
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param bw bandwidth of kernal density estimation
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param center_fun a function that returns the y center for each possible x in range of x.
#' @param method Which method of estimation should be used. Default is LOESS, similar to `geom_smooth` but sets negative values to zero.
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return a data frame
#' @export
geom_stream <- function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, na.rm = T, bw = 0.75, n_grid = 3000, method = c("loess", "density", "raw"), center_fun = NULL, ...) {
  method <- match.arg(method)
  ggplot2::layer(
    stat = StatStreamDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bw = bw, center_fun = center_fun, n_grid = n_grid, method = method, ...)
  )
}
