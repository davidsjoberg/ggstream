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
make_smooth_density <- function(df, bw = bw, n_grid = n_grid) {

  group <- df$group[1]

  df <-  df[stats::complete.cases(df), ]

  rnge <- range(df$x, na.rm = T)

  min_x <- rnge[1]

  max_x <- rnge[2]

  range_dist <- diff(rnge)

  bwidth <- bw

  w <- df$y / sum(df$y)

  m <- stats::density(df$x, weights = w, from = min_x - range_dist, to = max_x + range_dist, n = n_grid, bw = bwidth)

  df <- data.frame(x = m$x,
                   y = m$y)

  df <- df[(df$x <= max_x & df$x >= min_x) | df$y > 1/10000 * max(df$y), ]


  # Unnormalize density so that height matches true data relative size

  group_average_y <- mean(df$y)

  mulitplier <- abs(range_dist) * group_average_y

  df$y <- df$y * mulitplier

  data.frame(x = df$x,
             y = df$y,
             group = group)
}

# @title stack_densities
#
# Takes density lines of equal x range and stack them on top of each other symmetrically aournd zero.
#
# @param data a data frame
# @param bw bandwidth of kernal density
# @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#
# @return a data frame
stack_densities <- function(data, bw = bw, n_grid = n_grid) {

  list <- lapply(split(data, data$group), make_smooth_density, bw = bw, n_grid = n_grid)

  data <- do.call(rbind, list)

  data$group_tmp <- as.numeric(factor(data$group))

  data <- data[order(data$x, data$group_tmp),]

  data_list <- lapply(split(data, data$x), calc_y_offsets)

  data <- do.call(rbind, data_list)

  data <- lapply(split(data, data$group), extend_data)

  do.call(rbind, data)

}



StatStreamDensity <- ggplot2::ggproto(
  "StatStreamDensity",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  extra_params = c("bw", "n_grid", "na.rm"),

  setup_data = function(data, params) {

    .panels <- unique(data$PANEL)

    .per_panel <- lapply(
      split(data, .panels),
        stack_densities,
        bw = params$bw,
        n_grid = params$n_grid
      )

    .per_panel <-
      lapply(seq_along(.per_panel), function(x) {
           data.frame(.per_panel[[x]], PANEL = .panels[x])
      })

    .per_panel <- setNames(do.call(rbind, .per_panel), c("x", "y", "group", "PANEL"))

    .per_panel$PANEL <- factor(.per_panel$PANEL)

    merge(unique(data[!names(data) %in% c("x", "y")]), .per_panel, by = c("group", "PANEL"))


  },

  compute_group = function(data, scales) {
    data
  }
)

#' @title geom_stream
#'
#' A streamgraph geom/stat for ggplot2
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param bw bandwidth of kernal density estimation
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return a data frame
#'
#'
#' @export
geom_stream <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE, na.rm = T, bw = 0.75, n_grid = 3000, ...) {
  ggplot2::layer(
    stat = StatStreamDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bw = bw, n_grid = n_grid, ...)
  )
}
