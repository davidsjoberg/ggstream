utils::globalVariables(c(".data", "x", "y"))

# Support function
points_on_line <- function(.x, x1, y1, x2, y2) {
  .slope <- (y2 - y1) / (x2 - x1)
  .intercept <- y2 - .slope * x2
  .intercept + .x * .slope
}


get_area <- function(.sh, ..x, .w) {
  out <- .sh %>%
    dplyr::filter(.data$x >= ..x,
                  .data$x <= (..x + .w)) %>%
    dplyr::summarise(ymin = max(.data$ymin),
              ymax = min(.data$ymax))
  (out$ymax - out$ymin) * .w
}

get_position <- function(..df) {
  .w <- (range(..df$x)[2]-range(..df$x)[1])/10

  sh <- dplyr::tibble(x = ..df$x[1:(nrow(..df)/2)],
               ymin =  ..df$y[1:(nrow(..df)/2)],
               ymax =  ..df$y[(nrow(..df)/2+1):nrow(..df)] %>% rev()
  )

  it <- sh %>%
    dplyr::filter((x + .w) <= max(x)) %>%
    dplyr::pull(x)

  v <- purrr::map_dbl(it, ~get_area(.sh = sh, ..x = .x, .w))
  i <- which.max(v)

  sh %>%
    dplyr::slice(i) %>%
    dplyr::transmute(x = .data$x + .w / 2,
              y = .data$ymin + (.data$ymax - .data$ymin) / 2)
}

sort_inside_out_naive <- function(df) {
  df %>%
    sort_onset %>%
    dplyr::mutate(onset_order = as.numeric(.data$fill),
           # even first desc, then odd ascending
           insideout_order = dplyr::if_else(.data$onset_order %% 2 == 0,
                                      -.data$onset_order,
                                      .data$onset_order),
           fill = forcats::fct_reorder(.data$fill, .data$insideout_order))
}


sort_onset <- function(df) {
  df %>%
    # Put zero or negative values last
    dplyr::arrange(.data$y <= 0, -.data$x) %>%
    dplyr::mutate(fill = forcats::fct_inorder(.data$fill))
}


stack_densities <- function(data,
                            bw = bw,
                            n_grid = n_grid,
                            extra_span = extra_span,
                            center_fun = center_fun,
                            method = method,
                            true_range = true_range,
                            type = type) {

  if(is.null(center_fun)) {center_fun <- function(x) {return(0)}
  }

  .min_x <- range(data$x, na.rm = TRUE)[1]
  .max_x <- range(data$x, na.rm = TRUE)[2]
  .range_length <- .max_x - .min_x
  .min_x_est <- .min_x - .range_length * extra_span
  .max_x_est <- .max_x + .range_length * extra_span

  fun <- if(method == "new_wiggle") {
    ~new_wiggle(.,
                bw = bw,
                n_grid = n_grid,
                min_x = .min_x_est,
                max_x = .max_x_est)
  }

  if(type == "mirror") {
    init <- quote(-(sum(.data$y) / 2))
  } else if (type %in% c("ridge", "proportional")) {
    init <- quote(0)
  }


  data <- purrr::map_dfr(data %>% split(data$group), fun) %>%
    dplyr::mutate(y = dplyr::if_else(y < 0, 0, y))

  if(true_range == "both") {
    data <- data %>%
      dplyr::filter(x >= .min_x,
                    x <= .max_x)
  } else if (true_range == "max_x") {
    data <- data %>%
      dplyr::filter(x <= .max_x)
  } else if (true_range == "min_x") {
    data <- data %>%
      dplyr::filter(x >= .min_x)
  }

  data <- data %>%
    dplyr::mutate(group_tmp = factor(.data$group) %>% as.numeric()) %>%
    dplyr::arrange(.data$x, .data$group_tmp) %>%
    dplyr::group_by(.data$x)

  if(type == "proportional") {
    data <- data %>%
    dplyr::mutate(y = .data$y / sum(.data$y, na.rm = TRUE))
  }
  data <- data %>%
    dplyr::mutate(ymin = purrr::accumulate(.data$y, ~.x + .y,
                                           .init = eval(init),
                                           .dir = "backward")[-1],
           ymax = .data$ymin + .data$y) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::mutate_at(dplyr::vars("ymin", "ymax"), ~{. + center_fun(.data$x)})

  data <- purrr::map_dfr(data %>% split(data$group),
                     ~{
                       .x <- .x %>% dplyr::arrange(x)
                       dplyr::tibble(
                         x = c(.x$x, rev(.x$x)),
                         y = c(.x$ymin, rev(.x$ymax)),
                         group = dplyr::first(.x$group))
                     }
  )
  data
}


StatStreamDensity <- ggplot2::ggproto("StatStreamDensity", ggplot2::Stat,
                             required_aes = c("x", "y", "fill"),
                             extra_params = c("bw", "n_grid", "extra_span", "na.rm", "center_fun", "method", "true_range", "type", "sorting"),
                             setup_data = function(data, params) {

                               if(is.character(data$fill)) {
                                 data$fill <- factor(data$fill)
                               }

                               # sorting
                               if(params$sorting == "inside_out") {
                                 data <- data %>% sort_inside_out_naive() %>%
                                   dplyr::arrange(.data$fill) %>%
                                   dplyr::mutate(group = as.numeric(.data$fill))
                               }
                               if(params$sorting == "onset") {
                                 data <- data %>% sort_onset() %>%
                                   dplyr::arrange(.data$fill) %>%
                                   dplyr::mutate(group = as.numeric(.data$fill))
                               }

                               if(params$na.rm) {
                                 data <- data %>%
                                   tidyr::drop_na(x, y, fill)
                               }

                               .panels <- unique(data$PANEL)
                               .per_panel <- purrr::map_dfr(.panels, ~{
                                 data %>%
                                   dplyr::filter(PANEL == .x) %>%
                                   stack_densities(
                                     bw = params$bw,
                                     n_grid = params$n_grid,
                                     extra_span = params$extra_span,
                                     center_fun = params$center_fun,
                                     method = params$method,
                                     true_range = params$true_range,
                                     type = params$type
                                     ) %>%
                                   dplyr::mutate(PANEL = .x)
                               }) %>%
                                 dplyr::mutate(PANEL = factor(PANEL))

                               suppressWarnings(data %>%
                                 dplyr::select(-"x", -"y") %>%
                                 dplyr::distinct() %>%
                                 dplyr::left_join(.per_panel, by = c("group", "PANEL"))
                               )
                             },

                             compute_group = function(data, scales) {
                               data
                             }
)

StatStreamDensityText <- ggplot2::ggproto("StatStreamDensityText", ggplot2::Stat,
                             required_aes = c("x", "y", "fill"),
                             extra_params = c("bw", "n_grid", "extra_span", "na.rm", "center_fun", "method", "true_range", "sorting", "type"),
                             setup_data = function(data, params) {

                               if(is.character(data$fill)) {
                                 data$fill <- factor(data$fill)
                               }

                               # sorting
                               if(params$sorting == "inside_out") {
                                 data <- data %>% sort_inside_out_naive() %>%
                                   dplyr::arrange(fill) %>%
                                   dplyr::mutate(group = as.numeric(fill))
                               }
                               if(params$sorting == "onset") {
                                 data <- data %>% sort_onset() %>%
                                   dplyr::arrange(fill) %>%
                                   dplyr::mutate(group = as.numeric(fill))
                               }

                               if(params$na.rm) {
                                 data <- data %>%
                                   tidyr::drop_na(x, y, fill)
                               }
                               .panels <- unique(data$PANEL)
                               .per_panel <- purrr::map_dfr(.panels, ~{
                                 data %>%
                                   dplyr::filter(PANEL == .x) %>%
                                   stack_densities(
                                     bw = params$bw,
                                     n_grid = params$n_grid,
                                     extra_span = params$extra_span,
                                     center_fun = params$center_fun,
                                     method = params$method,
                                     true_range = params$true_range,
                                     type = params$type
                                     ) %>%
                                   dplyr::mutate(PANEL = .x)
                               }) %>%
                                 dplyr::mutate(PANEL = factor(PANEL))

                               suppressWarnings(data %>%
                                 dplyr::select(-"x", -"y") %>%
                                 dplyr::distinct() %>%
                                 dplyr::left_join(.per_panel, by = c("group", "PANEL"))
                               )
                             },

                             compute_group = function(data, scales) {
                               get_position(data)
                             }
)

#' @title geom_stream
#'
#' geom to create stream plots
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param bw bandwidth of kernel density estimation
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param center_fun a function that returns the y center for each possible x in range of x.
#' @param method Only `new wiggle` is implemented so far.
#' @param type one of `mirror` which stacks symmetrically around the x axis, or `ridge` which stacks from the x-axis, or `proportional`
#' @param true_range should the true data range be used or the estimation range?
#' @param extra_span How many extra range should be used in estimation? Percent of x range added to min and max.
#' @param sorting Should the groups be sorted. Either the default 'none', 'onset' or 'inside_out'
#' @param inherit.aes should the geom inherits aesthetics
#' @param ... other arguments to be passed to the geom
#'
#' @return a 'ggplot' layer
#'
#' @examples
#'
#' library(ggplot2)
#' set.seed(123)
#'  df <- data.frame(x = rep(1:10, 3),
#'                   y = rpois(30, 2),
#'                   group = sort(rep(c("A", "B", "C"), 10)))
#' ggplot(df, aes(x, y, fill = group, label = group)) +
#'   geom_stream()
#'
#' @export
geom_stream <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE,
                       na.rm = TRUE,
                       bw = 0.75,
                       extra_span = 0.01,
                       n_grid = 1000,
                       method = c("new_wiggle"),
                       center_fun = NULL,
                       type = c("mirror", "ridge", "proportional"),
                       true_range = c("both", "min_x", "max_x", "none"),
                       sorting = c("none", "onset", "inside_out"), ...) {

  method <- match.arg(method)
  type <- match.arg(type)
  true_range <- match.arg(true_range)
  sorting <- match.arg(sorting)

  ggplot2::layer(
    stat = StatStreamDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  bw = bw,
                  extra_span = extra_span,
                  center_fun = center_fun,
                  n_grid = n_grid,
                  type = type,
                  true_range = true_range,
                  method = method,
                  sorting = sorting, ...)
  )
}

#' @title geom_stream_label
#'
#' geom to create labels to a geom_stream plot
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param bw bandwidth of kernel density estimation
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param center_fun a function that returns the y center for each possible x in range of x.
#' @param method Only `new wiggle` is implemented so far.
#' @param type one of `mirror` which stacks symmetrically around the x axis, or `ridge` which stacks from the x-axis, or `proportional`.
#' @param true_range should the true data range be used or the estimation range?
#' @param extra_span How many extra range should be used in estimation? Percent of x range added to min and max.
#' @param sorting Should the groups be sorted. Either the default 'none', 'onset' or 'inside_out'
#' @param inherit.aes should the geom inherits aesthetics
#' @param ... other arguments to be passed to the geom
#'
#' @return a 'ggplot' layer
#'
#' @examples
#' library(ggplot2)
#' set.seed(123)
#'  df <- data.frame(x = rep(1:10, 3),
#'                   y = rpois(30, 2),
#'                   group = sort(rep(c("A", "B", "C"), 10)))
#' ggplot(df, aes(x, y, fill = group, label = group)) +
#'   geom_stream() +
#'   geom_stream_label(n_grid = 100)
#'
#' @export
geom_stream_label <- function(mapping = NULL, data = NULL, geom = "text",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE,
                       na.rm = TRUE,
                       bw = 0.75,
                       extra_span = 0.01,
                       n_grid = 100,
                       method = c("new_wiggle"),
                       center_fun = NULL,
                       type = c("mirror", "ridge", "proportional"),
                       true_range = c("both", "min_x", "max_x", "none"),
                       sorting = c("none", "onset", "inside_out"), ...) {
  method <- match.arg(method)
  type <- match.arg(type)
  true_range <- match.arg(true_range)
  sorting <- match.arg(sorting)
  ggplot2::layer(
    stat = StatStreamDensityText, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  bw = bw,
                  extra_span = extra_span,
                  center_fun = center_fun,
                  n_grid = n_grid,
                  type = type,
                  true_range = true_range,
                  method = method,
                  sorting = sorting, ...)
  )
}


