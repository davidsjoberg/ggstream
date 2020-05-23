#' @title make_smooth_density
#'
#' Takes points and turns them into a density line.
#'
#' @param .df a data frame that must contain x and y
#' @param bw bandwidth of kernal density
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#' @param min_x minimum x value of all groups
#' @param max_x maximum x value of all groups
#'
#' @return a data frame
#'
#' @export
make_smooth_density <- function(.df, bw = bw, n_grid = n_grid, min_x, max_x){
  .group <- dplyr::first(.df$group)
  .df <- .df %>% tidyr::drop_na()
  range_dist <- max_x - min_x
  bwidth = bw

  w <- .df$y / sum(.df$y)
  m <- stats::density(.df$x, weights = w, from = min_x - range_dist, to = max_x + range_dist, n = n_grid, bw = bwidth)
  df <- dplyr::tibble(x = m$x,
               y = m$y) %>%
    dplyr::filter(dplyr::case_when(x <= max_x & x >= min_x ~ T,
                     y > 1/5000 * max(y) ~ T,
                     T ~ F))

  dplyr::tibble(x = df$x,
         y = df$y,
         group = .group)
}

#' @title stack_densities
#'
#' Takes density lines of equal x range and stack them on top of each other symmetrically aournd zero.
#'
#' @param data a data frame
#' @param bw bandwidth of kernal density
#' @param n_grid number of x points that should be calculated. The higher the more smooth plot.
#'
#' @return a data frame
#'
#' @export
stack_densities <- function(data, bw = bw, n_grid = n_grid) {
  data <- map_dfr(data %>% split(data$group), ~make_smooth_density(.,
                                                                   bw = bw,
                                                                   n_grid = n_grid,
                                                                   min_x = range(data$x, na.rm = T)[1],
                                                                   max_x = range(data$x, na.rm = T)[2]))

  data <- data %>%
    dplyr::mutate(group_tmp = factor(group) %>% as.numeric()) %>%
    dplyr::arrange(x, group_tmp) %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(ymin = purrr::accumulate(y, ~.x + .y, .init = -sum(y) / 2, .dir = "backward")[-1],
           ymax = ymin + y) %>%
    dplyr::ungroup()

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

#' @title StatStreamDensity
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
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return a data frame
#'
#'
#' @export
StatStreamDensity <- ggplot2::ggproto("StatStreamDensity", ggplot2::Stat,
                             required_aes = c("x", "y"),
                             extra_params = c("bw", "n_grid", "na.rm"),
                             setup_data = function(data, params) {
                               .panels <- unique(data$PANEL)
                               .per_panel <- purrr::map_dfr(.panels, ~{
                                 data %>%
                                   dplyr::filter(PANEL == .x) %>%
                                   stack_densities(
                                     params$bw, params$n_grid
                                     ) %>%
                                   dplyr::mutate(PANEL = .x)
                               }) %>%
                                 dplyr::mutate(PANEL = factor(PANEL))

                               suppressWarnings(data %>%
                                 dplyr::select(-x, -y) %>%
                                 dplyr::distinct() %>%
                                 dplyr::left_join(.per_panel, by = c("group", "PANEL"))
                               )
                             },

                             compute_group = function(data, scales) {
                               data
                             }
)


geom_density_stream <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", show.legend = NA,
                       inherit.aes = TRUE, na.rm = T, bw = 0.75, n_grid = 3000, ...) {
  layer(
    stat = StatStreamDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bw = bw, n_grid = n_grid, ...)
  )
}




# TEST -------------------------------------------------------------------------
# REMOVE
# pacman::p_load(tidyverse, hablar, KernSmooth, sf, feather, janitor, lubridate)
# options(stringsAsFactors = F)
#
# # Test data
#
set.seed(123)
make_group <- function(group, n) {
  tibble(
    x = 1:n,
    y = sample(1:100, n),
    group = group
  )
}

tst_df <- map_dfr(c("A", "B", "C", "D"), ~make_group(., 20))

tst_df %>%
  ggplot(aes(x, y, fill = group)) +
  geom_density_stream(alpha = .9, color = "black", size = .2, bw = .75) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = NULL) +
  theme(legend.position = "bottom")
#
# # Faceted
# bind_rows(
#   map_dfr(c("A", "B", "C", "D"), ~make_group(., 10)) %>% mutate(g = "No1"),
#   map_dfr(c("A", "B", "C", "D"), ~make_group(., 10)) %>% mutate(g = "No2")
# ) %>%
#   ggplot(aes(x, y, fill = group)) +
#   geom_density_stream(alpha = .9, color = "transparent", size = 1) +
#   scale_fill_viridis_d() +
#   theme_void() +
#   facet_wrap(~g) +
#   labs(fill = NULL) +
#   theme(legend.position = "bottom")

