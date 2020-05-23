# # REMOVE
# pacman::p_load(tidyverse, hablar, KernSmooth, sf, feather, janitor, lubridate)
# options(stringsAsFactors = F)
#
#
# # make_smooth(): Make smooth curve per group -----------------------------------
# make_smooth_abs <- function(.df, force_to_zero = FALSE, bw = NULL, spans_x_extra = 6, n_grid = 3000, range_x){
#   .x <- .df$x
#   .y <- .df$y
#   .group <- first(.df$group)
#   .x <- .x[!is.na(.x)]
#   .y <- .y[!is.na(.y)]
#   min_x <- min(.x)
#   max_x <- min(.y)
#   if(is.null(bw)) {
#     bwidth = KernSmooth::dpill(.x,.y)
#   } else {
#     bwidth <- bw
#   }
#
#   if(force_to_zero){
#     times <- 1:spans_x_extra
#     for(t in times) {
#       .x <- c(min_x - t * bwidth, .x, max_x + t * bwidth)
#       .y <- c(0, .y, 0)
#     }
#   }
#   fit <- KernSmooth::locpoly(.x, .y, range.x = range_x, bandwidth = bwidth, gridsize = n_grid)
#   # filter_vector <- !(fit$y == 0 & (fit$x < min_x | fit$x > max_x))
#   # fit$x <- fit$x[filter_vector]
#   # fit$y <- fit$y[filter_vector]
#   fit$y <- ifelse(fit$y <= 0, 0.000001, fit$y)
#
#   tibble(x = fit$x,
#          y = fit$y,
#          group = .group)
# }
#
# make_smooth_density <- function(.df, bw = NULL, n_grid = 3000, min_x, max_x){
#   .group <- first(.df$group)
#   .df <- .df %>% drop_na()
#   range_dist <- max_x - min_x
#   print(range_dist)
#   if(is.null(bw)) {
#     bwidth = .75
#   } else {
#     bwidth <- bw
#   }
#
#   w <- .df$y / sum(.df$y)
#   m <- stats::density(.df$x, weights = w, from = min_x - range_dist, to = max_x + range_dist, n = n_grid, bw = bwidth)
#   df <- tibble(x = m$x,
#                y = m$y) %>%
#     filter(case_when(x <= max_x & x >= min_x ~ T,
#                      y > 1/5000 * max(y) ~ T,
#                      T ~ F))
#
#   tibble(x = df$x,
#          y = df$y,
#          group = .group)
# }
#
# .df <- tibble(x = 1:10,
#               y = c(1, 3, 2, 5, 7, 5, 8, 9, 7, 2),
#               group = rep("A", 10))
# plt_df <- make_smooth_density(.df, min_x = range(.df$x, na.rm = T)[1], max_x = range(.df$x, na.rm = T)[2])
#
# ggplot(plt_df,
#        aes(x = x,
#            y = y)) +
#   geom_line()
#
#
# # TEST -------------------------------------------------------------------------
# # Test data
# set.seed(123)
# make_group <- function(group, n) {
#   tibble(
#     x = 1:n,
#     y = sample(1:100, n),
#     group = group
#   )
# }
#
# tst_df <- map_dfr(c("A", "B", "C", "D"), ~make_group(., 10))
#
# tst_df2 <- map_dfr(tst_df %>% split(tst_df$group), ~make_smooth_density(., min_x = range(tst_df$x)[1], max_x = range(tst_df$x)[2]))
#
# tst_df3 <- tst_df2 %>%
#   mutate(group_tmp = factor(group) %>% as.numeric()) %>%
#   arrange(x, group_tmp) %>%
#   group_by(x) %>%
#   mutate(ymin = accumulate(y, ~.x + .y, .init = -sum(y) / 2, .dir = "backward")[-1],
#          ymax = ymin + y) %>%
#   ungroup()
#
# tst_df3 %>%
#   ggplot(aes(x, y, color = group)) +
#   geom_segment(aes(x = x, xend = x, y = ymin, yend = ymax, color = group))
#
# tst_df4 <- map_dfr(tst_df3 %>% split(tst_df3$group),
#                    ~{
#                      .x <- .x %>% arrange(x)
#                      tibble(
#                        x = c(.x$x, rev(.x$x)),
#                        y = c(.x$ymin, rev(.x$ymax)),
#                        group = first(.x$group))
#                    }
# )
#
# tst_df4 %>%
#   ggplot(aes(x, y, fill = group)) +
#   geom_polygon(alpha = .9, color = "black", size = 1) +
#   scale_fill_viridis_d() +
#   theme_void()
#
#
#
# # smoother <- function(df){
# #   new_df <- tibble(x = seq(min(df$x), max(df$x), length = 50000))
# #   group <- first(df$group)
# #
# #   new_df <- new_df %>%
# #     mutate(pred = predict(
# #       stats::loess(y ~ x, data = df, span = .19),
# #       newdata = .),
# #       pred = if_else(pred < 0, 0, pred)) %>%
# #     mutate(group = group)
# #   return(new_df)
# # }
#
#
# # ggplot() +
# #   # geom_line(data = smoother(df), aes(x, pred)) +
# #   geom_hline(yintercept = 0) +
# #   geom_segment(data = plt_df, aes(x = x, xend = x, y = y, yend = yend, color = group)) +
# #   scale_y_continuous(limits = c(-180, 300)) +
# #   scale_x_continuous(expand = expansion(mult = .25),
# #                      breaks = c(0, 10, 20, 30),
# #                      labels = c("Summer", "Autumn", "Winter", "Spring")) +
# #   theme_minimal() +
# #   labs(color = NULL,
# #        x = NULL) +
# #   scale_color_manual(values = wesanderson::wes_palette("GrandBudapest1", 4)) +
# #   theme(legend.position = "bottom",
# #         panel.grid.major.y = element_blank(),
# #         panel.grid.minor.y = element_blank(),
# #         axis.title.y = element_blank(),
# #         axis.ticks.y = element_blank(),
# #         axis.text.y = element_blank()) +
# #   NULL
# # set_wd_to_script_path()
# # ggsave("geom_stream.png", dpi = 600, height = 4, width = 10)
