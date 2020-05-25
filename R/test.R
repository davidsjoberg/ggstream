TEST -------------------------------------------------------------------------

# Test data

library(tidyverse)
set.seed(123)
make_group <- function(group, n) {
  dplyr::tibble(
    x = 1:n,
    y = sample(1:100, n),
    group = group
  )
}

tst_df <- purrr::map_dfr(c("A", "B", "C", "D"), ~make_group(., 20))

tst_df %>%
  ggplot(aes(x, y, fill = group)) +
  geom_stream(alpha = .9, color = "black", size = .2, bw = .75) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = NULL) +
  theme(legend.position = "bottom")

# Faceted
bind_rows(
  map_dfr(c("A", "B", "C", "D"), ~make_group(., 10)) %>% mutate(g = "No1"),
  map_dfr(c("A", "B", "C", "D"), ~make_group(., 10)) %>% mutate(g = "No2")
) %>%
  ggplot(aes(x, y, fill = group)) +
  geom_density_stream(alpha = .9, color = "transparent", size = 1) +
  scale_fill_viridis_d() +
  theme_void() +
  facet_wrap(~g) +
  labs(fill = NULL) +
  theme(legend.position = "bottom")

## Real Object test -----
library(tidyverse)

.df <- tibble(x = mtcars$drat,
              y = mtcars$qsec)

w <- .df$y / sum(.df$y)
m <- stats::density(.df$x, weights = w, n = 5000, bw = .3)
df <- dplyr::tibble(x = m$x,
                    y = m$y)

# Unnormalize density so that height matches true data relative size
group_min_x <- min(.df$x, na.rm = T)
group_max_x <- max(.df$x, na.rm = T)
group_average_y <- mean(.df$y)
mulitplier <- abs(group_max_x - group_min_x) * group_average_y
df$y <- df$y * mulitplier

ggplot() +
  geom_point(data = .df, aes(x, y)) +
  geom_line(data = df, aes(x, y), color = "red")
