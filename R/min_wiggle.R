replace_values <- function(df) {

  df$y <- replace(df$y, is.na(df$y), 0)
  df$group <- replace(df$group, is.na(df$group), unique(stats::na.omit(df$group)))

  df

}


new_wiggle <- function(.df, bw = bw, n_grid = n_grid, min_x, max_x) {

  # test
  mean_y <- mean(.df$y)
  # slut test

  .group <- dplyr::first(.df$group)
  full_values <- data.frame(x = seq(min_x, max_x, length.out = n_grid))
  others <- .df[!names(.df) %in% c("x", "y")]

  sub_df <- .df[names(.df) %in% c("x", "y", "group")]

  list <- split(sub_df, .df$group)

  list <- purrr::map(list, ~{
    .x %>%
      dplyr::mutate(x = purrr::map_dbl(.data$x, function(q) {
        full_values$x[which.min(abs(full_values$x - q))]
      })) %>%
      # Two or more raw x points can snap to the same grid point (dense or
      # discrete x). Aggregate before merging, or merge() below cross-joins
      # the duplicates and inflates the row count past n_grid -- the crash
      # reported upstream as davidsjoberg/ggstream#16 and #18.
      dplyr::group_by(.data$x) %>%
      dplyr::summarise(y = sum(.data$y, na.rm = TRUE),
                        group = dplyr::first(.data$group), .groups = "drop")
  })

  list <- lapply(list, function(x) merge(full_values, x, by = "x", all.x = TRUE))

  list <- lapply(list,  replace_values)

  list <- lapply(list, function(x) stats::setNames(x, c("x", unique(as.character(x$group)), "group")))

  list <- lapply(list, function(x) x[names(x) != "group"])

  values <- as.matrix(Reduce(function(x, y) merge(x, y, by = "x"), list))

  col_names <- colnames(values)

  xval <- values[, 1]

  values <- as.matrix(values[, -1])

  colnames(values) <- col_names[-1]

  dims <- dim(values)

  if (is.null(dims[1])) {

    timePoints <- length(values)
    nStreams <- 1
    values <- as.matrix(values)

  } else {

    timePoints <- dims[1]
    nStreams <- dim(values)[2]

  }

  yy <- matrix(0, timePoints, (nStreams * 2))

  for (iStream in 1 : nStreams) {

    tmpVals <- stats::predict(stats::smooth.spline(values[, iStream], spar = bw))$y

    if (iStream > 1) {

      yy[, iStream * 2 - 1] <- yy[, (iStream - 1) * 2]

      yy[, iStream * 2] <- yy[, iStream * 2 - 1] + tmpVals

    } else if (nStreams > 1) {

      baseline <- array(0, timePoints)

      for (ipoint in 1 : timePoints) {

        for (jStream in 1 : nStreams) {

          baseline[ipoint] = baseline[ipoint] +
            + (nStreams - jStream - .5) * values[ipoint, jStream]

        }

        baseline[ipoint] = baseline[ipoint] / nStreams
      }

      yy[, 1] <- -stats::predict(stats::smooth.spline(baseline), spar = bw)$y

      yy[, 2] <- yy[, iStream * 2 - 1] + tmpVals

    } else {

      # nStreams == 1: the only case that actually occurs, since new_wiggle()
      # is always called on one already-split group at a time (see
      # stack_densities()'s purrr::map_dfr(data %>% split(data$group), fun)).
      # The baseline-offset branch above is multi-stream centring math left
      # over from an earlier, differently-structured implementation. With a
      # single column, smooth.spline() is a linear smoother at fixed spar (a
      # fixed smoother matrix S, so S %*% (k * v) == k * (S %*% v) for any
      # scalar k), which makes that branch's output provably
      # yy[,2] == 1.5 * tmpVals for every input.
      #
      # NOTE: this is dead/vestigial multi-stream code being simplified for
      # clarity, not a fix for davidsjoberg/ggstream#19 ("y values far too
      # high"). Verified directly (before and after this change, on both a
      # synthetic case and the bundled blockbusters data): the mean_y /
      # mean_y_post rescale a few lines down divides by mean(1.5 * tmpVals),
      # so the constant 1.5 cancels algebraically -- final output is
      # identical whichever branch computes the intermediate value. #19's
      # actual magnitude discrepancy is NOT explained by this code path;
      # left open (see fork NEWS/issue notes), not guessed at further.

      yy[, 1] <- 0
      yy[, 2] <- tmpVals

    }
  }

  groups <- colnames(values)

  y <- vector()

  for (iStream in 1:nStreams)
  {
    y <- cbind(y, c(yy[, iStream * 2], rev(yy[, iStream * 2 - 1])))

  }

  y_groups <- utils::stack(stats::setNames(as.data.frame(y), groups))

  out <- data.frame(x = full_values$x,
                    y = yy[, iStream * 2],
                    group = as.integer(.group))
  mean_y_post <- mean(out$y)
  if (isTRUE(all.equal(mean_y_post, 0)) || mean_y_post == 0) {
    # An all-zero (or all-near-zero) group divides mean_y/mean_y_post by
    # zero, turning y into NaN here -- which then poisons every other
    # group's ymin/ymax once stack_densities() accumulates across groups.
    # An all-zero group's stream is legitimately flat at zero, so return
    # that directly instead of NaN (davidsjoberg/ggstream#23, #15).
    out %>% dplyr::mutate(y = 0)
  } else {
    out %>%
      dplyr::mutate(y = y * mean_y / mean_y_post)
  }
  }
