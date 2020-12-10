replace_values <- function(df) {

  df$y <- replace(df$y, is.na(df$y), 0)
  df$group <- replace(df$group, is.na(df$group), unique(stats::na.omit(df$group)))

  df

}

decimal_places <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
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
      }))
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

    } else {

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
  out %>%
    dplyr::mutate(y = y * mean_y / mean_y_post)
  }
