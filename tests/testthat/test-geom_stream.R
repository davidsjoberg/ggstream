library(ggplot2)

test_that("geom_stream", {
  # ggplot2 4.0's S3->S7 migration adds extra dispatch classes to a built
  # ggplot object, so an exact class(...) == c("gg","ggplot") equality is
  # brittle and broke CRAN's checks (archived 2025-11-07; see
  # davidsjoberg/ggstream#26). inherits() checks membership, not the full
  # class vector, and is robust to ggplot2 adding classes in either
  # direction.
  expect_true(inherits(
    ggplot(blockbusters, aes(year, box_office, fill = genre)) + geom_stream(),
    "ggplot"
  ))
})

test_that("geom_stream builds without error", {
  expect_no_error(ggplot_build(
    ggplot(blockbusters, aes(year, box_office, fill = genre)) + geom_stream()
  ))
})

test_that("an all-zero category does not produce an empty plot (#23)", {
  bb0 <- blockbusters
  bb0$box_office[bb0$genre == "Action"] <- 0
  built <- ggplot_build(
    ggplot(bb0, aes(year, box_office, fill = genre)) + geom_stream()
  )
  expect_gt(nrow(built$data[[1]]), 0)
  expect_false(any(is.nan(built$data[[1]]$y)))
})

test_that("a dense/discrete x axis does not crash the row-count merge (#16, #18)", {
  df <- data.frame(x = rep(c("a", "b"), each = 20),
                    y = rpois(40, 3),
                    group = rep(c("A", "B"), 20))
  expect_no_error(ggplot_build(
    ggplot(df, aes(x, y, fill = group)) + geom_stream()
  ))
})
