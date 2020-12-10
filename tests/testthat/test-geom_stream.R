library(ggplot2)

test_that("geom_stream", {
  expect_equal(class(ggplot(blockbusters, aes(year, box_office, fill = genre)) +
                 geom_stream()), c("gg", "ggplot"))
})
