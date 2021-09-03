
test_that("NAs are not dropped from the data", {
  df <- tibble(x = 1:5, y = c(1, 2, NA, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon()

  expect_equal(sum(is.na(layer_data(p)$ymax)), length(unique(layer_data(p)$group)))
})
