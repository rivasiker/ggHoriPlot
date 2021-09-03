
test_that("NAs are not dropped from the data", {
  df <- tibble(x = 1:5, y = c(1, 2, NA, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon()

  expect_equal(sum(is.na(layer_data(p)$ymax)), length(unique(layer_data(p)$group)))
})


test_that("Outliers aare removed", {
  df <- tibble(x = 1:5, y = c(1, 2, 66, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon(rm.outliers = T)

  expect_equal(levels(layer_data(p)$Cutpoints)[1], "[4.33333, 5)")
})
