

test_that("The origin is valid", {
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  p <- ggplot(df, aes(x, y))+stat_horizon(origin = 'origin')

  expect_warning(print(p), 'valid origin')
})
