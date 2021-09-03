
test_that("NAs are not dropped from the data", {
  df <- tibble(x = 1:5, y = c(1, 2, NA, 4, 5))

  p <- ggplot(df, aes(x))+
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))

  expect_equal(sum(is.na(layer_data(p)$ymax)), length(unique(layer_data(p)$group)))
})
