test_that("hcl.colors work", {
  expect_equal(hcl.colors(5),
               c("#4B0055", "#00588B", "#009B95",
                 "#53CC67", "#FDE333"))
})

test_that("scale_fill_hc work", {
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  p <- ggplot(df, aes(x, y))+
    geom_horizon()+
    scale_fill_hcl()

  expect_snapshot(layer_data(p)$fill)
})
