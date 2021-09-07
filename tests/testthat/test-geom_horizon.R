
test_that("NAs are not dropped from the data", {
  df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon()

  expect_equal(sum(is.na(layer_data(p)$ymax)),
               length(unique(layer_data(p)$group)))
})


test_that("Outliers are removed", {
  df <- data.frame(x = 1:5, y = c(1, 2, 66, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon(rm.outliers = T)

  expect_equal(levels(layer_data(p)$Cutpoints)[1], "[4.33333, 5)")
})


test_that("The origin is valid", {
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'origin')

  expect_warning(print(p), 'valid origin')
})

test_that("The horizonscale is valid", {
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  p <- ggplot(df, aes(x, y))+geom_horizon(horizonscale = 'horizonscale')

  expect_warning(print(p), 'valid horizonscale')
})

test_that("The quantiles origin has a valid horizonscale", {
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))

  p <- ggplot(df, aes(x, y))+
    geom_horizon(origin = 'quantiles', horizonscale = 2.5)

  expect_warning(print(p),
                 "positive integer when using origin = 'quantiles'")
})


test_that("The midpoint origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'midpoint')

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[7.66667, 9)",  "[6.33333, 7.66667)", "[5, 6.33333)",
                 "[3.66667, 5)",  "[2.33333, 3.66667)", "[1, 2.33333)"  ))
})

test_that("The mean origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'mean')

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[6.56667, 7.9)", "[5.23333, 6.56667)", "[3.9, 5.23333)",
                 "[2.56667, 3.9)", "[1.23333, 2.56667)", "[-0.1, 1.23333)" ))
})

test_that("The median origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'median')

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[5.16667, 6.5)", "[3.83333, 5.16667)", "[2.5, 3.83333)",
                 "[1.16667, 2.5)", "[-0.166667, 1.16667)", "[-1.5, -0.166667)" ))
})

test_that("The cutpoints of min origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'min')

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[7.66667, 9)",  "[6.33333, 7.66667)", "[5, 6.33333)",
                 "[3.66667, 5)",  "[2.33333, 3.66667)", "[1, 2.33333)" ))
})

test_that("The category assignment of min origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'min')

  expect_equal(sum(str_detect(layer_data(p)$group, 'neg')),
               0)
})

test_that("The quantiles origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 'quantiles')

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[7, 9)",   "[5, 7)",   "[2.5, 5)",
                 "[2, 2.5)", "[1.5, 2)", "[1, 1.5)"))
})

test_that("The numeric origin works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(origin = 3)

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[5.66667, 7)", "[4.33333, 5.66667)",  "[3, 4.33333)",
                 "[1.66667, 3)", "[0.333333, 1.66667)", "[-1, 0.333333)"))
})

test_that("The integer horizonscale works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+geom_horizon(horizonscale = 10)

  expect_equal(length(levels(layer_data(p)$Cutpoints)),
               10)
})

test_that("The custom horizonscale works", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+
    geom_horizon(horizonscale = c(1, 3, 6, 8))

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[6, 8)", "[5, 6)", "[3, 5)", "[1, 3)"))
})

test_that("The custom horizonscale works with numeric origin", {
  df <- data.frame(x = 1:10, y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x, y))+
    geom_horizon(horizonscale = c(1, 3, 6, 8),
                 origin = 2)

  expect_equal(levels(layer_data(p)$Cutpoints),
               c("[6, 8)", "[3, 6)", "[2, 3)", "[1, 2)"))
})

test_that("The processed table has the right dimensions", {
  df <- data.frame(x = 1:10,
               xend = (1:10)+0.999,
               y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x = x, y = y))+geom_horizon()

  expect_equal(nrow(layer_data(p)),
               nrow(df)*length(levels(layer_data(p)$Cutpoints)))
})


test_that("The processed table has the right dimensions with xend", {
  df <- data.frame(x = 1:10,
               xend = (1:10)+0.999,
               y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x = x, xend = xend, y = y))+geom_horizon()

  expect_equal(nrow(layer_data(p)),
               nrow(df)*2*length(levels(layer_data(p)$Cutpoints)))
})


test_that("Mirror works", {
  df <- data.frame(x = 1:10,
                   y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x = x, y = y))+geom_horizon(mirror = T)

  expect_true(all(layer_data(p)$ymax == 1))
})


test_that("Reverse works", {
  df <- data.frame(x = 1:10,
                   y = c(1, 3, 5, 8, 1, 2, 6, 9, 2, 2))

  p <- ggplot(df, aes(x = x, y = y))+geom_horizon(reverse = T)

  expect_true(
    all(
      layer_data(p)$ymin[str_detect(layer_data(p)$Categories, 'neg')] == 1))

  expect_true(
    all(
      layer_data(p)$ymin[str_detect(layer_data(p)$Categories, 'pos')] == 0))
})














