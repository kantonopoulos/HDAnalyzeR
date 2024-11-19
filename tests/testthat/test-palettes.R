# Test get_hpa_palettes --------------------------------------------------------
test_that("get_hpa_palettes returns a list of palettes", {
  palettes <- get_hpa_palettes()

  # Check that the result is a list
  expect_true(is.list(palettes))

  # Check that specific palettes are in the list
  expect_true("sex" %in% names(palettes))
  expect_true("diff_exp" %in% names(palettes))
  expect_true("cancers12" %in% names(palettes))
  expect_true("cancers15" %in% names(palettes))

  # Check that the "sex" palette has the correct colors
  expect_equal(palettes$sex, c("F" = "red", "M" = "blue"))
})


# Test scale_color_hpa ---------------------------------------------------------
test_that("scale_color_hpa works with valid palettes", {
  # Create a sample data frame
  data <- data.frame(
    var1 = 1:10,
    var2 = seq(2, 20, by = 2),
    Sex = rep(c("Male", "Female"), each = 5)
  )

  # Generate a plot using the "sex" palette
  p <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, color = Sex)) +
    ggplot2::geom_point() +
    scale_color_hpa("sex")

  # Check that the plot object is created without error
  expect_s3_class(p, "gg")
})


test_that("scale_color_hpa throws an error with invalid palette", {
  expect_error(scale_color_hpa("invalid_palette"), "Palette not found")
})


# Test scale_fill_hpa ----------------------------------------------------------
test_that("scale_fill_hpa works with valid palettes", {
  # Create a sample data frame
  data <- data.frame(
    Sex = c("Male", "Female"),
    Count = c(60, 40)
  )

  # Generate a plot using the "sex" palette
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Sex, y = Count, fill = Sex)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    scale_fill_hpa("sex")

  # Check that the plot object is created without error
  expect_s3_class(p, "gg")
})


test_that("scale_fill_hpa throws an error with invalid palette", {
  expect_error(scale_fill_hpa("invalid_palette"), "Palette not found")
})
