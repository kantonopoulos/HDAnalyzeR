# Test hd_palettes -------------------------------------------------------------
test_that("hd_palettes returns a list of palettes", {
  palettes <- hd_palettes()

  # Check that the result is a list
  expect_true(is.list(palettes))

  # Check that specific palettes are in the list
  expect_true("sex" %in% names(palettes))
  expect_true("diff_exp" %in% names(palettes))
  expect_true("secreted" %in% names(palettes))
  expect_true("specificity" %in% names(palettes))

  # Check that the "sex" palette has the correct colors
  expect_equal(palettes$sex, c("F" = "#8a72be", "M" = "#A9D0EF"))
})


# Test scale_color_hpa ---------------------------------------------------------
test_that("scale_color_hd works with valid palettes", {
  # Create a sample data frame
  data <- data.frame(
    var1 = 1:10,
    var2 = seq(2, 20, by = 2),
    Sex = rep(c("Male", "Female"), each = 5)
  )

  # Generate a plot using the "sex" palette
  p <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, color = Sex)) +
    ggplot2::geom_point() +
    scale_color_hd("sex")

  # Check that the plot object is created without error
  expect_s3_class(p, "gg")
})


# Test scale_fill_hd -----------------------------------------------------------
test_that("scale_fill_hd works with valid palettes", {
  # Create a sample data frame
  data <- data.frame(
    Sex = c("Male", "Female"),
    Count = c(60, 40)
  )

  # Generate a plot using the "sex" palette
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Sex, y = Count, fill = Sex)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    scale_fill_hd("sex")

  # Check that the plot object is created without error
  expect_s3_class(p, "gg")
})
