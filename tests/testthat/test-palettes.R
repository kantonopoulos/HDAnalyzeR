test_that("hd_palettes() returns named colour vectors", {
  pals <- hd_palettes()

  expect_type(pals, "list")
  expect_gt(length(pals), 0)
  expect_true(all(nzchar(names(pals))))

  for (name in names(pals)) {
    expect_type(pals[[name]], "character")
    expect_true(all(nzchar(names(pals[[name]]))), info = name)
  }
})

test_that("every palette colour is a valid colour specification", {
  for (name in names(hd_palettes())) {
    expect_no_error(grDevices::col2rgb(hd_palettes()[[name]]))
  }
})

test_that("palette entries are unique within a palette", {
  for (name in names(hd_palettes())) {
    expect_false(anyDuplicated(names(hd_palettes()[[name]])) > 0, info = name)
  }
})

test_that("the documented palettes are present", {
  expect_true(all(
    c("sex", "diff_exp", "cancers12", "secreted", "class") %in% names(hd_palettes())
  ))
  expect_equal(
    hd_palettes()$diff_exp[["not significant"]],
    "grey"
  )
})

test_that("hd_show_palettes() returns a renderable plot", {
  expect_renderable_ggplot(hd_show_palettes())
})

test_that("hd_show_palettes() can limit the number of colours shown", {
  built <- ggplot2::ggplot_build(hd_show_palettes(n = 2))
  # at most two tiles per palette
  expect_lte(max(built$data[[1]]$x), 2)
})


# scale_color_hd / scale_fill_hd ---------------------------------------------

test_that("scale_color_hd() and scale_fill_hd() build usable scales", {
  expect_s3_class(scale_color_hd("sex"), "ScaleDiscrete")
  expect_s3_class(scale_fill_hd("sex"), "ScaleDiscrete")
})

test_that("the scales use the palette colours", {
  scale <- scale_color_hd("sex")
  expect_equal(unname(scale$palette(2)), unname(hd_palettes()$sex[1:2]))
})

test_that("the scales reject unknown palettes", {
  expect_error(scale_color_hd("nope"), "Palette not found")
  expect_error(scale_fill_hd("nope"), "Palette not found")
})


# apply_palette ---------------------------------------------------------------

test_that("apply_palette() leaves the plot alone when no palette is given", {
  p <- ggplot2::ggplot(tiny_meta(), ggplot2::aes(x = .data$Sex)) + ggplot2::geom_bar()
  expect_equal(length(apply_palette(p, NULL)$scales$scales), length(p$scales$scales))
})

test_that("apply_palette() accepts a named custom palette", {
  p <- ggplot2::ggplot(tiny_meta(), ggplot2::aes(x = .data$Sex, fill = .data$Sex)) +
    ggplot2::geom_bar()
  res <- apply_palette(p, c(F = "red", M = "blue"), type = "fill")

  expect_renderable_ggplot(res)
})

test_that("apply_palette() rejects an unknown named palette", {
  p <- ggplot2::ggplot(tiny_meta(), ggplot2::aes(x = .data$Sex)) + ggplot2::geom_bar()
  expect_error(apply_palette(p, "nope"), "not valid")
  expect_error(apply_palette(p, "nope", type = "fill"), "not valid")
})


# theme_hd --------------------------------------------------------------------

test_that("theme_hd() returns a ggplot2 theme that can be added to a plot", {
  expect_s3_class(theme_hd(), "theme")

  p <- ggplot2::ggplot(tiny_meta(), ggplot2::aes(x = .data$Sex)) +
    ggplot2::geom_bar() +
    theme_hd()
  expect_renderable_ggplot(p)
})

test_that("theme_hd() angles the x-axis text when asked", {
  expect_equal(theme_hd(angled = 90)$axis.text.x$angle, 90)
})

test_that("theme_hd() can drop the axes and the facet title", {
  expect_s3_class(theme_hd(axis_x = FALSE)$axis.text.x, "element_blank")
  expect_s3_class(theme_hd(axis_y = FALSE)$axis.text.y, "element_blank")
  expect_s3_class(theme_hd(facet_title = FALSE)$strip.text, "element_blank")
})
