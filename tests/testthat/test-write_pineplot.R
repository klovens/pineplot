context("Write pineplot")

test_that("pineplot is drawn", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  plts <- lapply(ms, draw_heatmap)
  pdf("../figs/pineplot.pdf")
  write_pine_plot(plts, leg=TRUE)
  dev.off()
})
