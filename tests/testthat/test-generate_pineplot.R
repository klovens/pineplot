context("Generate pineplot")

test_that("pineplots with different heights are generated", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  pdf('../figs/generate_pineplot-1.pdf', height=4)
  generate_pineplot(ms, height=4)
  dev.off()
})

test_that("pineplot is generated with height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  pdf('../figs/generate_pineplot-2.pdf', height=8)
  generate_pineplot(ms, height=8)
  dev.off()
})

test_that("pineplot is generated with tiny height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  pdf('../figs/generate_pineplot-3.pdf', height=12)
  generate_pineplot(ms, height=12)
  dev.off()
})
