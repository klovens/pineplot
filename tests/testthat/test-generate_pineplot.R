context("Generate pineplot")

test_that("pineplots with different heights are generated", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  generate_pineplot(ms, height=4, filename='../figs/generate_pineplot.pdf')
})

test_that("pineplot is generated with height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  generate_pineplot(ms, height=8, filename='../figs/generate_pineplot-2.pdf')
})

test_that("pineplot is generated with tiny height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  generate_pineplot(ms, height=12, filename='../figs/generate_pineplot-3.pdf')
})
