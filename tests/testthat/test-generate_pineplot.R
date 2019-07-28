context("Generate pineplot")

test_that("pineplots with different heights are generated", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  ggsave('../figs/generate_pineplot-1.pdf', generate_pineplot(ms), height=4)
})

test_that("pineplot is generated with height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  ggsave('../figs/generate_pineplot-2.pdf', generate_pineplot(ms), height=8)
})

test_that("pineplot is generated with tiny height option specified", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  ggsave('../figs/generate_pineplot-3.pdf', generate_pineplot(ms), height=12)
})

test_that("pineplot accomodates labels", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  ms <- lapply(ms, `rownames<-`, rep('a very long label', 7))
  ms <- lapply(ms, `colnames<-`, rep('a very long label', 7))
  ggsave('../figs/generate_pineplot-4.pdf', generate_pineplot(ms), height=12)
})
