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

test_that("pineplot is generated without legend", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  ggsave('../figs/generate_pineplot-5.pdf', generate_pineplot(ms, legend.show=FALSE), height=12)
})

test_that("pineplot is generated and legend is returned with the pineplot object", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  pp1 <- generate_pineplot(ms, legend.show=F, legend.scale=0.)
  pp2 <- generate_pineplot(ms, legend.show=F, legend.scale=0.)
  pdf('../figs/generate_pineplot-6.pdf')
  gridExtra::grid.arrange(pp1, pp2, pp1$legend_grob, layout_matrix=rbind(c(1, 2), c(3, 3)), heights=c(5, 1))
  dev.off()
})

test_that("pineplot is generated and custom legend is returned with the pineplot object", {
  # check tests/figs directory to validate output
  ms <- replicate(4, outer(-3:3, -3:3, "+"), simplify = FALSE)
  pp1 <- generate_pineplot(ms, legend.show=F, legend.scale=0.,
                           limits=c(-10., 10.),
                           midpoint=0.)
  pp2 <- generate_pineplot(ms, legend.show=F, legend.scale=0.,
                           limits=c(-10., 10.),
                           midpoint=0.,
                           breaks=c(-10., -5., 0., 5., 10.),
                           legend.args=list(barwidth=unit(6, 'in')))
  pdf('../figs/generate_pineplot-7.pdf')
  gridExtra::grid.arrange(pp1,
                          pp2,
                          pp2$legend_grob,
                          layout_matrix=rbind(c(1, 2),
                                              c(3, 3)), # for the legend
                          heights=c(5, 1))
  dev.off()
})
