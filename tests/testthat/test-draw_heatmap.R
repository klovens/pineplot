context("Draw heatmap")

test_that("heatmap is drawn", {
	# check tests/figs directory to validate output
  m <- outer(-3:3, -3:3, "+")
	plt <- draw_heatmap(m)
	ggsave("../figs/heatmap.pdf", plot=plt)
})
