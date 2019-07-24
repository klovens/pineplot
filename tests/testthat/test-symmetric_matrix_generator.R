context("Testing symmetric matrices")

test_that("symmetric_matrix_generator is creating symmetric matrices", {
  m <- matrix(runif(10 * 3), 3, 10)
  r <- symmetric_matrix_generator(m, 1)
  expect_equal(r, t(r))
  m <- matrix(runif(100), 1, 100)
  r <- symmetric_matrix_generator(m, 1)
  expect_equal(r, t(r))
  m <- matrix(runif(100), 100, 1)
  r <- symmetric_matrix_generator(m, 1)
  expect_equal(r, t(r))
})

test_that("Check if symmetric matrices converted to triangular matrices", {

})

test_that("Non symmetric matrix causes message", {

})
