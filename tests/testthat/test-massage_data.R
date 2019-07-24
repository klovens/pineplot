context("Massage data")

test_that("massage_data only accepts symmetric matrices", {
  m <- matrix(rnorm(9), 3, 3)
  m[1, 1] <- 0
  m[3, 3] <- 1
  expect_error(massage_data(m, TRUE, "value"))
})

test_that("massage_data has the correct shape.", {
  m <- matrix(rnorm(9), 3, 3)
  m <- m + t(m)
  r <- massage_data(m, TRUE, "value")
  expect_equal(nrow(r), dim(m)[1] * dim(m)[2])
})

test_that("massage_data has the correct values", {
  m <- matrix(rnorm(9), 3, 3)
  m <- m + t(m)
  r <- massage_data(m, TRUE, "value")
  print(m)
  print(r)
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (j < i) {
        expect_true(is.na(r[r$Var1 == i & r$Var2 == j, "value"]))
      } else {
        expect_equal(r[r$Var1 == i & r$Var2 == j, "value"], m[i, j])
      }
    }
  }
})
