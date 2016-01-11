context("Extreme distances")

test_that("Main example", {
  mu <- c(1/7,2/7,4/7)
  nu <- c(1/4,1/4,1/2)
  x <- edistances(mu, nu)
  expect_true(length(x[[1]])==15 && length(x[[2]])==15)
  expect_equal(x$joinings[[1]], structure(c(0.142857142857143, 0, 0.107142857142857, 0, 0, 0.25,
                                        0, 0.285714285714286, 0.214285714285714), .Dim = c(3L, 3L), .Dimnames = list(
                                          c("1", "2", "3"), c("1", "2", "3"))))
  expect_equal(x$distances[[1]], 0.642857142857143)
  #
  if(requireNamespace("gmp", quietly = TRUE)){
    mu <- as.bigq(c(1,2,4),7)
    nu <- as.bigq(c(1,1,1),c(4,4,2))
    x <- edistances(mu, nu)
    expect_true(length(x[[1]])==15 && length(x[[2]])==15)
    expect_equal(x$joinings[[1]], structure(c("1/7", "0", "3/28", "0", "0", "1/4", "0", "2/7",
                                          "3/14"), .Dim = c(3L, 3L), .Dimnames = list(c("1", "2", "3"),
                                                                                      c("1", "2", "3"))))
    expect_equal(x$distances[[1]], as.bigq(9,14))
  }
})
