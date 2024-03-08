context("ompr")

test_that("kantorovich_ompr default distance", {
  mu <- c(1/7,2/7,4/7)
  nu <- c(1/4,1/4,1/2)
  suppressMessages(x <- kantorovich_ompr(mu, nu, dist = 1 - diag(3L)))
  expect_equal(x, 0.107142857142857)
})

test_that("kantorovich_ompr - specified distance", {
  mu <- c(1/4, 3/4, 0, 0)
  nu <- c(0, 1/2, 1/2, 0)
  dist <- structure(c(0, 1/3, 2/3, 1, 1/3, 0, 1/3, 2/3,
                      2/3, 1/3, 0, 1/3, 1, 2/3, 1/3, 0), .Dim = c(4L, 4L))
  # doit trouver 1/4 et deux solutions
  x <- suppressMessages(kantorovich_ompr(mu, nu, dist=dist))
  expect_equal(x, 1/4)
})

test_that("kantorovich_ompr - nonsymmetric dist", {
  mu <- c(1,2,4) / 7
  nu <- c(3,1,5) / 9
  D <- matrix(
    c(
      c(0, 1, 3),
      c(1, 0, 4),
      c(2, 4, 0)
    ),
    byrow = TRUE, nrow = 3L)
  x <- suppressMessages(kantorovich_ompr(mu, nu, dist = D))
  expect_equal(x, 13/63)
})
