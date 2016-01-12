context("Kantorovich distance")

test_that("Main example - numeric mode", {
  # unnamed mu and nu
  mu <- c(1/7, 2/7, 4/7)
  nu <- c(1/4, 1/4, 1/2)
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.107142857142857)
  # named mu and nu - same names
  mu <- setNames(mu, c("a","b","c"))
  nu <- setNames(nu, c("a","b","c"))
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.107142857142857)
  # named mu and nu - different names
  mu <- setNames(mu, c("a","b","c"))
  nu <- c(c=1/2, a=1/4, b=1/4)
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.107142857142857)
})

test_that("Main example - bigq mode", {
    # unnamed mu and nu
    mu <- as.bigq(c(1,2,4), 7)
    nu <- as.bigq(c(1,1,1), c(4,4,2))
    x <- kantorovich(mu, nu)
    expect_true(x==as.bigq(3,28))
    # named mu and nu - same names
    mu <- setNames(as.bigq(c(1,2,4), 7), c("a","b","c"))
    nu <- setNames(as.bigq(c(1,1,1), c(4,4,2)), c("a","b","c"))
    x <- kantorovich(mu, nu)
    expect_true(x==as.bigq(3,28))
    # named mu and nu - different names
    mu <- setNames(as.bigq(c(1,2,4), 7), c("a","b","c"))
    nu <- setNames(as.bigq(c(1,1,1), c(2,4,4)), c("c","a","b"))
    x <- kantorovich(mu, nu)
    expect_true(x==as.bigq(3,28))
})

test_that("Non-square example - numeric mode", {
  # unnamed mu and nu
  mu <- c(2/5,3/5)
  nu <- c(1/4,1/4,1/4,1/4)
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.5)
  # named mu and nu - same names
  mu <- setNames(mu, c("a","b"))
  nu <- setNames(nu, c("a","b","c","d"))
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.5)
  # named mu and nu - different names
  mu <- setNames(mu, c("b","a"))
  x <- kantorovich(mu, nu)
  expect_equal(x, 0.5)
})

test_that("Non-square example - bigq mode", {
    # unnamed mu and nu
    mu <- as.bigq(c(2/5,3/5))
    nu <- as.bigq(c(1/4,1/4,1/4,1/4))
    x <- kantorovich(mu, nu)
    expect_identical(x, as.bigq(0.5))
    # named mu and nu - same names
    mu <- setNames(mu, c("a","b"))
    nu <- setNames(nu, c("a","b","c","d"))
    x <- kantorovich(mu, nu)
    expect_identical(x, as.bigq(0.5))
    # named mu and nu - different names
    mu <- setNames(mu, c("b","a"))
    x <- kantorovich(mu, nu)
    expect_identical(x, as.bigq(0.5))
})
