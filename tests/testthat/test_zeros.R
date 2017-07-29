library(testthat)

context("test zeros")

# q = zeros(N, 1);

test_that("zeros match 1D", {
    N <- 5
    expected <- array(0, c(N, N))
    expect_equal(zeros(N), expected)
})

test_that("zeros match 2D", {
    N <- 5
    expected <- array(0, c(N, 10))
    expect_equal(zeros(N, 10), expected)
})

test_that("zeros match 3D", {
    N <- 5
    expected <- array(0, c(N, 10, 7))
    expect_equal(zeros(N, 10, 7), expected)
})
