library(testthat)

context("test ndims as in Matlab")

test_that("# dimensions of a scalar is 2", {
    v1 <- 4
    expect_equal(ndims(v1), 2)
})


test_that("# dimensions of a vector is 2", {
    v2 <- c(1, 2, 3, 4)
    expect_equal(ndims(v2), 2)
})


test_that("# dimensions of a matrix is 2", {
    m1 <- matrix(2, nrow=5, ncol=4)
    expect_equal(ndims(m1), 2)
})

test_that("# dimensions of 2x3x4 array is 3", {
    r1 <- array(2, dim = c(2, 3, 4))
    expect_equal(ndims(r1), 3)
})


test_that("# dimensions of 2x3x4x7 array is 4", {
    r4 <- array(2, dim = c(2, 3, 4, 7))
    expect_equal(ndims(r4), 4)
})
