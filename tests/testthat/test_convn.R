library(testthat)

context("test convolution")

test_that("matrix 3x3 of ones gives desired result", {
    A <- ones(3)
    result <- m.convn(A, A)
    expected <- rbind(
        c(1,    2,    3,    2,    1),
        c(2,    4,    6,    4,    2),
        c(3,    6,    9,    6,    3),
        c(2,    4,    6,    4,    2),
        c(1,    2,    3,    2,    1)
    )
    expect_equal(expected, result)
})




test_that("matrix of ones is converted to array", {
    X = ones(3)     # doesn't work because it is a matrix
    result <- m.conv3d(X, X)
    expected <- rbind(
        c(1,    2,    3,    2,    1),
        c(2,    4,    6,    4,    2),
        c(3,    6,    9,    6,    3),
        c(2,    4,    6,    4,    2),
        c(1,    2,    3,    2,    1) )

    expect_equal(result, expected)
})



test_that("array of ones 5x4x3 gives correct result", {
    C = ones(5, 4, 3)
    # print(C)
    result <- m.conv3d(C, C)
    expected <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "convn_5x4x3.txt")), dim=c(9,7,5))
    # print(expected)
    # print(result)
    expect_equal(expected, result)


})

test_that("array of ones 4x3x2 gives correct result", {
    Y = ones(4, 3, 2)
    result <- m.conv3d(Y, Y)
    expected <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                file = "convn_4x3x2.txt")), dim=c(7,5,3))
    expect_equal(expected, result)
})


test_that("different size arrays work", {
    # >> convn(A, B)
    #
    # ans(:,:,1) =
    #
    # 1     2     3     2     1
    # 2     4     6     4     2
    # 3     6     9     6     3
    # 3     6     9     6     3
    # 2     4     6     4     2
    # 1     2     3     2     1
    #
    #
    # ans(:,:,2) =
    #
    # 1     2     3     2     1
    # 2     4     6     4     2
    # 3     6     9     6     3
    # 3     6     9     6     3
    # 2     4     6     4     2
    # 1     2     3     2     1

    # A = ones(3,3,1)
    # B = ones(4,3,2)
    # C <- m.conv3d(A, B)
    # print(C)
})
