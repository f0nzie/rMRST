library(testthat)

context("test smooth3 as in Matlab")

#test_that("array of ones 5x4x3 gives correct result", {
    sample <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                file = "K_3x32x32.txt")), dim=c(3,32,32))
    # print(expected)
    # print(result)
    # expect_equal(expected, result)
    # print(dim(sample))
    #print(Smooth3(sample))

#})


context("test padreplicate")

test_that("padreplicate of a matrix 3x4 pads to 5x6 with c(1, 1)", {
    expected <- rbind(
        c(8,    8,    4,    5,    2,    2),
        c(8,    8,    4,    5,    2,    2),
        c(7,    7,    4,    1,    8,    8),
        c(4,    4,    2,    9,    6,    6),
        c(4,    4,    2,    9,    6,    6)
    )

    set.seed(123456)
    mx = matrix(sample.int(9, size = 9*100, replace = TRUE), nrow = 3, ncol = 4)
    result <- padreplicate(mx, c(1, 1))
    expect_equal(expected, result)
    expect_equal(c(5,6), dim(result))
})


test_that("padreplicate of a matrix 4x3 pads to 5x6 with c(1, 1)", {
    expected <- rbind(
        c(8,    8,    4,    9,    9),
        c(8,    8,    4,    9,    9),
        c(7,    7,    2,    2,    2),
        c(4,    4,    5,    8,    8),
        c(4,    4,    1,    6,    6),
        c(4,    4,    1,    6,    6)
    )

    set.seed(123456)
    mx = matrix(sample.int(9, size = 9*100, replace = TRUE), nrow = 4, ncol = 3)
    result <- padreplicate(mx, c(1, 1))
    expect_equal(expected, result)
    expect_equal(c(6,5), dim(result))
})


