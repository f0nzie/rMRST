library(testthat)

context("test smooth3 as in Matlab")

test_that("array of K 3x32x32 gives correct padded array of 5x34x34", {
    load(file = "padreplicate_3x32x32.rda")
    sample <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                              file = "K_3x32x32.txt")), dim=c(3,32,32))
    # print(dim(sample))
    #print(Smooth3(sample))
    result <- Smooth3(sample)
    # expected <- result; save(expected, file = "padreplicate_3x32x32.rda")
    print(dim(result))
    # expect_equal(expected, result)
    # expect_equal(c(5,34,34), dim(result))
})





context("test padreplicate")

test_that("array of K 3x32x32 gives correct padded array of 5x34x34", {
    load(file = "padreplicate_3x32x32.rda")
    sample <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                file = "K_3x32x32.txt")), dim=c(3,32,32))
    # print(dim(sample))
    #print(Smooth3(sample))
    result <- padreplicate(sample, c(1, 1, 1))
    # expected <- result; save(expected, file = "padreplicate_3x32x32.rda")
    # print(dim(result))
    expect_equal(expected, result)
    expect_equal(c(5,34,34), dim(result))
})


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

test_that("padreplicate of an array 4x3 pads to 5x6 with c(1,1,1)", {
    load(file = "padreplicate_3x3x1.rda")    # expected values
    set.seed(123456)
    ar = array(sample.int(9, size = 9*100, replace = TRUE), dim = c(3, 3, 1))
    result <- padreplicate(ar, c(1, 1, 1))
    # expected <- result; save(expected, file = "padreplicate_3x3x1.rda")
    expect_equal(expected, result)
    expect_equal(c(5,5,3), dim(result))

})




