library(testthat)

context("test spdiags")

test_that("Example 1: test that we get nonzero diagonals and indices", {

    A = rbind(c(0, 5, 0, 10, 0, 0),
            c(0, 0, 6, 0, 11, 0),
            c(3, 0, 0, 7, 0, 12),
            c(1, 4, 0, 0, 8, 0),
            c(0, 2, 5, 0, 0, 9)
    )

    xB <- m.spdiags(A)$B
    xd <- m.spdiags(A)$d

    load(file = "spdiags.rda")
    # save(B, d, file = "spdiags.rda")
    expect_equal(B, xB)     # nonzero diagonals
    expect_equal(d, xd)     # indices
    # print(xB)
    # print(xd)

})




test_that("Example 3: match a triadiagonal", {

    A3 =rbind(
        c(11,    0,   13,    0),
         c(0,   22,    0,   24),
         c(0,    0,   33,    0),
         c(41,   0,    0,   44),
         c(0,   52,    0,    0),
         c(0,    0,   63,    0),
         c(0,    0,    0,   74)
         )
    xB3 <- m.spdiags(A3)$B
    xd3 <- m.spdiags(A3)$d

    load(file = "spdiags.rda" )
    expect_equal(xB3, B3)     # nonzero diagonals
    #       [,1] [,2] [,3]
    # [1,]   41   11    0
    # [2,]   52   22    0
    # [3,]   63   33   13
    # [4,]   74   44   24
    expect_equal(xd3, d3)     # indices
    #       [,1]
    # [1,]   -3
    # [2,]    0
    # [3,]    2

     # print(B3)
     # print(d3)
    # Conversely, with the above B and d, the expression spdiags(B,d,7,4)
    # reproduces the original A.
    xC3 <- m.spdiags(xB3, d3, 7, 4)
    expect_equal(xC3, C3)     # original A
    # print(C3$B)

    # resave(B3, d3, file = "spdiags.rda")
    # resave(C3, file = "spdiags.rda")
})


test_that("Example 4: longer than the diagonal they are replacing", {
    B4 <- pracma::repmat(matrix(1:6), 1, 7)

    .d4 <- c(-4, -2, -1, 0, 3, 4, 5)
    xA4 <- m.spdiags(B4,.d4,6,6)$B
    xd4 <- m.spdiags(B4,.d4,6,6)$d

    load(file = "spdiags.rda" )
    expect_equal(xA4, A4)     # nonzero diagonals
    expect_equal(xd4, d4)     # indices
    # print(A4)
    # resave(A4, d4, file = "spdiags.rda")

})



