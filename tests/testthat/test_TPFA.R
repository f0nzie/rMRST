library(testthat)

context("TPFA function")


# % TPFA function for global environment
# % function [P, V] = TPFA(Grid, K, q)
# % Compute transmissibilities by harmonic averaging.

Grid.Nx <- 8; Grid.Ny <- 8; Grid.Nz <- 1
N <- Grid.Nx * Grid.Ny * Grid.Nz
K <- ones(3, Grid.Nx, Grid.Ny)
q <- zeros(N, 1)
q[c(1, N)] <- c(1, -1)

grid <- Grid$new(8, 8, 1)
# print(grid)

test_that("Nx, Ny, Nz are assigned", {
    result <- TPFA01(grid, K, q)
    expected <- list(Nx = 8, Ny = 8, Nz = 1, hx = 0.125, hy = 0.125, hz = 1)
    expect_equal(result, expected)
})

test_that("K is 3x8x8 and L is raised to the power of -1", {
    # L = K.^(-1);
    expected <- ones(3, 8, 8)
    result <- TPFA02(grid, K, q)
    expect_equal(result[["L"]], expected)
    expect_equal(result[["K"]], expected)
})

test_that("TX, TY, TZ return right matrices", {
    # Matlab code
    # tx = 2*hy*hz/hx;
    # TX = zeros(Nx+1,Ny,Nz);
    # ty = 2*hx*hz/hy;
    # TY = zeros(Nx,Ny+1,Nz);
    # tz = 2*hx*hy/hz;
    # TZ = zeros(Nx,Ny,Nz+1);
    result <- TPFA03(grid, K, q)   # list

    expected_TX <- "
        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
     [1,]    0    0    0    0    0    0    0    0
     [2,]    1    1    1    1    1    1    1    1
     [3,]    1    1    1    1    1    1    1    1
     [4,]    1    1    1    1    1    1    1    1
     [5,]    1    1    1    1    1    1    1    1
     [6,]    1    1    1    1    1    1    1    1
     [7,]    1    1    1    1    1    1    1    1
     [8,]    1    1    1    1    1    1    1    1
     [9,]    0    0    0    0    0    0    0    0
    "
    expected_TY <- "
        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    [1,]    0    1    1    1    1    1    1    1    0
    [2,]    0    1    1    1    1    1    1    1    0
    [3,]    0    1    1    1    1    1    1    1    0
    [4,]    0    1    1    1    1    1    1    1    0
    [5,]    0    1    1    1    1    1    1    1    0
    [6,]    0    1    1    1    1    1    1    1    0
    [7,]    0    1    1    1    1    1    1    1    0
    [8,]    0    1    1    1    1    1    1    1    0
    "

    expected_TZ <- zeros(8, 8, 2)
    expected_TX <- array(simplify2array(read.table(header = TRUE,
                                             text = expected_TX)), dim=c(9,8,1))
    expected_TY <- array(simplify2array(read.table(header = TRUE,
                                                   text = expected_TY)), dim=c(8,9,1))

    expect_equal(result[["TX"]], expected_TX)
    expect_equal(result[["TY"]], expected_TY)
    expect_equal(result[["TZ"]], expected_TZ)

})


test_that("Assemble TPFA discretization matrix.", {
    result <- TPFA04(grid, K, q)   # list
    # print(result)
    expected_x1 <- array(simplify2array(read.table(header = TRUE,
                                                   file = "x1.txt")), dim=c(64,1))
    expected_y1 <- array(simplify2array(read.table(header = TRUE,
                                                   file = "y1.txt")), dim=c(64,1))
    expected_z1 <- array(simplify2array(read.table(header = TRUE,
                                                   file = "z1.txt")), dim=c(64,1))
    expect_equal(result[["x1"]], expected_x1)
    expect_equal(result[["y1"]], expected_y1)
    expect_equal(result[["z1"]], expected_z1)
    # print(result[["x2"]])
})

test_that("DiagsVecs work", {
    result <- TPFA05(grid, K, q)   # list
    expected_DiagVecs <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "DiagVecs.txt")), dim=c(64,7))
    expected_DiagIndx <- c( -64,    -8,    -1,     0,     1,     8,    64)

    # print(dim(result[["DiagVecs"]]))
    # print(expected_DiagVecs)
    expect_equal(result[["DiagVecs"]], expected_DiagVecs)
    expect_equal(result[["DiagIndx"]], expected_DiagIndx)
})

test_that("sparse matrix A returns correct A1 values", {
    result <- TPFA06(grid, K, q)
    # print(result)
    # print(dim(result))    # 64x64 sparse matrix
    # print(typeof(result)) # double
    expected_A <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "A1.txt")), dim=c(64,64))
    # print(dim(expected_A))     # 64x64 sparse matrix
    # print(typeof(expected_A))  # integer
    expect_equal(expected_A, result)
})

test_that("A transformed match A2 values", {
    result <- TPFA07(grid, K, q)
    expected_A <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                  file = "A2.txt")), dim=c(64,64))
    # print(dim(expected_A))     # 64x64 sparse matrix
    # print(typeof(expected_A))  # integer
    expect_equal(expected_A, result)
})

test_that("solving (A, q) yields result", {
    result <- TPFA08(grid, K, q)
    # print(result)
    expected_u <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                  file = "u.txt")), dim=c(64,1))
    # print(expected_u)
    expect_equal(result, expected_u, tolerance = 1e-4)
})

test_that("Pressure and flux return good values", {
    result <- TPFA09(grid, K, q)
    # print(dim(result[["P"]]))
    expected_P <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                  file = "P.txt")), dim=c(8, 8, 1))
    # print(dim(expected_P))
    expect_equal(result[["P"]], expected_P, tolerance = 1e-4)
    # print(result[["V"]])
    V <- result[["V"]]
    # print(dim(V$z));    print(V$z)
    expect_equal(V$x, zeros(9, 8, 1))
    expect_equal(V$y, zeros(8, 9, 1))
    expect_equal(V$z, zeros(8, 8, 2))
})

test_that("V return good values after transformation", {
    result <- TPFA10(grid, K, q)
    # print(result)
    # print(dim(result))
    # V <- result[["Grid"]]
    # Vx <- result$x
    V <- result
    expected_Vx <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                  file = "Vx.txt")), dim=c(9, 8, 1))
    expected_Vy <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "Vy.txt")), dim=c(8, 9, 1))
    expected_Vz <- zeros(8, 8, 2)

    expect_equal(expected_Vx, V$x, tolerance = 1e-5)
    expect_equal(expected_Vy, V$y, tolerance = 1e-5)
    expect_equal(expected_Vz, V$z)
})

test_that("final Pressure returns correct values", {
    result <- TPFA11(grid, K, q)
    P <- result
    # print(P)
    expected_P <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                          file = "Pfinal.txt")), dim=c(8, 8, 1))
    # print(expected_P)
    expect_equal(expected_P, P, tolerance = 1e-4)
})


test_that("P, V return correct values", {
    result <- TPFA(grid, K, q)
    # print(result)
    # print(names(result))
    expected_P <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                  file = "Pfinal.txt")), dim=c(8, 8, 1))
    Vx <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "Vx.txt")), dim=c(9, 8, 1))
    Vy <- array(simplify2array(read.table(header = FALSE, sep = ",",
                                                   file = "Vy.txt")), dim=c(8, 9, 1))
    Vz <- zeros(8, 8, 2)

    V   <- Var3d()
    V$x <- Vx; V$y <- Vy; V$z <- Vz
    expected_V <- V

    expect_equal(expected_P, result$P, tolerance = 1e-4)
    expect_equal(expected_V, result$V, tolerance = 1e-5)
})
