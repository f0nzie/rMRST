library(testthat)
# library(RcppOctave)

context("Matlab example_01")


Grid.Nx <- 8; Grid.Ny <- 8; Grid.Nz <- 1
N <- Grid.Nx * Grid.Ny * Grid.Nz

test_that("Grid dims match", {
    # N = Grid.Nx*Grid.Ny*Grid.Nz;
    expect_equal(c(8, 8, 1), c(Grid.Nx, Grid.Ny, Grid.Nz))
})


test_that("flow rate is a matrix of zeros", {
    # q = zeros(N,1);
    q <- .CallOctave("zeros", N, 1)
    expect_equal(q, zeros(N, 1))
})

test_that("permeability K is a matrix of ones", {
    # Grid.K = ones(3, Grid.Nx, Grid.Ny);
    K <- .CallOctave("ones", 3, Grid.Nx, Grid.Ny)
    expect_equal(K, ones(3, Grid.Nx, Grid.Ny))
})


test_that("flow rate assignment on the ends works", {
    # Matlab code
    # q([1 N])=[1 -1];
    RcppOctave::o_source(text="
      function [res] = test(N)
        q = zeros(N,1);
        res = q([1 N])=[1 -1];
      end
    ")
    expected <- RcppOctave::.O$test(N)

    # R code
    q <- zeros(N, 1)
    result <- q[c(1, N)] <- c(1, -1)
    expect_equal(expected, result)
})
