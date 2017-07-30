#' @export
m.convn <- function(x, y) {
    # stopifnot(is.matrix(x), is.matrix(y))
    if (is.matrix(x) & is.matrix(y)) return(.CallOctave("convn", x, y))
    # if (is.array(x) & is.array(y)) {
    #     sapply(c(x,y), function(x) function(y) .CallOctave("convn", x, y))

    # }
    .CallOctave("convn", x, y)
}



#' @export
#' @importFrom RcppOctave .O
m.conv3d <- function(A, B) {
    if (is.matrix(A) & is.matrix(B)) {
        A <- array(A, dim = c(dim(A), 1))
        B <- array(B, dim = c(dim(B), 1))
    }

    if (any(dim(A) < dim(B))) {
        # cat("array B bigger than A")
        temp <- A
        A <- B
        B <- temp
    }

    D <- array(0, dim = c(2*dim(A)[1]-1, 2*dim(A)[2]-1, 2*dim(A)[3]-1))

    for (i in 1:dim(A)[3]) {  # 1, 2, 3
        D[,,i] <- i *.O$convn(A[,,i], B[,,i])
    }

    if (dim(A)[3] > 1) {
        for (j in (dim(A)[3]-1):1) {   # 2, 1
            D[,,i+(dim(A)[3]-j)] <- j *.O$convn(A[,,j], B[,,j])
        }
    } else {
        D <- matrix(D, nrow=2*dim(A)[1]-1, ncol=2*dim(A)[2]-1)
    }
    D
}



