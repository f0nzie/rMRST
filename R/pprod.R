#' Product of elements as in Matlab
#' @param A array or matrix
#' @export
pprod <- function(A) {
    if (is.matrix(A))
        return (apply(A, 2, prod))
    if (is.array(A))
        array(apply(A, 2, prod), dim = c(1, dim(A)[2], dim(A)[3]))

}
