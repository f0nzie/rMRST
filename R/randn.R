#' random normal array
#' @param ... additional parameters. size of the array
#' @param mean as in rnorm
#' @param sd as in rnorm
#' @export
#' @importFrom stats rnorm
randn <- function(..., mean = 0, sd = 1) {
    args <- list(...)
    # print(length(args))
    if (length(args)) {
        stopifnot(sapply(args, is.numeric))
    }
    print(args)
    if (length(args) == 1) {
        array(rnorm(args[[1]], mean, sd), dim = c(args[[1]]))
    } else if (length(args) == 2) {
        array(rnorm(args[[1]]*args[[1]], mean, sd), dim = c(args[[1]], args[[2]]))
    } else if (length(args) == 3) {
        array(rnorm(args[[1]]*args[[2]]*args[[3]], mean, sd),
              dim = c(args[[1]], args[[2]], args[[3]]))
    }
    else stop("above 3D not implemented")
}
