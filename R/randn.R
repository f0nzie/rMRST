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
        stopifnot(sapply(args,  is.numeric))
    }
    if (length(args) == 1) array(rnorm(2), dim = c(args[[1]], args[[1]]))
    else array(rnorm(2), dim = c(...))
}
