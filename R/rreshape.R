
#' Reshapes an array to a new n-dimensional array. Like Matlab
#' @param x array
#' @param ... additional parameters
#' @export
rreshape <- function(x, ...) {
    stopifnot(is.array(x))
    args <- list(...)
    if (length(args)) stopifnot(sapply(args,  is.numeric))
    array(x, dim = c(...))
}
