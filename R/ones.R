#' Fills an array with ones as in Matlab
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
#' @importFrom RcppOctave .CallOctave
m.ones <- function(x, ...) {
    .CallOctave("ones", x, ...)
}


#' Fills an array with ones
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
ones <- function(x, ...) {
    stopifnot(is.numeric(x))
    args <- list(...)
    if (length(args)) {
        stopifnot(sapply(args,  is.numeric))
        array(1, dim=c(x, ...))
    } else if (length(x) > 1) {
        # a vector supplied for dimensions
        array(1, dim=c(x))
    } else {
        # two dimensions given
        array(1, dim=c(x, x))
    }
}
