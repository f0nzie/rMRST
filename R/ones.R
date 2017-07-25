#' Fills an array with ones (Octave)
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
#' @importFrom RcppOctave .CallOctave
m.ones <- function(x, ...) {
    .CallOctave("ones", x, ...)
}

#' @export
#' @importFrom RcppOctave .CallOctave
m.zeros <- function(x, ...) {
    .CallOctave("zeros", x, ...)
}

#' Fills an array with ones
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
ones <- function(x, ...) {
    stopifnot(is.numeric(x))
    args <- list(...)
    if (length(args)) {
        stopifnot(is.numeric(args[[1]]), is.numeric(args[[2]]))
        array(1, dim=c(x, ...))
    } else {
        array(1, dim=x)
    }
}
