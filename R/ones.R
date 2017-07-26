#' Fills an array with ones (Octave)
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
    } else {
        array(1, dim=x)
    }
}
