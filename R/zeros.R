#' Fills an array with zeros (Octave)
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
#' @importFrom RcppOctave .CallOctave
m.zeros <- function(x, ...) {
    .CallOctave("zeros", x, ...)
}

#' Fills an array with zeros
#' @param x 1-D dimension
#' @param ... additional parameters
#' @export
zeros <- function(x, ...) {
    stopifnot(is.numeric(x))
    args <- list(...)
    if (length(args)) {
        stopifnot(is.numeric(args[[1]]), is.numeric(args[[2]]))
        array(0, dim=c(x, ...))
    } else {
        array(0, dim=x)
    }
}


