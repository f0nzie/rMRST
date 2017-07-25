#' @export
#' @importFrom RcppOctave .CallOctave
m.zeros <- function(x, ...) {
    .CallOctave("zeros", x, ...)
}

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


# zeros <- function(x, y, z=NULL) {
#     if (is.null(z)) array(0, dim=c(x, y))
#     if (missing(y)) array(0, dim=c(x, z))
#     if (missing(x)) array(0, dim=c(y, z))
#     array(0, dim=c(x, y, z))
# }
