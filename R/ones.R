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
