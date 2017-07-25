#' Extract and create sparse band and diagonal matrices (Octave)
#' @param ... additional parameters
#' @export
#' @importFrom RcppOctave .CallOctave
m.spdiags <- function(...) {
    .CallOctave("spdiags", ...)
}
