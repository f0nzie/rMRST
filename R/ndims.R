
#' Number of dimensions of the object. As in Matlab
#' @export
ndims <- function(X) {
    if (!is.numeric(X))
        stop("Argument 'n' must be a numeric value.")
    if (is.vector(X))
        X <- matrix(X)
    length(dim(X))
}
